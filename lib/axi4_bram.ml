open Core
open Hardcaml
open Hardcaml_xilinx
open Signal

module Make
    (M : sig
       val capacity_in_bytes : int
       val synthetic_pushback : int
     end)
    (Axi : Axi4.S) =
struct
  let () =
    if Axi.data_width % 8 <> 0 then raise_s [%message "BUG: data bus must be in bytes"]
  ;;

  let data_width_bytes = Axi.data_width / 8

  let () =
    if M.capacity_in_bytes % data_width_bytes <> 0
    then
      raise_s
        [%message
          "BUG: cannot request a capacity that is not a multiple of data_width_bytes"
            (M.capacity_in_bytes : int)
            (data_width_bytes : int)]
  ;;

  let capacity_in_words = M.capacity_in_bytes / data_width_bytes

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; memory : 'a Axi.O.t [@rtlprefix "memory_i$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { memory : 'a Axi.I.t [@rtlprefix "memory_o$"] }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let cell_bits = address_bits_for data_width_bytes
  let address_bits = address_bits_for capacity_in_words

  let lock_read ~ready scope (i : _ I.t) =
    let reg_spec = Clocking.to_spec i.clock in
    let reg_spec_no_clear = Clocking.to_spec i.clock in
    let%hw read_locked = wire 1 in
    let%hw read_ctr = wire 8 in
    let%hw accepting_new_transfer = i.memory.arvalid &: ready &: ~:read_locked in
    let%hw incoming_address =
      drop_bottom ~width:cell_bits i.memory.araddr |> sel_bottom ~width:address_bits
    in
    read_locked
    <-- reg_fb
          ~width:1
          ~f:(fun t ->
            mux2
              (accepting_new_transfer &: (i.memory.arlen <>:. 0))
              vdd
              (mux2 (read_ctr ==:. 1) gnd t))
          reg_spec;
    let%hw read_address_reg =
      reg_fb
        ~width:address_bits
        ~f:(fun t -> mux2 accepting_new_transfer (incr incoming_address) (incr t))
        reg_spec_no_clear
    in
    read_ctr
    <-- reg_fb
          ~width:8
          ~f:(fun t ->
            mux2 accepting_new_transfer (uresize ~width:8 i.memory.arlen) (t -:. 1))
          reg_spec_no_clear;
    ( read_locked |: accepting_new_transfer
    , cut_through_reg ~enable:accepting_new_transfer reg_spec_no_clear i.memory.arid
    , mux2 read_locked read_address_reg incoming_address
    , ((accepting_new_transfer &: (i.memory.arlen ==:. 0)) |: ( read_ctr ==:. 1))
    , ready &: ~:read_locked )
  ;;

  let create ~build_mode ~read_latency scope ({ clock; memory } as i : _ I.t) =
    let reg_spec = Clocking.to_spec clock in
    (* Synthetic pushback mechanism for testing. Raises ready only once in M.synthetic_pushback cycles. *)
    let%hw should_push_back =
      if M.synthetic_pushback > 0
      then
        reg_fb
          ~width:(address_bits_for M.synthetic_pushback)
          ~f:(fun t -> mod_counter ~max:(M.synthetic_pushback - 1) t)
          reg_spec
        <>:. 0
      else gnd
    in
    let write_address = drop_bottom ~width:cell_bits memory.awaddr in
    let%hw write_ctr =
      reg_fb
        ~width:8
        ~enable:(memory.wvalid &: ~:should_push_back)
        ~f:(fun t -> mux2 memory.wlast (zero (width t)) (incr t))
        reg_spec
    in
    let%hw write_address =
      write_address +: uresize ~width:(width write_address) write_ctr
    in
    let%hw read_valid, read_id, read_address, last_read, read_ready =
      lock_read ~ready:~:should_push_back (Scope.sub_scope scope "lock_read") i
    in
    let%hw read_address_in_range =
      if Int.pow 2 (width read_address) <= capacity_in_words
      then vdd
      else read_address <:. capacity_in_words
    in
    let%hw write_address_in_range =
      if Int.pow 2 (width write_address) <= capacity_in_words
      then vdd
      else write_address <:. capacity_in_words
    in
    let%hw write_enable =
      repeat
        ~count:(width memory.wstrb)
        (memory.awvalid &: memory.wvalid &: write_address_in_range &: ~:should_push_back)
      &: memory.wstrb
    in
    let%hw read_data =
      Simple_dual_port_ram.create
        ~simulation_name:"main_memory_bram"
        ~byte_write_width:B8
        ~arch:(Blockram Read_before_write)
        ~address_collision_protection:Mux_output_ports
        ~address_collision_model:Lfsr
        ~size:capacity_in_words
        ~build_mode
        ~clock:clock.clock
        ~clear:clock.clear
        ~write_enable
        ~write_address
        ~data:memory.wdata
        ~read_enable:read_valid
        ~read_address
        ~read_latency
        ()
    in
    { O.memory =
        { Axi.I.bvalid = reg reg_spec (memory.awvalid &: ~:should_push_back)
        ; bid = reg reg_spec memory.awid
        ; bresp = zero 2
        ; rvalid = pipeline ~n:read_latency reg_spec read_valid
        ; rid = pipeline ~n:read_latency reg_spec read_id
        ; rdata =
            pipeline
              ~n:read_latency
              reg_spec
              (repeat ~count:(width read_data) read_address_in_range)
            &: read_data
        ; rresp = zero 2
        ; wready = ~:should_push_back
        ; awready =
            ~:should_push_back &: memory.awvalid &: memory.wvalid &: i.memory.wlast
        ; arready = read_ready
        ; rlast = pipeline ~n:read_latency reg_spec last_read
        }
    }
  ;;

  let hierarchical ~build_mode ~read_latency (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"axi4_bram" (create ~build_mode ~read_latency) input
  ;;
end
