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
      { clock : 'a
      ; clear : 'a
      ; memory : 'a Axi.O.t [@rtlprefix "memory_i$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { memory : 'a Axi.I.t [@rtlprefix "memory_o$"] }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create ~build_mode ~read_latency scope ({ clock; clear; memory } : _ I.t) =
    let reg_spec = Reg_spec.create ~clock ~clear () in
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
    let%hw read_address =
      drop_bottom ~width:(address_bits_for data_width_bytes) memory.araddr
    in
    let%hw write_address =
      drop_bottom ~width:(address_bits_for data_width_bytes) memory.awaddr
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
    let%hw read_data =
      Simple_dual_port_ram.create
        ~simulation_name:"main_memory_bram"
        ~byte_write_width:B8
        ~arch:(Blockram Read_before_write)
        ~address_collision_protection:Mux_output_ports
        ~address_collision_model:Lfsr
        ~size:capacity_in_words
        ~build_mode
        ~clock
        ~clear
        ~write_enable:
          (repeat
             ~count:(width memory.wstrb)
             (memory.awvalid &: write_address_in_range &: ~:should_push_back)
           &: memory.wstrb)
        ~write_address
        ~data:memory.wdata
        ~read_enable:memory.arvalid
        ~read_address
        ~read_latency
        ()
    in
    { O.memory =
        { Axi.I.bvalid = reg reg_spec (memory.awvalid &: ~:should_push_back)
        ; bid = reg reg_spec memory.awid
        ; bresp = zero 2
        ; rvalid = pipeline ~n:read_latency reg_spec (memory.arvalid &: ~:should_push_back)
        ; rid = pipeline ~n:read_latency reg_spec memory.arid
        ; rdata =
            pipeline
              ~n:read_latency
              reg_spec
              (repeat ~count:(width read_data) read_address_in_range)
            &: read_data
        ; rresp = zero 2
        ; wready = ~:should_push_back
        ; awready = ~:should_push_back
        ; arready = ~:should_push_back
        ; rlast = vdd
        }
    }
  ;;

  let hierarchical ~build_mode ~read_latency (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"axi4_bram" (create ~build_mode ~read_latency) input
  ;;
end
