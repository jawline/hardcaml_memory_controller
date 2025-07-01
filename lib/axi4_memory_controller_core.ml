open Core
open Hardcaml
open Signal

module Make
    (Memory_bus : Memory_bus_intf.S)
    (M : sig
       val capacity_in_bytes : int
       val num_read_channels : int
       val num_write_channels : int
       val data_bus_width : int
     end)
    (Axi4 : Axi4.S) =
struct
  open Memory_bus

  let () =
    if M.data_bus_width % 8 <> 0 then raise_s [%message "BUG: data bus must be in bytes"]
  ;;

  let data_bus_in_bytes = M.data_bus_width / 8

  let () =
    if M.capacity_in_bytes % data_bus_in_bytes <> 0
    then
      raise_s
        [%message
          "BUG: cannot request a capacity that is not a multiple of data_bus_width"]
  ;;

  let () =
    if Axi4.data_width <> M.data_bus_width
    then
      raise_s
        [%message "BUG: currently bus widths different to axi width are not supported"]
  ;;

  let () =
    if Axi4.id_width < M.num_read_channels
    then
      raise_s
        [%message
          "BUG: The AXI4 MIG config should have an ID width large enough to address each \
           read channel"];
    if Axi4.id_width < M.num_write_channels
    then
      raise_s
        [%message
          "BUG: The AXI4 MIG config should have an ID with large enough to address each \
           write channel"];
    ()
  ;;

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; which_read_ch : 'a [@bits address_bits_for M.num_read_channels]
      ; selected_read_ch : 'a Memory_bus.Read_bus.Source.t
      ; which_write_ch : 'a [@bits address_bits_for M.num_write_channels]
      ; selected_write_ch : 'a Memory_bus.Write_bus.Source.t
      ; memory : 'a Axi4.I.t [@rtlprefix "memory_i$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { read_response : 'a Read_response.With_valid.t list [@length M.num_read_channels]
      ; read_ready : 'a
      ; read_error : 'a
      ; write_response : 'a Write_response.With_valid.t list
            [@length M.num_write_channels]
      ; write_ready : 'a
      ; write_error : 'a
      ; memory : 'a Axi4.O.t [@rtlprefix "memory_o$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let request_fifo ~clock ~clear ~wr ~d ~consume =
    (* TODO: Make these cut through - it will notably improve memory latency. *)
    let empty = wire 1 in
    let fifo =
      Fifo.create
        ~clock
        ~clear
        ~read_latency:0
        ~showahead:true
        ~capacity:8
        ~wr
        ~d
        ~rd:(consume &: ~:empty)
        ()
    in
    empty <-- fifo.empty;
    { With_valid.valid = ~:empty; value = fifo.q }, fifo.full
  ;;

  module Address_and_channel_id = struct
    type 'a t =
      { address : 'a [@bits Memory_bus.Write_bus.Source.port_widths.data.address]
      ; id : 'a [@bits I.port_widths.which_write_ch]
      }
    [@@deriving hardcaml]
  end

  module Data_and_wstrb = struct
    type 'a t =
      { data : 'a [@bits Memory_bus.Write_bus.Source.port_widths.data.write_data]
      ; wstrb : 'a [@bits Memory_bus.Write_bus.Source.port_widths.data.wstrb]
      }
    [@@deriving hardcaml]
  end

  let fixed_burst = of_unsigned_int ~width:Axi4.O.port_widths.awburst 0

  let burst_size =
    of_unsigned_int
      ~width:Axi4.O.port_widths.awsize
      (match Axi4.data_width with
       | 32 -> 0b101
       | _ -> raise_s [%message "BUG: Unimplemented conversion"])
  ;;

  (* We send a single entry for all burst lengths. This is 0 (as the length is defined as field + 1). *)
  let burst_length = of_unsigned_int ~width:Axi4.O.port_widths.awlen 0

  (* Axi4 is byte addressed. Internally in this core we only support data_width
     addressing, so we need to cast the data width address to a byte address.
     *)
  let translate_to_axi_byte_address t =
    let data_width = Axi4.data_width / 8 in
    let word_address_bits =
      sel_bottom ~width:(Axi4.O.port_widths.araddr - address_bits_for data_width) t
    in
    concat_lsb [ word_address_bits; zero (address_bits_for data_width) ]
  ;;

  let create
        scope
        ({ clock
         ; clear
         ; which_read_ch
         ; selected_read_ch
         ; which_write_ch
         ; selected_write_ch
         ; memory
         } :
          _ I.t)
    =
    (* AXI4 has separate channels for the write datas and write addresses. They
       might not both go valid at the same time. For simplicity, we instantiate two
       FIFOs (one for the address and the second for the data). We stop pushing
       if either fifo is full. *)
    let%hw both_fifos_have_capacity = wire 1 in
    let address_fifo, address_fifo_full =
      request_fifo
        ~clock
        ~clear
        ~wr:(both_fifos_have_capacity &: selected_write_ch.valid)
        ~d:
          ({ Address_and_channel_id.address = selected_write_ch.data.address
           ; id = which_write_ch
           }
           |> Address_and_channel_id.Of_signal.pack)
        ~consume:memory.awready
    in
    let%hw.Address_and_channel_id.Of_signal address_fifo_data =
      Address_and_channel_id.Of_signal.unpack address_fifo.value
    in
    let data_fifo, data_fifo_full =
      request_fifo
        ~clock
        ~clear
        ~wr:(both_fifos_have_capacity &: selected_write_ch.valid)
        ~d:
          ({ Data_and_wstrb.data = selected_write_ch.data.write_data
           ; wstrb = selected_write_ch.data.wstrb
           }
           |> Data_and_wstrb.Of_signal.pack)
        ~consume:memory.wready
    in
    let%hw.Data_and_wstrb.Of_signal data_fifo_data =
      Data_and_wstrb.Of_signal.unpack data_fifo.value
    in
    both_fifos_have_capacity <-- (~:address_fifo_full &: ~:data_fifo_full);
    (* TODO: This is ugly, we need to guard against the address being legal
       when pushing it to the memory controller. This mostly just matters for
       tests, as illegal reads are undefined in our core. 
    
      Instead, what I think we should do here is have an axi4_downcast shim
      that is capable of taking a wider address range and signalling error. *)
    let%hw no_bits_out_of_range_write =
      drop_bottom ~width:Axi4.O.port_widths.awaddr address_fifo_data.address ==:. 0
    in
    let%hw no_bits_out_of_range_read =
      drop_bottom ~width:Axi4.O.port_widths.araddr selected_read_ch.data.address ==:. 0
    in
    { O.read_response =
        List.init
          ~f:(fun channel ->
            let is_channel = memory.rid ==:. channel in
            { With_valid.valid = memory.rvalid &: is_channel
            ; value = { Read_response.read_data = memory.rdata }
            })
          M.num_read_channels
    ; read_ready = memory.arready
    ; read_error = gnd
    ; write_response =
        List.init
          ~f:(fun channel ->
            let is_channel = memory.bid ==:. channel in
            { With_valid.valid = memory.bvalid &: is_channel
            ; value = { Write_response.dummy = gnd }
            })
          M.num_write_channels
    ; write_ready = both_fifos_have_capacity
    ; write_error = gnd
    ; memory =
        { wvalid = data_fifo.valid
        ; awvalid = address_fifo.valid &: no_bits_out_of_range_write
        ; awid = uextend ~width:Axi4.O.port_widths.awid address_fifo_data.id
        ; awaddr = translate_to_axi_byte_address address_fifo_data.address
        ; wdata = data_fifo_data.data
        ; wstrb = data_fifo_data.wstrb
        ; wlast = vdd
        ; awburst = fixed_burst
        ; awlen = burst_length
        ; awsize = burst_size
        ; arvalid = selected_read_ch.valid &: no_bits_out_of_range_read
        ; arid = uextend ~width:Axi4.O.port_widths.arid which_read_ch
        ; araddr = translate_to_axi_byte_address selected_read_ch.data.address
        ; rready = vdd
        ; bready = vdd
        ; arburst = fixed_burst
        ; arlen = burst_length
        ; arsize = burst_size
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_controller_core" create input
  ;;
end
