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
    (Config : Axi4_config_intf.Config)
    (Axi4 : Axi4_intf.M(Config).S) =
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

  let capacity_in_words = M.capacity_in_bytes / data_bus_in_bytes

  (** There are two alignments of the base address. The base address is host addressed (bytes),
      our core requires that memory be aligned to a word address (4 bytes / 8 bytes) and the AXI4
      interface has a wider addressing scheme (usually 32 / 64 bytes). *)
  let unaligned_bits_data_bus = address_bits_for (M.data_bus_width / 8)

  let () =
    if Config.data_width <> M.data_bus_width
    then
      raise_s
        [%message "BUG: currently bus widths different to axi width are not supported"]
  ;;

  let () =
    if Config.id_width < M.num_read_channels
    then
      raise_s
        [%message
          "BUG: The AXI4 MIG config should have an ID width large enough to address each \
           read channel"];
    if Config.id_width < M.num_write_channels
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
      ; axi : 'a Axi4.I.t
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
      ; axi : 'a Axi4.O.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let real_address ~scope address =
    let%hw base_address = address in
    drop_bottom ~width:unaligned_bits_data_bus base_address
  ;;

  let illegal_operation ~scope address =
    let%hw is_unaligned = sel_bottom ~width:unaligned_bits_data_bus address <>:. 0 in
    let%hw is_out_of_range = real_address ~scope address >=:. capacity_in_words in
    is_unaligned |: is_out_of_range
  ;;

  let create
        scope
        ({ clock = _
         ; clear = _
         ; which_read_ch
         ; selected_read_ch
         ; which_write_ch
         ; selected_write_ch
         ; axi
         } :
          _ I.t)
    =
    let read_invalid = illegal_operation ~scope selected_read_ch.data.address in
    let write_invalid = illegal_operation ~scope selected_write_ch.data.address in
    { O.read_response =
        List.init
          ~f:(fun channel ->
            let is_channel = axi.rid ==:. channel in
            { With_valid.valid = axi.rvalid &: is_channel
            ; value = { Read_response.read_data = axi.rdata }
            })
          M.num_read_channels
    ; read_ready = axi.rready
    ; read_error = selected_read_ch.valid &: read_invalid
    ; write_response =
        List.init
          ~f:(fun channel ->
            let is_channel = axi.bid ==:. channel in
            { With_valid.valid = axi.bvalid &: is_channel
            ; value = { Write_response.dummy = gnd }
            })
          M.num_write_channels
    ; write_ready = axi.wready
    ; write_error = selected_write_ch.valid
    ; axi =
        { awvalid = selected_write_ch.valid &: ~:write_invalid
        ; awid = uextend ~width:Axi4.O.port_widths.awid which_write_ch
        ; awaddr =
            drop_bottom ~width:unaligned_bits_data_bus selected_write_ch.data.address
            |> sel_bottom ~width:Axi4.O.port_widths.awaddr
        ; awlena = one 8
        ; awsize = one 3
        ; awburst = zero 2
        ; wdata = selected_write_ch.data.write_data
        ; wstrb = selected_write_ch.data.wstrb
        ; wlast = vdd
        ; arvalid = selected_read_ch.valid &: ~:read_invalid
        ; arid = uextend ~width:Axi4.O.port_widths.arid which_read_ch
        ; araddr =
            drop_bottom ~width:unaligned_bits_data_bus selected_read_ch.data.address
            |> sel_bottom ~width:Axi4.O.port_widths.araddr
        ; arlen = one 8
        ; arsize = one 3
        ; arburst = zero 2
        ; rready = vdd
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_controller_core" create input
  ;;
end
