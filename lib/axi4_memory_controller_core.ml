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

  let illegal_operation ~scope address =
    let%hw is_out_of_range = address >=:. capacity_in_words in
    is_out_of_range
  ;;

  let create
        scope
        ({ clock = _
         ; clear = _
         ; which_read_ch
         ; selected_read_ch
         ; which_write_ch
         ; selected_write_ch
         ; memory
         } :
          _ I.t)
    =
    let read_invalid = illegal_operation ~scope selected_read_ch.data.address in
    let write_invalid = illegal_operation ~scope selected_write_ch.data.address in
    { O.read_response =
        List.init
          ~f:(fun channel ->
            let is_channel = memory.rid ==:. channel in
            { With_valid.valid = memory.rvalid &: is_channel
            ; value = { Read_response.read_data = memory.rdata }
            })
          M.num_read_channels
    ; read_ready = memory.rready
    ; read_error = selected_read_ch.valid &: read_invalid
    ; write_response =
        List.init
          ~f:(fun channel ->
            let is_channel = memory.bid ==:. channel in
            { With_valid.valid = memory.bvalid &: is_channel
            ; value = { Write_response.dummy = gnd }
            })
          M.num_write_channels
    ; write_ready = memory.wready
    ; write_error = selected_write_ch.valid
    ; memory =
        { awvalid = selected_write_ch.valid &: ~:write_invalid
        ; awid = uextend ~width:Axi4.O.port_widths.awid which_write_ch
        ; awaddr = uresize ~width:Axi4.O.port_widths.awaddr selected_write_ch.data.address
        ; wdata = selected_write_ch.data.write_data
        ; wstrb = selected_write_ch.data.wstrb
        ; wlast = vdd
        ; arvalid = selected_read_ch.valid &: ~:read_invalid 
        ; arid = uextend ~width:Axi4.O.port_widths.arid which_read_ch
        ; araddr = uresize ~width:Axi4.O.port_widths.araddr selected_read_ch.data.address
        ; rready = vdd
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_controller_core" create input
  ;;
end
