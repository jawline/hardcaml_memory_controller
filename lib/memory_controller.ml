open! Core
open Hardcaml
open Signal

module Make
    (M : sig
       val address_width : int
       val data_bus_width : int
       val capacity_in_bytes : int
       val num_read_channels : int
       val num_write_channels : int
       val cache_memory : (module Axi4_cache.Config) option
     end)
    (Axi : Axi4.S) =
struct
  module Memory_bus = Memory_bus.Make (struct
      include M
    end)

  module Cross_clocks = Cross_clock.Make (Memory_bus)
  open Memory_bus

  module Read_arbitrator =
    Memory_channel_arbitrator.Make
      (Read_bus)
      (struct
        let num_channels = M.num_read_channels
      end)

  module Write_arbitrator =
    Memory_channel_arbitrator.Make
      (Write_bus)
      (struct
        let num_channels = M.num_write_channels
      end)

  module Core = Axi4_memory_controller_core.Make (Memory_bus) (M) (Axi)

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; write_to_controller : 'a Write_bus.Source.t list [@length M.num_write_channels]
      ; read_to_controller : 'a Read_bus.Source.t list [@length M.num_read_channels]
      ; memory : 'a Axi.I.t [@rtlprefix "memory_i$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { write_to_controller : 'a Write_bus.Dest.t list [@length M.num_write_channels]
      ; read_to_controller : 'a Read_bus.Dest.t list [@length M.num_read_channels]
      ; write_response : 'a Write_response.With_valid.t list
            [@length M.num_write_channels]
      ; read_response : 'a Read_response.With_valid.t list [@length M.num_read_channels]
      ; memory : 'a Axi.O.t [@rtlprefix "memory_o$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
        ~priority_mode
        ~build_mode
        scope
        ({ clock; write_to_controller; read_to_controller; memory } : _ I.t)
    =
    let write_arbitrator =
      Write_arbitrator.hierarchical
        ~instance:"write"
        ~priority_mode
        scope
        { Write_arbitrator.I.clock; ch_to_controller = write_to_controller }
    in
    let read_arbitrator =
      Read_arbitrator.hierarchical
        ~instance:"read"
        ~priority_mode
        scope
        { Read_arbitrator.I.clock; ch_to_controller = read_to_controller }
    in
    (* TODO: We stall on a bad read request rather than trap. We rely on the CPU stalling in tests so we stop on a known PC. It would be better to use a breakpoint as test completion. *)
    let cap_check t =
      if Int.pow 2 (width t) >= M.capacity_in_bytes
      then t <:. M.capacity_in_bytes
      else vdd
    in
    let can_ack_read = cap_check read_arbitrator.selected_ch.data.address in
    let can_ack_write = cap_check write_arbitrator.selected_ch.data.address in
    let selected_read =
      { read_arbitrator.selected_ch with
        valid = read_arbitrator.selected_ch.valid &: can_ack_read
      }
    in
    let selected_write =
      { write_arbitrator.selected_ch with
        valid = write_arbitrator.selected_ch.valid &: can_ack_write
      }
    in
    let core =
      match M.cache_memory with
      | Some (module C : Axi4_cache.Config) ->
        let module Axi4_cache = Axi4_cache.Make (C) (Memory_bus) (Axi) in
        let o =
          Axi4_cache.hierarchical
            ~build_mode
            scope
            { Axi4_cache.I.clock
            ; requests =
                { which_write_ch = write_arbitrator.which_ch
                ; selected_write_ch = selected_write
                ; which_read_ch = read_arbitrator.which_ch
                ; selected_read_ch = selected_read
                }
            ; dn = memory
            }
        in
        { Core.O.write_ready = o.write_ready
        ; read_ready = o.read_ready
        ; write_response = o.response.write_response
        ; read_response = o.response.read_response
        ; read_error = gnd (* TODO: *)
        ; write_error = gnd (* TODO: *)
        ; memory = o.dn
        }
      | None ->
        Core.hierarchical
          scope
          { Core.I.clock
          ; which_write_ch = write_arbitrator.which_ch
          ; selected_write_ch = selected_write
          ; which_read_ch = read_arbitrator.which_ch
          ; selected_read_ch = selected_read
          ; memory
          }
    in
    (* TODO: Propagate errors *)
    { O.write_to_controller =
        List.map
          ~f:(fun t ->
            { Write_bus.Dest.ready = t.ready &: core.write_ready &: can_ack_write })
          write_arbitrator.acks
    ; read_to_controller =
        List.map
          ~f:(fun t ->
            { Read_bus.Dest.ready = t.ready &: core.read_ready &: can_ack_read })
          read_arbitrator.acks
    ; write_response = core.write_response
    ; read_response = core.read_response
    ; memory = core.memory
    }
  ;;

  let hierarchical ~priority_mode ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:"memory_controller"
      (create ~priority_mode ~build_mode)
      input
  ;;
end
