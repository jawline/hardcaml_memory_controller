open! Core
open Hardcaml
open Signal

module Make
    (M : Shared_access_ports_intf.Config)
    (Memory_bus : Memory_bus_intf.S)
    (Axi : Axi4.S) =
struct
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

  module Request = struct
    type 'a t =
      { write_to_controller : 'a Memory_bus.Write_bus.Source.t list
            [@length M.num_write_channels]
      ; read_to_controller : 'a Memory_bus.Read_bus.Source.t list
            [@length M.num_read_channels]
      }
    [@@deriving hardcaml]
  end

  module Response = struct
    type 'a t =
      { write_to_controller : 'a Memory_bus.Write_bus.Dest.t list
            [@length M.num_write_channels]
      ; read_to_controller : 'a Memory_bus.Read_bus.Dest.t list
            [@length M.num_read_channels]
      ; write_response : 'a Memory_bus.Write_response.With_valid.t list
            [@length M.num_write_channels]
      ; read_response : 'a Memory_bus.Read_response.With_valid.t list
            [@length M.num_read_channels]
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; flush : 'a
      ; request : 'a Request.t
      ; memory : 'a Axi.I.t [@rtlprefix "memory_i$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { response : 'a Response.t
      ; memory : 'a Axi.O.t [@rtlprefix "memory_o$"]
      ; locked : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
        ~capacity_in_bytes
        ~priority_mode
        ~build_mode
        scope
        ({ clock; flush; request = { write_to_controller; read_to_controller }; memory } :
          _ I.t)
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
      let capacity_in_words = capacity_in_bytes / (data_bus_width / 8) in
      if Int.pow 2 (width t) > capacity_in_words then t <:. capacity_in_words else vdd
    in
    let%hw can_ack_read = cap_check read_arbitrator.selected_ch.data.address in
    let%hw can_ack_write = cap_check write_arbitrator.selected_ch.data.address in
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
            ; flush
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
        ; locked = o.locked
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
    { O.response =
        { Response.write_to_controller =
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
        }
    ; memory = core.memory
    ; locked = core.locked
    }
  ;;

  let hierarchical
        ~capacity_in_bytes
        ~priority_mode
        ~build_mode
        (scope : Scope.t)
        (input : Signal.t I.t)
    =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:"memory_controller"
      (create ~capacity_in_bytes ~priority_mode ~build_mode)
      input
  ;;
end
