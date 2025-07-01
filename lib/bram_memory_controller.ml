open! Core
open Hardcaml
open Signal

module Make (M : sig
    val data_bus_width : int
    val capacity_in_bytes : int
    val num_read_channels : int
    val num_write_channels : int
    val address_width : int
  end) =
struct
  module Axi_config = struct
    let id_bits = 8
    let data_bits = M.data_bus_width
    let addr_bits = address_bits_for (M.capacity_in_bytes / (data_bits / 8))
    let burst_length_bits = 1
  end

  module Axi4 = Axi4.Make (Axi_config)

  module Memory =
    Axi4_bram.Make
      (struct
        let capacity_in_bytes = M.capacity_in_bytes
        let synthetic_pushback = 0
      end)
      (Axi4)

  module Memory_controller = Memory_controller.Make (M) (Axi4)
  module Memory_bus = Memory_controller.Memory_bus
  open Memory_bus

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; write_to_controller : 'a Write_bus.Source.t list [@length M.num_write_channels]
      ; read_to_controller : 'a Read_bus.Source.t list [@length M.num_read_channels]
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
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
        ~build_mode
        ~read_latency
        ~priority_mode
        scope
        ({ clock; clear; write_to_controller; read_to_controller } : _ I.t)
    =
    let memory = Axi4.O.Of_signal.wires () in
    let mem =
      Memory.hierarchical
        ~build_mode
        ~read_latency
        scope
        { Memory.I.clock; clear; memory }
    in
    let core =
      Memory_controller.hierarchical
        ~priority_mode
        scope
        { Memory_controller.I.clock
        ; clear
        ; write_to_controller
        ; read_to_controller
        ; memory = mem.memory
        }
    in
    Axi4.O.Of_signal.assign memory core.memory;
    { O.write_to_controller = core.write_to_controller
    ; read_to_controller = core.read_to_controller
    ; write_response = core.write_response
    ; read_response = core.read_response
    }
  ;;

  let hierarchical
        ~build_mode
        ~read_latency
        ~priority_mode
        (scope : Scope.t)
        (input : Signal.t I.t)
    =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:"bram_memory_controller"
      (create ~build_mode ~read_latency ~priority_mode)
      input
  ;;
end
