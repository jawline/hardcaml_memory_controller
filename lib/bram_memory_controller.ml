open! Core
open Hardcaml
open Signal

module Make (M : sig
    val address_width : int
    val data_bus_width : int
    val capacity_in_bytes : int

    module Instruction_config : Shared_access_ports_intf.Config
    module Data_config : Shared_access_ports_intf.Config
  end) =
struct
  module Axi_in_config = struct
    let id_bits = 8
    let data_bits = M.data_bus_width
    let addr_bits = address_bits_for M.capacity_in_bytes
    let burst_length_bits = 1
  end

  module Axi_out_config = struct
    let id_bits = 1
    let data_bits = M.data_bus_width
    let addr_bits = address_bits_for M.capacity_in_bytes
    let burst_length_bits = 1
  end

  module Axi4_in = Axi4.Make (Axi_in_config)
  module Axi4_out = Axi4.Make (Axi_out_config)

  module Memory =
    Axi4_bram.Make
      (struct
        let capacity_in_bytes = M.capacity_in_bytes
        let synthetic_pushback = 0
      end)
      (Axi4_out)

  module Memory_controller = Memory_controller.Make (M) (Axi4_in) (Axi4_out)
  include Memory_controller

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; instruction : 'a Instruction.Request.t
      ; data : 'a Data.Request.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { instruction : 'a Instruction.Response.t
      ; data : 'a Data.Response.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
        ~build_mode
        ~read_latency
        ~priority_mode
        scope
        ({ clock; instruction; data } : _ I.t)
    =
    let memory = Axi4_out.O.Of_signal.wires () in
    let mem =
      Memory.hierarchical ~build_mode ~read_latency scope { Memory.I.clock; memory }
    in
    let core =
      Memory_controller.hierarchical
        ~build_mode
        ~priority_mode
        scope
        { Memory_controller.I.clock; instruction; data; memory = mem.memory }
    in
    Axi4_out.O.Of_signal.assign memory core.memory;
    { O.instruction = core.instruction; data = core.data }
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
