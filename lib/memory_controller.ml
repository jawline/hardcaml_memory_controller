open! Core
open Hardcaml

module Make
    (M : sig
       val address_width : int
       val data_bus_width : int
       val capacity_in_bytes : int

       module Instruction_config : Shared_access_ports_intf.Config
       module Data_config : Shared_access_ports_intf.Config
     end)
    (Axi : Axi4.S) =
struct
  open M

  let () =
    if M.capacity_in_bytes % (M.data_bus_width / 8) <> 0
    then
      raise_s
        [%message
          "BUG: cannot request a capacity that is not a multiple of data_bus_width"]
  ;;

  module Memory_bus = Memory_bus.Make (struct
      include M
    end)

  module Cross_clocks = Cross_clock.Make (Memory_bus)
  module Instruction = Shared_access_ports.Make (M.Instruction_config) (Memory_bus) (Axi)
  module Data = Shared_access_ports.Make (M.Data_config) (Memory_bus) (Axi)
  module Arbitrator = Axi4_arbitrator.Make (Axi) (Axi) (Axi)

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; instruction : 'a Instruction.Request.t
      ; data : 'a Data.Request.t
      ; memory : 'a Axi.I.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { instruction : 'a Instruction.Response.t
      ; data : 'a Data.Response.t
      ; memory : 'a Axi.O.t
      }
    [@@deriving hardcaml]
  end

  let create
        ~priority_mode
        ~build_mode
        scope
        ({ clock; instruction; data; memory } : _ I.t)
    =
    let instruction_axi4 = Axi.I.Of_signal.wires () in
    let instruction =
      Instruction.hierarchical
        ~capacity_in_bytes
        ~priority_mode
        ~build_mode
        scope
        { clock; request = instruction; memory = instruction_axi4 }
    in
    let data_axi4 = Axi.I.Of_signal.wires () in
    let data =
      Data.hierarchical
        ~capacity_in_bytes
        ~priority_mode
        ~build_mode
        scope
        { clock; request = data; memory = data_axi4 }
    in
    let arb =
      Arbitrator.hierarchical
        scope
        { Arbitrator.I.clock; m0 = instruction.memory; m1 = data.memory; s_in = memory }
    in
    Axi.I.Of_signal.(assign instruction_axi4 arb.m0_out);
    Axi.I.Of_signal.(assign data_axi4 arb.m1_out);
    { O.instruction = instruction.response; data = data.response; memory = arb.s_out }
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
