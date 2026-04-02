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
    (Axi_in : Axi4.S)
    (Axi_out : Axi4.S) : sig
  module Memory_bus : Memory_bus_intf.S
  module Cross_clocks : module type of Cross_clock.Make (Memory_bus)

  module Instruction :
    Shared_access_ports_intf.M(M.Instruction_config)(Memory_bus)(Axi_in).S

  module Data : Shared_access_ports_intf.M(M.Data_config)(Memory_bus)(Axi_in).S

  module I : sig
    type 'a t =
      { clock : 'a Clocking.t
      ; instruction : 'a Instruction.Request.t
      ; data : 'a Data.Request.t
      ; memory : 'a Axi_out.I.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { instruction : 'a Instruction.Response.t
      ; data : 'a Data.Response.t
      ; memory : 'a Axi_out.O.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical
    :  priority_mode:Priority_mode.t
    -> build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
