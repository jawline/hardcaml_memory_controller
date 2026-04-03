(** This constructs a BRAM based multi channel memory controller. Under the
    hood it uses the generic Axi4 memory controller and a BRAM based Axi4
    memory. *)
open! Core

open Hardcaml

module Make (M : sig
    val address_width : int
    val data_bus_width : int
    val capacity_in_bytes : int

    module Instruction_config : Shared_access_ports_intf.Config
    module Data_config : Shared_access_ports_intf.Config
  end) : sig
  module Memory_bus : Memory_bus_intf.S

  module Instruction :
    Shared_access_ports_intf.Without_memory(M.Instruction_config)(Memory_bus).S

  module Data : Shared_access_ports_intf.Without_memory(M.Data_config)(Memory_bus).S

  module I : sig
    type 'a t =
      { clock : 'a Clocking.t
      ; flush : 'a 
      ; instruction : 'a Instruction.Request.t
      ; data : 'a Data.Request.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { instruction : 'a Instruction.Response.t
      ; data : 'a Data.Response.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical
    :  build_mode:Build_mode.t
    -> read_latency:int
    -> priority_mode:Priority_mode.t
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
