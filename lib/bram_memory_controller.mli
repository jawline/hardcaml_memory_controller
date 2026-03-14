(** This constructs a BRAM based multi channel memory controller. Under the
    hood it uses the generic Axi4 memory controller and a BRAM based Axi4
    memory. *)
open! Core

open Hardcaml

module Make (M : sig
    val address_width : int
    val capacity_in_bytes : int
    val num_read_channels : int
    val num_write_channels : int
    val data_bus_width : int
    val cache_memory : (module Axi4_cache.Config) option
  end) : sig
  module Memory_bus : Memory_bus_intf.S

  module I : sig
    type 'a t =
      { clock : 'a Clocking.t
      ; write_to_controller : 'a Memory_bus.Write_bus.Source.t list
            [@length M.num_write_channels]
      ; read_to_controller : 'a Memory_bus.Read_bus.Source.t list
            [@length M.num_read_channels]
      }
    [@@deriving hardcaml]
  end

  module O : sig
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

  val hierarchical
    :  build_mode:Build_mode.t
    -> read_latency:int
    -> priority_mode:Priority_mode.t
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
