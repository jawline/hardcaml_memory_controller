(** This memory controller implementation drives a two channel (one read, one
    write) AXI4 interface to read and write to RAM. It is compatible with the
    BRAM based memory controller and can interoperate with the DDR MIG
    controller provided by Xilinx. *)
open! Core

open Hardcaml

module Make
    (Memory_bus : Memory_bus_intf.S)
    (M : sig
       val capacity_in_bytes : int
       val num_read_channels : int
       val num_write_channels : int
       val data_bus_width : int
     end)
    (Config : Axi4_config_intf.Config)
    (Axi4 : Axi4_intf.M(Config).S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; which_read_ch : 'a
      ; selected_read_ch : 'a Memory_bus.Read_bus.Source.t
      ; which_write_ch : 'a
      ; selected_write_ch : 'a Memory_bus.Write_bus.Source.t
      ; axi : 'a Axi4.I.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { read_response : 'a Memory_bus.Read_response.With_valid.t list
      ; read_ready : 'a
      ; read_error : 'a
      ; write_response : 'a Memory_bus.Write_response.With_valid.t list
      ; write_ready : 'a
      ; write_error : 'a
      ; axi : 'a Axi4.O.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
