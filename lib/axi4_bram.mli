open! Core
open Hardcaml

module Make
    (M : sig
       val capacity_in_bytes : int
       val synthetic_pushback : int
     end)
    (Axi : Axi4.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory : 'a Axi.O.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t = { memory : 'a Axi.I.t } [@@deriving hardcaml]
  end

  val hierarchical
    :  build_mode:Build_mode.t
    -> read_latency:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
