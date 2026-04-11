open! Core
open Hardcaml

module Make (M0 : Axi4.S) (M1 : Axi4.S) (S : Axi4.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a Clocking.t
      ; m0 : 'a M0.O.t
      ; m1 : 'a M1.O.t
      ; s_in : 'a S.I.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { m0_out : 'a M0.I.t
      ; m1_out : 'a M1.I.t
      ; s_out : 'a S.O.t
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
