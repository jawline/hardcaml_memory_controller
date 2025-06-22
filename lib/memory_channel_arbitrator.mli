open! Core
open Hardcaml
open Hardcaml_custom_handshake

module Make
    (S : Handshake_intf.S)
    (M : sig
       val num_channels : int
     end) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; ch_to_controller : 'a S.Source.t list
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { which_ch : 'a
      ; selected_ch : 'a S.Source.t
      ; acks : 'a S.Dest.t list
      }
    [@@deriving hardcaml]
  end

  val hierarchical
    :  ?instance:string
    -> priority_mode:Priority_mode.t
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
