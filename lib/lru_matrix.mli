open! Core
open! Hardcaml

module Make (Config : sig
    val num_ways : int
  end) : sig
  module State : sig
    type 'a t = { grid : 'a [@bits (num_ways * num_ways) - num_ways] }
    [@@deriving hardcaml]
  end

  val initial_state : Bits.t State.t
  val least_row : Signal.t State.t -> Signal.t
  val update : way:Signal.t -> Signal.t State.t -> Signal.t State.t
end
