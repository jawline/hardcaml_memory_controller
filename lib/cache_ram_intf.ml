open! Core
open! Hardcaml

module type S = sig
  module Read : sig
    type 'a t =
      { valid : 'a
      ; cache_address : 'a
      }
    [@@deriving hardcaml]
  end

  module Write : sig
    type 'a t =
      { valid : 'a
      ; cell_valid : 'a
      ; cache_address : 'a
      ; datas : 'a list
      ; address : 'a
      ; meta_wstrb : 'a
      ; real_wstrb : 'a
      ; dirty : 'a
      }
    [@@deriving hardcaml]
  end

  module Line_metadata : sig
    type 'a t =
      { valid : 'a
      ; address : 'a
      ; strb : 'a
      ; dirty : 'a
      }
    [@@deriving hardcaml]
  end

  module I : sig
    type 'a t =
      { clock : 'a Clocking.t
      ; read : 'a Read.t
      ; write : 'a Write.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { meta : 'a Line_metadata.t
      ; read_data : 'a list
      }
    [@@deriving hardcaml]
  end

  val read_latency : int
  val cache_address_width : int
  val num_cache_lines : int
  val cell_width : int
  val line_width : int

  val cache_address_to_hashed_line_address_generic
    :  (module Comb.S with type t = 'a)
    -> 'a
    -> 'a

  val cache_address_to_hashed_line_address : Signal.t -> Signal.t
  val cache_address_to_byte_address : Signal.t -> Signal.t
  val cell_to_cache_address : Signal.t -> Signal.t
  val cell_address_to_bytes : Signal.t -> Signal.t
  val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
