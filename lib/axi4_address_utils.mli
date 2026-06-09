open! Core
open! Hardcaml

module type Config = sig
  val line_width : int
  val num_cache_lines : int
  val num_ways : int
  val cell_width : int
  val cell_address_width : int
end

module type S = sig
  val cache_addr_width : int
  val cell_bytes : int
  val cell_to_bytes_bits : int
  val line_to_cell_bits : int
  val line_size_alignment_bits : int
  val way_index_bits : int
  val num_sets : int
  val ram_metadata_address_width : int
  val byte_to_cell_address : Signal.t -> Signal.t
  val cache_address_to_byte_address : Signal.t -> Signal.t
  val cell_to_cache_address : Signal.t -> Signal.t

  val cache_address_to_hashed_line_address_generic
    :  which_line:int
    -> (module Comb.S with type t = 'a)
    -> 'a
    -> 'a

  val cache_address_to_hashed_line_address : which_line:int -> Signal.t -> Signal.t
end

module Make : functor (Config : Config) -> S
