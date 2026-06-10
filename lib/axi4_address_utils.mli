open! Core
open! Hardcaml

module type Config = sig
  val line_width : int
  val num_cache_lines : int
  val num_ways : int
  val cell_width : int
  val cell_address_width : int
end

module Make : functor (Config : Config) -> Axi4_address_utils_intf.S
