open! Core
open! Hardcaml

module Make (Config : sig
    val cell_width : int
    val line_width : int
    val num_cache_lines : int
    val memory_address_width : int
  end) : Cache_ram_intf.S
