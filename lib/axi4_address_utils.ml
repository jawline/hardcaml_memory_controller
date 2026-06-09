open Core
open Hardcaml
open Signal

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

module Make (Config : Config) = struct
  open Config

  let cache_addr_width = address_bits_for num_cache_lines
  let cell_bytes = cell_width / 8
  let cell_to_bytes_bits = address_bits_for cell_bytes
  let line_to_cell_bits = address_bits_for line_width
  let line_size_alignment_bits = address_bits_for (cell_bytes * line_width)
  let way_index_bits = if num_ways = 1 then 0 else address_bits_for num_ways
  let num_sets = num_cache_lines / num_ways

  let ram_metadata_address_width =
    cell_address_width + cell_to_bytes_bits - line_size_alignment_bits  
  ;;

  let byte_to_cell_address t =
    drop_bottom ~width:cell_to_bytes_bits t |> uresize ~width:cell_address_width
  ;;

  let cache_address_to_byte_address t =
    concat_msb [ t; zero (line_to_cell_bits + cell_to_bytes_bits) ]
  ;;

  let cell_to_cache_address t =
    drop_bottom ~width:line_to_cell_bits t
    |> uresize ~width:(ram_metadata_address_width )
  ;;

  let cache_address_to_hashed_line_address_generic
        (type a)
        ~which_line
        (module Comb : Comb.S with type t = a)
        (t : a)
    =
    let open Comb in
    if width t <= cache_addr_width
    then (
      print_s
        [%message
          "WARN: Line addr width is smaller than address width so the cache has some \
           unaddressable cells"];
      uextend ~width:cache_addr_width t)
    else (
      let lower_bits = sel_bottom ~width:cache_addr_width t in
      match which_line with
      | 0 -> lower_bits
      | 1 ->
        let upper_bits =
          drop_bottom ~width:cache_addr_width t |> sel_bottom ~width:cache_addr_width
        in
        lower_bits ^: upper_bits
      | _ -> raise_s [%message "TODO: This hashfn is not generic"])
  ;;

  let cache_address_to_hashed_line_address =
    cache_address_to_hashed_line_address_generic (module Signal)
  ;;
end
