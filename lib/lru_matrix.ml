open Core
open Hardcaml
open Signal

(* Implements an LRU computing matrix. By setting the entries in a row to 1 and column to zero during an update we can find the least used element by finding the row that is all zeros. *)

module Make (Config : sig
    val num_ways : int
  end) =
struct
  open Config

  module State = struct
    type 'a t =
      { grid : 'a [@bits (num_ways * num_ways) - num_ways]
        (* We do not include the diagonals in the matrix *)
      }
    [@@deriving hardcaml]
  end

  let initial_state = { State.grid = zero State.port_widths.grid }
  let rows t = split_lsb ~part_width:(num_ways - 1) t

  let get (t : _ State.t) x y =
    let stride = num_ways - 1 in
    let row = y * stride in
    let col =
      if x = y
      then raise_s [%message "BUG: Diagonals are not included here"]
      else if x < y
      then x
      else x - 1
    in
    let bit = row + col in
    t.grid.:(bit)
  ;;

  let least_row t =
    let indices =
      List.mapi
        ~f:(fun row_idx row ->
          { With_valid.valid = row ==:. 0
          ; value = of_unsigned_int ~width:(address_bits_for num_ways) row_idx
          })
        (rows t)
    in
    onehot_select indices
  ;;

  let update ~way (t : _ State.t) =
    List.init
      ~f:(fun y ->
        let row_with_holes =
          List.init
            ~f:(fun x ->
              if x = y
              then None
              else (
                let cur = get t x y in
                let set = way ==:. x in
                let unset = way ==:. y in
                Some (cur |: set &: ~:unset)))
            num_ways
        in
        With_zero_width.(concat_lsb row_with_holes |> Option.value_exn))
      num_ways
  ;;
end
