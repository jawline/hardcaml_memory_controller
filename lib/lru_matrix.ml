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

  let initial_state =
    (* We want there to only be one valid outcome for the initial assignment so we can onehot select rather than priority select. Because of this we need to carefull craft the initial grid. *)
          let open Bits in 
    let rows =
      List.init
        ~f:(fun row ->
          let cols =
            List.init
              ~f:(fun col ->
                if row = col then None else Some (if row < col then vdd else gnd))
              num_ways
          in
          With_zero_width.concat_lsb cols |> Option.value_exn)
        num_ways
    in
    { State.grid = concat_lsb rows }
  ;;

  let rows (t : _ State.t) = split_lsb ~part_width:(num_ways - 1) t.grid

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

  let least_row (t : _ State.t) =
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
    let next =
      List.init
        ~f:(fun y ->
          let row_with_holes =
            List.init
              ~f:(fun x ->
                if x = y
                then None
                else (
                  let cur = get t x y in
                  let set = way ==:. y in
                  let unset = way ==:. x in
                  Some (cur |: set &: ~:unset)))
              num_ways
          in
          With_zero_width.(concat_lsb row_with_holes |> Option.value_exn))
        num_ways
    in
    { State.grid = concat_lsb next }
  ;;
end
