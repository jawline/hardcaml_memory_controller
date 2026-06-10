open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_memory_controller
open! Bits

let debug = true
let num_ways = 4
let grid_width = (num_ways * num_ways) - num_ways

module Lru = Lru_matrix.Make (struct
    let num_ways = num_ways
  end)

module I = struct
  type 'a t =
    { clock : 'a Clocking.t
    ; update_en : 'a
    ; way : 'a [@bits address_bits_for num_ways]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { grid : 'a [@bits grid_width]
    ; least_row : 'a [@bits address_bits_for num_ways]
    }
  [@@deriving hardcaml]
end

module Harness = Cyclesim_harness.Make (I) (O)

let hierarchical ~build_mode:_ _scope (i : Signal.t I.t) =
  let open Signal in
  let clocking = i.clock in
  let reg_spec = Clocking.to_spec clocking in
  let state_signal = wire grid_width in
  let next_state = (Lru.update ~way:i.way { Lru.State.grid = state_signal }).grid in
  state_signal <-- reg reg_spec ~enable:i.update_en ~initialize_to:Lru.initial_state.grid ~clear_to:(Signal.of_bits Lru.initial_state.grid) next_state;
  { O.grid = state_signal
  ; least_row = Lru.least_row { Lru.State.grid = state_signal }
  }
;;

let create_sim f =
  Harness.run
    ~waves_config:(Waves_config.to_home_subdirectory_when debug)
    ~create:(hierarchical ~build_mode:Build_mode.Simulation)
    f
;;

let print_grid grid_val =
  let rows =
    List.init num_ways ~f:(fun y ->
      let stride = y * (num_ways - 1) in
      List.init num_ways ~f:(fun x ->
        if x = y
        then "."
        else (
          let col = if x < y then x else x - 1 in
          let bit = stride + col in
          string_of_int ((grid_val lsr bit) land 1)))
      |> String.concat ~sep:" ")
  in
  List.iter rows ~f:(fun r -> printf "  %s\n" r)
;;

let%expect_test "lru matrix update sequence" =
  create_sim (fun ~inputs ~outputs sim ->
    let (inputs : Bits.t ref I.t) = inputs in
    let (outputs : Bits.t ref O.t) = outputs in
    inputs.clock.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clock.clear := gnd;
    Cyclesim.cycle sim;
    let update ~way =
      inputs.update_en := vdd;
      inputs.way := of_unsigned_int ~width:(address_bits_for num_ways) way;
      Cyclesim.cycle sim;
      inputs.update_en := gnd;
      let grid_val = to_int_trunc !(outputs.grid) in
      let lr = to_int_trunc !(outputs.least_row) in
      printf "After way %d:\n" way;
      printf "  grid = 0x%03x (%d)\n" grid_val grid_val;
      printf "  matrix:\n";
      print_grid grid_val;
      printf "  least_row = %d\n" lr;
      printf "\n";
      lr
    in
    Sequence.range 0 8 |> Sequence.fold ~init:0 ~f:(fun last _t -> update ~way:last))
  |> (ignore : int -> unit);
  [%expect
    {|
    After way 0:
      grid = 0x137 (311)
      matrix:
      . 1 1 1
      0 . 1 1
      0 0 . 1
      0 0 0 .
      least_row = 3

    After way 3:
      grid = 0xe13 (3603)
      matrix:
      . 1 1 0
      0 . 1 0
      0 0 . 0
      1 1 1 .
      least_row = 2

    After way 2:
      grid = 0x7c1 (1985)
      matrix:
      . 1 0 0
      0 . 0 0
      1 1 . 1
      1 1 0 .
      least_row = 1

    After way 1:
      grid = 0x378 (888)
      matrix:
      . 0 0 0
      1 . 1 1
      1 0 . 1
      1 0 0 .
      least_row = 0

    After way 0:
      grid = 0x137 (311)
      matrix:
      . 1 1 1
      0 . 1 1
      0 0 . 1
      0 0 0 .
      least_row = 3

    After way 3:
      grid = 0xe13 (3603)
      matrix:
      . 1 1 0
      0 . 1 0
      0 0 . 0
      1 1 1 .
      least_row = 2

    After way 2:
      grid = 0x7c1 (1985)
      matrix:
      . 1 0 0
      0 . 0 0
      1 1 . 1
      1 1 0 .
      least_row = 1

    After way 1:
      grid = 0x378 (888)
      matrix:
      . 0 0 0
      1 . 1 1
      1 0 . 1
      1 0 0 .
      least_row = 0

    Saved waves to /home/blake/waves//_lru_matrix_update_sequence.hardcamlwaveform
    |}]
;;
