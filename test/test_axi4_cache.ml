open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_memory_controller
open! Bits

let debug = false
let verbose = false
let cell_width = 32
let cell_bytes = 32 / 8
let read_channels = 7
let write_channels = 5
let capacity_in_bytes = 1024

module Axi_config = struct
  let id_bits = 8
  let data_bits = 256
  let addr_bits = address_bits_for capacity_in_bytes
  let burst_length_bits = 1
end

module Axi4 = Axi4.Make (Axi_config)

module Memory =
  Axi4_bram.Make
    (struct
      let capacity_in_bytes = capacity_in_bytes
      let synthetic_pushback = 4
    end)
    (Axi4)

module Bus_config = struct
  let capacity_in_bytes = capacity_in_bytes
  let num_read_channels = read_channels
  let num_write_channels = write_channels
  let address_width = Axi_config.addr_bits - 2 (* DW addressed *)
  let data_bus_width = 32
  let line_width = 8

  let num_cache_lines =
    8 (* Eviction is hard to force in a test with lots of cache lines *)
  ;;
end

module Memory_bus = Memory_bus.Make (Bus_config)
module Axi4_cache = Axi4_cache.Make (Bus_config) (Memory_bus) (Axi4)

module Machine = struct
  module I = Axi4_cache.I
  module O = Axi4_cache.O

  let create scope ({ I.clock; _ } as i) =
    let memory = Axi4.O.Of_signal.wires () in
    let ram =
      Memory.hierarchical
        ~build_mode:Simulation
        ~read_latency:30
        scope
        { Memory.I.clock; memory }
    in
    let ctrl =
      Axi4_cache.hierarchical ~build_mode:Simulation scope { i with dn = ram.memory }
    in
    Axi4.O.Of_signal.assign memory ctrl.dn;
    ctrl
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"top" create input
  ;;
end

module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O)

let create_sim f =
  Harness.run
    ~waves_config:(Waves_config.to_home_subdirectory_when debug)
    ~create:Machine.hierarchical
    f
;;

let rec wait_for_write_ack ~timeout ~ch sim =
  if timeout = 0 then raise_s [%message "BUG: Timeout writing ack"];
  let outputs : _ Axi4_cache.O.t = Cyclesim.outputs ~clock_edge:Before sim in
  let ch_rx = List.nth_exn outputs.response.write_response ch in
  Cyclesim.cycle sim;
  if to_bool !(ch_rx.valid) then () else wait_for_write_ack ~timeout:(timeout - 1) ~ch sim
;;

let rec write ~timeout ~address ~value ~ch sim =
  if timeout = 0 then raise_s [%message "BUG: Timeout writing"];
  let inputs : _ Axi4_cache.I.t = Cyclesim.inputs sim in
  let ch_tx = inputs.requests.selected_write_ch in
  ch_tx.valid := vdd;
  ch_tx.data.address := of_unsigned_int ~width:(Axi_config.addr_bits - 2) address;
  ch_tx.data.write_data := of_unsigned_int ~width:32 value;
  ch_tx.data.wstrb := ones 4;
  inputs.requests.which_write_ch := of_unsigned_int ~width:3 ch;
  let outputs : _ Axi4_cache.O.t = Cyclesim.outputs ~clock_edge:Before sim in
  Cyclesim.cycle sim;
  if to_bool !(outputs.write_ready)
  then (
    ch_tx.valid := gnd;
    wait_for_write_ack ~timeout:1000 ~ch sim)
  else write ~timeout:(timeout - 1) ~address ~value ~ch sim
;;

let read ~address ~ch sim =
  Cyclesim.cycle sim;
  let inputs : _ Axi4_cache.I.t = Cyclesim.inputs sim in
  let outputs : _ Axi4_cache.O.t = Cyclesim.outputs ~clock_edge:Before sim in
  let ch_rx = List.nth_exn outputs.response.read_response ch in
  let ch_tx = inputs.requests.selected_read_ch in
  let rec wait_for_ready timeout =
    ch_tx.valid := vdd;
    ch_tx.data.address := of_unsigned_int ~width:(Axi_config.addr_bits - 2) address;
    inputs.requests.which_read_ch := of_unsigned_int ~width:3 ch;
    Cyclesim.cycle sim;
    if timeout = 0 then raise_s [%message "BUG: Timeout"];
    if to_bool !(outputs.read_ready)
    then (
      ch_tx.valid := gnd;
      ())
    else wait_for_ready (timeout - 1)
  in
  let rec wait_for_data timeout =
    if timeout = 0 then raise_s [%message "BUG: Timeout"];
    Cyclesim.cycle sim;
    if to_bool !(ch_rx.valid)
    then to_int_trunc !(ch_rx.value.read_data)
    else wait_for_data (timeout - 1)
  in
  wait_for_ready 1000;
  wait_for_data 1000
;;

let stats sim =
  Cyclesim.cycle sim;
  let outputs : _ Axi4_cache.O.t = Cyclesim.outputs ~clock_edge:After sim in
  (Axi4_cache.O.map ~f:( ! ) outputs |> Axi4_cache.O.map ~f:Bits.to_int_trunc).statistics
;;

let read_and_assert ~address ~value ~ch sim =
  let result = read ~address ~ch sim in
  if result <> value
  then
    raise_s
      [%message "BUG: Received" (result : int) "expected" (value : int) (address : int)]
;;

let%expect_test "manufactured miss" =
  print_s [%message "Config width" (Axi_config.addr_bits : int)];
  create_sim (fun ~inputs ~outputs:_ sim ->
    inputs.clock.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clock.clear := gnd;
    let addr1 = 0 in
    let addr2 = 8 * 8 in
    let cache_address_to_hashed_line_address_generic =
      Axi4_cache.cache_address_to_hashed_line_address_generic (module Bits)
    in
    (* Check these addresses collide in our hashfn *)
    assert (
      Bits.(
        cache_address_to_hashed_line_address_generic (of_unsigned_int ~width:8 addr1)
        ==: cache_address_to_hashed_line_address_generic (of_unsigned_int ~width:8 addr2)
        |> to_bool));
    write ~timeout:1000 ~address:addr1 ~value:1234 ~ch:0 sim;
    write ~timeout:1000 ~address:(addr1 + 1) ~value:999 ~ch:0 sim;
    write ~timeout:1000 ~address:(addr1 + 2) ~value:111 ~ch:0 sim;
    write ~timeout:1000 ~address:(addr1 + 3) ~value:333 ~ch:0 sim;
    write ~timeout:1000 ~address:(addr1 + 4) ~value:39 ~ch:0 sim;
    write ~timeout:1000 ~address:addr2 ~value:4321 ~ch:0 sim;
    read_and_assert ~address:addr1 ~value:1234 ~ch:0 sim;
    read_and_assert ~address:(addr1 + 1) ~value:999 ~ch:0 sim;
    read_and_assert ~address:(addr1 + 2) ~value:111 ~ch:0 sim;
    read_and_assert ~address:(addr1 + 3) ~value:333 ~ch:0 sim;
    read_and_assert ~address:(addr1 + 4) ~value:39 ~ch:0 sim;
    read_and_assert ~address:addr2 ~value:4321 ~ch:0 sim;
    print_s [%message (stats sim : int Axi4_cache.Request_stage.Statistics.t)]);
  [%expect
    {|
    ("Config width" (Axi_config.addr_bits 10))
    ("stats sim"
     ((incoming 12) (incoming_write 6) (incoming_need_to_write_back 2)
      (incoming_hit 4) (total_cycles 104) (locked_cycles 78)))
    |}]
;;

let%expect_test "burst of linear writes" =
  print_s [%message "Config width" (Axi_config.addr_bits : int)];
  create_sim (fun ~inputs ~outputs:_ sim ->
    inputs.clock.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clock.clear := gnd;
    (* Write the entire ram a few times. *)
    Sequence.range 0 8
    |> Sequence.iter ~f:(fun _round ->
      Sequence.range 0 (capacity_in_bytes / cell_bytes)
      |> Sequence.iter ~f:(fun cell ->
        write ~timeout:1000 ~address:cell ~value:cell ~ch:0 sim));
    (* Write different values to the ram *)
    Sequence.range 0 (capacity_in_bytes / cell_bytes)
    |> Sequence.iter ~f:(fun cell ->
      write ~timeout:1000 ~address:cell ~value:(cell + 1) ~ch:0 sim);
    (* Finally check the values we just wrote are correct. *)
    Sequence.range 0 (capacity_in_bytes / cell_bytes)
    |> Sequence.iter ~f:(fun cell ->
      read_and_assert ~address:cell ~value:(cell + 1) ~ch:0 sim);
    print_s [%message (stats sim : int Axi4_cache.Request_stage.Statistics.t)]);
  [%expect
    {|
    ("Config width" (Axi_config.addr_bits 10))
    ("stats sim"
     ((incoming 2560) (incoming_write 2304) (incoming_need_to_write_back 288)
      (incoming_hit 224) (total_cycles 6413) (locked_cycles 3588)))
    |}]
;;

let%expect_test "loopback" =
  print_s [%message "Config width" (Axi_config.addr_bits : int)];
  let oracle_ram = Array.init ~f:(fun _ -> 0) (capacity_in_bytes / cell_bytes) in
  create_sim (fun ~inputs ~outputs:_ sim ->
    inputs.clock.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clock.clear := gnd;
    let random = Splittable_random.of_int 1 in
    Sequence.range 0 20000
    |> Sequence.iter ~f:(fun _ ->
      let next_write =
        Splittable_random.int ~lo:Int.min_value ~hi:Int.max_value random land 0xFFFFFFFF
      in
      let write_ch = Splittable_random.int ~lo:0 ~hi:(write_channels - 1) random in
      let read_ch = Splittable_random.int ~lo:0 ~hi:(read_channels - 1) random in
      let address_read =
        Splittable_random.int ~lo:0 ~hi:((capacity_in_bytes / cell_bytes) - 1) random
      in
      let address_write =
        Splittable_random.int ~lo:0 ~hi:((capacity_in_bytes / cell_bytes) - 1) random
      in
      if verbose then print_s [%message "Write" (address_write : int) (next_write : int)];
      write ~timeout:1000 ~address:address_write ~value:next_write ~ch:write_ch sim;
      Array.set oracle_ram address_write next_write;
      if verbose then print_s [%message "Read" (address_read : int)];
      read_and_assert
        ~address:address_read
        ~value:(Array.get oracle_ram address_read)
        ~ch:read_ch
        sim);
    print_s [%message (stats sim : int Axi4_cache.Request_stage.Statistics.t)]);
  print_s [%message "Finished"];
  [%expect
    {|
    ("Config width" (Axi_config.addr_bits 10))
    ("stats sim"
     ((incoming 40000) (incoming_write 20000) (incoming_need_to_write_back 17294)
      (incoming_hit 6407) (total_cycles 629124) (locked_cycles 570349)))
    Finished
    |}]
;;
(* TODO: WSTRB Tests for partial (byte) writes. *)
