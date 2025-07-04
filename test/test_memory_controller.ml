open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_memory_controller
open! Bits

let debug = false

module Make_tests (C : sig
    val num_channels : int
    val read_latency : int
    val synthetic_pushback : int
  end) =
struct
  let data_width = 32

  module Axi_config = struct
    let id_bits = 8
    let data_bits = data_width
    let addr_bits = address_bits_for 128
    let burst_length_bits = 1
  end

  module Axi4 = Axi4.Make (Axi_config)

  module Memory =
    Axi4_bram.Make
      (struct
        let capacity_in_bytes = 128
        let synthetic_pushback = C.synthetic_pushback
      end)
      (Axi4)

  module Memory_controller =
    Memory_controller.Make
      (struct
        let capacity_in_bytes = 128
        let num_read_channels = C.num_channels
        let num_write_channels = C.num_channels
        let address_width = 32
        let data_bus_width = data_width
      end)
      (Axi4)

  module Machine = struct
    module I = Memory_controller.I
    module O = Memory_controller.O

    let create scope ({ I.clock; clear; _ } as i) =
      let memory = Axi4.O.Of_signal.wires () in
      let ram =
        Memory.hierarchical
          ~build_mode:Simulation
          ~read_latency:C.read_latency
          scope
          { Memory.I.clock; clear; memory }
      in
      let ctrl =
        Memory_controller.hierarchical
          ~priority_mode:Priority_order
          scope
          { i with memory = ram.memory }
      in
      Axi4.O.Of_signal.assign memory ctrl.memory;
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
      ~waves_config:(if debug then Waves_config.to_home_subdirectory () else No_waves)
      ~create:Machine.hierarchical
      f
  ;;

  let rec wait_for_write_ack ~timeout ~ch sim =
    if timeout = 0 then raise_s [%message "BUG: Timeout writing ack"];
    let outputs : _ Memory_controller.O.t = Cyclesim.outputs ~clock_edge:Before sim in
    let ch_rx = List.nth_exn outputs.write_response ch in
    Cyclesim.cycle sim;
    if to_bool !(ch_rx.valid)
    then ()
    else wait_for_write_ack ~timeout:(timeout - 1) ~ch sim
  ;;

  let rec write ~timeout ~address ~value ~ch sim =
    if timeout = 0 then raise_s [%message "BUG: Timeout writing"];
    (* Delay a cycle so we know we don't pick up the state of the previous read. *)
    Cyclesim.cycle sim;
    let inputs : _ Memory_controller.I.t = Cyclesim.inputs sim in
    let ch_tx = List.nth_exn inputs.write_to_controller ch in
    ch_tx.valid := vdd;
    ch_tx.data.address := of_unsigned_int ~width:32 address;
    ch_tx.data.write_data := of_unsigned_int ~width:32 value;
    ch_tx.data.wstrb := ones 4;
    let outputs : _ Memory_controller.O.t = Cyclesim.outputs ~clock_edge:Before sim in
    let ch_rx = List.nth_exn outputs.write_to_controller ch in
    Cyclesim.cycle sim;
    if to_bool !(ch_rx.ready)
    then (
      List.iteri
        ~f:(fun i rx ->
          if i <> ch
          then (
            if to_bool !(rx.ready)
            then
              print_s [%message "BUG: We only expect one channel to have a ready signal."];
            ())
          else ())
        outputs.write_to_controller;
      ch_tx.valid := gnd;
      wait_for_write_ack ~timeout:1000 ~ch sim)
    else write ~timeout:(timeout - 1) ~address ~value ~ch sim
  ;;

  let read ~address ~ch sim =
    Cyclesim.cycle sim;
    let inputs : _ Memory_controller.I.t = Cyclesim.inputs sim in
    let outputs : _ Memory_controller.O.t = Cyclesim.outputs ~clock_edge:Before sim in
    let ch_rx = List.nth_exn outputs.read_response ch in
    let ch_rx_ack = List.nth_exn outputs.read_to_controller ch in
    let ch_tx = List.nth_exn inputs.read_to_controller ch in
    let rec wait_for_ready timeout =
      ch_tx.valid := vdd;
      ch_tx.data.address := of_unsigned_int ~width:32 address;
      Cyclesim.cycle sim;
      if timeout = 0 then raise_s [%message "BUG: Timeout"];
      if to_bool !(ch_rx_ack.ready)
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

  let read_and_assert ~address ~value ~ch sim =
    let result = read ~address ~ch sim in
    if result <> value
    then print_s [%message "BUG: Expected" (result : int) "received" (value : int)]
  ;;

  let%expect_test "read/write" =
    create_sim (fun ~inputs ~outputs:_ sim ->
      inputs.clear := vdd;
      Cyclesim.cycle sim;
      inputs.clear := gnd;
      let random = Splittable_random.of_int 1 in
      Sequence.range 0 1000
      |> Sequence.iter ~f:(fun _ ->
        let next =
          Splittable_random.int ~lo:Int.min_value ~hi:Int.max_value random land 0xFFFFFFFF
        in
        let ch = Splittable_random.int ~lo:0 ~hi:(C.num_channels - 1) random in
        let address = Splittable_random.int ~lo:0 ~hi:(128 / data_width) random in
        write ~timeout:1000 ~address ~value:next ~ch sim;
        read_and_assert ~address ~value:next ~ch sim));
    [%expect {| |}]
  ;;

  (* TODO: Fix error reporting 
  let%expect_test "read unaligned" =
    create_sim (fun ~inputs:_ ~outputs:_ sim ->
      let ch = 0 in
      read_and_assert ~assertion:`Error ~address:1 ~value:0 ~ch sim;
      read_and_assert ~assertion:`Error ~address:2 ~value:0 ~ch sim;
      read_and_assert ~assertion:`Error ~address:3 ~value:0 ~ch sim;
      read_and_assert ~assertion:`No_error ~address:4 ~value:0 ~ch sim;
      ());
    [%expect {| |}]
  ;;

  let%expect_test "write unaligned" =
    create_sim (fun ~inputs:_ ~outputs:_ sim ->
      let ch = 0 in
      write ~assertion:`Error ~address:1 ~value:0 ~ch sim;
      write ~assertion:`Error ~address:2 ~value:0 ~ch sim;
      write ~assertion:`Error ~address:3 ~value:0 ~ch sim;
      write ~assertion:`No_error ~address:4 ~value:0 ~ch sim;
      ());
    [%expect {| |}]
  ;; *)
end

include Make_tests (struct
    let num_channels = 1
    let read_latency = 2
    let synthetic_pushback = 1
  end)

include Make_tests (struct
    let num_channels = 2
    let read_latency = 1
    let synthetic_pushback = 7
  end)

include Make_tests (struct
    let num_channels = 3
    let read_latency = 5
    let synthetic_pushback = 0
  end)

include Make_tests (struct
    let num_channels = 7
    let read_latency = 19
    let synthetic_pushback = 4
  end)

(* TODO: Add errors to the memory controller and report them via a side channel. *)

(* TODO: Test byte enables *)
