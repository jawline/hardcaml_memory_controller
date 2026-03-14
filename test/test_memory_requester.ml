open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_memory_controller
open! Bits

let debug = false
let data_width = 128

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
      let synthetic_pushback = 4
    end)
    (Axi4)

module Memory_requester =
  Memory_requester.Make
    (struct
      let id_bits = 4
      let data_bits = data_width
    end)
    (Axi4)

module Machine = struct
  module I = Memory_requester.I
  module O = Memory_requester.O

  let create scope ({ I.clock; _ } as i) =
    let memory = Axi4.O.Of_signal.wires () in
    let ram =
      Memory.hierarchical
        ~build_mode:Simulation
        ~read_latency:10
        scope
        { Memory.I.clock; memory }
    in
    let ctrl = Memory_requester.hierarchical scope { i with axi = ram.memory } in
    Axi4.O.Of_signal.assign memory ctrl.axi;
    ctrl
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"top" create input
  ;;
end

module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O)

let issue_write ~address ~data ~wstrb sim =
  Cyclesim.cycle sim;
  let inputs : _ Memory_requester.I.t = Cyclesim.inputs sim in
  let outputs : Bits.t ref Memory_requester.O.t =
    Cyclesim.outputs ~clock_edge:Before sim
  in
  inputs.request.valid := vdd;
  inputs.request.address := of_unsigned_int ~width:Axi_config.addr_bits address;
  inputs.request.write := vdd;
  inputs.request.write_data := of_unsigned_int ~width:data_width data;
  inputs.request.wstrb := of_unsigned_int ~width:(data_width / 8) wstrb;
  Cyclesim.cycle sim;
  inputs.request.valid := gnd;
  let rec loop () =
    if to_bool !(outputs.finished)
    then ()
    else (
      Cyclesim.cycle sim;
      loop ())
  in
  loop ()
;;

let issue_read ~address sim =
  Cyclesim.cycle sim;
  let inputs : _ Memory_requester.I.t = Cyclesim.inputs sim in
  let outputs : Bits.t ref Memory_requester.O.t =
    Cyclesim.outputs ~clock_edge:Before sim
  in
  inputs.request.valid := vdd;
  inputs.request.address := of_unsigned_int ~width:Axi_config.addr_bits address;
  inputs.request.write := gnd;
  Cyclesim.cycle sim;
  inputs.request.valid := gnd;
  let rec loop () =
    if to_bool !(outputs.finished)
    then ()
    else (
      Cyclesim.cycle sim;
      loop ())
  in
  loop ();
  Memory_requester.O.map ~f:( ! ) outputs |> Memory_requester.O.map ~f:Bits.to_int_trunc
;;

let create_sim f =
  Harness.run
    ~timeout:20_000
    ~waves_config:(Waves_config.to_home_subdirectory_when debug)
    ~create:Machine.hierarchical
    f
;;

let%expect_test "read/write" =
  create_sim (fun ~inputs:_ ~outputs:_ sim ->
    let () = issue_write ~address:0 ~data:0xDEADBEEF ~wstrb:0b1111_1111_1111_1111 sim in
    let () = issue_write ~address:16 ~data:0x11111111 ~wstrb:0b1111_1111_1111_1111 sim in
    let () = issue_write ~address:32 ~data:1234567 ~wstrb:0b1111_1111_1111_1111 sim in
    let read = issue_read ~address:0 sim in
    print_s [%message (read : int Memory_requester.O.t)];
    let read = issue_read ~address:16 sim in
    print_s [%message (read : int Memory_requester.O.t)];
    let read = issue_read ~address:32 sim in
    print_s [%message (read : int Memory_requester.O.t)];
    ());
  [%expect
    {|
    (read
     ((finished 1) (address 0) (id 0) (was_write 0) (data 3735928559)
      (axi
       ((awaddr 0) (awburst 1) (awid 0) (awlen 1) (awsize 5) (awvalid 0)
        (wlast 1) (wvalid 0) (wdata 1234567) (wstrb 65535) (araddr 0) (arburst 1)
        (arid 0) (arlen 1) (arsize 5) (arvalid 0) (bready 1) (rready 1)))))
    (read
     ((finished 1) (address 16) (id 0) (was_write 0) (data 286331153)
      (axi
       ((awaddr 16) (awburst 1) (awid 0) (awlen 1) (awsize 5) (awvalid 0)
        (wlast 1) (wvalid 0) (wdata 1234567) (wstrb 65535) (araddr 16)
        (arburst 1) (arid 0) (arlen 1) (arsize 5) (arvalid 0) (bready 1)
        (rready 1)))))
    (read
     ((finished 1) (address 32) (id 0) (was_write 0) (data 1234567)
      (axi
       ((awaddr 32) (awburst 1) (awid 0) (awlen 1) (awsize 5) (awvalid 0)
        (wlast 1) (wvalid 0) (wdata 1234567) (wstrb 65535) (araddr 32)
        (arburst 1) (arid 0) (arlen 1) (arsize 5) (arvalid 0) (bready 1)
        (rready 1)))))
    |}]
;;
