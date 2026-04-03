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
  module I = struct
    type 'a t =
      { read : 'a Memory_requester.Read.I.t
      ; write : 'a Memory_requester.Write.I.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { read : 'a Memory_requester.Read.O.t
      ; write : 'a Memory_requester.Write.O.t
      }
    [@@deriving hardcaml]
  end

  let create scope ({ I.read = { clock; _ }; _ } as i) =
    let memory = Axi4.O.Of_signal.wires () in
    let ram =
      Memory.hierarchical
        ~build_mode:Simulation
        ~read_latency:10
        scope
        { Memory.I.clock; memory }
    in
    let rd = Memory_requester.Read.hierarchical scope { i.read with axi = ram.memory } in
    let wr =
      Memory_requester.Write.hierarchical scope { i.write with axi = ram.memory }
    in
    Axi4.O.Of_signal.assign memory (Axi4.O.map2 ~f:Signal.( |: ) rd.axi wr.axi);
    { O.read = rd; write = wr }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"top" create input
  ;;
end

module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O)

let issue_write ~address ~data ~wstrb sim =
  Cyclesim.cycle sim;
  let inputs : _ Machine.I.t = Cyclesim.inputs sim in
  let outputs : Bits.t ref Machine.O.t = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.write.request.valid := vdd;
  inputs.write.request.address := of_unsigned_int ~width:Axi_config.addr_bits address;
  inputs.write.request.write_data := of_unsigned_int ~width:data_width data;
  inputs.write.request.wstrb := of_unsigned_int ~width:(data_width / 8) wstrb;
  Cyclesim.cycle sim;
  inputs.write.request.valid := gnd;
  let rec loop () =
    if to_bool !(outputs.write.response.finished)
    then ()
    else (
      Cyclesim.cycle sim;
      loop ())
  in
  loop ()
;;

let issue_read ~address sim =
  Cyclesim.cycle sim;
  let inputs : _ Machine.I.t = Cyclesim.inputs sim in
  let outputs : Bits.t ref Machine.O.t = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.read.request.valid := vdd;
  inputs.read.request.address := of_unsigned_int ~width:Axi_config.addr_bits address;
  Cyclesim.cycle sim;
  inputs.read.request.valid := gnd;
  let rec loop () =
    if to_bool !(outputs.read.finished)
    then ()
    else (
      Cyclesim.cycle sim;
      loop ())
  in
  loop ();
  Machine.O.map ~f:( ! ) outputs |> Machine.O.map ~f:Bits.to_int_trunc
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
    print_s [%message (read : int Machine.O.t)];
    let read = issue_read ~address:16 sim in
    print_s [%message (read : int Machine.O.t)];
    let read = issue_read ~address:32 sim in
    print_s [%message (read : int Machine.O.t)];
    ());
  [%expect
    {|
    (read
     ((read
       ((finished 1) (address 0) (id 0) (data 3735928559)
        (axi
         ((awaddr 0) (awburst 0) (awid 0) (awlen 0) (awsize 0) (awvalid 0)
          (wlast 0) (wvalid 0) (wdata 0) (wstrb 0) (araddr 0) (arburst 1)
          (arid 0) (arlen 0) (arsize 5) (arvalid 0) (bready 0) (rready 1)))))
      (write
       ((response ((finished 0) (busy 0) (address 32) (id 0)))
        (axi
         ((awaddr 32) (awburst 1) (awid 0) (awlen 0) (awsize 5) (awvalid 0)
          (wlast 1) (wvalid 0) (wdata 1234567) (wstrb 65535) (araddr 0)
          (arburst 0) (arid 0) (arlen 0) (arsize 0) (arvalid 0) (bready 1)
          (rready 0)))))))
    (read
     ((read
       ((finished 1) (address 16) (id 0) (data 286331153)
        (axi
         ((awaddr 0) (awburst 0) (awid 0) (awlen 0) (awsize 0) (awvalid 0)
          (wlast 0) (wvalid 0) (wdata 0) (wstrb 0) (araddr 16) (arburst 1)
          (arid 0) (arlen 0) (arsize 5) (arvalid 0) (bready 0) (rready 1)))))
      (write
       ((response ((finished 0) (busy 0) (address 32) (id 0)))
        (axi
         ((awaddr 32) (awburst 1) (awid 0) (awlen 0) (awsize 5) (awvalid 0)
          (wlast 1) (wvalid 0) (wdata 1234567) (wstrb 65535) (araddr 0)
          (arburst 0) (arid 0) (arlen 0) (arsize 0) (arvalid 0) (bready 1)
          (rready 0)))))))
    (read
     ((read
       ((finished 1) (address 32) (id 0) (data 1234567)
        (axi
         ((awaddr 0) (awburst 0) (awid 0) (awlen 0) (awsize 0) (awvalid 0)
          (wlast 0) (wvalid 0) (wdata 0) (wstrb 0) (araddr 32) (arburst 1)
          (arid 0) (arlen 0) (arsize 5) (arvalid 0) (bready 0) (rready 1)))))
      (write
       ((response ((finished 0) (busy 0) (address 32) (id 0)))
        (axi
         ((awaddr 32) (awburst 1) (awid 0) (awlen 0) (awsize 5) (awvalid 0)
          (wlast 1) (wvalid 0) (wdata 1234567) (wstrb 65535) (araddr 0)
          (arburst 0) (arid 0) (arlen 0) (arsize 0) (arvalid 0) (bready 1)
          (rready 0)))))))
    |}]
;;
