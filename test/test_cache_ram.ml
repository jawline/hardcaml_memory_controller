open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_memory_controller
open! Bits

let debug = false
let cell_width = 32
let line_size = 8

module Ram = Cache_ram.Make (struct
    let cell_width = cell_width
    let line_size = line_size
    let num_cache_lines = 512
    let memory_address_width = 32
  end)

module Harness = Cyclesim_harness.Make (Ram.I) (Ram.O)

let create_sim f =
  Harness.run
    ~waves_config:(Waves_config.to_home_subdirectory_when debug)
    ~create:(Ram.hierarchical ~build_mode:Simulation)
    f
;;

let write ~cache_line ~address ~wstrb ~datas sim =
  let inputs : _ Ram.I.t = Cyclesim.inputs sim in
  inputs.write.valid := vdd;
  inputs.write.cache_address := of_unsigned_int ~width:Ram.cache_address_width cache_line;
  inputs.write.address := of_unsigned_int ~width:32 address;
  List.iter
    ~f:(fun (data, v) -> data := of_unsigned_int ~width:cell_width v)
    (List.zip_exn inputs.write.datas datas);
  inputs.write.wstrb := of_unsigned_int ~width:line_size wstrb;
  Cyclesim.cycle sim;
  inputs.write.valid := vdd
;;

let read ~cache_line sim =
  let inputs : _ Ram.I.t = Cyclesim.inputs sim in
  inputs.read.valid := vdd;
  inputs.read.cache_address := of_unsigned_int ~width:Ram.cache_address_width cache_line;
  Cyclesim.cycle sim;
  let outputs : _ Ram.O.t = Cyclesim.outputs ~clock_edge:After sim in
  Ram.O.map ~f:( ! ) outputs |> Ram.O.map ~f:Bits.to_int_trunc
;;

let read_write ~cache_line ~address ~datas ~wstrb sim =
  write ~cache_line ~address ~wstrb ~datas sim;
  print_s [%message (read ~cache_line:1 sim : int Ram.O.t)]
;;

let%expect_test "basic read/write" =
  create_sim (fun ~inputs ~outputs:_ sim ->
    inputs.clock.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clock.clear := gnd;
    read_write
      ~cache_line:1
      ~address:0xDEADBEEF
      ~wstrb:0b1111_1111
      ~datas:[ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ]
      sim;
    read_write
      ~cache_line:1
      ~address:0xDEADBEEF
      ~wstrb:0b1111_1111
      ~datas:[ 0x1; 0; 0xFF; 0x3; 0x6; 0x4; 0x9; 0x3 ]
      sim;
    read_write
      ~cache_line:1
      ~address:0xAFAFAF
      ~wstrb:0b0011_1100
      ~datas:[ 0xFF; 0; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ]
      sim);
  [%expect
    {|
    ("read ~cache_line:1 sim"
     ((meta ((valid 1) (address 3735928559) (strb 255) (dirty 0)))
      (read_data (255 255 255 255 255 255 255 255))))
    ("read ~cache_line:1 sim"
     ((meta ((valid 1) (address 3735928559) (strb 255) (dirty 0)))
      (read_data (1 0 255 3 6 4 9 3))))
    ("read ~cache_line:1 sim"
     ((meta ((valid 1) (address 11513775) (strb 60) (dirty 0)))
      (read_data (1 0 255 255 255 255 9 3))))
    |}]
;;
