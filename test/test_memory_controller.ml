open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_memory_controller
open! Bits

let debug = true
let capacity_in_bytes = 65536

module Make_tests (C : sig
    val num_channels : int
    val read_latency : int
    val synthetic_pushback : int
    val cache : bool
  end) =
struct
  let data_width = 32
  let data_bytes = data_width / 8

  module Axi_in_config = struct
    let id_bits = 8
    let data_bits = if C.cache then data_width * 4 else data_width
    let addr_bits = address_bits_for capacity_in_bytes
    let burst_length_bits = 2
  end

  module Axi_out_config = struct
    let id_bits = 1
    let data_bits = if C.cache then data_width * 4 else data_width
    let addr_bits = address_bits_for capacity_in_bytes
    let burst_length_bits = 2
  end

  module Axi4_in = Axi4.Make (Axi_in_config)
  module Axi4_out = Axi4.Make (Axi_out_config)

  module Memory =
    Axi4_bram.Make
      (struct
        let capacity_in_bytes = capacity_in_bytes
        let synthetic_pushback = C.synthetic_pushback
      end)
      (Axi4_out)

  module Memory_controller =
    Memory_controller.Make
      (struct
        let address_width = 32
        let capacity_in_bytes = capacity_in_bytes
        let data_bus_width = data_width

        module Instruction_config = struct
          let num_read_channels = C.num_channels
          let num_write_channels = C.num_channels

          let cache_memory =
            if C.cache
            then
              Some
                (module struct
                  let line_width = 16
                  let num_cache_lines = 256
                  let num_read_channels = C.num_channels
                  let num_write_channels = C.num_channels
                  let register_responses = true
                  let register_axi_requests = true
                end : Axi4_cache.Config)
            else None
          ;;
        end

        module Data_config = struct
          let num_read_channels = C.num_channels
          let num_write_channels = C.num_channels

          let cache_memory =
            if C.cache
            then
              Some
                (module struct
                  let line_width = 16
                  let num_cache_lines = 256
                  let num_read_channels = C.num_channels
                  let num_write_channels = C.num_channels
                  let register_responses = true
                  let register_axi_requests = true
                end : Axi4_cache.Config)
            else None
          ;;
        end
      end)
      (Axi4_in)
      (Axi4_out)

  let addr_bits = Memory_controller.Memory_bus.address_width

  module Machine = struct
    module I = Memory_controller.I
    module O = Memory_controller.O

    let create scope ({ I.clock; _ } as i) =
      let memory = Axi4_out.O.Of_signal.wires () in
      let ram =
        Memory.hierarchical
          ~build_mode:Simulation
          ~read_latency:C.read_latency
          scope
          { Memory.I.clock; memory }
      in
      let ctrl =
        Memory_controller.hierarchical
          ~build_mode:Simulation
          ~priority_mode:Round_robin
          scope
          { i with memory = ram.memory }
      in
      Axi4_out.O.Of_signal.assign memory ctrl.memory;
      ctrl
    ;;

    let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"top" create input
    ;;
  end

  module Harness = Step_harness.Functional.Make_effectful (Machine.I) (Machine.O)
  module Step = Harness.Step

  let create_sim f =
    Harness.run
      ~waves_config:(Waves_config.to_home_subdirectory_when debug)
      ~create:Machine.hierarchical
      f
  ;;

  let rec wait_for_write_ack ~timeout ~ch h =
    if timeout = 0 then raise_s [%message "BUG: Timeout writing ack" (ch : int)];
    let o =
      Step.cycle
        h
        { Step.input_hold with
          data =
            { Step.input_hold.data with
              write_to_controller =
                List.mapi
                  ~f:(fun i v -> if i = ch then { v with valid = gnd } else v)
                  Step.input_hold.data.write_to_controller
            }
        }
    in
    let ch_rx = List.nth_exn o.before_edge.data.write_response ch in
    if to_bool ch_rx.valid then () else wait_for_write_ack ~timeout:(timeout - 1) ~ch h
  ;;

  let rec write ~shared_mem ~timeout ~address ~value ~ch h =
    if timeout = 0 then raise_s [%message "BUG: Timeout writing" (ch : int)];
    let o =
      Step.cycle
        h
        { Step.input_hold with
          data =
            { Step.input_hold.data with
              write_to_controller =
                List.mapi
                  ~f:(fun i v ->
                    if i = ch
                    then
                      { Memory_controller.Memory_bus.Write_bus.Source.valid = vdd
                      ; data =
                          { address = of_unsigned_int ~width:addr_bits address
                          ; write_data = of_unsigned_int ~width:32 value
                          ; wstrb = ones 4
                          }
                      }
                    else v)
                  Step.input_hold.data.write_to_controller
            }
        }
    in
    let ch_tx = List.nth_exn o.before_edge.data.write_to_controller ch in
    let ch_rx = List.nth_exn o.before_edge.data.write_response ch in
    if to_bool ch_tx.ready
    then (
      let () =
        if to_bool ch_rx.valid
        then (
          let _o =
            Step.cycle
              h
              { Step.input_hold with
                data =
                  { Step.input_hold.data with
                    write_to_controller =
                      List.mapi
                        ~f:(fun i v -> if i = ch then { v with valid = gnd } else v)
                        Step.input_hold.data.write_to_controller
                  }
              }
          in
          ())
        else wait_for_write_ack ~timeout:10000 ~ch h
      in
      Array.set shared_mem address value;
      ())
    else write ~shared_mem ~timeout:(timeout - 1) ~address ~value ~ch h
  ;;

  let read_and_assert ~shared_mem ~address ~ch h =
    let cached = ref 0 in
    let rec wait_for_ready timeout =
      let o =
        Step.cycle
          h
          { Step.input_hold with
            data =
              { Step.input_hold.data with
                read_to_controller =
                  List.mapi
                    ~f:(fun i v ->
                      if i = ch
                      then
                        { Memory_controller.Memory_bus.Read_bus.Source.valid = vdd
                        ; data = { address = of_unsigned_int ~width:addr_bits address }
                        }
                      else v)
                    Step.input_hold.data.read_to_controller
              }
          }
      in
      cached := Array.get shared_mem address;
      let ch_tx = List.nth_exn o.before_edge.data.read_to_controller ch in
      if timeout = 0 then raise_s [%message "BUG: Timeout (Read)" (ch : int)];
      if to_bool ch_tx.ready then () else wait_for_ready (timeout - 1)
    in
    let rec wait_for_data timeout =
      if timeout = 0 then raise_s [%message "BUG: Timeout (Read response)"];
      let o =
        Step.cycle
          h
          { Step.input_hold with
            data =
              { Step.input_hold.data with
                read_to_controller =
                  List.mapi
                    ~f:(fun i v -> if i = ch then { v with valid = gnd } else v)
                    Step.input_hold.data.read_to_controller
              }
          }
      in
      let ch_rx = List.nth_exn o.before_edge.data.read_response ch in
      if to_bool ch_rx.valid
      then to_int_trunc ch_rx.value.read_data
      else wait_for_data (timeout - 1)
    in
    wait_for_ready 3000;
    let v = wait_for_data 3000 in
    if v <> !cached
    then
      raise_s
        [%message "BUG: Expected" (address : int) (!cached : int) "received" (v : int)];
    ()
  ;;

  let read_thread ~random ~ch ~num_ops ~shared_mem h =
    let rec loop i =
      if i = 0
      then ()
      else (
        let backpressure = Splittable_random.int ~lo:0 ~hi:(C.num_channels * 3) random in
        let address =
          Splittable_random.int ~lo:0 ~hi:((capacity_in_bytes / data_bytes) - 1) random
        in
        if backpressure = 0
        then (
          read_and_assert ~shared_mem ~address ~ch h;
          loop (i - 1))
        else (
          let _o = Step.cycle h Step.input_hold in
          loop i))
    in
    loop num_ops
  ;;

  let write_thread ~random ~ch ~num_ops ~shared_mem h =
    let rec loop i =
      if i = 0
      then ()
      else (
        let backpressure = Splittable_random.int ~lo:0 ~hi:(C.num_channels * 3) random in
        let address =
          Splittable_random.int ~lo:0 ~hi:((capacity_in_bytes / data_bytes) - 1) random
        in
        let value = Splittable_random.int ~lo:0 ~hi:0xDEADBEEF random in
        if backpressure = 0
        then (
          write ~timeout:5000 ~shared_mem ~address ~value ~ch h;
          loop (i - 1))
        else (
          let _o = Step.cycle h Step.input_hold in
          loop i))
    in
    loop num_ops
  ;;

  let%expect_test "read/write" =
    create_sim (fun h ->
      let _clear =
        Step.cycle
          h
          { Step.input_zero with clock = { Step.input_zero.clock with clear = vdd } }
      in
      let _clear =
        Step.cycle
          h
          { Step.input_zero with clock = { Step.input_zero.clock with clear = gnd } }
      in
      let shared_mem = Array.create ~len:(capacity_in_bytes / 4) 0 in
      let random = Splittable_random.of_int 1 in
      let write_threads =
        List.init
          ~f:(fun i ->
            Step.spawn h (fun h _o ->
              write_thread ~random ~ch:i ~num_ops:1000 ~shared_mem h))
          C.num_channels
      in
      let read_threads =
        List.init
          ~f:(fun i ->
            Step.spawn h (fun h _o ->
              read_thread ~random ~ch:i ~num_ops:1000 ~shared_mem h))
          C.num_channels
      in
      List.iter ~f:(fun t -> Step.wait_for h t) read_threads;
      List.iter ~f:(fun t -> Step.wait_for h t) write_threads;
      ());
    [%expect
      {|
      (* CR expect_test: Test ran multiple times with different test outputs *)
      ============================= Output 1 / 8 ==============================
      Saved waves to /home/ubuntu/waves//_read_write.hardcamlwaveform

      ============================= Output 2 / 8 ==============================
      Saved waves to /home/ubuntu/waves//_read_write_1.hardcamlwaveform

      ============================= Output 3 / 8 ==============================
      Saved waves to /home/ubuntu/waves//_read_write_2.hardcamlwaveform

      ============================= Output 4 / 8 ==============================
      Saved waves to /home/ubuntu/waves//_read_write_3.hardcamlwaveform

      ============================= Output 5 / 8 ==============================
      <expect test ran without test output>
      ============================= Output 6 / 8 ==============================
      <expect test ran without test output>
      ============================= Output 7 / 8 ==============================
      <expect test ran without test output>
      ============================= Output 8 / 8 ==============================
      <expect test ran without test output>
      |}]
  [@@expect.uncaught_exn
    {|
    (* CR expect_test: Test ran multiple times with different uncaught exceptions *)
    ================================= Output 1 / 8 =================================
    <expect test ran without uncaught exception>
    ================================= Output 2 / 8 =================================
    <expect test ran without uncaught exception>
    ================================= Output 3 / 8 =================================
    <expect test ran without uncaught exception>
    ================================= Output 4 / 8 =================================
    <expect test ran without uncaught exception>
    ================================= Output 5 / 8 =================================
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)
    "BUG: Timeout (Read response)"
    Raised at Base__Error.raise in file "src/error.ml", line 15, characters 34-62
    Called from Base__Error.raise_s in file "src/error.ml" (inlined), line 24, characters 48-72
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_and_asse in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 237, characters 26-75
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_and_asse in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 257, characters 12-30
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_thread.l in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 276, characters 10-52
    Called from Hardcaml_step_testbench_effectful__Functional.Make.start in file "effectful/functional.ml", line 77, characters 17-41
    Called from Digital_components__Step_effect.create_component.t.(fun) in file "digital_components/src/step_effect.ml", line 91, characters 24-39
    Re-raised at Digital_components__Step_core.Runner.update_state.handle_eff in file "digital_components/src/step_core.ml", line 192, characters 25-34
    Called from Digital_components__Step_core.Runner.update_state.maybe_stall in file "digital_components/src/step_core.ml" (inlined), line 253, characters 13-17
    Called from Digital_components__Step_core.Runner.update_state in file "digital_components/src/step_core.ml", lines 261-267, characters 8-14
    Called from Digital_components__Component.update_state in file "digital_components/src/component.ml" (inlined), line 42, characters 2-61
    Called from Digital_components__Step_core.Runner.update_state.(fun) in file "digital_components/src/step_core.ml", lines 296-300, characters 11-25
    Called from Base__List0.fold_alloc__'value_or_null_value_or_null_value_or_null_value_or_null in file "src/list0.ml", line 138, characters 22-31
    Called from Digital_components__Step_core.Runner.update_state in file "digital_components/src/step_core.ml", lines 290-302, characters 7-74
    Called from Digital_components__Component.update_state in file "digital_components/src/component.ml" (inlined), line 42, characters 2-61
    Called from Digital_components__Component.create_step_function.(fun) in file "digital_components/src/component.ml", lines 66-71, characters 4-11
    Called from Digital_components__Component.Run_component_until_finished.run_component_until_f in file "digital_components/src/component.ml", line 85, characters 19-37
    Called from Hardcaml_step_testbench_effectful__Functional_cyclesim.Make.run_with_timeout in file "effectful/functional_cyclesim.ml", lines 68-72, characters 4-75
    Called from Hardcaml_test_harness__Step_harness_functional.Make_effectful.run_advanced.(fun) in file "src/step_harness_functional.ml", lines 117-122, characters 8-12
    Called from Base__Exn.protectx in file "src/exn.ml", line 53, characters 8-11
    Re-raised at Base__Exn.raise_with_original_backtrace in file "src/exn.ml" (inlined), line 33, characters 2-50
    Called from Base__Exn.protectx in file "src/exn.ml", line 60, characters 13-49
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.(fun) in file "hardcaml_memory_controller/test/test_memory_controller.ml", lines 307-336, characters 4-9
    Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 350, characters 10-25

    Trailing output
    ---------------
    Saved waves to /home/ubuntu/waves//_read_write_4.hardcamlwaveform

    ================================= Output 6 / 8 =================================
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)
    ("BUG: Timeout (Read)" (ch 0))
    Raised at Base__Error.raise in file "src/error.ml", line 15, characters 34-62
    Called from Base__Error.raise_s in file "src/error.ml" (inlined), line 24, characters 48-72
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_and_asse in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 233, characters 26-77
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_and_asse in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 256, characters 4-23
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_thread.l in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 276, characters 10-52
    Called from Hardcaml_step_testbench_effectful__Functional.Make.start in file "effectful/functional.ml", line 77, characters 17-41
    Called from Digital_components__Step_effect.create_component.t.(fun) in file "digital_components/src/step_effect.ml", line 91, characters 24-39
    Re-raised at Digital_components__Step_core.Runner.update_state.handle_eff in file "digital_components/src/step_core.ml", line 192, characters 25-34
    Called from Digital_components__Step_core.Runner.update_state.maybe_stall in file "digital_components/src/step_core.ml" (inlined), line 253, characters 13-17
    Called from Digital_components__Step_core.Runner.update_state in file "digital_components/src/step_core.ml", lines 261-267, characters 8-14
    Called from Digital_components__Component.update_state in file "digital_components/src/component.ml" (inlined), line 42, characters 2-61
    Called from Digital_components__Step_core.Runner.update_state.(fun) in file "digital_components/src/step_core.ml", lines 296-300, characters 11-25
    Called from Base__List0.fold_alloc__'value_or_null_value_or_null_value_or_null_value_or_null in file "src/list0.ml", line 138, characters 22-31
    Called from Digital_components__Step_core.Runner.update_state in file "digital_components/src/step_core.ml", lines 290-302, characters 7-74
    Called from Digital_components__Component.update_state in file "digital_components/src/component.ml" (inlined), line 42, characters 2-61
    Called from Digital_components__Component.create_step_function.(fun) in file "digital_components/src/component.ml", lines 66-71, characters 4-11
    Called from Digital_components__Component.Run_component_until_finished.run_component_until_f in file "digital_components/src/component.ml", line 85, characters 19-37
    Called from Hardcaml_step_testbench_effectful__Functional_cyclesim.Make.run_with_timeout in file "effectful/functional_cyclesim.ml", lines 68-72, characters 4-75
    Called from Hardcaml_test_harness__Step_harness_functional.Make_effectful.run_advanced.(fun) in file "src/step_harness_functional.ml", lines 117-122, characters 8-12
    Called from Base__Exn.protectx in file "src/exn.ml", line 53, characters 8-11
    Re-raised at Base__Exn.raise_with_original_backtrace in file "src/exn.ml" (inlined), line 33, characters 2-50
    Called from Base__Exn.protectx in file "src/exn.ml", line 60, characters 13-49
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.(fun) in file "hardcaml_memory_controller/test/test_memory_controller.ml", lines 307-336, characters 4-9
    Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 350, characters 10-25

    Trailing output
    ---------------
    Saved waves to /home/ubuntu/waves//_read_write_5.hardcamlwaveform

    ================================= Output 7 / 8 =================================
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)
    ("BUG: Timeout (Read)" (ch 2))
    Raised at Base__Error.raise in file "src/error.ml", line 15, characters 34-62
    Called from Base__Error.raise_s in file "src/error.ml" (inlined), line 24, characters 48-72
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_and_asse in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 233, characters 26-77
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_and_asse in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 256, characters 4-23
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_thread.l in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 276, characters 10-52
    Called from Hardcaml_step_testbench_effectful__Functional.Make.start in file "effectful/functional.ml", line 77, characters 17-41
    Called from Digital_components__Step_effect.create_component.t.(fun) in file "digital_components/src/step_effect.ml", line 91, characters 24-39
    Re-raised at Digital_components__Step_core.Runner.update_state.handle_eff in file "digital_components/src/step_core.ml", line 192, characters 25-34
    Called from Digital_components__Step_core.Runner.update_state.maybe_stall in file "digital_components/src/step_core.ml" (inlined), line 253, characters 13-17
    Called from Digital_components__Step_core.Runner.update_state in file "digital_components/src/step_core.ml", lines 261-267, characters 8-14
    Called from Digital_components__Component.update_state in file "digital_components/src/component.ml" (inlined), line 42, characters 2-61
    Called from Digital_components__Step_core.Runner.update_state.(fun) in file "digital_components/src/step_core.ml", lines 296-300, characters 11-25
    Called from Base__List0.fold_alloc__'value_or_null_value_or_null_value_or_null_value_or_null in file "src/list0.ml", line 138, characters 22-31
    Called from Digital_components__Step_core.Runner.update_state in file "digital_components/src/step_core.ml", lines 290-302, characters 7-74
    Called from Digital_components__Component.update_state in file "digital_components/src/component.ml" (inlined), line 42, characters 2-61
    Called from Digital_components__Component.create_step_function.(fun) in file "digital_components/src/component.ml", lines 66-71, characters 4-11
    Called from Digital_components__Component.Run_component_until_finished.run_component_until_f in file "digital_components/src/component.ml", line 85, characters 19-37
    Called from Hardcaml_step_testbench_effectful__Functional_cyclesim.Make.run_with_timeout in file "effectful/functional_cyclesim.ml", lines 68-72, characters 4-75
    Called from Hardcaml_test_harness__Step_harness_functional.Make_effectful.run_advanced.(fun) in file "src/step_harness_functional.ml", lines 117-122, characters 8-12
    Called from Base__Exn.protectx in file "src/exn.ml", line 53, characters 8-11
    Re-raised at Base__Exn.raise_with_original_backtrace in file "src/exn.ml" (inlined), line 33, characters 2-50
    Called from Base__Exn.protectx in file "src/exn.ml", line 60, characters 13-49
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.(fun) in file "hardcaml_memory_controller/test/test_memory_controller.ml", lines 307-336, characters 4-9
    Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 350, characters 10-25

    Trailing output
    ---------------
    Saved waves to /home/ubuntu/waves//_read_write_6.hardcamlwaveform

    ================================= Output 8 / 8 =================================
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)
    ("BUG: Timeout (Read)" (ch 3))
    Raised at Base__Error.raise in file "src/error.ml", line 15, characters 34-62
    Called from Base__Error.raise_s in file "src/error.ml" (inlined), line 24, characters 48-72
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_and_asse in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 233, characters 26-77
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_and_asse in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 256, characters 4-23
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.read_thread.l in file "hardcaml_memory_controller/test/test_memory_controller.ml", line 276, characters 10-52
    Called from Hardcaml_step_testbench_effectful__Functional.Make.start in file "effectful/functional.ml", line 77, characters 17-41
    Called from Digital_components__Step_effect.create_component.t.(fun) in file "digital_components/src/step_effect.ml", line 91, characters 24-39
    Re-raised at Digital_components__Step_core.Runner.update_state.handle_eff in file "digital_components/src/step_core.ml", line 192, characters 25-34
    Called from Digital_components__Step_core.Runner.update_state.maybe_stall in file "digital_components/src/step_core.ml" (inlined), line 253, characters 13-17
    Called from Digital_components__Step_core.Runner.update_state in file "digital_components/src/step_core.ml", lines 261-267, characters 8-14
    Called from Digital_components__Component.update_state in file "digital_components/src/component.ml" (inlined), line 42, characters 2-61
    Called from Digital_components__Step_core.Runner.update_state.(fun) in file "digital_components/src/step_core.ml", lines 296-300, characters 11-25
    Called from Base__List0.fold_alloc__'value_or_null_value_or_null_value_or_null_value_or_null in file "src/list0.ml", line 138, characters 22-31
    Called from Digital_components__Step_core.Runner.update_state in file "digital_components/src/step_core.ml", lines 290-302, characters 7-74
    Called from Digital_components__Component.update_state in file "digital_components/src/component.ml" (inlined), line 42, characters 2-61
    Called from Digital_components__Component.create_step_function.(fun) in file "digital_components/src/component.ml", lines 66-71, characters 4-11
    Called from Digital_components__Component.Run_component_until_finished.run_component_until_f in file "digital_components/src/component.ml", line 85, characters 19-37
    Called from Hardcaml_step_testbench_effectful__Functional_cyclesim.Make.run_with_timeout in file "effectful/functional_cyclesim.ml", lines 68-72, characters 4-75
    Called from Hardcaml_test_harness__Step_harness_functional.Make_effectful.run_advanced.(fun) in file "src/step_harness_functional.ml", lines 117-122, characters 8-12
    Called from Base__Exn.protectx in file "src/exn.ml", line 53, characters 8-11
    Re-raised at Base__Exn.raise_with_original_backtrace in file "src/exn.ml" (inlined), line 33, characters 2-50
    Called from Base__Exn.protectx in file "src/exn.ml", line 60, characters 13-49
    Called from Hardcaml_memory_controller_test__Test_memory_controller.Make_tests.(fun) in file "hardcaml_memory_controller/test/test_memory_controller.ml", lines 307-336, characters 4-9
    Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 350, characters 10-25

    Trailing output
    ---------------
    Saved waves to /home/ubuntu/waves//_read_write_7.hardcamlwaveform
    |}]
  ;;
end

include Make_tests (struct
    let num_channels = 1
    let read_latency = 2
    let synthetic_pushback = 1
    let cache = false
  end)

include Make_tests (struct
    let num_channels = 2
    let read_latency = 1
    let synthetic_pushback = 7
    let cache = false
  end)

include Make_tests (struct
    let num_channels = 3
    let read_latency = 5
    let synthetic_pushback = 0
    let cache = false
  end)

include Make_tests (struct
    let num_channels = 7
    let read_latency = 19
    let synthetic_pushback = 4
    let cache = false
  end)

include Make_tests (struct
    let num_channels = 1
    let read_latency = 2
    let synthetic_pushback = 1
    let cache = true
  end)

include Make_tests (struct
    let num_channels = 2
    let read_latency = 1
    let synthetic_pushback = 7
    let cache = true
  end)

include Make_tests (struct
    let num_channels = 3
    let read_latency = 3
    let synthetic_pushback = 0
    let cache = true
  end)

include Make_tests (struct
    let num_channels = 7
    let read_latency = 13
    let synthetic_pushback = 3
    let cache = true
  end)
