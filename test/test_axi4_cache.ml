open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_memory_controller
open! Bits

let debug = true
let cell_width = 32
let read_channels = 4
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
  (* Delay a cycle so we know we don't pick up the state of the previous read. *)
  Cyclesim.cycle sim;
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
    inputs.requests.which_read_ch := of_unsigned_int ~width:2 ch;
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

let read_and_assert ~address ~value ~ch sim =
  let result = read ~address ~ch sim in
  if result <> value
  then print_s [%message "BUG: Received" (result : int) "expected" (value : int)]
;;

let%expect_test "read/write" =
  print_s [%message "Config width" (Axi_config.addr_bits : int)];
  create_sim (fun ~inputs ~outputs:_ sim ->
    inputs.clock.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clock.clear := gnd;
    let random = Splittable_random.of_int 1 in
    Sequence.range 0 10000
    |> Sequence.iter ~f:(fun _ ->
      let next =
        Splittable_random.int ~lo:Int.min_value ~hi:Int.max_value random land 0xFFFFFFFF
      in
      let write_ch = Splittable_random.int ~lo:0 ~hi:(write_channels - 1) random in
      let read_ch = Splittable_random.int ~lo:0 ~hi:(read_channels - 1) random in
      let address =
        Splittable_random.int ~lo:0 ~hi:(capacity_in_bytes / cell_width) random
      in
      write ~timeout:1000 ~address ~value:next ~ch:write_ch sim;
      read_and_assert ~address ~value:next ~ch:read_ch sim));
  print_s [%message "Finished"];
  [%expect
    {|
    ("Config width" (Axi_config.addr_bits 10))
    ("width (arb.selected).address" 8)
    ("Width of address:" ("width t" 8))
    "TODO: Use a real hash function"
    ("Width of address:" ("width t" 8))
    ("Width of address:" ("width t" 8))
    ("Width of address:" ("width t" 8))
    "TODO: Use a real hash function"
    (("width onehot" 8) ("width existing_strb" 8))
    ("Width of address:" ("width t" 8))
    ("Width of address:" ("width t" 8))
    "TODO: Use a real hash function"
    (("width mem_op_write_back.address" 5)
     ("width incoming_write_back.address" 5))
    ("BUG: Received" (result 0) expected (value 1434810019))
    ("BUG: Received" (result 1488711876) expected (value 1520457813))
    ("BUG: Received" (result 0) expected (value 297140583))
    ("BUG: Received" (result 0) expected (value 4043316580))
    ("BUG: Received" (result 0) expected (value 496019591))
    ("BUG: Received" (result 0) expected (value 4284186811))
    ("BUG: Received" (result 0) expected (value 387924456))
    ("BUG: Received" (result 0) expected (value 772340941))
    ("BUG: Received" (result 3650086817) expected (value 2201784610))
    ("BUG: Received" (result 3484863643) expected (value 3753995967))
    ("BUG: Received" (result 0) expected (value 3160446301))
    ("BUG: Received" (result 0) expected (value 3054141123))
    ("BUG: Received" (result 0) expected (value 4127705064))
    ("BUG: Received" (result 0) expected (value 667553319))
    ("BUG: Received" (result 0) expected (value 1162001127))
    ("BUG: Received" (result 1502223823) expected (value 2839838342))
    ("BUG: Received" (result 0) expected (value 2534895239))
    ("BUG: Received" (result 0) expected (value 3926062743))
    ("BUG: Received" (result 0) expected (value 973063158))
    ("BUG: Received" (result 0) expected (value 2357397652))
    ("BUG: Received" (result 0) expected (value 2146463993))
    ("BUG: Received" (result 0) expected (value 789025118))
    ("BUG: Received" (result 2346204086) expected (value 2015840196))
    ("BUG: Received" (result 0) expected (value 4237496142))
    ("BUG: Received" (result 0) expected (value 3424772319))
    ("BUG: Received" (result 0) expected (value 2691466828))
    ("BUG: Received" (result 0) expected (value 3765308892))
    ("BUG: Received" (result 1877628155) expected (value 526704686))
    ("BUG: Received" (result 0) expected (value 1885803386))
    ("BUG: Received" (result 0) expected (value 2153589964))
    ("BUG: Received" (result 2855145985) expected (value 1636770557))
    ("BUG: Received" (result 0) expected (value 2376514541))
    ("BUG: Received" (result 0) expected (value 4018064305))
    ("BUG: Received" (result 2855145985) expected (value 3366427219))
    ("BUG: Received" (result 0) expected (value 1312165621))
    ("BUG: Received" (result 0) expected (value 3697079043))
    ("BUG: Received" (result 0) expected (value 3073545695))
    ("BUG: Received" (result 0) expected (value 2452772014))
    ("BUG: Received" (result 0) expected (value 3931785638))
    ("BUG: Received" (result 0) expected (value 1990004832))
    ("BUG: Received" (result 0) expected (value 2819559015))
    ("BUG: Received" (result 0) expected (value 874882458))
    ("BUG: Received" (result 0) expected (value 1305509414))
    ("BUG: Received" (result 0) expected (value 3197347761))
    ("BUG: Received" (result 0) expected (value 3735645621))
    ("BUG: Received" (result 0) expected (value 681037422))
    ("BUG: Received" (result 0) expected (value 2300120369))
    ("BUG: Received" (result 0) expected (value 2789652474))
    ("BUG: Received" (result 2430941894) expected (value 3738488435))
    ("BUG: Received" (result 0) expected (value 3763148498))
    ("BUG: Received" (result 0) expected (value 2917078145))
    ("BUG: Received" (result 0) expected (value 350575332))
    ("BUG: Received" (result 4067654955) expected (value 451975121))
    ("BUG: Received" (result 0) expected (value 244939591))
    ("BUG: Received" (result 0) expected (value 1746031788))
    ("BUG: Received" (result 0) expected (value 245246621))
    ("BUG: Received" (result 0) expected (value 3206824423))
    ("BUG: Received" (result 0) expected (value 1722144310))
    ("BUG: Received" (result 0) expected (value 960131714))
    ("BUG: Received" (result 0) expected (value 80277))
    ("BUG: Received" (result 0) expected (value 4006523943))
    ("BUG: Received" (result 0) expected (value 3486703794))
    ("BUG: Received" (result 0) expected (value 2838174135))
    ("BUG: Received" (result 0) expected (value 1078955527))
    ("BUG: Received" (result 0) expected (value 803605180))
    ("BUG: Received" (result 0) expected (value 960770943))
    ("BUG: Received" (result 0) expected (value 1615870633))
    ("BUG: Received" (result 0) expected (value 3386182529))
    ("BUG: Received" (result 0) expected (value 887565121))
    ("BUG: Received" (result 0) expected (value 1901586005))
    ("BUG: Received" (result 0) expected (value 2868976355))
    ("BUG: Received" (result 0) expected (value 3719504212))
    ("BUG: Received" (result 0) expected (value 2728510565))
    ("BUG: Received" (result 0) expected (value 1785258156))
    ("BUG: Received" (result 117824568) expected (value 3770493006))
    ("BUG: Received" (result 0) expected (value 2124729366))
    ("BUG: Received" (result 0) expected (value 1986716075))
    ("BUG: Received" (result 0) expected (value 3001324289))
    ("BUG: Received" (result 0) expected (value 1466034734))
    ("BUG: Received" (result 0) expected (value 2110291469))
    ("BUG: Received" (result 0) expected (value 2416160061))
    ("BUG: Received" (result 0) expected (value 917166830))
    ("BUG: Received" (result 0) expected (value 1742652023))
    ("BUG: Received" (result 0) expected (value 2227248194))
    ("BUG: Received" (result 3200760933) expected (value 681259620))
    ("BUG: Received" (result 0) expected (value 1153081853))
    ("BUG: Received" (result 0) expected (value 3367623640))
    ("BUG: Received" (result 0) expected (value 1081519476))
    ("BUG: Received" (result 0) expected (value 3728672655))
    ("BUG: Received" (result 0) expected (value 1100955664))
    ("BUG: Received" (result 0) expected (value 3776393970))
    ("BUG: Received" (result 0) expected (value 1967704317))
    ("BUG: Received" (result 0) expected (value 4077240226))
    ("BUG: Received" (result 0) expected (value 1861483700))
    ("BUG: Received" (result 0) expected (value 1276438963))
    ("BUG: Received" (result 0) expected (value 1704153692))
    ("BUG: Received" (result 0) expected (value 1940971294))
    ("BUG: Received" (result 0) expected (value 730325418))
    ("BUG: Received" (result 0) expected (value 3294562439))
    ("BUG: Received" (result 0) expected (value 3271330264))
    ("BUG: Received" (result 0) expected (value 882141979))
    ("BUG: Received" (result 3307634205) expected (value 968462840))
    ("BUG: Received" (result 0) expected (value 2403113210))
    ("BUG: Received" (result 2267746167) expected (value 638279310))
    ("BUG: Received" (result 0) expected (value 3282530032))
    ("BUG: Received" (result 0) expected (value 3260205654))
    ("BUG: Received" (result 0) expected (value 3658845817))
    ("BUG: Received" (result 0) expected (value 3108430726))
    ("BUG: Received" (result 0) expected (value 4044450015))
    ("BUG: Received" (result 0) expected (value 2934017415))
    ("BUG: Received" (result 2280915219) expected (value 3221213174))
    ("BUG: Received" (result 0) expected (value 1096169739))
    ("BUG: Received" (result 0) expected (value 2226178028))
    ("BUG: Received" (result 0) expected (value 192661105))
    ("BUG: Received" (result 0) expected (value 1774733648))
    ("BUG: Received" (result 0) expected (value 2028767162))
    ("BUG: Received" (result 0) expected (value 802576869))
    ("BUG: Received" (result 4154672605) expected (value 663787297))
    ("BUG: Received" (result 4154672605) expected (value 3155080212))
    ("BUG: Received" (result 0) expected (value 16062630))
    ("BUG: Received" (result 644210632) expected (value 926679276))
    ("BUG: Received" (result 644210632) expected (value 3224709592))
    ("BUG: Received" (result 0) expected (value 459403553))
    ("BUG: Received" (result 0) expected (value 2280782208))
    ("BUG: Received" (result 1241854637) expected (value 3924068115))
    ("BUG: Received" (result 0) expected (value 2667191817))
    ("BUG: Received" (result 0) expected (value 1492138526))
    ("BUG: Received" (result 0) expected (value 1381540697))
    ("BUG: Received" (result 0) expected (value 4029396393))
    ("BUG: Received" (result 4154672605) expected (value 2187791388))
    ("BUG: Received" (result 0) expected (value 3159597662))
    ("BUG: Received" (result 1515289453) expected (value 2570515664))
    ("BUG: Received" (result 0) expected (value 3096109748))
    ("BUG: Received" (result 3396067569) expected (value 3878836921))
    ("BUG: Received" (result 0) expected (value 3966738510))
    ("BUG: Received" (result 0) expected (value 3619278409))
    ("BUG: Received" (result 1515289453) expected (value 165009562))
    ("BUG: Received" (result 0) expected (value 793858708))
    ("BUG: Received" (result 753595498) expected (value 1239098540))
    ("BUG: Received" (result 0) expected (value 1640893599))
    ("BUG: Received" (result 0) expected (value 2179184574))
    ("BUG: Received" (result 0) expected (value 3049372545))
    ("BUG: Received" (result 0) expected (value 1088993676))
    ("BUG: Received" (result 0) expected (value 3643107143))
    ("BUG: Received" (result 1980785433) expected (value 1260924287))
    ("BUG: Received" (result 0) expected (value 1656701362))
    ("BUG: Received" (result 0) expected (value 1122460922))
    ("BUG: Received" (result 0) expected (value 3201354646))
    ("BUG: Received" (result 0) expected (value 2911007136))
    ("BUG: Received" (result 0) expected (value 816342452))
    ("BUG: Received" (result 0) expected (value 3499410835))
    ("BUG: Received" (result 0) expected (value 3062373954))
    ("BUG: Received" (result 0) expected (value 20640073))
    ("BUG: Received" (result 1242306234) expected (value 4177309374))
    ("BUG: Received" (result 3102931169) expected (value 1833590519))
    ("BUG: Received" (result 0) expected (value 3544220017))
    ("BUG: Received" (result 0) expected (value 4294749911))
    ("BUG: Received" (result 0) expected (value 2966768780))
    ("BUG: Received" (result 0) expected (value 3075700732))
    ("BUG: Received" (result 0) expected (value 78780483))
    ("BUG: Received" (result 0) expected (value 3140569768))
    ("BUG: Received" (result 0) expected (value 2092320335))
    ("BUG: Received" (result 0) expected (value 2169807753))
    ("BUG: Received" (result 0) expected (value 892230634))
    ("BUG: Received" (result 0) expected (value 1730987642))
    ("BUG: Received" (result 0) expected (value 2146085494))
    ("BUG: Received" (result 0) expected (value 4119709467))
    ("BUG: Received" (result 0) expected (value 1933509299))
    ("BUG: Received" (result 0) expected (value 3242768588))
    ("BUG: Received" (result 0) expected (value 582939930))
    ("BUG: Received" (result 0) expected (value 872327199))
    ("BUG: Received" (result 0) expected (value 820397043))
    ("BUG: Received" (result 2048993188) expected (value 555219737))
    ("BUG: Received" (result 0) expected (value 1712036788))
    ("BUG: Received" (result 0) expected (value 3157689640))
    ("BUG: Received" (result 0) expected (value 3028489874))
    ("BUG: Received" (result 0) expected (value 3655005145))
    ("BUG: Received" (result 3228157624) expected (value 3224664068))
    ("BUG: Received" (result 0) expected (value 1465417374))
    ("BUG: Received" (result 0) expected (value 1897853001))
    ("BUG: Received" (result 0) expected (value 1985454211))
    ("BUG: Received" (result 317646590) expected (value 4213143275))
    ("BUG: Received" (result 0) expected (value 567004977))
    ("BUG: Received" (result 0) expected (value 43097490))
    ("BUG: Received" (result 0) expected (value 1236553844))
    ("BUG: Received" (result 0) expected (value 1384638364))
    ("BUG: Received" (result 0) expected (value 641050287))
    ("BUG: Received" (result 0) expected (value 183522679))
    ("BUG: Received" (result 317646590) expected (value 1840816989))
    ("BUG: Received" (result 0) expected (value 2195586438))
    ("BUG: Received" (result 0) expected (value 4156751917))
    ("BUG: Received" (result 0) expected (value 3931185515))
    ("BUG: Received" (result 0) expected (value 2004526443))
    ("BUG: Received" (result 0) expected (value 2746150946))
    ("BUG: Received" (result 0) expected (value 1045383785))
    ("BUG: Received" (result 0) expected (value 382876366))
    ("BUG: Received" (result 0) expected (value 2043717742))
    ("BUG: Received" (result 0) expected (value 1542500787))
    ("BUG: Received" (result 0) expected (value 747095931))
    ("BUG: Received" (result 0) expected (value 1533806420))
    ("BUG: Received" (result 0) expected (value 3198679648))
    ("BUG: Received" (result 0) expected (value 1591663146))
    ("BUG: Received" (result 0) expected (value 875439311))
    ("BUG: Received" (result 0) expected (value 3333960117))
    ("BUG: Received" (result 0) expected (value 3192061828))
    ("BUG: Received" (result 0) expected (value 930822747))
    ("BUG: Received" (result 0) expected (value 3464575971))
    ("BUG: Received" (result 0) expected (value 531328369))
    ("BUG: Received" (result 0) expected (value 3252956844))
    ("BUG: Received" (result 0) expected (value 4222315318))
    ("BUG: Received" (result 0) expected (value 2556156487))
    ("BUG: Received" (result 0) expected (value 3380553443))
    ("BUG: Received" (result 0) expected (value 1209959942))
    ("BUG: Received" (result 0) expected (value 164292253))
    ("BUG: Received" (result 0) expected (value 278204493))
    ("BUG: Received" (result 4286680860) expected (value 1234124991))
    ("BUG: Received" (result 0) expected (value 2160284672))
    ("BUG: Received" (result 4286680860) expected (value 4112993438))
    ("BUG: Received" (result 0) expected (value 1970719096))
    ("BUG: Received" (result 0) expected (value 1362472904))
    ("BUG: Received" (result 0) expected (value 1572966458))
    ("BUG: Received" (result 0) expected (value 4139592022))
    ("BUG: Received" (result 0) expected (value 3409791922))
    ("BUG: Received" (result 0) expected (value 1832079740))
    ("BUG: Received" (result 3880981439) expected (value 2326512114))
    ("BUG: Received" (result 93312150) expected (value 2070665165))
    ("BUG: Received" (result 0) expected (value 860675678))
    ("BUG: Received" (result 1697008221) expected (value 2514283676))
    ("BUG: Received" (result 0) expected (value 112080262))
    ("BUG: Received" (result 0) expected (value 3098101494))
    ("BUG: Received" (result 4109957933) expected (value 1955623447))
    ("BUG: Received" (result 0) expected (value 1256586930))
    ("BUG: Received" (result 0) expected (value 1738515331))
    ("BUG: Received" (result 0) expected (value 1714220414))
    ("BUG: Received" (result 0) expected (value 1705730902))
    ("BUG: Received" (result 0) expected (value 592165362))
    ("BUG: Received" (result 2812669958) expected (value 1193892385))
    ("BUG: Received" (result 0) expected (value 463150730))
    ("BUG: Received" (result 0) expected (value 2345244874))
    ("BUG: Received" (result 0) expected (value 2038014741))
    ("BUG: Received" (result 0) expected (value 1952777327))
    Saved waves to /var/home/blake/waves//_read_write.hardcamlwaveform
    Finished
    |}]
;;
