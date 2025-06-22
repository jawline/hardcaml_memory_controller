open Core
open Hardcaml
open Hardcaml_xilinx
open Signal

module Make
    (M : sig
       val capacity_in_bytes : int
     end)
    (Axi_config : Axi4_config_intf.Config)
    (Axi : Axi4_intf.M(Axi_config).S) =
struct
  let () =
    if Axi_config.data_width % 8 <> 0
    then raise_s [%message "BUG: data bus must be in bytes"]
  ;;

  let data_bus_in_bytes = Axi_config.data_width / 8

  let () =
    if M.capacity_in_bytes % data_bus_in_bytes <> 0
    then
      raise_s
        [%message
          "BUG: cannot request a capacity that is not a multiple of data_bus_width"]
  ;;

  let capacity_in_words = M.capacity_in_bytes / data_bus_in_bytes

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; axi : 'a Axi.O.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { axi : 'a Axi.I.t } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create ~build_mode ~read_latency scope ({ clock; clear; axi } : _ I.t) =
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let memory =
      Simple_dual_port_ram.create
        ~simulation_name:"main_memory_bram"
        ~size:capacity_in_words
        ~build_mode
        ~clock
        ~clear
        ~write_enable:(repeat ~count:(width axi.wstrb) axi.awvalid &: axi.wstrb)
        ~write_address:axi.awaddr
        ~data:axi.wdata
        ~read_enable:axi.arvalid
        ~read_address:axi.araddr
        ()
    in
    let%hw read_data = memory in
    { O.axi =
        { Axi.I.bvalid = reg reg_spec axi.awvalid
        ; bid = reg reg_spec axi.awid
        ; bresp = zero 2
        ; rvalid = pipeline ~n:read_latency reg_spec axi.arvalid
        ; rid = pipeline ~n:read_latency reg_spec axi.arid
        ; rdata = read_data
        ; rresp = zero 2
        ; wready = vdd
        ; rready = vdd
        ; rlast = vdd
        }
    }
  ;;

  let hierarchical ~build_mode ~read_latency (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"axi4_bram" (create ~build_mode ~read_latency) input
  ;;
end
