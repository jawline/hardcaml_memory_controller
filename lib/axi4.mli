open Hardcaml_axi

module type S = Axi4_intf.S

module Make (Config : Axi4_xilinx.Config) : Axi4_intf.S
