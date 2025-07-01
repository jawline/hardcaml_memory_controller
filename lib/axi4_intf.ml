open Hardcaml_axi

module type S = sig
  val address_width : int
  val data_width : int
  val id_width : int

  module I : Axi4_xilinx.Slave_to_master
  module O : Axi4_xilinx.Master_to_slave
end
