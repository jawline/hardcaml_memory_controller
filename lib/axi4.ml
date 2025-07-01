open Hardcaml_axi

module type S = Axi4_intf.S

module Make (Config : Axi4_xilinx.Config) = struct
  let address_width = Config.addr_bits
  let data_width = Config.data_bits
  let id_width = Config.id_bits

  module I = Axi4_xilinx.Slave_to_master (Config)
  module O = Axi4_xilinx.Master_to_slave (Config)
end
