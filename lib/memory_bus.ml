open Core
open Hardcaml
open Signal
open Hardcaml_custom_handshake

module Make (M : sig
    val address_width : int
    val data_bus_width : int
  end) =
struct
  module Read = struct
    type 'a t = { address : 'a [@bits M.address_width] }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module Write = struct
    type 'a t =
      { address : 'a [@bits M.address_width]
      ; write_data : 'a [@bits M.data_bus_width]
      ; wstrb : 'a [@bits M.data_bus_width / 8]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module Read_response = struct
    module T = struct
      type 'a t = { read_data : 'a [@bits M.data_bus_width] }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    include T
    module With_valid = With_valid.Wrap.Make (T)
  end

  module Write_response = struct
    module T = struct
      type 'a t = { dummy : 'a } [@@deriving hardcaml ~rtlmangle:"$"]
    end

    include T
    module With_valid = With_valid.Wrap.Make (T)
  end

  let address_width = M.address_width
  let data_bus_width = M.data_bus_width
  let data_bus_bytes = data_bus_width / 8

  let byte_address_to_memory_address address =
    let aligned_top = drop_bottom ~width:(address_bits_for data_bus_bytes) address in
    let misaligned_bottom = sel_bottom ~width:(address_bits_for data_bus_bytes) address in
    { With_valid.valid = misaligned_bottom ==:. 0
    ; value = uextend ~width:address_width aligned_top
    }
  ;;

  module Read_bus = Handshake.Make (Read)
  module Write_bus = Handshake.Make (Write)
end
