open Hardcaml
open Hardcaml_custom_handshake

module type S = sig
  module Read : sig
    type 'a t = { address : 'a } [@@deriving hardcaml]
  end

  module Write : sig
    type 'a t =
      { address : 'a
      ; write_data : 'a
      ; wstrb : 'a
      }
    [@@deriving hardcaml]
  end

  module Read_response : sig
    type 'a t = { read_data : 'a } [@@deriving hardcaml]

    module With_valid : With_valid.Wrap.S with type 'a value := 'a t
  end

  module Write_response : sig
    type 'a t = { dummy : 'a } [@@deriving hardcaml]

    module With_valid : With_valid.Wrap.S with type 'a value := 'a t
  end

  val address_width : int
  val data_bus_width : int
  val byte_address_to_memory_address : Signal.t -> Signal.t With_valid.t

  module Read_bus : Handshake_intf.S with type 'a data = 'a Read.t
  module Write_bus : Handshake_intf.S with type 'a data = 'a Write.t
end
