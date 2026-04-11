open! Core
open! Hardcaml

module type Config = sig
  val line_width : int
  val num_cache_lines : int
  val num_read_channels : int
  val num_write_channels : int
  val register_responses : bool
  val register_axi_requests : bool
end

module Make (Config : Config) (Memory_bus : Memory_bus_intf.S) (Axi4_out : Axi4.S) : sig
  module Memory_requests : sig
    type 'a t =
      { which_read_ch : 'a [@bits address_bits_for num_read_channels]
      ; selected_read_ch : 'a Memory_bus.Read_bus.Source.t
      ; which_write_ch : 'a [@bits address_bits_for num_write_channels]
      ; selected_write_ch : 'a Memory_bus.Write_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  module Memory_responses : sig
    type 'a t =
      { read_response : 'a Memory_bus.Read_response.With_valid.t list
            [@length num_read_channels]
      ; write_response : 'a Memory_bus.Write_response.With_valid.t list
            [@length num_write_channels]
      }
    [@@deriving hardcaml]
  end

  module Statistics : sig
    type 'a t =
      { incoming : 'a [@bits 32]
      ; incoming_write : 'a [@bits 32]
      ; incoming_need_to_write_back : 'a [@bits 32]
      ; incoming_hit : 'a [@bits 32]
      ; total_cycles : 'a [@bits 32]
      ; locked_cycles : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  module I : sig
    type 'a t =
      { clock : 'a Clocking.t
      ; flush : 'a
      ; requests : 'a Memory_requests.t
      ; dn : 'a Axi4_out.I.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { response : 'a Memory_responses.t
      ; dn : 'a Axi4_out.O.t
      ; read_ready : 'a
      ; write_ready : 'a
      ; locked : 'a
      ; statistics : 'a Statistics.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
