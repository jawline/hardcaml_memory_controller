open Hardcaml

module type Config = sig
  val num_read_channels : int
  val num_write_channels : int
  val cache_memory : (module Axi4_cache.Config) option
end

module Without_memory (M : Config) (Memory_bus : Memory_bus_intf.S) = struct
  module type S = sig
    module Request : sig
      type 'a t =
        { write_to_controller : 'a Memory_bus.Write_bus.Source.t list
              [@length M.num_write_channels]
        ; read_to_controller : 'a Memory_bus.Read_bus.Source.t list
              [@length M.num_read_channels]
        }
      [@@deriving hardcaml]
    end

    module Response : sig
      type 'a t =
        { write_to_controller : 'a Memory_bus.Write_bus.Dest.t list
              [@length M.num_write_channels]
        ; read_to_controller : 'a Memory_bus.Read_bus.Dest.t list
              [@length M.num_read_channels]
        ; write_response : 'a Memory_bus.Write_response.With_valid.t list
              [@length M.num_write_channels]
        ; read_response : 'a Memory_bus.Read_response.With_valid.t list
              [@length M.num_read_channels]
        }
      [@@deriving hardcaml]
    end
  end
end

module M (M : Config) (Memory_bus : Memory_bus_intf.S) (Axi : Axi4.S) = struct
  module type S = sig
    include Without_memory(M)(Memory_bus).S

    module I : sig
      type 'a t =
        { clock : 'a Clocking.t
        ; request : 'a Request.t
        ; memory : 'a Axi.I.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { response : 'a Response.t
        ; memory : 'a Axi.O.t
        }
      [@@deriving hardcaml]
    end

    val hierarchical
      :  capacity_in_bytes:int
      -> priority_mode:Priority_mode.t
      -> build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t
  end
end
