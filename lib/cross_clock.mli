open! Core
open Hardcaml

module Make (Memory_bus : Memory_bus_intf.S) : sig
  module Write : sig
    module I : sig
      type 'a t =
        { clocking_i : 'a Clocking.t
        ; clocking_o : 'a Clocking.t
        ; i : 'a Memory_bus.Write_bus.Source.t
        ; o : 'a Memory_bus.Write_bus.Dest.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { i : 'a Memory_bus.Write_bus.Source.t
        ; o : 'a Memory_bus.Write_bus.Dest.t
        }
      [@@deriving hardcaml]
    end

    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end

  module Read : sig
    module I : sig
      type 'a t =
        { clocking_i : 'a Clocking.t
        ; clocking_o : 'a Clocking.t
        ; i : 'a Memory_bus.Read_bus.Source.t
        ; o : 'a Memory_bus.Read_bus.Dest.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { i : 'a Memory_bus.Read_bus.Source.t
        ; o : 'a Memory_bus.Read_bus.Dest.t
        }
      [@@deriving hardcaml]
    end

    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end
end
