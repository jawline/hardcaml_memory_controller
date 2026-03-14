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

    val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
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

    val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
  end

  module Write_response : sig
    module I : sig
      type 'a t =
        { clocking_i : 'a Clocking.t
        ; clocking_o : 'a Clocking.t
        ; i : 'a Memory_bus.Write_response.With_valid.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t = { i : 'a Memory_bus.Write_response.With_valid.t } [@@deriving hardcaml]
    end

    val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
  end

  module Read_response : sig
    module I : sig
      type 'a t =
        { clocking_i : 'a Clocking.t
        ; clocking_o : 'a Clocking.t
        ; i : 'a Memory_bus.Read_response.With_valid.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t = { i : 'a Memory_bus.Read_response.With_valid.t } [@@deriving hardcaml]
    end

    val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
  end

  val maybe_cross_read_request
    :  build_mode:Build_mode.t
    -> clock_domain_memory:Custom_clock_domain.t
    -> clock_domain_user:Custom_clock_domain.t
    -> Scope.t
    -> Signal.t Read.I.t
    -> Signal.t Read.O.t

  val maybe_cross_read_response
    :  build_mode:Build_mode.t
    -> clock_domain_memory:Custom_clock_domain.t
    -> clock_domain_user:Custom_clock_domain.t
    -> Scope.t
    -> Signal.t Read_response.I.t
    -> Signal.t Read_response.O.t

  val maybe_cross_write_request
    :  build_mode:Build_mode.t
    -> clock_domain_memory:Custom_clock_domain.t
    -> clock_domain_user:Custom_clock_domain.t
    -> Scope.t
    -> Signal.t Write.I.t
    -> Signal.t Write.O.t

  val maybe_cross_write_response
    :  build_mode:Build_mode.t
    -> clock_domain_memory:Custom_clock_domain.t
    -> clock_domain_user:Custom_clock_domain.t
    -> Scope.t
    -> Signal.t Write_response.I.t
    -> Signal.t Write_response.O.t
end
