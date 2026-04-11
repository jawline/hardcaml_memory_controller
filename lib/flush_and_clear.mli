open! Core
open Hardcaml

module Make
    (Ram : Cache_ram_intf.S)
    (Axi : Axi4_intf.S)
    (Memory_requester : Memory_requester_intf.M(Axi).S) : sig
  module I : sig
    type 'a t =
      { clock : 'a Clocking.t
      ; ram : 'a Ram.O.t
      ; memory : 'a Memory_requester.Write.Response.t [@rtlprefix "memory_write_response"]
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { active : 'a
      ; ram_read : 'a Ram.Read.t
      ; ram_write : 'a Ram.Write.t
      ; memory : 'a Memory_requester.Write.Request.t [@rtlprefix "memory_write_request"]
      }
    [@@deriving hardcaml]
  end

  module State : sig
    type t =
      | Fetch
      | Await_flush
      | Await_last_write
      | Finished
    [@@deriving compare ~localize, enumerate, sexp_of]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
