open! Core
open Hardcaml

module M (Axi : Axi4.S) = struct
  module type S = sig
    module Write : sig
      module Request : sig
        type 'a t =
          { valid : 'a
          ; address : 'a
          ; id : 'a
          ; wstrb : 'a
          ; write_data : 'a
          }
        [@@deriving hardcaml]
      end

      module Response : sig
        type 'a t =
          { finished : 'a
          ; busy : 'a
          ; address : 'a [@bits byte_address_width]
          ; id : 'a [@bits id_bits]
          } [@@deriving hardcaml]
      end

      module I : sig
        type 'a t =
          { clock : 'a Clocking.t
          ; request : 'a Request.t
          ; axi : 'a Axi.I.t
          }
        [@@deriving hardcaml]
      end

      module O : sig
        type 'a t =
          { response : 'a Response.t
          ; axi : 'a Axi.O.t
          }
        [@@deriving hardcaml]
      end

      val create : Scope.t -> Signal.t I.t -> Signal.t O.t
      val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
    end

    module Read : sig
      module Request : sig
        type 'a t =
          { valid : 'a
          ; address : 'a
          ; id : 'a
          }
        [@@deriving hardcaml]
      end

      module I : sig
        type 'a t =
          { clock : 'a Clocking.t
          ; request : 'a Request.t
          ; axi : 'a Axi.I.t
          }
        [@@deriving hardcaml]
      end

      module O : sig
        type 'a t =
          { finished : 'a
          ; address : 'a
          ; id : 'a
          ; data : 'a
          ; axi : 'a Axi.O.t
          }
        [@@deriving hardcaml]
      end

      val create : Scope.t -> Signal.t I.t -> Signal.t O.t
      val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
    end
  end
end
