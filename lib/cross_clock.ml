open! Core
open Hardcaml
open Signal
open Hardcaml_custom_handshake

module Make (Memory_bus : Memory_bus_intf.S) = struct
  module Cross_handshake (M : Handshake_intf.S) = struct
    module I = struct
      type 'a t =
        { clocking_i : 'a Clocking.t
        ; clocking_o : 'a Clocking.t
        ; i : 'a M.Source.t
        ; o : 'a M.Dest.t
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { i : 'a M.Source.t
        ; o : 'a M.Dest.t
        }
      [@@deriving hardcaml]
    end

    module Async_fifo = Async_fifo.Make (struct
        let width = M.Source.sum_of_port_widths
        let log2_depth = 3
        let optimize_for_same_clock_rate_and_always_reading = false
      end)

    let create scope (i : _ I.t) =
      let full = wire 1 in
      let empty = wire 1 in
      let fifo =
        Async_fifo.hierarchical
          scope
          { Async_fifo.I.clock_write = i.clocking_i.clock
          ; clock_read = i.clocking_o.clock
          ; reset_write = i.clocking_i.clear
          ; reset_read = i.clocking_o.clear
          ; data_in = M.Source.Of_signal.pack i.i
          ; write_enable = i.i.valid &: ~:full
          ; read_enable = i.o.ready &: ~:empty
          }
      in
      full <-- fifo.full;
      empty <-- ~:(fifo.valid);
      { O.i =
          { M.Source.valid = fifo.valid
          ; data = (M.Source.Of_signal.unpack fifo.data_out).data
          }
      ; o = { ready = ~:full }
      }
    ;;

    let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope create input
    ;;
  end

  module Write = Cross_handshake (Memory_bus.Write_bus)
  module Read = Cross_handshake (Memory_bus.Read_bus)
end
