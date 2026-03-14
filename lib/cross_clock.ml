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
        ; i : 'a M.Source.t [@rtlprefix "input$"]
        ; o : 'a M.Dest.t [@rtlprefix "input$"]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { i : 'a M.Source.t [@rtlprefix "output$"]
        ; o : 'a M.Dest.t [@rtlprefix "output$"]
        }
      [@@deriving hardcaml]
    end

    module Async_fifo = Async_fifo.Make (struct
        let width = M.Source.sum_of_port_widths
        let log2_depth = 3
        let optimize_for_same_clock_rate_and_always_reading = false
      end)

    let create ~build_mode scope (i : _ I.t) =
      let full = wire 1 in
      let empty = wire 1 in
      let fifo =
        let i =
          { Async_fifo.I.clock_write = i.clocking_i.clock
          ; clock_read = i.clocking_o.clock
          ; reset_write = i.clocking_i.clear
          ; reset_read = i.clocking_o.clear
          ; data_in = M.Source.Of_signal.pack i.i
          ; write_enable = i.i.valid &: ~:full
          ; read_enable = i.o.ready &: ~:empty
          }
        in
        match build_mode with
        | Build_mode.Synthesis -> Async_fifo.hierarchical ~name:"async_fifo" scope i
        | Simulation ->
          Async_fifo.For_testing
          .create_with_synchronous_clear_semantics_for_simulation_only
            ~scope
            i
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

    let hierarchical ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"cross_handshake" (create ~build_mode) input
    ;;
  end

  module Cross_response (W : With_valid.Wrap.S) = struct
    module I = struct
      type 'a t =
        { clocking_i : 'a Clocking.t
        ; clocking_o : 'a Clocking.t
        ; i : 'a W.t [@rtlprefix "input$"]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { i : 'a W.t [@rtlprefix "output$"] } [@@deriving hardcaml]
    end

    module Async_fifo = Async_fifo.Make (struct
        let width = W.sum_of_port_widths
        let log2_depth = 3
        let optimize_for_same_clock_rate_and_always_reading = false
      end)

    let create ~build_mode scope (i : _ I.t) =
      let full = wire 1 in
      let empty = wire 1 in
      let fifo =
        let i =
          { Async_fifo.I.clock_write = i.clocking_i.clock
          ; clock_read = i.clocking_o.clock
          ; reset_write = i.clocking_i.clear
          ; reset_read = i.clocking_o.clear
          ; data_in = W.Of_signal.pack i.i
          ; write_enable = i.i.valid &: ~:full
          ; read_enable = ~:empty
          }
        in
        match build_mode with
        | Build_mode.Synthesis -> Async_fifo.hierarchical ~name:"async_fifo" scope i
        | Simulation ->
          Async_fifo.For_testing
          .create_with_synchronous_clear_semantics_for_simulation_only
            ~scope
            i
      in
      full <-- fifo.full;
      empty <-- ~:(fifo.valid);
      { O.i =
          { With_valid.valid = fifo.valid
          ; value = (W.Of_signal.unpack fifo.data_out).value
          }
      }
    ;;

    let hierarchical ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"cross_response" (create ~build_mode) input
    ;;
  end

  module Write = Cross_handshake (Memory_bus.Write_bus)
  module Read = Cross_handshake (Memory_bus.Read_bus)

  module Write_response = Cross_response (struct
      (* TODO: I don't understand why we need to provide 'a value here *)
      include Memory_bus.Write_response.With_valid

      type 'a value = 'a Memory_bus.Write_response.t
    end)

  module Read_response = Cross_response (struct
      include Memory_bus.Read_response.With_valid

      type 'a value = 'a Memory_bus.Read_response.t
    end)

  let maybe_cross_read_request
        ~build_mode
        ~clock_domain_memory
        ~clock_domain_user
        scope
        (i : _ Read.I.t)
    =
    if Custom_clock_domain.equal clock_domain_memory clock_domain_user
    then { Read.O.i = i.i; o = i.o }
    else Read.hierarchical ~build_mode scope i
  ;;

  let maybe_cross_read_response
        ~build_mode
        ~clock_domain_memory
        ~clock_domain_user
        scope
        (i : _ Read_response.I.t)
    =
    if Custom_clock_domain.equal clock_domain_memory clock_domain_user
    then { Read_response.O.i = i.i }
    else Read_response.hierarchical ~build_mode scope i
  ;;

  let maybe_cross_write_request
        ~build_mode
        ~clock_domain_memory
        ~clock_domain_user
        scope
        (i : _ Write.I.t)
    =
    if Custom_clock_domain.equal clock_domain_memory clock_domain_user
    then { Write.O.i = i.i; o = i.o }
    else Write.hierarchical ~build_mode scope i
  ;;

  let maybe_cross_write_response
        ~build_mode
        ~clock_domain_memory
        ~clock_domain_user
        scope
        (i : _ Write_response.I.t)
    =
    if Custom_clock_domain.equal clock_domain_memory clock_domain_user
    then { Write_response.O.i = i.i }
    else Write_response.hierarchical ~build_mode scope i
  ;;
end
