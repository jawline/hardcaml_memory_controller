open Core
open Hardcaml
open Signal

(* This module flushes and clears the caches. We need to do this on start up
     to put the cache RAM into a good state. We also do this on DMA clear to
     flush icaches as instruction code will be rewritten. 
     
     After this module completes the cache will be empty (all invalid) and any
     previous data will have been flushed out to main memory. *)
module Make
    (Ram : Cache_ram_intf.S)
    (Axi : Axi4_intf.S)
    (Memory_requester : Memory_requester_intf.M(Axi).S) =
struct
  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; ram : 'a Ram.O.t
      ; memory : 'a Memory_requester.Write.Response.t [@rtlprefix "memory_write_response"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { active : 'a
      ; ram_read : 'a Ram.Read.t
      ; ram_write : 'a Ram.Write.t
      ; memory : 'a Memory_requester.Write.Request.t [@rtlprefix "memory_write_request"]
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Fetch
      | Await_flush
      | Await_last_write
        (* TODO: We do this because the second stage assumes it is always
             free to write into a write buffer, so if we start doing writes
             while the final flush is going out we can cause issues. Instead we
             should not block here and make the second stage lock until it can
             write. *)
      | Finished
    [@@deriving compare ~localize, enumerate, sexp_of]
  end

  (* TODO: We could interleave the cell fetches and stores with a little more
       RTL which would speed up the module when RAM is fast or no cells are
       dirty. *)

  open Always

  let last_cache_line = Ram.num_cache_lines - 1

  let create scope (i : _ I.t) =
    let reg_spec = Clocking.to_spec i.clock in
    let reg_spec_no_clear = Clocking.to_spec_no_clear i.clock in
    let%hw_var which_cache_line =
      Variable.reg ~width:(address_bits_for Ram.num_cache_lines) reg_spec
    in
    let%hw.State_machine current_state = State_machine.create (module State) reg_spec in
    let%hw need_to_write_main_memory = i.ram.meta.dirty in
    let%hw ready_to_flush_line = ~:need_to_write_main_memory |: ~:(i.memory.busy) in
    compile
      [ current_state.switch
          [ State.Fetch, [ current_state.set_next Await_flush ]
          ; ( Await_flush
            , [ when_
                  ready_to_flush_line
                  [ incr which_cache_line
                  ; if_
                      (which_cache_line.value ==:. last_cache_line)
                      [ current_state.set_next Await_last_write ]
                      [ current_state.set_next Fetch ]
                  ]
              ] )
          ; ( Await_last_write
            , [ when_ ~:(i.memory.busy) [ current_state.set_next Finished ] ] )
          ; ( Finished
            , [ (* Once finished this module will only again become active on clear. *) ]
            )
          ]
      ];
    let active = ~:(current_state.is Finished) in
    let cache_ram_write =
      { Ram.Write.valid = current_state.is Await_flush &: ready_to_flush_line
      ; cache_address = which_cache_line.value
      ; cell_valid = gnd
      ; address = zero Ram.Write.port_widths.address
      ; datas = List.init ~f:(fun _ -> zero Ram.cell_width) Ram.line_width
      ; real_wstrb = zero Ram.Write.port_widths.real_wstrb
      ; meta_wstrb = zero Ram.Write.port_widths.meta_wstrb
      ; dirty = zero Ram.Write.port_widths.dirty
      }
    in
    let main_memory_request =
      { Memory_requester.Write.Request.valid =
          current_state.is Await_flush &: ~:(i.memory.busy) &: need_to_write_main_memory
      ; address =
          sel_bottom
            ~width:Memory_requester.Write.Request.port_widths.address
            (Ram.cache_address_to_byte_address i.ram.meta.address)
      ; id = zero Memory_requester.Write.Request.port_widths.id
      ; wstrb = i.ram.meta.strb
      ; write_data = concat_lsb i.ram.read_data
      }
    in
    { O.active
    ; ram_read = { valid = active; cache_address = which_cache_line.value }
    ; ram_write =
        (* We register the write to ease timings. This is because the final
           write will still have taken effect by the time the first request
           comes in. *)
        Ram.Write.Of_signal.reg reg_spec_no_clear cache_ram_write
    ; memory =
        (* We delay the memory flush by a cycle as the BRAM -> AXI4 MIG path is very tight. *)
        Memory_requester.Write.Request.Of_signal.reg reg_spec_no_clear main_memory_request
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"flush_and_clear" create input
  ;;
end
