open Core
open Hardcaml
open Signal

module type Config = sig
  val line_width : int (* in memory cells *)
  val num_cache_lines : int
  val num_read_channels : int
  val num_write_channels : int
end

module Make (Config : Config) (Memory_bus : Memory_bus_intf.S) (Axi4_out : Axi4.S) =
struct
  open Config
  open Memory_bus

  let axi_address_width = Axi4_out.O.port_widths.awaddr
  let cell_width = Memory_bus.Write.port_widths.write_data
  let cell_bytes = cell_width / 8
  let line_size_alignment_bits = address_bits_for (cell_bytes * line_width)
  let cell_to_bytes_bits = address_bits_for cell_bytes
  let line_to_cell_bits = address_bits_for line_width
  let cell_address_width = Memory_bus.Read.port_widths.address
  let byte_to_cell_address = drop_bottom ~width:cell_to_bytes_bits

  let cache_address_to_hashed_line_address_generic
        (type a)
        (module Comb : Comb.S with type t = a)
        (t : a)
    =
    let line_addr_width = Comb.address_bits_for num_cache_lines in
    Comb.uresize ~width:line_addr_width t
  ;;

  let cache_address_to_hashed_line_address =
    cache_address_to_hashed_line_address_generic (module Signal)
  ;;

  let cache_address_to_byte_address t =
    concat_msb [ t; zero (line_to_cell_bits + cell_to_bytes_bits) ]
  ;;

  let ram_metadata_address_width =
    Memory_bus.Read.port_widths.address + cell_to_bytes_bits - line_size_alignment_bits
  ;;

  let cell_to_cache_address t =
    drop_bottom ~width:line_to_cell_bits t |> uresize ~width:ram_metadata_address_width
  ;;

  let cell_address_to_bytes t = concat_msb [ t; zero cell_to_bytes_bits ]
  let id_width = Int.max num_read_channels num_write_channels
  let id_bits = address_bits_for id_width

  module Ram = Cache_ram.Make (struct
      let cell_width = cell_width
      let line_size = line_width
      let num_cache_lines = num_cache_lines
      let memory_address_width = ram_metadata_address_width
    end)

  module Memory_requester =
    Memory_requester.Make
      (struct
        let data_bits = cell_width * line_width
        let id_bits = id_width
      end)
      (Axi4_out)

  module Memory_requests = struct
    type 'a t =
      { which_read_ch : 'a [@bits address_bits_for num_read_channels]
      ; selected_read_ch : 'a Memory_bus.Read_bus.Source.t
      ; which_write_ch : 'a [@bits address_bits_for num_write_channels]
      ; selected_write_ch : 'a Memory_bus.Write_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  module Memory_responses = struct
    type 'a t =
      { read_response : 'a Read_response.With_valid.t list [@length num_read_channels]
      ; write_response : 'a Write_response.With_valid.t list [@length num_write_channels]
      }
    [@@deriving hardcaml]
  end

  module Selected_request = struct
    type 'a t =
      { valid : 'a
      ; is_write : 'a
      ; address : 'a [@bits cell_address_width]
      ; write_data : 'a [@bits Write.port_widths.write_data]
      ; id : 'a [@bits address_bits_for id_width]
      }
    [@@deriving hardcaml]
  end

  module Arb = struct
    module I = struct
      type 'a t =
        { clock : 'a Clocking.t
        ; requests : 'a Memory_requests.t
        ; downstream_locked : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { read_ready : 'a
        ; write_ready : 'a
        ; selected : 'a Selected_request.t
        }
      [@@deriving hardcaml]
    end

    let create _scope (i : _ I.t) =
      (* TODO:  Warning, the hash function for the AXI4 cache is pretty bad (Fn.id) *)
      let sel = mux2 i.requests.selected_write_ch.valid in
      (* For now, lets just do write priority since our only writers are DMA and the core which needs to read in between. *)
      { O.read_ready = ~:(i.downstream_locked) &: ~:(i.requests.selected_write_ch.valid)
      ; write_ready = ~:(i.downstream_locked)
      ; selected =
          { valid =
              ~:(i.downstream_locked)
              &: (i.requests.selected_write_ch.valid |: i.requests.selected_read_ch.valid)
          ; is_write = i.requests.selected_write_ch.valid
          ; address =
              sel
                i.requests.selected_write_ch.data.address
                i.requests.selected_read_ch.data.address
          ; write_data = i.requests.selected_write_ch.data.write_data
          ; id =
              sel
                (uextend ~width:id_width i.requests.which_write_ch)
                (uextend ~width:id_width i.requests.which_read_ch)
          }
      }
    ;;

    let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"axi4_cache_arb" create input
    ;;
  end

  module Request_stage = struct
    module Statistics = struct
      (* These counters are small so are unlikely to be useful outside of tests. *)
      type 'a t =
        { incoming : 'a [@bits 32]
        ; incoming_write : 'a [@bits 32]
        ; incoming_need_to_write_back : 'a [@bits 32]
        ; incoming_hit : 'a [@bits 32]
        }
      [@@deriving hardcaml]
    end

    module I = struct
      type 'a t =
        { clock : 'a Clocking.t
        ; selected : 'a Selected_request.t
        ; ram_read : 'a Ram.O.t
        ; read_response : 'a Memory_requester.Read.O.t
        ; write_response : 'a Memory_requester.Write.O.t
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { locked : 'a
        ; ram_write : 'a Ram.Write.t
        ; read_request : 'a Memory_requester.Read.Request.t
        ; write_request : 'a Memory_requester.Write.Request.t
        ; memory_responses : 'a Memory_responses.t
        ; statistics : 'a Statistics.t
        }
      [@@deriving hardcaml]
    end

    let create scope (i : _ I.t) =
      (* a one hot encoded cell mask of the cell we want to access. *)
      let%hw selected_strb =
        binary_to_onehot
          (sel_bottom ~width:(address_bits_for line_width) i.selected.address)
      in
      let%hw locked_reg = wire 1 in
      let%hw incoming = i.selected.valid in
      let%hw incoming_is_write = i.selected.is_write in
      let%hw incoming_is_correct_line =
        i.ram_read.meta.valid
        &: (i.ram_read.meta.address ==: cell_to_cache_address i.selected.address)
      in
      let%hw incoming_read_is_hit =
        i.ram_read.meta.valid
        &: (i.ram_read.meta.address ==: cell_to_cache_address i.selected.address)
        &: (selected_strb &: i.ram_read.meta.strb <>:. 0)
      in
      let%hw need_to_flush_line =
        let%hw flush_because_evict_on_read =
          incoming
          &: ~:incoming_is_write
          &: ~:incoming_read_is_hit
          &: i.ram_read.meta.valid
          &: i.ram_read.meta.dirty
        in
        let%hw flush_because_write_miss =
          incoming
          &: incoming_is_write
          &: ~:incoming_is_correct_line
          &: i.ram_read.meta.valid
          &: i.ram_read.meta.dirty
        in
        flush_because_evict_on_read |: flush_because_write_miss
      in
      let%hw issuing_read_request =
        incoming &: ~:incoming_is_write &: ~:incoming_read_is_hit
      in
      let%hw issuing_write_request = incoming &: need_to_flush_line in
      let%hw acked_write_request = issuing_write_request &: i.write_response.finished in
      (* We issue the cache line eviction write and read concurrently. In the case that we are flushing the same cell
         (e.g., we have dws 1 and 2 and we need dw 3), we need to strobe the ram write back to make sure we don't
         read the stale data we just wrote. *)
      let%hw memory_write_back_strobe =
        Clocking.reg
          ~enable:issuing_read_request
          i.clock
          (mux2
             (incoming &: incoming_is_correct_line)
             ~:(i.ram_read.meta.strb)
             (ones (width i.ram_read.meta.strb)))
      in
      let%hw awaiting_read =
        Clocking.reg_fb
          ~width:1
          ~f:(fun t -> mux2 i.read_response.finished gnd (t |: issuing_read_request))
          i.clock
      in
      let%hw awaiting_write =
        Clocking.reg_fb
          ~width:1
          ~f:(fun t ->
            mux2
              i.write_response.finished
              gnd
              (t |: (issuing_write_request &: ~:acked_write_request)))
          i.clock
      in
      let%hw all_memory_operations_done =
        awaiting_read
        |: awaiting_write
        &: (concat_lsb
              [ awaiting_read &: ~:(i.read_response.finished)
              ; awaiting_write &: ~:(i.write_response.finished)
              ]
            ==:. 0)
      in
      let%hw mem_read_done = i.read_response.finished in
      locked_reg
      <-- Clocking.reg_fb
            ~width:1
            ~f:(fun t ->
              t
              |: (issuing_write_request &: ~:acked_write_request |: issuing_read_request)
              &: ~:all_memory_operations_done)
            i.clock;
      let%hw.Ram.Write.Of_signal mem_op_write_back =
        { Ram.Write.valid = mem_read_done
        ; cache_address =
            byte_to_cell_address i.read_response.address
            |> cell_to_cache_address
            |> cache_address_to_hashed_line_address
        ; address = byte_to_cell_address i.read_response.address |> cell_to_cache_address
        ; datas = split_lsb ~part_width:cell_width i.read_response.data
        ; wstrb = memory_write_back_strobe
        ; dirty = gnd (* Fresh from ram *)
        }
      in
      let%hw.Ram.Write.Of_signal incoming_write_back =
        { Ram.Write.valid = incoming &: incoming_is_write
        ; cache_address =
            cell_to_cache_address i.selected.address
            |> cache_address_to_hashed_line_address
        ; address = cell_to_cache_address i.selected.address (* Already in cell space *)
        ; datas =
            List.mapi
              ~f:(fun which_cell existing_data ->
                mux2 selected_strb.:(which_cell) i.selected.write_data existing_data)
              i.ram_read.read_data
        ; wstrb =
            (* On a hit we just rewrite a DW, on a miss we zero the rest of
               the cell. This happens asynchronously with a transmission of
               the cell to memory in which time the cell remains locked. *)
            (let existing_strb =
               repeat ~count:(width i.ram_read.meta.strb) incoming_is_correct_line
               &: i.ram_read.meta.strb
             in
             selected_strb |: existing_strb)
        ; dirty =
            vdd
            (* We're doing the write in cache only for now so mark the line as dirty *)
        }
      in
      let%hw read_done =
        incoming &: incoming_read_is_hit &: ~:incoming_is_write |: mem_read_done
      in
      let%hw read_channel =
        mux2 (incoming &: incoming_read_is_hit) i.selected.id i.read_response.id
      in
      let%hw write_channel = i.selected.id in
      (* We can pre ack the write even if it's a cache miss as the controller will lock while it flushes the line. *)
      let%hw write_done = incoming &: incoming_is_write in
      let%hw byte_response_address = byte_to_cell_address i.read_response.address in
      let%hw which_read_data_cell =
        sel_bottom ~width:(address_bits_for line_width) byte_response_address
      in
      { O.locked =
          (* We lock for this cycle if it's a write so we can write back to the
             cache (otherwise Read_before_write might lead to incoherent data).
             *)
          incoming &: (~:incoming_read_is_hit |: incoming_is_write) |: locked_reg
      ; ram_write =
          Ram.Write.Of_signal.mux2 mem_read_done mem_op_write_back incoming_write_back
      ; memory_responses =
          { read_response =
              List.init
                ~f:(fun ch ->
                  { With_valid.valid = read_done &: (read_channel ==:. ch)
                  ; value =
                      { Read_response.read_data =
                          (let%hw read_parts =
                             mux2
                               incoming
                               (concat_lsb i.ram_read.read_data)
                               i.read_response.data
                           in
                           mux
                             which_read_data_cell
                             (split_lsb ~part_width:cell_width read_parts))
                      }
                  })
                num_read_channels
          ; write_response =
              List.init
                ~f:(fun ch ->
                  { With_valid.valid = write_done &: (write_channel ==:. ch)
                  ; value = { Write_response.dummy = gnd }
                  })
                num_write_channels
          }
      ; read_request =
          { Memory_requester.Read.Request.valid = issuing_read_request
          ; address =
              sel_bottom
                ~width:axi_address_width
                (cell_address_to_bytes i.selected.address)
          ; id = i.selected.id
          }
      ; write_request =
          { Memory_requester.Write.Request.valid = issuing_write_request
          ; wstrb =
              bits_lsb i.ram_read.meta.strb
              |> List.map ~f:(repeat ~count:(cell_width / 8))
              |> concat_lsb
          ; address =
              sel_bottom
                ~width:axi_address_width
                (cache_address_to_byte_address i.ram_read.meta.address)
          ; write_data = concat_lsb i.ram_read.read_data
          ; id = i.selected.id
          }
      ; statistics =
          { Statistics.incoming =
              Clocking.reg_fb
                ~enable:incoming
                ~width:Statistics.port_widths.incoming
                ~f:incr
                i.clock
          ; incoming_write =
              Clocking.reg_fb
                ~width:Statistics.port_widths.incoming
                ~enable:(incoming &: incoming_is_write)
                ~f:incr
                i.clock
          ; incoming_need_to_write_back =
              Clocking.reg_fb
                ~width:Statistics.port_widths.incoming
                ~enable:need_to_flush_line
                ~f:incr
                i.clock
          ; incoming_hit =
              Clocking.reg_fb
                ~width:Statistics.port_widths.incoming
                ~enable:(incoming &: incoming_read_is_hit)
                ~f:incr
                i.clock
          }
      }
    ;;

    let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"request_stage" create input
    ;;
  end

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; requests : 'a Memory_requests.t
      ; dn : 'a Axi4_out.I.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { response : 'a Memory_responses.t
      ; dn : 'a Axi4_out.O.t
      ; read_ready : 'a
      ; write_ready : 'a
      ; statistics : 'a Request_stage.Statistics.t
      }
    [@@deriving hardcaml]
  end

  let create ~build_mode scope ({ clock; requests; dn } : _ I.t) =
    let downstream_locked = wire 1 in
    let arb = Arb.hierarchical scope { Arb.I.clock; requests; downstream_locked } in
    let write = Ram.Write.Of_signal.wires () in
    let ram =
      Ram.hierarchical
        ~build_mode
        scope
        { Ram.I.clock
        ; read =
            { valid = arb.selected.valid
            ; cache_address =
                cell_to_cache_address arb.selected.address
                |> cache_address_to_hashed_line_address
            }
        ; write
        }
    in
    let read_response = Memory_requester.Read.O.Of_signal.wires () in
    let write_response = Memory_requester.Write.O.Of_signal.wires () in
    let request_stage =
      Request_stage.hierarchical
        scope
        { Request_stage.I.clock
        ; selected =
            Selected_request.Of_signal.pipeline
              ~n:Ram.read_latency
              (Clocking.to_spec clock)
              arb.selected
        ; ram_read = ram
        ; read_response
        ; write_response
        }
    in
    let memory_read =
      Memory_requester.Read.hierarchical
        scope
        { Memory_requester.Read.I.clock; request = request_stage.read_request; axi = dn }
    in
    let memory_write =
      Memory_requester.Write.hierarchical
        scope
        { Memory_requester.Write.I.clock
        ; request = request_stage.write_request
        ; axi = dn
        }
    in
    downstream_locked <-- request_stage.locked;
    Memory_requester.Read.O.Of_signal.(read_response <-- memory_read);
    Memory_requester.Write.O.Of_signal.(write_response <-- memory_write);
    Ram.Write.Of_signal.(write <-- request_stage.ram_write);
    { O.response = request_stage.memory_responses
    ; dn = Axi4_out.O.map2 ~f:( |: ) memory_read.axi memory_write.axi
    ; read_ready = arb.read_ready
    ; write_ready = arb.write_ready
    ; statistics = request_stage.statistics
    }
  ;;

  let hierarchical ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"axi4_cache" (create ~build_mode) input
  ;;
end
