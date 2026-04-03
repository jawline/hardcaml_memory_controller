open Core
open Hardcaml
open Signal

module type Config = sig
  val line_width : int (* in memory cells *)
  val num_cache_lines : int
  val num_read_channels : int
  val num_write_channels : int
  val register_responses : bool
  val register_axi_requests : bool
end

module Make (Config : Config) (Memory_bus : Memory_bus_intf.S) (Axi4_out : Axi4.S) =
struct
  open Config
  open Memory_bus

  let axi_address_width = Axi4_out.O.port_widths.awaddr
  let cell_width = Memory_bus.Write.port_widths.write_data
  let cell_bytes = cell_width / 8
  let cell_to_bytes_bits = address_bits_for cell_bytes
  let line_to_cell_bits = address_bits_for line_width
  let cell_address_width = Memory_bus.Read.port_widths.address
  let line_size_alignment_bits = address_bits_for (cell_bytes * line_width)

  let ram_metadata_address_width =
    Memory_bus.Read.port_widths.address + cell_to_bytes_bits - line_size_alignment_bits
  ;;

  module Ram = Cache_ram.Make (struct
      let cell_width = cell_width
      let line_width = line_width
      let num_cache_lines = num_cache_lines
      let memory_address_width = ram_metadata_address_width
    end)

  let byte_to_cell_address t =
    drop_bottom ~width:cell_to_bytes_bits t
    |>
    (* We uresize here as the address range might be able to address more than the AXI range. *)
    uresize ~width:cell_address_width
  ;;

  let id_width = Int.max num_read_channels num_write_channels
  let id_bits = address_bits_for id_width

  module Memory_requester =
    Memory_requester.Make
      (struct
        let data_bits = cell_width * line_width
        let id_bits = id_bits
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

  module Flush_and_clear = Flush_and_clear.Make (Ram) (Axi4_out) (Memory_requester)

  module Selected_request = struct
    type 'a t =
      { valid : 'a
      ; is_write : 'a
      ; address : 'a [@bits cell_address_width]
      ; cache_cell_one_hot : 'a [@bits line_width]
      ; write_data : 'a [@bits Write.port_widths.write_data]
      ; line_strb : 'a [@bits cell_bytes * line_width]
      ; id : 'a [@bits id_bits]
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

    let create scope (i : _ I.t) =
      let%hw valid =
        ~:(i.downstream_locked)
        &: (i.requests.selected_write_ch.valid |: i.requests.selected_read_ch.valid)
      in
      let%hw last_was_write = wire 1 in
      let%hw want_to_write = i.requests.selected_write_ch.valid in
      let%hw want_to_read = i.requests.selected_read_ch.valid in
      let%hw selected_write = want_to_write &: (~:want_to_read |: ~:last_was_write) in
      let sel = mux2 selected_write in
      let%hw address =
        sel
          i.requests.selected_write_ch.data.address
          i.requests.selected_read_ch.data.address
      in
      let%hw word_strb =
        sel i.requests.selected_write_ch.data.wstrb (ones (cell_width / 8))
      in
      let%hw cache_cell_one_hot =
        (* a one hot encoded cell mask of the cell we want to access. *)
        binary_to_onehot (sel_bottom ~width:(address_bits_for line_width) address)
      in
      let%hw line_strb =
        (* TODO: Think harder about this, if we write one byte out and then request 4 bytes we always need to read from cache which sucks in write then read the same memory scenarios. *)
        bits_lsb cache_cell_one_hot
        |> List.map ~f:(fun valid -> repeat ~count:cell_bytes valid &: word_strb)
        |> concat_lsb
      in
      last_was_write <-- Clocking.reg ~enable:valid i.clock selected_write;
      { O.read_ready = ~:(i.downstream_locked) &: ~:selected_write
      ; write_ready = ~:(i.downstream_locked) &: selected_write
      ; selected =
          { valid
          ; is_write = selected_write
          ; address
          ; write_data = i.requests.selected_write_ch.data.write_data
          ; line_strb
          ; id =
              sel
                (uextend ~width:id_bits i.requests.which_write_ch)
                (uextend ~width:id_bits i.requests.which_read_ch)
          ; cache_cell_one_hot
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
        ; total_cycles : 'a [@bits 32]
        ; locked_cycles : 'a [@bits 32]
        }
      [@@deriving hardcaml]
    end

    module I = struct
      type 'a t =
        { clock : 'a Clocking.t
        ; selected : 'a Selected_request.t
        ; ram_read : 'a Ram.O.t
        ; read_response : 'a Memory_requester.Read.O.t
        ; write_response : 'a Memory_requester.Write.Response.t
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
      (* If we're registering requests also register the responses. This can help with fanout from the finished signal which is pretty troublesome on smaller boards. *)
      let read_response =
        if Config.register_axi_requests
        then
          { (Memory_requester.Read.O.Of_signal.reg
               (Clocking.to_spec_no_clear i.clock)
               i.read_response)
            with
            finished = Clocking.reg i.clock i.read_response.finished
          }
        else i.read_response
      in
      let write_response =
        if Config.register_responses
        then
          { (Memory_requester.Write.Response.Of_signal.reg
               (Clocking.to_spec_no_clear i.clock)
               i.write_response)
            with
            finished = Clocking.reg i.clock i.write_response.finished
          }
        else i.write_response
      in
      let%hw selected_strb =
        (* TODO: Think harder about this, if we write one byte out and then request 4 bytes we always need to read from cache which sucks in write then read the same memory scenarios. *)
        i.selected.line_strb
      in
      let%hw incoming = i.selected.valid in
      let%hw incoming_is_write = i.selected.is_write in
      let%hw incoming_is_correct_line =
        i.ram_read.meta.valid
        &: (i.ram_read.meta.address ==: Ram.cell_to_cache_address i.selected.address)
      in
      let%hw incoming_read_is_hit =
        i.ram_read.meta.valid
        &: (i.ram_read.meta.address ==: Ram.cell_to_cache_address i.selected.address)
        &: (selected_strb &: i.ram_read.meta.strb ==: selected_strb)
      in
      let%hw need_to_flush_line =
        let%hw flush_because_evict_on_read =
          incoming
          &: ~:incoming_is_correct_line
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
      (* We issue the cache line eviction write and read concurrently. In the case that we are flushing the same cell
         (e.g., we have dws 1 and 2 and we need dw 3), we need to strobe the ram write back to make sure we don't
         read the stale data we just wrote. *)
      let%hw memory_write_back_strobe =
        Clocking.reg
          ~enable:issuing_read_request
          (* Clear on finished otherwise successor reads will use the read data strobe. *)
          (Clocking.add_clear i.clock read_response.finished)
          (mux2
             (incoming &: incoming_is_correct_line)
             ~:(i.ram_read.meta.strb)
             (ones (width i.ram_read.meta.strb)))
      in
      let%hw awaiting_read =
        Clocking.reg
          ~enable:issuing_read_request
          (Clocking.add_clear i.clock read_response.finished)
          vdd
      in
      let%hw awaiting_write =
        Clocking.reg
          ~enable:(issuing_write_request &: ~:(write_response.finished))
          (Clocking.add_clear i.clock write_response.finished)
          vdd
      in
      let%hw.Ram.Write.Of_signal mem_op_write_back =
        { Ram.Write.valid = read_response.finished
        ; cell_valid = vdd
        ; cache_address =
            byte_to_cell_address read_response.address
            |> Ram.cell_to_cache_address
            |> Ram.cache_address_to_hashed_line_address
        ; address =
            byte_to_cell_address read_response.address |> Ram.cell_to_cache_address
        ; datas = split_lsb ~part_width:cell_width read_response.data
        ; real_wstrb = memory_write_back_strobe
        ; meta_wstrb = ones (width memory_write_back_strobe)
        ; dirty = ~:(all_bits_set memory_write_back_strobe)
        }
      in
      let%hw.Ram.Write.Of_signal incoming_write_back =
        { Ram.Write.valid = incoming &: incoming_is_write
        ; cell_valid = vdd
        ; cache_address =
            Ram.cell_to_cache_address i.selected.address
            |> Ram.cache_address_to_hashed_line_address
        ; address =
            Ram.cell_to_cache_address i.selected.address (* Already in cell space *)
        ; datas =
            List.mapi
              ~f:(fun which_cell existing_data ->
                mux2
                  i.selected.cache_cell_one_hot.:(which_cell)
                  i.selected.write_data
                  existing_data)
              i.ram_read.read_data
        ; real_wstrb = selected_strb (* Only actually write the bytes we see to RAM. *)
        ; meta_wstrb =
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
        incoming &: incoming_read_is_hit &: ~:incoming_is_write |: read_response.finished
      in
      let%hw read_channel =
        mux2 (incoming &: incoming_read_is_hit) i.selected.id read_response.id
      in
      let%hw write_channel = i.selected.id in
      (* We can pre ack the write even if it's a cache miss as the controller will lock while it flushes the line. *)
      let%hw write_done = incoming &: incoming_is_write in
      let%hw byte_response_address =
        mux2 incoming i.selected.address (byte_to_cell_address read_response.address)
      in
      let%hw which_read_data_cell =
        cut_through_reg
          ~enable:i.selected.valid
          (Clocking.to_spec_no_clear i.clock)
          (sel_bottom ~width:(address_bits_for line_width) byte_response_address)
      in
      (* TODO: We could lock the RAM to the same location instead. *)
      let byte_enable_data ~strb t =
        let bytes_ = split_lsb ~part_width:8 t in
        List.zip_exn bytes_ (bits_lsb strb)
        |> List.map ~f:(fun (byte_, strb) -> byte_ &: repeat ~count:8 strb)
        |> concat_lsb
      in
      let%hw cached_read_data =
        (* By pipelining this a single cycle we can avoid fanout of the CE on the issuing write request signal.*)
        if Config.register_axi_requests
        then
          Clocking.reg
            ~enable:(Clocking.reg i.clock issuing_read_request)
            i.clock
            (reg (Clocking.to_spec_no_clear i.clock) (concat_lsb i.ram_read.read_data))
        else
          Clocking.reg
            ~enable:issuing_read_request
            i.clock
            (concat_lsb i.ram_read.read_data)
      in
      let%hw unit_locked =
        (* We lock for this cycle if it's a write so we can write back to the
             cache (otherwise Read_before_write might lead to incoherent data).
             *)
        (* TODO: This permits an op only every other cycle but without it we end up with very tight timings. Needs pipelining. *)
        incoming |: (awaiting_read |: awaiting_write)
        (* 
        incoming &: (~:incoming_read_is_hit |: incoming_is_write) |: locked_reg *)
      in
      { O.locked = unit_locked
      ; ram_write =
          Ram.Write.Of_signal.mux2
            read_response.finished
            mem_op_write_back
            incoming_write_back
      ; memory_responses =
          { read_response =
              (let%hw read_data =
                 let%hw reconstituted_read_data =
                   byte_enable_data ~strb:memory_write_back_strobe read_response.data
                   |: byte_enable_data ~strb:~:memory_write_back_strobe cached_read_data
                 in
                 let%hw read_parts =
                   mux2 incoming (concat_lsb i.ram_read.read_data) reconstituted_read_data
                 in
                 mux which_read_data_cell (split_lsb ~part_width:cell_width read_parts)
               in
               List.init
                 ~f:(fun ch ->
                   let without_registering =
                     { With_valid.valid = read_done &: (read_channel ==:. ch)
                     ; value = { Read_response.read_data }
                     }
                   in
                   if Config.register_responses
                   then
                     Read_response.With_valid.Of_signal.reg
                       (Clocking.to_spec i.clock)
                       without_registering
                   else without_registering)
                 num_read_channels)
          ; write_response =
              List.init
                ~f:(fun ch ->
                  let without_registering =
                    { With_valid.valid = write_done &: (write_channel ==:. ch)
                    ; value = { Write_response.dummy = gnd }
                    }
                  in
                  if Config.register_responses
                  then
                    Write_response.With_valid.Of_signal.reg
                      (Clocking.to_spec i.clock)
                      without_registering
                  else without_registering)
                num_write_channels
          }
      ; read_request =
          ({ Memory_requester.Read.Request.valid = issuing_read_request
           ; address =
               sel_bottom
                 ~width:axi_address_width
                 (Ram.cell_to_cache_address i.selected.address
                  |> Ram.cache_address_to_byte_address)
           ; id = i.selected.id
           }
           |>
           if Config.register_axi_requests
           then Memory_requester.Read.Request.Of_signal.reg (Clocking.to_spec i.clock)
           else Fn.id)
      ; write_request =
          ({ Memory_requester.Write.Request.valid = issuing_write_request
           ; wstrb = i.ram_read.meta.strb
           ; address =
               sel_bottom
                 ~width:axi_address_width
                 (Ram.cache_address_to_byte_address i.ram_read.meta.address)
           ; write_data = concat_lsb i.ram_read.read_data
           ; id = i.selected.id
           }
           |>
           if Config.register_axi_requests
           then Memory_requester.Write.Request.Of_signal.reg (Clocking.to_spec i.clock)
           else Fn.id)
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
          ; total_cycles =
              Clocking.reg_fb ~width:Statistics.port_widths.total_cycles ~f:incr i.clock
          ; locked_cycles =
              Clocking.reg_fb
                ~width:Statistics.port_widths.locked_cycles
                ~enable:unit_locked
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
      ; flush : 'a
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
      ; locked : 'a
      ; statistics : 'a Request_stage.Statistics.t
      }
    [@@deriving hardcaml]
  end

  let create ~build_mode scope ({ clock; flush; requests; dn } : _ I.t) =
    (* TODO:  Warning, the hash function for the AXI4 cache is pretty bad (Fn.id) *)
    (* Register based round robin *)
    let downstream_locked = wire 1 in
    let arb = Arb.hierarchical scope { Arb.I.clock; requests; downstream_locked } in
    let ram_write = Ram.Write.Of_signal.wires () in
    let ram_read = Ram.Read.Of_signal.wires () in
    let ram =
      Ram.hierarchical
        ~build_mode
        scope
        { Ram.I.clock; read = ram_read; write = ram_write }
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
        ; write_response = write_response.response
        }
    in
    let memory_read =
      Memory_requester.Read.hierarchical
        scope
        { Memory_requester.Read.I.clock; request = request_stage.read_request; axi = dn }
    in
    let memory_write_request = Memory_requester.Write.Request.Of_signal.wires () in
    let memory_write =
      Memory_requester.Write.hierarchical
        scope
        { Memory_requester.Write.I.clock; request = memory_write_request; axi = dn }
    in
    let startup_clear =
      Flush_and_clear.hierarchical
        scope
        { Flush_and_clear.I.clock = { clock with clear = clock.clear |: flush }
        ; ram
        ; memory = memory_write.response
        }
    in
    downstream_locked <-- (startup_clear.active |: request_stage.locked |: flush);
    Ram.Read.Of_signal.(
      ram_read
      <-- mux2
            startup_clear.active
            startup_clear.ram_read
            { valid = arb.selected.valid
            ; cache_address =
                Ram.cell_to_cache_address arb.selected.address
                |> Ram.cache_address_to_hashed_line_address
            });
    Memory_requester.Read.O.Of_signal.(read_response <-- memory_read);
    Memory_requester.Write.O.Of_signal.(write_response <-- memory_write);
    Memory_requester.Write.Request.Of_signal.(
      memory_write_request
      <-- mux2 startup_clear.active startup_clear.memory request_stage.write_request);
    Ram.Write.Of_signal.(
      ram_write
      <-- Ram.Write.Of_signal.mux2
            startup_clear.active
            startup_clear.ram_write
            request_stage.ram_write);
    { O.response = request_stage.memory_responses
    ; dn = Axi4_out.O.map2 ~f:( |: ) memory_read.axi memory_write.axi
    ; read_ready = arb.read_ready
    ; write_ready = arb.write_ready
    ; statistics = request_stage.statistics
    ; locked = startup_clear.active
    }
  ;;

  let hierarchical ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"axi4_cache" (create ~build_mode) input
  ;;
end
