open Core
open Hardcaml
open Signal

module Make
    (Config : sig
       val line_width : int (* in memory cells *)
       val num_cache_lines : int
       val num_read_channels : int
       val num_write_channels : int
     end)
    (Memory_bus : Memory_bus_intf.S)
    (Axi4_out : Axi4.S) =
struct
  open Config
  open Memory_bus

  let cell_width = Memory_bus.Write.port_widths.write_data
  let cell_bytes = cell_width / 8
  let line_size_alignment_bits = address_bits_for (cell_bytes * line_width)
  let cell_to_bytes_bits = address_bits_for cell_bytes
  let byte_address_width = Memory_bus.Read.port_widths.address + cell_to_bytes_bits

  let line_address_width =
    Memory_bus.Read.port_widths.address + cell_to_bytes_bits - line_size_alignment_bits
  ;;

  let id_width = Int.max num_read_channels num_write_channels

  module Ram = Cache_ram.Make (struct
      let cell_width = cell_width
      let line_size = line_width
      let num_cache_lines = num_cache_lines
      let memory_address_width = line_address_width
    end)

  module Memory_requester =
    Memory_requester.Make
      (struct
        let data_bits = cell_width * line_width
        let id_bits = id_width
      end)
      (Axi4_out)

  let address_to_bytes t = concat_msb [ t; zero cell_to_bytes_bits ]

  let address_to_cacheline_address t =
    address_to_bytes t |> drop_bottom ~width:line_size_alignment_bits
  ;;

  let hash_cell_address address =
    print_s [%message "TODO: Use a real hash function"];
    sel_bottom ~width:(address_bits_for num_cache_lines) address
  ;;

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
      ; address : 'a [@bits byte_address_width]
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
          ; id = sel i.requests.which_write_ch i.requests.which_read_ch
          }
      }
    ;;

    let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"axi4_cache_arb" create input
    ;;
  end

  module Request_stage = struct
    module I = struct
      type 'a t =
        { clock : 'a Clocking.t
        ; selected : 'a Selected_request.t
        ; ram_read : 'a Ram.O.t
        ; request_response : 'a Memory_requester.O.t
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { locked : 'a
        ; ram_write : 'a Ram.Write.t
        ; memory_request : 'a Memory_requester.Request.t
        ; memory_responses : 'a Memory_responses.t
        }
      [@@deriving hardcaml]
    end

    let create scope (i : _ I.t) =
      let%hw locked_reg = wire 1 in
      let%hw incoming = i.selected.valid in
      let%hw incoming_is_write = i.selected.is_write in
      let%hw incoming_is_hit =
        i.ram_read.meta.valid &: (i.ram_read.meta.address ==: i.selected.address)
      in
      let%hw no_write_flush_needed = incoming_is_hit |: ~:(i.ram_read.meta.valid) in
      let%hw mem_op_done = i.request_response.finished in
      let%hw mem_op_was_read = ~:(i.request_response.was_write) in
      locked_reg
      <-- Clocking.reg_fb
            ~width:1
            ~f:(fun t ->
              t
              |: (incoming
                  &: mux2 incoming_is_write ~:no_write_flush_needed ~:incoming_is_hit)
              &: ~:mem_op_done)
            i.clock;
      let byte_to_cell_addr = drop_bottom ~width:cell_to_bytes_bits in
      let byte_to_hashed t = hash_cell_address (byte_to_cell_addr t) in
      let mem_op_write_back =
        { Ram.Write.valid = mem_op_done
        ; cell_address = byte_to_hashed i.request_response.address
        ; address = byte_to_cell_addr i.request_response.address
        ; datas = split_lsb ~part_width:cell_width i.request_response.data
        ; wstrb = ones (width i.ram_read.meta.strb) (* We read the entire line *)
        }
      in
      let incoming_write_back =
        { Ram.Write.valid = incoming &: incoming_is_write
        ; cell_address = byte_to_hashed i.selected.address
        ; address = byte_to_cell_addr i.selected.address
        ; datas =
            List.init ~f:(fun _ -> i.selected.write_data) line_width
            (* We repeat the same data as the wstrb guards against writes to the other cells. *)
        ; wstrb =
            (* On a hit we just rewrite a DW, on a miss we zero the rest of
               the cell. This happens asynchronously with a transmission of
               the cell to memory in which time the cell remains locked. *)
            repeat ~count:(width i.ram_read.meta.strb) incoming_is_hit
            &: i.ram_read.meta.strb
            |: binary_to_onehot
                 (sel_bottom ~width:line_size_alignment_bits i.selected.address)
        }
      in
      let%hw read_done =
        incoming
        &: incoming_is_hit
        &: ~:incoming_is_write
        |: (mem_op_done &: mem_op_was_read)
      in
      let%hw read_channel =
        mux2 (incoming &: incoming_is_hit) i.selected.id i.request_response.id
      in
      let%hw write_channel = i.selected.id in
      (* We can pre ack the write even if it's a cache miss as the controller will lock while it flushes the line. *)
      let%hw write_done = incoming &: incoming_is_write in
      let%hw which_read_data_cell =
        byte_to_cell_addr i.request_response.address
        |> sel_bottom ~width:(address_bits_for line_width)
      in
      { O.locked =
          (* We lock for this cycle if it's a write so we can write back to the
             cache (otherwise Read_before_write might lead to incoherent data).
             *)
          incoming &: (~:incoming_is_hit |: incoming_is_write) |: locked_reg
      ; ram_write =
          Ram.Write.Of_signal.mux2
            (mem_op_done &: mem_op_was_read)
            mem_op_write_back
            incoming_write_back
      ; memory_responses =
          { read_response =
              List.init
                ~f:(fun ch ->
                  { With_valid.valid = read_done &: (read_channel ==:. ch)
                  ; value =
                      { Read_response.read_data =
                          mux
                            which_read_data_cell
                            (split_lsb ~part_width:cell_width i.request_response.data)
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
      ; memory_request =
          { Memory_requester.Request.valid =
              incoming &: mux2 incoming_is_write ~:no_write_flush_needed ~:incoming_is_hit
          ; write = incoming_is_write
          ; wstrb =
              bits_lsb i.ram_read.meta.strb
              |> List.map ~f:(repeat ~count:cell_width)
              |> concat_lsb
          ; address = i.selected.address
          ; write_data = concat_lsb i.ram_read.read_data
          ; id = i.selected.id
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
            ; address = hash_cell_address arb.selected.address
            }
        ; write
        }
    in
    let request_response = Memory_requester.O.Of_signal.wires () in
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
        ; request_response
        }
    in
    let memory_stage =
      Memory_requester.hierarchical
        scope
        { Memory_requester.I.clock; request = request_stage.memory_request; axi = dn }
    in
    Memory_requester.O.Of_signal.(request_response <-- memory_stage);
    Ram.Write.Of_signal.(write <-- request_stage.ram_write);
    { O.response = request_stage.memory_responses; dn = memory_stage.axi }
  ;;
end
