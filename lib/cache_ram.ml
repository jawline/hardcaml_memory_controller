open Core
open Hardcaml
open Hardcaml_xilinx
open Signal

module Make (Config : sig
    val cell_width : int (* The size of each cell (used for strb and line segmentation) *)
    val line_size : int
    (* The width of the cache line in cells (e.g., if cell_width = 32 and line_size = 4 then the width of a cache line will be 128 bits). *)

    val num_cache_lines : int (* The depth the cache in cache lines *)
    val memory_address_width : int
    (* The width of the memory being addressed (used for metadata not for addressing cache lines). *)
  end) =
struct
  open Config

  let cache_address_width = address_bits_for num_cache_lines

  module Read = struct
    type 'a t =
      { valid : 'a
      ; cache_address : 'a [@bits cache_address_width]
      }
    [@@deriving hardcaml]
  end

  module Write = struct
    type 'a t =
      { valid : 'a
      ; cell_valid : 'a
      ; cache_address : 'a [@bits cache_address_width]
      ; datas : 'a list [@bits cell_width] [@length line_size]
      ; address : 'a [@bits memory_address_width]
      ; wstrb : 'a [@bits line_size]
      ; dirty : 'a
      }
    [@@deriving hardcaml]
  end

  module Line_metadata = struct
    type 'a t =
      { valid : 'a
      ; address : 'a [@bits memory_address_width]
      ; strb : 'a [@bits line_size]
      ; dirty : 'a
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; read : 'a Read.t
      ; write : 'a Write.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { meta : 'a Line_metadata.t
      ; read_data : 'a list [@bits cell_width] [@length line_size]
      }
    [@@deriving hardcaml]
  end

  let read_latency = 1

  let mkram ~(clock : _ Clocking.t) =
    Simple_dual_port_ram.create
      ~simulation_name:"cache_line"
      ~byte_write_width:B8
      ~arch:(Blockram Read_before_write)
      ~address_collision_protection:Mux_output_ports
      ~address_collision_model:Lfsr
      ~size:num_cache_lines
      ~clock:clock.clock
      ~clear:clock.clear
  ;;

  let create ~build_mode _scope (i : _ I.t) =
    let reg_spec = Clocking.to_spec i.clock in
    let wstrb_per_cell = bits_lsb i.write.wstrb in
    let line_read_datas =
      List.map
        ~f:(fun (write_data, wstrb) ->
          mkram
            ~build_mode
            ~clock:i.clock
            ~write_enable:(repeat ~count:(cell_width / 8) (i.write.valid &: wstrb))
            ~write_address:i.write.cache_address
            ~data:write_data
            ~read_enable:i.read.valid
            ~read_address:i.read.cache_address
            ~read_latency
            ())
        (List.zip_exn i.write.datas wstrb_per_cell)
    in
    let line_metadata =
      let min_metadata_size =
        Line_metadata.sum_of_port_widths |> Int.round_up ~to_multiple_of:8
      in
      mkram
        ~build_mode
        ~clock:i.clock
        ~write_enable:(repeat ~count:(min_metadata_size / 8) i.write.valid)
        ~write_address:i.write.cache_address
        ~data:
          (Line_metadata.Of_signal.pack
             { valid = i.write.cell_valid
             ; address = i.write.address
             ; strb = i.write.wstrb
             ; dirty = i.write.dirty
             }
           |> uextend ~width:min_metadata_size)
        ~read_enable:i.read.valid
        ~read_address:i.read.cache_address
        ~read_latency
        ()
      |> sel_bottom ~width:Line_metadata.sum_of_port_widths
      |> Line_metadata.Of_signal.unpack
    in
    { O.meta =
        { Line_metadata.valid =
            pipeline ~n:read_latency reg_spec i.read.valid &: line_metadata.valid
        ; address = line_metadata.address
        ; strb = line_metadata.strb
        ; dirty = line_metadata.dirty
        }
    ; read_data = line_read_datas
    }
  ;;

  let hierarchical ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"cache_ram" (create ~build_mode) input
  ;;
end
