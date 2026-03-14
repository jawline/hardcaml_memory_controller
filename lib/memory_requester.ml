open Core
open Hardcaml
open Signal

(** A module that requests fixed size blocks of data from an AXI bus. One in flight. *)

module Make
    (Config : sig
       val data_bits : int
       val id_bits : int
     end)
    (Axi : Axi4.S) =
struct
  open Config

  let byte_address_width = Axi.O.port_widths.awaddr
  let data_bytes = data_bits / 8

  module Request = struct
    type 'a t =
      { valid : 'a
      ; address : 'a [@bits byte_address_width]
      ; id : 'a [@bits id_bits]
      ; write : 'a
      ; wstrb : 'a [@bits data_bytes]
      ; write_data : 'a [@bits data_bits]
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; request : 'a Request.t
      ; axi : 'a Axi.I.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { finished : 'a
      ; address : 'a [@bits byte_address_width]
      ; id : 'a [@bits id_bits]
      ; was_write : 'a
      ; data : 'a [@bits data_bits]
      ; axi : 'a Axi.O.t
      }
    [@@deriving hardcaml]
  end

  let () =
    if data_bits <> Axi.I.port_widths.rdata
    then
      raise_s
        [%message
          "TODO: For now we only support single AXI beat bursts."
            (data_bits : int)
            (Axi.I.port_widths.rdata : int)]
  ;;

  (* TODO: This module needs tidying up, particularly the address transferred logic. *)

  let create scope (i : _ I.t) =
    let%hw locked = wire 1 in
    let%hw.Request.Of_signal request_reg =
      Request.Of_signal.reg (Clocking.to_spec_no_clear i.clock) i.request
    in
    let%hw finishing_this_cycle =
      locked
      &: request_reg.write
      &: i.axi.wready
      |: (i.request.valid &: i.request.write &: i.axi.wready)
      |: (locked &: ~:(request_reg.write) &: i.axi.rvalid)
      |: (i.request.valid &: ~:(i.request.write) &: i.axi.rvalid)
    in
    locked
    <-- Clocking.reg_fb
          ~width:1
          ~f:(fun t -> i.request.valid |: t &: ~:finishing_this_cycle)
          i.clock;
    let o_req = Request.Of_signal.mux2 locked request_reg i.request in
    let%hw address_transferred =
      Clocking.reg_fb
        ~width:1
        ~f:(fun t ->
          let%hw write_request_start =
            i.request.valid &: i.request.write &: i.axi.awready
          in
          let%hw read_request_start =
            i.request.valid &: ~:(i.request.write) &: i.axi.awready
          in
          let%hw write_req_in_process = locked &: o_req.write &: i.axi.awready in
          let%hw read_req_in_process = locked &: ~:(o_req.write) &: i.axi.arready in
          mux2
            finishing_this_cycle
            gnd
            (t
             |: write_request_start
             |: read_request_start
             |: write_req_in_process
             |: read_req_in_process))
        i.clock
    in
    { O.finished = finishing_this_cycle
    ; address = o_req.address
    ; id = o_req.id
    ; was_write = o_req.write
    ; data = i.axi.rdata
    ; axi =
        { Axi.O.awvalid =
            locked
            &: ~:address_transferred
            &: request_reg.write
            |: (i.request.valid &: i.request.write)
        ; wvalid = locked &: request_reg.write |: (i.request.valid &: i.request.write)
        ; awaddr = o_req.address
        ; awburst = of_unsigned_int ~width:2 0b01 (* INCR *)
        ; awid =
            zero Axi.O.port_widths.arid
            (* TODO: Maybe we should give the cache an ID in case it is on an interconnect? *)
        ; awlen = of_unsigned_int ~width:Axi.O.port_widths.awlen 1
        ; awsize =
            of_unsigned_int
              ~width:Axi.O.port_widths.awsize
              (num_bits_to_represent data_bytes)
        ; wlast = vdd
        ; wdata = o_req.write_data
        ; wstrb = o_req.wstrb
        ; bready = vdd (* Unconditionally ack writes *)
        ; arvalid =
            locked
            &: ~:address_transferred
            &: ~:(request_reg.write)
            |: (i.request.valid &: ~:(i.request.write))
        ; araddr = o_req.address
        ; arburst = of_unsigned_int ~width:2 0b01 (* INCR *)
        ; arid =
            zero Axi.O.port_widths.arid
            (* TODO: Maybe we should give the cache an ID in case it is on an interconnect? *)
        ; arlen = of_unsigned_int ~width:Axi.O.port_widths.awlen 1
        ; arsize =
            of_unsigned_int
              ~width:Axi.O.port_widths.awsize
              (num_bits_to_represent data_bytes)
        ; rready = vdd (* We can always receive a read response *)
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_requester" create input
  ;;
end
