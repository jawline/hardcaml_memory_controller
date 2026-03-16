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

  let () =
    if data_bits <> Axi.I.port_widths.rdata
    then
      raise_s
        [%message
          "TODO: For now we only support single AXI beat bursts."
            (data_bits : int)
            (Axi.I.port_widths.rdata : int)]
  ;;

  module Write = struct
    module Request = struct
      type 'a t =
        { valid : 'a
        ; address : 'a [@bits byte_address_width]
        ; id : 'a [@bits id_bits]
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
        ; axi : 'a Axi.O.t
        }
      [@@deriving hardcaml]
    end

    let create scope (i : _ I.t) =
      let%hw locked = wire 1 in
      let%hw.Request.Of_signal request_reg =
        Request.Of_signal.reg
          ~enable:i.request.valid
          (Clocking.to_spec_no_clear i.clock)
          i.request
      in
      let%hw address_transferred = wire 1 in
      let%hw finishing_this_cycle =
        let%hw finishing_in_progress = locked &: (address_transferred |: i.axi.awready) &: i.axi.wready in
        let%hw finishing_pulse =
          ~:locked &: (i.request.valid &: i.axi.awready &: i.axi.wready)
        in
        finishing_in_progress |: finishing_pulse
      in
      locked
      <-- Clocking.reg_fb
            ~width:1
            ~f:(fun t -> i.request.valid |: t &: ~:finishing_this_cycle)
            i.clock;
      let o_req = Request.Of_signal.mux2 locked request_reg i.request in
      address_transferred
      <-- Clocking.reg_fb
            ~width:1
            ~f:(fun t ->
              let%hw write_request_start = i.request.valid &: i.axi.awready in
              let%hw write_req_in_process = locked &: i.axi.awready in
              mux2
                finishing_this_cycle
                gnd
                (t |: write_request_start |: write_req_in_process))
            i.clock;
      { O.finished = finishing_this_cycle
      ; address = o_req.address
      ; id = o_req.id
      ; axi =
          { Axi.O.awvalid = locked &: ~:address_transferred |: i.request.valid
          ; wvalid = locked |: i.request.valid
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
          ; arvalid = gnd
          ; araddr = zero Axi.O.port_widths.araddr
          ; arburst = zero Axi.O.port_widths.arburst
          ; arid = zero Axi.O.port_widths.arid
          ; arlen = zero Axi.O.port_widths.arlen
          ; arsize = zero Axi.O.port_widths.arsize
          ; rready = gnd (* We can always receive a read response *)
          }
      }
    ;;

    let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"memory_requester_write" create input
    ;;
  end

  module Read = struct
    module Request = struct
      type 'a t =
        { valid : 'a
        ; address : 'a [@bits byte_address_width]
        ; id : 'a [@bits id_bits]
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

    let create scope (i : _ I.t) =
      let%hw locked = wire 1 in
      let%hw.Request.Of_signal request_reg =
        Request.Of_signal.reg
          ~enable:i.request.valid
          (Clocking.to_spec_no_clear i.clock)
          i.request
      in
      let%hw finishing_this_cycle = locked &: i.axi.rvalid in
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
            let%hw read_request_start = i.request.valid &: i.axi.arready in
            let%hw read_req_in_process = locked &: i.axi.arready in
            mux2 finishing_this_cycle gnd (t |: read_request_start |: read_req_in_process))
          i.clock
      in
      { O.finished = finishing_this_cycle
      ; address = o_req.address
      ; id = o_req.id
      ; data = i.axi.rdata
      ; axi =
          { Axi.O.awvalid = gnd
          ; wvalid = gnd
          ; awaddr = zero Axi.O.port_widths.awaddr
          ; awburst = zero Axi.O.port_widths.awburst
          ; awid = zero Axi.O.port_widths.arid
          ; awlen = zero Axi.O.port_widths.awlen
          ; awsize = zero Axi.O.port_widths.awsize
          ; wlast = gnd
          ; wdata = zero Axi.O.port_widths.wdata
          ; wstrb = zero Axi.O.port_widths.wstrb
          ; bready = gnd
          ; arvalid = locked &: ~:address_transferred |: i.request.valid
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
      H.hierarchical ~scope ~name:"memory_requester_read" create input
    ;;
  end
end
