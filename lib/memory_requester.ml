open Core
open Hardcaml
open Signal

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
  let axi_bytes = Axi.O.port_widths.wdata / 8
  let axi_to_bus_ratio = Config.data_bits / Axi.I.port_widths.rdata

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

    module Response = struct
      type 'a t =
        { finished : 'a
        ; busy : 'a
        ; address : 'a [@bits byte_address_width]
        ; id : 'a [@bits id_bits]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { response : 'a Response.t
        ; axi : 'a Axi.O.t
        }
      [@@deriving hardcaml]
    end

    module Request_tracker = struct
      type 'a t =
        { locked : 'a
        ; which_beat : 'a
        ; need_to_transfer_address : 'a
        ; need_to_transfer_data : 'a
        ; finishing : 'a
        }

      let create scope (i : _ I.t) =
        let%hw locked = wire 1 in
        let%hw address_transferred = wire 1 in
        (* TODO: Using only a counter is probably not optimal vs also precomputing a finished reg. *)
        let%hw beat_counter = wire (num_bits_to_represent axi_to_bus_ratio) in
        let%hw data_transferred = beat_counter ==:. axi_to_bus_ratio in
        let%hw finishing_this_cycle =
          let%hw is_address_transferred = address_transferred |: i.axi.awready in
          let%hw is_final_beat_transferred =
            Unsigned.(beat_counter +: i.axi.wready) ==:. axi_to_bus_ratio
          in
          let%hw finishing_in_progress =
            locked &: is_address_transferred &: is_final_beat_transferred
          in
          let%hw finishing_pulse =
            (* At single beat bus ratios we might do a burst in a single beat. *)
            if axi_to_bus_ratio = 1
            then ~:locked &: (i.request.valid &: i.axi.awready &: i.axi.wready)
            else gnd
          in
          finishing_in_progress |: finishing_pulse
        in
        locked
        <-- Clocking.reg_fb
              ~width:1
              ~f:(fun t -> i.request.valid |: t &: ~:finishing_this_cycle)
              i.clock;
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
        beat_counter
        <-- Clocking.reg_fb
              ~width:(num_bits_to_represent axi_to_bus_ratio)
              ~f:(fun t ->
                let%hw is_active_request = i.request.valid |: locked in
                let%hw final_beat_not_transferred = t <:. axi_to_bus_ratio in
                mux2
                  finishing_this_cycle
                  (zero (width t))
                  (mux2
                     (is_active_request &: final_beat_not_transferred &: i.axi.wready)
                     (incr t)
                     t))
              i.clock;
        { locked
        ; which_beat = beat_counter
        ; need_to_transfer_address = locked |: i.request.valid &: ~:address_transferred
        ; need_to_transfer_data = locked |: i.request.valid &: ~:data_transferred
        ; finishing = finishing_this_cycle
        }
      ;;
    end

    let last_beat = axi_to_bus_ratio - 1

    let create scope (i : _ I.t) =
      let%hw.Request.Of_signal request_reg =
        Request.Of_signal.reg
          ~enable:i.request.valid
          (Clocking.to_spec_no_clear i.clock)
          i.request
      in
      let { Request_tracker.locked
          ; which_beat
          ; need_to_transfer_address
          ; need_to_transfer_data
          ; finishing
          }
        =
        Request_tracker.create scope i
      in
      let output_memory_request = Request.Of_signal.mux2 locked request_reg i.request in
      { O.response =
          { Response.finished = finishing
          ; busy = locked
          ; address = output_memory_request.address
          ; id = output_memory_request.id
          }
      ; axi =
          (* Note that downstream controllers may ack data before addresses (wready vs awready). *)
          { Axi.O.awvalid = need_to_transfer_address
          ; wvalid = need_to_transfer_data
          ; awaddr = output_memory_request.address
          ; awburst = of_unsigned_int ~width:2 0b01 (* INCR *)
          ; awid =
              zero Axi.O.port_widths.arid
              (* TODO: Maybe we should give the cache an ID in case it is on an interconnect? *)
          ; awlen = of_unsigned_int ~width:Axi.O.port_widths.awlen last_beat
          ; awsize =
              of_unsigned_int
                ~width:Axi.O.port_widths.awsize
                (num_bits_to_represent axi_bytes)
          ; wlast = which_beat ==:. last_beat
          ; wdata =
              mux
                which_beat
                (split_lsb ~part_width:(axi_bytes * 8) output_memory_request.write_data)
          ; wstrb =
              mux which_beat (split_lsb ~part_width:axi_bytes output_memory_request.wstrb)
          ; bready = vdd (* Unconditionally ack writes *)
          ; arvalid = gnd
          ; araddr = zero Axi.O.port_widths.araddr
          ; arburst = zero Axi.O.port_widths.arburst
          ; arid = zero Axi.O.port_widths.arid
          ; arlen = zero Axi.O.port_widths.arlen
          ; arsize = zero Axi.O.port_widths.arsize
          ; rready = gnd
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

    module Request_tracker = struct
      type 'a t =
        { locked : 'a
        ; which_beat : 'a
        ; finishing : 'a
        }

      let create scope (i : _ I.t) =
        let%hw locked = wire 1 in
        let%hw beat_counter = wire (address_bits_for axi_to_bus_ratio) in
        let%hw finishing_this_cycle =
          locked &: i.axi.rvalid &: (beat_counter ==:. axi_to_bus_ratio - 1)
        in
        locked
        <-- Clocking.reg_fb
              ~width:1
              ~f:(fun t -> i.request.valid |: t &: ~:finishing_this_cycle)
              i.clock;
        beat_counter
        <-- Clocking.reg_fb
              ~width:(address_bits_for axi_to_bus_ratio)
              ~enable:i.axi.rvalid
              ~f:(fun t -> mux2 finishing_this_cycle (zero (width t)) (incr t))
              i.clock;
        { locked; which_beat = beat_counter; finishing = finishing_this_cycle }
      ;;
    end

    let create scope (i : _ I.t) =
      let%hw.Request.Of_signal request_reg =
        Request.Of_signal.reg
          ~enable:i.request.valid
          (Clocking.to_spec_no_clear i.clock)
          i.request
      in
      let { Request_tracker.locked; which_beat; finishing } =
        Request_tracker.create scope i
      in
      let%hw address_transferred =
        Clocking.reg_fb
          ~width:1
          ~f:(fun t ->
            let%hw read_request_start = i.request.valid &: i.axi.arready in
            let%hw read_req_in_process = locked &: i.axi.arready in
            mux2 finishing gnd (t |: read_request_start |: read_req_in_process))
          i.clock
      in
      let read_parts =
        List.init
          ~f:(fun part ->
            Clocking.reg
              ~enable:(i.axi.rvalid &: (which_beat ==:. part))
              i.clock
              i.axi.rdata)
          (axi_to_bus_ratio - 1)
      in
      let output_memory_request = Request.Of_signal.mux2 locked request_reg i.request in
      { O.finished = finishing
      ; address = output_memory_request.address
      ; id = output_memory_request.id
      ; data = concat_lsb (List.concat [ read_parts; [ i.axi.rdata ] ])
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
          ; araddr = output_memory_request.address
          ; arburst = of_unsigned_int ~width:2 0b01 (* INCR *)
          ; arid =
              zero Axi.O.port_widths.arid
              (* TODO: Maybe we should give the cache an ID in case it is on an interconnect? *)
          ; arlen = of_unsigned_int ~width:Axi.O.port_widths.awlen (axi_to_bus_ratio - 1)
          ; arsize =
              of_unsigned_int
                ~width:Axi.O.port_widths.awsize
                (num_bits_to_represent axi_bytes)
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
