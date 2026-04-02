open Hardcaml
open Signal

module Make (M0 : Axi4.S) (M1 : Axi4.S) (S : Axi4.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; m0 : 'a M0.O.t
      ; m1 : 'a M1.O.t
      ; s_in : 'a S.I.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { m0_out : 'a M0.I.t
      ; m1_out : 'a M1.I.t
      ; s_out : 'a S.O.t
      }
    [@@deriving hardcaml]
  end

  let id_storage_width = Int.max M0.id_width M1.id_width

  module Packed_id = struct
    type 'a t =
      { owner : 'a
      ; id : 'a [@bits id_storage_width]
      }
    [@@deriving hardcaml]
  end

  let pack_id owner id =
    Packed_id.Of_signal.pack { owner; id = uextend ~width:id_storage_width id }
  ;;

  let unpack_id t = Packed_id.Of_signal.unpack t

  let id_fifo ~(clock : _ Clocking.t) ~wr ~rd ~d =
    let fifo =
      Fifo.create
        ~capacity:16
        ~clock:clock.clock
        ~clear:clock.clear
        ~showahead:true
        ~wr
        ~d
        ~rd
        ()
    in
    ~:(fifo.empty), fifo.full, unpack_id fifo.q
  ;;

  let rd_fifo scope ({ I.clock; m0; m1; s_in } : _ I.t) =
    let rd_master_ready = wire 1 in
    let r_fifo_valid, r_fifo_full, r_fifo_q =
      id_fifo
        ~clock
        ~wr:(s_in.arready &: (m0.arvalid |: m1.arvalid))
        ~rd:(s_in.rvalid &: s_in.rlast &: rd_master_ready)
        ~d:(mux2 m0.arvalid (pack_id gnd m0.arid) (pack_id vdd m1.arid))
    in
    let%hw r_fifo_valid = r_fifo_valid in
    let%hw r_fifo_full = r_fifo_full in
    let%hw m0_ar = m0.arvalid &: ~:r_fifo_full in
    let%hw m1_ar = ~:(m0.arvalid) &: m1.arvalid &: ~:r_fifo_full in
    (* Unpack Read Owner and Original ID *)
    let%hw r_owner_is_m1 = r_fifo_q.owner in
    rd_master_ready <-- mux2 r_owner_is_m1 m1.rready m0.rready;
    let r_orig_id = r_fifo_q.id in
    r_owner_is_m1, m0_ar, m1_ar, r_orig_id, r_fifo_valid, r_fifo_full
  ;;

  let w_fifo ~can_write scope ({ I.clock; m0; m1; s_in } : _ I.t) =
    let%hw w_master_valid_and_last = wire 1 in
    (* We cut through wvalid if the fifo is empty. *)
    let%hw empty_and_incoming_is_last = wire 1 in
    let w_fifo_valid, w_fifo_full, w_fifo_q =
      id_fifo
        ~clock
        ~wr:
          (can_write
           &: s_in.awready
           &: (m0.awvalid |: m1.awvalid)
           &: ~:empty_and_incoming_is_last)
        ~rd:(s_in.awready &: w_master_valid_and_last)
        ~d:(mux2 m0.awvalid (pack_id gnd m0.awid) (pack_id vdd m1.awid))
    in
    (* Unpack Read Owner and Original ID *)
    let w_owner_is_m1 = w_fifo_q.owner in
    w_master_valid_and_last
    <-- mux2 w_owner_is_m1 (m1.wvalid &: m1.wlast) (m0.wvalid &: m0.wlast);
    empty_and_incoming_is_last
    <-- (~:w_fifo_valid
         &: (m0.awvalid &: m0.wvalid &: m0.wlast)
         |: (~:(m0.awvalid) &: m1.awvalid &: m1.wvalid &: m1.wlast));
    let w_orig_id = w_fifo_q.id in
    w_owner_is_m1, w_orig_id, w_fifo_valid, w_fifo_full
  ;;

  let b_fifo ~can_write ({ I.clock; m0; m1; s_in } : _ I.t) =
    let b_master_ready = wire 1 in
    let b_fifo_valid, b_fifo_full, r_fifo_q =
      id_fifo
        ~clock
        ~wr:(can_write &: s_in.awready &: (m0.awvalid |: m1.awvalid))
        ~rd:(s_in.bvalid &: b_master_ready)
        ~d:(mux2 m0.awvalid (pack_id gnd m0.awid) (pack_id vdd m1.awid))
    in
    (* Unpack Read Owner and Original ID *)
    let b_owner_is_m1 = r_fifo_q.owner in
    b_master_ready <-- mux2 b_owner_is_m1 m1.bready m0.bready;
    let b_orig_id = r_fifo_q.id in
    b_owner_is_m1, b_orig_id, b_fifo_valid, b_fifo_full
  ;;

  (* This file arbitrates two axi4 interfaces taking N bit ID inputs and
     emitting a 1 bit (M0 or M1) ID output. The module uses Fifos to coordinate
     the read, write, and response channels so we know which side we should
     handle next. *)
  let create scope ({ I.m0; m1; s_in; _ } as inputs : _ I.t) =
    let%hw w_and_b_have_capacity = wire 1 in
    let r_owner_is_m1, m0_ar, m1_ar, r_orig_id, r_fifo_valid, _r_fifo_full =
      rd_fifo scope inputs
    in
    let w_owner_is_m1, _w_orig_id, w_fifo_valid, w_fifo_full =
      w_fifo ~can_write:w_and_b_have_capacity scope inputs
    in
    let%hw m0_aw = m0.awvalid &: w_and_b_have_capacity in
    let%hw m1_aw = ~:(m0.awvalid) &: m1.awvalid &: w_and_b_have_capacity in
    let b_owner_is_m1, b_orig_id, b_fifo_valid, b_fifo_full =
      b_fifo ~can_write:w_and_b_have_capacity inputs
    in
    w_and_b_have_capacity <-- (~:w_fifo_full &: ~:b_fifo_full);
    let%hw r_fifo_valid = r_fifo_valid in
    let%hw w_fifo_valid = w_fifo_valid in
    let%hw b_fifo_valid = b_fifo_valid in
    let%hw w_owner_is_m1 = mux2 w_fifo_valid w_owner_is_m1 ~:(m0.awvalid) in
    let select ~(m0_signal : 'a) ~(m1_signal : 'a) ~(owner_is_m1 : Signal.t) : 'a =
      mux2 owner_is_m1 m1_signal m0_signal
    in
    let araddr = mux2 m1_ar m1.araddr m0.araddr in
    let arlen = mux2 m1_ar m1.arlen m0.arlen in
    let arsize = mux2 m1_ar m1.arsize m0.arsize in
    let arburst = mux2 m1_ar m1.arburst m0.arburst in
    let awaddr = mux2 m1_aw m1.awaddr m0.awaddr in
    let awlen = mux2 m1_aw m1.awlen m0.awlen in
    let awsize = mux2 m1_aw m1.awsize m0.awsize in
    let awburst = mux2 m1_aw m1.awburst m0.awburst in
    let m0_out =
      let%hw wready_cut_through = ~:w_fifo_valid &: m0.awvalid &: s_in.wready in
      let%hw wready_fifo = s_in.wready &: w_fifo_valid &: ~:w_owner_is_m1 in
      { M0.I.arready = m0_ar &: s_in.arready
      ; awready = m0_aw &: s_in.awready
      ; wready = wready_cut_through |: wready_fifo
      ; rvalid = s_in.rvalid &: r_fifo_valid &: ~:r_owner_is_m1
      ; rdata = s_in.rdata
      ; rresp = s_in.rresp
      ; rlast = s_in.rlast
      ; rid = sel_bottom ~width:M0.id_width r_orig_id
      ; bvalid = s_in.bvalid &: b_fifo_valid &: ~:b_owner_is_m1
      ; bresp = s_in.bresp
      ; bid = sel_bottom ~width:M0.id_width b_orig_id
      }
    in
    let m1_out =
      let%hw wready_cut_through =
        ~:w_fifo_valid &: ~:(m0.awvalid) &: m1.awvalid &: s_in.wready
      in
      let%hw wready_fifo = s_in.wready &: w_fifo_valid &: w_owner_is_m1 in
      { M1.I.arready = m1_ar &: s_in.arready
      ; awready = m1_aw &: s_in.awready
      ; wready = wready_cut_through |: wready_fifo
      ; rvalid = s_in.rvalid &: r_fifo_valid &: r_owner_is_m1
      ; rdata = s_in.rdata
      ; rresp = s_in.rresp
      ; rlast = s_in.rlast
      ; rid = sel_bottom ~width:M1.id_width r_orig_id
      ; bvalid = s_in.bvalid &: b_fifo_valid &: b_owner_is_m1
      ; bresp = s_in.bresp
      ; bid = sel_bottom ~width:M1.id_width b_orig_id
      }
    in
    let s_out =
      { S.O.arvalid = m0_ar |: m1_ar
      ; araddr
      ; arlen
      ; arsize
      ; arburst
      ; arid = m1_ar
      ; awaddr
      ; awlen
      ; awsize
      ; awburst
      ; awid = m1_aw
      ; awvalid = m0_aw |: m1_aw
      ; wvalid =
          mux2
            w_fifo_valid
            (w_fifo_valid &: mux2 w_owner_is_m1 m1.wvalid m0.wvalid)
            (m0.wvalid |: m1.wvalid)
      ; wdata = select ~m0_signal:m0.wdata ~m1_signal:m1.wdata ~owner_is_m1:w_owner_is_m1
      ; wstrb = select ~m0_signal:m0.wstrb ~m1_signal:m1.wstrb ~owner_is_m1:w_owner_is_m1
      ; wlast = select ~m0_signal:m0.wlast ~m1_signal:m1.wlast ~owner_is_m1:w_owner_is_m1
      ; rready =
          r_fifo_valid
          &: select ~m0_signal:m0.rready ~m1_signal:m1.rready ~owner_is_m1:r_owner_is_m1
      ; bready =
          b_fifo_valid
          &: select ~m0_signal:m0.bready ~m1_signal:m1.bready ~owner_is_m1:b_owner_is_m1
      }
    in
    { O.m0_out; m1_out; s_out }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"axi4_arbitrator" create input
  ;;
end
