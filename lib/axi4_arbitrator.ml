open Hardcaml
open Hardcaml_axi
open Signal

module Make (M0 : Axi4.S) (M1 : Axi4.S) (S : Axi4.S) =
struct


  module I = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      m0    : 'a M0.O.t;
      m1    : 'a M1.O.t;
      s_in  : 'a S.I.t;
    } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      m0_out : 'a M0.I.t;
      m1_out : 'a M1.I.t;
      s_out  : 'a S.O.t;
    } [@@deriving hardcaml]
  end

  let create (inputs : _ I.t) =
    let { I.clock; reset; m0; m1; s_in } = inputs in

    (* We must pack the Master Selection Bit + Original ID into the FIFOs *)
    (* Widths: 1 bit for owner + max(M0_id, M1_id) *)
    let id_storage_width = Int.max M0.id_width M1.id_width in
    let fifo_width = 1 + id_storage_width in

    let pack_id owner id =
      concat_msb [ owner; uresize ~width:id_storage_width id ]
    in

    (* --- READ CHANNEL --- *)
    let r_fifo = Fifo.create () ~capacity:16 ~clock ~clear:reset ~showahead:true
        ~wr:(s_in.arready &: (m0.arvalid |: m1.arvalid))
        ~d:(mux2 m0.arvalid
              (pack_id gnd m0.arid)
              (pack_id vdd m1.arid))
        ~rd:(s_in.rvalid &: assert false (* The master we are writing back to is ready *) &: s_in.rlast)
    in

    let g0_ar = m0.arvalid &: ~:(r_fifo.full) in
    let g1_ar = ~:m0.arvalid &: m1.arvalid &: ~:(r_fifo.full) in

    (* Unpack Read Owner and Original ID *)
    let r_owner_is_m1 = bit r_fifo.q id_storage_width in
    let r_orig_id     = sel_bottom r_fifo.q id_storage_width in

    (* --- WRITE CHANNEL --- *)
    let b_fifo = Fifo.create () ~capacity:16 ~clock ~clear:reset ~showahead:true
        ~wr:(s_in.awready &: (m0.awvalid |: m1.awvalid))
        ~d:(mux2 m0.awvalid
              (pack_id gnd m0.awid)
              (pack_id vdd m1.awid))
        ~rd:(s_in.bvalid &: s_in.bready)
    in

    (* W FIFO to prevent data interleaving *)
    let w_fifo = Fifo.create () ~capacity:16 ~clock ~clear:reset ~showahead:true
        ~wr:(s_in.awready &: (m0.awvalid |: m1.awvalid))
        ~d:(~:m0.awvalid &: m1.awvalid)
        ~rd:(s_in.wready &: (mux2 (~:m0.awvalid &: m1.awvalid) m1.wvalid m0.wvalid) &: (mux2 (~:m0.awvalid &: m1.awvalid) m1.wlast m0.wlast))
    in

    let g0_aw = m0.awvalid &: ~:(b_fifo.full) in
    let g1_aw = ~:m0.awvalid &: m1.awvalid &: ~:(b_fifo.full) in

    let b_owner_is_m1 = bit b_fifo.q id_storage_width in
    let b_orig_id     = sel_bottom b_fifo.q id_storage_width in
    let w_owner_is_m1 = w_fifo.q in

    (* --- OUTPUTS TO MASTERS --- *)
    let m0_out = { M0.Slave.unused with
      arready = g0_ar &: s_in.arready;
      awready = g0_aw &: s_in.awready;
      wready  = s_in.wready &: ~:(w_fifo.empty) &: ~:w_owner_is_m1;
      rvalid  = s_in.rvalid &: ~:(r_fifo.empty) &: ~:r_owner_is_m1;
      rdata   = s_in.rdata; rresp = s_in.rresp; rlast = s_in.rlast;
      rid     = sel_bottom r_orig_id M0_cfg.id_width;
      bvalid  = s_in.bvalid &: ~:(b_fifo.empty) &: ~:b_owner_is_m1;
      bresp   = s_in.bresp;
      bid     = sel_bottom b_orig_id M0_cfg.id_width;
    } in

    let m1_out = { M1.Slave.unused with
      arready = g1_ar &: s_in.arready;
      awready = g1_aw &: s_in.awready;
      wready  = s_in.wready &: ~:(w_fifo.empty) &: w_owner_is_m1;
      rvalid  = s_in.rvalid &: ~:(r_fifo.empty) &: r_owner_is_m1;
      rdata   = s_in.rdata; rresp = s_in.rresp; rlast = s_in.rlast;
      rid     = sel_bottom r_orig_id M1_cfg.id_width;
      bvalid  = s_in.bvalid &: ~:(b_fifo.empty) &: b_owner_is_m1;
      bresp   = s_in.bresp;
      bid     = sel_bottom b_orig_id M1_cfg.id_width;
    } in

    (* --- OUTPUT TO SLAVE --- *)
    let s_out = { S.Master.unused with
      araddr  = mux2 g1_ar m1.araddr m0.araddr;
      arid    = uresizer (mux2 g1_ar (consti 2 1) (consti 2 0)) S_cfg.id_width;
      arvalid = g0_ar |: g1_ar;
      awaddr  = mux2 g1_aw m1.awaddr m0.awaddr;
      awid    = uresizer (mux2 g1_aw (consti 2 1) (consti 2 0)) S_cfg.id_width;
      awvalid = g0_aw |: g1_aw;
      wdata   = mux2 w_owner_is_m1 m1.wdata m0.wdata;
      wstrb   = mux2 w_owner_is_m1 m1.wstrb m0.wstrb;
      wvalid  = (mux2 w_owner_is_m1 m1.wvalid m0.wvalid) &: ~:(w_fifo.empty);
      wlast   = mux2 w_owner_is_m1 m1.wlast m0.wlast;
      rready  = s_in.rready;
      bready  = s_in.bready;
    } in

    { O. m0_out; m1_out; s_out }
end
 
