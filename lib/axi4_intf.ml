module M (Config : Axi4_config_intf.Config) = struct
  module type S = sig
    module I : sig
      type 'a t =
        { bvalid : 'a
        ; wready : 'a
        ; bid : 'a
        ; bresp : 'a
        ; rvalid : 'a
        ; rid : 'a
        ; rdata : 'a
        ; rresp : 'a
        ; rlast : 'a
        ; rready : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { awvalid : 'a
        ; awid : 'a
        ; awaddr : 'a
        ; awlena : 'a
        ; awsize : 'a
        ; awburst : 'a
        ; wdata : 'a
        ; wstrb : 'a
        ; wlast : 'a
        ; arvalid : 'a
        ; arid : 'a
        ; araddr : 'a
        ; arlen : 'a
        ; arsize : 'a
        ; arburst : 'a
        ; rready : 'a
        }
      [@@deriving hardcaml]
    end
  end
end
