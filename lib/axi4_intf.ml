module M (Config : Axi4_config_intf.Config) = struct
  module type S = sig
    module I : sig
      type 'a t =
        { bvalid : 'a
        ; awready : 'a
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
        { wvalid : 'a
        ; awvalid : 'a
        ; awid : 'a
        ; awaddr : 'a
        ; wdata : 'a
        ; wstrb : 'a
        ; wlast : 'a
        ; arvalid : 'a
        ; arid : 'a
        ; araddr : 'a
        ; rready : 'a
        }
      [@@deriving hardcaml]
    end
  end
end
