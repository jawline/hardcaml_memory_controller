module Make (Config : Axi4_config_intf.Config) = struct
  open Config

  module I = struct
    type 'a t =
      { bvalid : 'a
      ; wready : 'a
      ; bid : 'a [@bits id_width]
      ; bresp : 'a [@bits 2]
      ; rvalid : 'a
      ; rid : 'a [@bits id_width]
      ; rdata : 'a [@bits data_width]
      ; rresp : 'a [@bits 2]
      ; rlast : 'a [@bits 1]
      ; rready : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { awvalid : 'a
      ; awid : 'a [@bits id_width]
      ; awaddr : 'a [@bits addr_width]
      ; awlena : 'a [@bits 8]
      ; awsize : 'a [@bits 3]
      ; awburst : 'a [@bits 2]
      ; wdata : 'a [@bits data_width]
      ; wstrb : 'a [@bits data_width / 8]
      ; wlast : 'a
      ; arvalid : 'a
      ; arid : 'a [@bits id_width]
      ; araddr : 'a [@bits addr_width]
      ; arlen : 'a [@bits 8]
      ; arsize : 'a [@bits 3]
      ; arburst : 'a [@bits 2]
      ; rready : 'a
      }
    [@@deriving hardcaml]
  end
end
