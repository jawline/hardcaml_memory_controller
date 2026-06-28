open! Core

module Make
    (Config : sig
       val data_bits : int
       val id_bits : int
       val delay_write_request : bool
     end)
    (Axi : Axi4.S) : Memory_requester_intf.M(Axi).S
