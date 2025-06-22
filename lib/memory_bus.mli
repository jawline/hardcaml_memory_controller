(* This module describes the read and write bus types we use to communicate
   between the components in the system and the memory arbitrator. The read and
   write requests each have a handshake (a ready signal that pulses when the
   read is acknowledged) and should be held valid until acknowledged. The
   responses are guaranteed to arrive in order but without a fixed latency. *)
open! Core

module Make (M : sig
    val address_width : int
    val data_bus_width : int
  end) : Memory_bus_intf.S
