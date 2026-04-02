open! Core
open! Hardcaml

module Make
    (M : Shared_access_ports_intf.Config)
    (Memory_bus : Memory_bus_intf.S)
    (Axi : Axi4.S) : Shared_access_ports_intf.M(M)(Memory_bus)(Axi).S
