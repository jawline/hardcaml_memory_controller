open! Core

type t =
  { uuid : int ; frequency : int } [@@deriving sexp, equal]

val create : int -> t 

