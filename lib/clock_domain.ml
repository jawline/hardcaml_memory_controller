open Core

type t =
  { uuid : int ; frequency : int } [@@deriving sexp, equal]
let last = ref 0 


let create frequency = 
  let uuid = !last in
  incr last;


  { uuid ; frequency }

