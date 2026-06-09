module type Config = sig
  val id_width : int
  val addr_width : int
  val data_width : int
  val num_lines : int
  (* if > 0 this implements a skewed associative cache (multiple hashfns mix the bits of the address *)
end
