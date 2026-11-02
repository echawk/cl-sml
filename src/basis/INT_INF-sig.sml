(*
 * cl-sml: Standard ML Basis Library, signature INT_INF
 * (not part of HaMLet's basis).
 *
 * INT_INF_CORE is INT_INF without toLarge and fromLarge, for IntInf before
 * LargeInt exists; INT_INF itself is in LargeInt.sml.
 *)

signature INT_INF_CORE =
sig
  include INTEGER_CORE
  val divMod : int * int -> int * int
  val quotRem : int * int -> int * int
  val pow : int * Int.int -> int
  val log2 : int -> Int.int
  val orb : int * int -> int
  val xorb : int * int -> int
  val andb : int * int -> int
  val notb : int -> int
  val << : int * word -> int
  val ~>> : int * word -> int
end;
