(*
 * (c) Andreas Rossberg 2001-2025
 *
 * Standard ML Basis Library
 *
 * cl-sml: LargeInt is IntInf (the Basis allows it; Poly/ML and MLton do
 * the same).  Here, after IntInf exists, come INTEGER and INT_INF, and
 * IntInf gains toLarge and fromLarge.
 *)

structure LargeInt = IntInf

signature INTEGER =
sig
  include INTEGER_CORE
  val toLarge : int -> LargeInt.int
  val fromLarge : LargeInt.int -> int
end;

signature INT_INF =
sig
  include INTEGER
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

structure IntInf :> INT_INF where type int = IntInf.int =
struct
  open IntInf
  fun toLarge i = i
  fun fromLarge i = i
end;

structure LargeInt = IntInf;
