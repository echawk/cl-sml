(*
 * (c) Andreas Rossberg 2001-2025
 *
 * Standard ML Basis Library
 *)

(* cl-sml: LargeInt is IntInf, whose type is made before INTEGER can
 * mention it, so INTEGER_CORE is INTEGER without toLarge and fromLarge, and
 * INTEGER itself is in LargeInt.sml, after IntInf. *)

structure Int      = struct type int = int end

signature INTEGER_CORE =
sig
  eqtype int
  val toInt : int -> Int.int
  val fromInt : Int.int -> int
  val precision : Int.int option
  val minInt : int option
  val maxInt : int option
  val + : int * int -> int
  val - : int * int -> int
  val * : int * int -> int
  val div : int * int -> int
  val mod : int * int -> int
  val quot : int * int -> int
  val rem : int * int -> int
  val compare : int * int -> order
  val > : int * int -> bool
  val >= : int * int -> bool
  val < : int * int -> bool
  val <= : int * int -> bool
  val ~ : int -> int
  val abs : int -> int
  val min : int * int -> int
  val max : int * int -> int
  val sign : int -> Int.int
  val sameSign : int * int -> bool
  val fmt : StringCvt.radix -> int -> string
  val toString : int -> string
  val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
  val fromString : string -> int option
end;
