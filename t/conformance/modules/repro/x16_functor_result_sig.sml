(* functors with result signatures : SIG and :> SIG *)
signature NUM = sig
  type t
  val one : t
  val add : t * t -> t
  val toInt : t -> int
  end

functor Twice (N : NUM) : NUM =
struct
  type t = N.t
  val one = N.add (N.one, N.one)
  fun add (a, b) = N.add (a, b)
  fun toInt x = N.toInt x
  val leaked = 0
end

functor Opaque (N : NUM) :> NUM =
struct
  type t = N.t
  val one = N.one
  fun add (a, b) = N.add (N.add (a, b), N.one)
  fun toInt x = N.toInt x
end

structure I = struct
  type t = int
  val one = 1
  fun add (a, b) = a + b
  fun toInt x = x
  end
structure T = Twice (I)
structure O = Opaque (I)

val _ = print (Int.toString (T.toInt (T.add (T.one, T.one))) ^ "\n")
val _ = print (Int.toString (O.toInt (O.add (O.one, O.one))) ^ "\n")

val leaked = 123
open T
val _ = print (Int.toString leaked ^ "\n")
