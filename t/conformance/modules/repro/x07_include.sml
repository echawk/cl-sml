(* include in signatures *)
signature BASE =
sig
  type t
  val zero : t
  val show : t -> string
end

signature EXT =
sig
  include BASE
  val succ : t -> t
end

structure N : EXT =
struct
  type t = int
  val zero = 0
  fun show n = "N" ^ Int.toString n
  fun succ n = n + 1
  val notInSig = 5
end

val _ = print (N.show (N.succ (N.succ N.zero)) ^ "\n")

(* include of a signature expression *)
signature EXT2 =
sig
  include sig
  type u
  val one : u
  end
  val toInt : u -> int
end

structure O : EXT2 = struct
  type u = int
  val one = 1
  fun toInt x = x
  end
val _ = print (Int.toString (O.toInt O.one) ^ "\n")

(* a member hidden by the included signature does not shadow an outer one *)
val notInSig = 77
open N
val _ = print (Int.toString notInSig ^ "\n")
