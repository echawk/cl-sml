(* structure specs and eqtype specs *)
signature POINT =
sig
  eqtype p
  val origin : p
  val make : int * int -> p
  structure Ops : sig val dist2 : p -> int end
end

structure Pt :> POINT =
struct
  type p = int * int
  val origin = (0, 0)
  fun make (x, y) = (x, y)
  structure Ops = struct fun dist2 (x, y) = x * x + y * y end
end

val a = Pt.make (3, 4)
val _ = print (Int.toString (Pt.Ops.dist2 a) ^ "\n")
val _ = print (if a = Pt.make (3, 4) then "equal\n" else "not equal\n")
val _ = print (if a = Pt.origin then "origin\n" else "not origin\n")
