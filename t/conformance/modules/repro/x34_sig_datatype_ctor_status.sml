(* datatype specs keep constructors; a val spec turns a constructor into a value *)
signature D = sig
  datatype t = A | B of int
  val c : int -> t
  end
structure M : D = struct
  datatype t = A | B of int
  val c = B
  end
fun f M.A = "A" | f (M.B n) = "B" ^ Int.toString n
val _ = print (f (M.c 3) ^ " " ^ f M.A ^ "\n")

(* A variable named like a hidden constructor *)
signature E = sig
  type t
  val A : t
  val isA : t -> bool
  end
structure N : E = struct
  datatype t = A | Z
  fun isA A = true | isA Z = false
  end
val _ = print ((if N.isA N.A then "isA" else "notA") ^ "\n")
