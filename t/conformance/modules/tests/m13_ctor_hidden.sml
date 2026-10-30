(* constructors are hidden when the signature specifies only `type t` *)
datatype outer = A | B of int

signature ABS = sig type t val mk : int -> t val show : t -> string end

structure M :> ABS =
struct
  datatype t = A of int | B
  fun mk 0 = B
    | mk n = A n
  fun show (A n) = "A " ^ Int.toString n
    | show B = "B"
end

structure N : ABS =
struct
  datatype t = A of int | B
  fun mk 0 = B
    | mk n = A n
  fun show (A n) = "NA " ^ Int.toString n
    | show B = "NB"
end

open M
open N
(* A and B are still the outer constructors *)
fun outerShow A = "outer A"
  | outerShow (B n) = "outer B " ^ Int.toString n
val _ = print (outerShow A ^ "\n")
val _ = print (outerShow (B 7) ^ "\n")
val _ = print (M.show (M.mk 3) ^ "\n")
val _ = print (M.show (M.mk 0) ^ "\n")
val _ = print (show (mk 4) ^ "\n")
