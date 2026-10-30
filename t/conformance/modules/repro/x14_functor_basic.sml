(* basic functor with a named signature argument *)
signature ORD = sig
  type t
  val lt : t * t -> bool
  val show : t -> string
  end

functor Sort (O : ORD) =
struct
  fun insert (x, []) = [x]
    | insert (x, y :: ys) = if O.lt (x, y) then x :: y :: ys else y :: insert (x, ys)
  fun sort xs = foldl insert [] xs
  fun show xs = concat (map (fn x => O.show x ^ " ") xs)
end

structure IntOrd = struct
  type t = int
  fun lt (a : int, b) = a < b
  fun show n = Int.toString n
  end
structure RevOrd = struct
  type t = int
  fun lt (a : int, b) = a > b
  fun show n = "r" ^ Int.toString n
  end

structure IS = Sort (IntOrd)
structure RS = Sort (RevOrd)

val _ = print (IS.show (IS.sort [3, 1, 2, 5, 4]) ^ "\n")
val _ = print (RS.show (RS.sort [3, 1, 2, 5, 4]) ^ "\n")
(* use the first instance again after the second was created *)
val _ = print (IS.show (IS.sort [9, 8, 7]) ^ "\n")
