signature ORD = sig type t val lt : t * t -> bool end
functor Sort (O : ORD) =
struct
  fun insert (x, []) = [x]
    | insert (x, y :: ys) = if O.lt (x, y) then x :: y :: ys else y :: insert (x, ys)
end
structure IntOrd = struct type t = int
  fun lt (a : int, b) = a < b end
structure IS = Sort (IntOrd)
val _ = print (Int.toString (length (IS.insert (2, [1, 3]))) ^ "\n")
