datatype a = A of b | Z
and b = B of {
   x : int,
   y : a }
fun f (A (B {x, ...})) = x | f Z = 0
val _ = print (Int.toString (f (A (B {x = 4, y = Z}))) ^ "\n")
