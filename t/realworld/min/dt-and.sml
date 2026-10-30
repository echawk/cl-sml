datatype a = A of b | Z
and b = B of int
fun f (A (B x)) = x | f Z = 0
val _ = print (Int.toString (f (A (B 4))) ^ "\n")
