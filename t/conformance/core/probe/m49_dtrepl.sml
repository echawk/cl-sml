datatype t1 = X | Y of int
datatype t2 = datatype t1
fun f X = 0 | f (Y n) = n
val _ = print (Int.toString (f (Y 5)) ^ "\n")
datatype myopt = datatype option
val _ = print (Int.toString (case (SOME 3 : int myopt) of SOME k => k | NONE => 0) ^ "\n")
