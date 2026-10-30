fun p n = print (Int.toString n ^ "\n")
datatype t1 = X | Y of int
datatype t2 = datatype t1
fun f X = 0 | f (Y n) = n
val v : t2 = Y 5
val _ = p (f v)
datatype myopt = datatype option
val _ = p (case SOME 3 : int myopt of SOME k => k | NONE => 0)
structure S = struct datatype u = U1 | U2 of string end
datatype u' = datatype S.u
fun g U1 = "one" | g (U2 s) = s
val _ = print (g (U2 "two") ^ g S.U1 ^ "\n")
