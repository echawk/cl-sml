(* @basis *)
(* @reference poly: MLton prints "1" for 1.0; Poly/ML and Moscow ML print "1.0" *)
fun p r = print (Real.toString r ^ "\n")
val _ = p 1.0
val _ = p 0.1
val _ = p 2.5
val _ = p ~2.5
val _ = p 0.0
val _ = p 100.0
val _ = p 1234567.0
val _ = p 0.001
val _ = p 1.5E~3
val _ = p (1.0 / 3.0)
val _ = p 1E10
val _ = p 123456789.0
val _ = p 1E22
val _ = p 3.14159
val _ = p (real 7)
val _ = p (2.0 * 0.5)
