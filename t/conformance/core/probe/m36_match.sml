val _ = print (Int.toString ((fn 0 => 1) 2 handle Match => 7) ^ "\n")
fun partial (SOME x) = x
val _ = print (Int.toString (partial NONE handle Match => ~1) ^ "\n")
