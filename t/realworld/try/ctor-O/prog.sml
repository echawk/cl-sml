datatype p = X | O
fun f X = 1 | f O = 2
val _ = print (Int.toString (f O) ^ "\n")
