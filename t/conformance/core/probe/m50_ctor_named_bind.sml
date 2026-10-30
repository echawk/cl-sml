datatype d = Bind of int | Other
fun f (Bind n) = n | f Other = 0
val _ = print (Int.toString (f (Bind 3)) ^ "\n")
