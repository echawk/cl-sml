fun f ~1 = 0 | f n = n
val _ = print (Int.toString (f ~1 + f 5) ^ "\n")
