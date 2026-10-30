fun <*> x = x + 100
val _ = print (Int.toString (<*> 1) ^ "\n")
