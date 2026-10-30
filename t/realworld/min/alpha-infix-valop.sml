infix 9 over
val op over = (fn (a, b) => a * 10 + b : int)
val x = ~2 over 1
val _ = print (Int.toString x ^ "\n")
