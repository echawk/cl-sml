abstype t = T of int with fun mk n = T n fun get (T n) = n end
val _ = print (Int.toString (get (mk 3)) ^ "\n")
