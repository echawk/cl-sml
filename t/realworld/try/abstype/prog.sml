abstype t = T of int with fun mk x = T x fun get (T x) = x end
val _ = print (Int.toString (get (mk 3)) ^ "\n")
