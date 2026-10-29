val rec f = fn 0 => 0 | n => g (n - 1) and g = fn n => f n
val _ = print (Int.toString (f 3) ^ "\n")
