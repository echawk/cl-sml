infix 5 @@
fun [] @@ ys = ys | (x :: xs) @@ ys = x :: (xs @@ ys)
val _ = print (Int.toString (length ([1] @@ [2, 3])) ^ "\n")
