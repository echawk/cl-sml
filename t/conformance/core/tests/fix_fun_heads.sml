fun p n = print (Int.toString n ^ "\n")
infix 5 ++
fun x ++ y = x * 10 + y
val _ = p (1 ++ 2)
infix 5 <*>
fun (f <*> g) x = f (g x)
val _ = p (((fn x => x + 1) <*> (fn x => x * 3)) 4)
infix 5 @@
fun [] @@ ys = ys
  | (x :: xs) @@ ys = x :: (xs @@ ys)
val _ = p (length ([1, 2] @@ [3, 4, 5]))
infix 5 %
fun (a, b) % (c, d) = a * d - b * c
val _ = p ((1, 2) % (3, 4))
fun op // (a, b) = a div b
val _ = p (// (9, 2))
infix 7 //
val _ = p (9 // 2 // 2)
