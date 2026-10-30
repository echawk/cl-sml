fun p n = print (Int.toString n ^ "\n")
fun f 0 y = y
  | f x y = f (x - 1) (y + 1)
val _ = p (f 3 4)
fun g (x, 0) = x
  | g (x, y) = g (x * 2, y - 1)
val _ = p (g (1, 5))
fun even 0 = true
  | even n = odd (n - 1)
and odd 0 = false
  | odd n = even (n - 1)
val _ = print ((if even 4 andalso odd 3 then "ok" else "bad") ^ "\n")
fun h (SOME x) [] = x
  | h (SOME x) (y :: _) = x + y
  | h NONE _ = 0
val _ = p (h (SOME 1) [] + h (SOME 2) [3] + h NONE [9])
fun k a b c = a * 100 + b * 10 + c
val _ = p (k 1 2 3)
fun first (x) (y) = x
val _ = p (first 7 8)
fun ack 0 n = n + 1
  | ack m 0 = ack (m - 1) 1
  | ack m n = ack (m - 1) (ack m (n - 1))
val _ = p (ack 2 3)
fun nul NONE = 0 | nul (SOME _) = 1
val _ = p (nul NONE + nul (SOME 3))
fun tr true x = x | tr false _ = 0
val _ = p (tr true 5 + tr false 6)
