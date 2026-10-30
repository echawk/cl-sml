fun p n = print (Int.toString n ^ "\n")
val _ = p ((fn (x : int) => x + 1) 1)
val (a : int, b : string) = (3, "four")
val _ = p (a + size b)
val _ = p (case [1, 2] of (l : int list) => length l)
fun f (x : int) (y : int) : int = x * y
val _ = p (f 6 7)
fun g ((x, y) : int * int) = x - y
val _ = p (g (9, 2))
val h = fn (SOME (n : int)) => n | NONE => 0
val _ = p (h (SOME 8))
fun k (x : int list as _ :: _) = length x | k _ = 0
val _ = p (k [1, 2, 3])
