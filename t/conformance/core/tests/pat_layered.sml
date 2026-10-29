fun p n = print (Int.toString n ^ "\n")
fun f (l as h :: _) = h + length l
  | f [] = 0
val _ = p (f [10, 20, 30])
val (x as (a, b)) = (1, 2)
val _ = p (a + b + #1 x)
fun g (y : int as 3) = y * 100
  | g z = z
val _ = p (g 3)
val _ = p (g 4)
val t as (u, v as (w, _)) = (5, (6, 7))
val _ = p (u + w + #2 v + #1 (#2 t))
fun h (whole as SOME (inner as (q, _))) = q + (case whole of SOME (r, _) => r | NONE => 0) + #2 inner
  | h NONE = ~1
val _ = p (h (SOME (4, 5)))
