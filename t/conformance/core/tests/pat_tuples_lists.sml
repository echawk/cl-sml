fun p n = print (Int.toString n ^ "\n")
fun len [] = 0 | len [_] = 1 | len [_, _] = 2 | len (_ :: _ :: _ :: rest) = 3 + len rest
val _ = p (len [1, 2, 3, 4, 5])
fun firsts [] = [] | firsts ((a, _) :: r) = a :: firsts r
val _ = p (length (firsts [(1, 2), (3, 4)]))
fun pairs [(a, b)] = a * b | pairs [(a, b), (c, d)] = a * b + c * d | pairs _ = 0
val _ = p (pairs [(2, 3), (4, 5)])
val (q, (r, s), [t, u]) = (1, (2, 3), [4, 5])
val _ = p (q + r + s + t + u)
fun nested [[x]] = x | nested [[x, y], [z]] = x + y + z | nested _ = 0
val _ = p (nested [[1, 2], [3]])
val _ = p (case [1, 2, 3] of x :: y :: _ => x + y | _ => 0)
val _ = p (case (1, [2]) of (a, [b]) => a + b | _ => 0)
val _ = p (case [(1, "a")] of [(n, "a")] => n | _ => 0)
fun opcons (op :: (h, _)) = h | opcons [] = 0
val _ = p (opcons [7, 8])
