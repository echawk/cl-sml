fun p n = print (Int.toString n ^ "\n")
val x = 1
val x = 2 and y = x
val _ = p (x * 10 + y)
val a = 5 and b = 6 and c = 7
val _ = p (a + b + c)
val (m, n) = (1, 2) and k = 3
val _ = p (m + n + k)
val f = fn () => x
val x = 100
val _ = p (f ())
