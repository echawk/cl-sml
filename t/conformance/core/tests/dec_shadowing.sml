fun p n = print (Int.toString n ^ "\n")
val x = 1
fun getx () = x
val x = 2
val _ = p (getx ())
val y = 10
val f = let val y = 20 in fn () => y end
val _ = p (f ())
val _ = p y
fun g z = let val y = z in fn () => y end
val g1 = g 1
val g2 = g 2
val _ = p (g1 () + g2 () * 10)
fun shadow n = let val n = n + 1 in let val n = n * 2 in n end end
val _ = p (shadow 3)
datatype k = K of int
fun unK (K n) = n
val old = K 7
datatype k = K of string
val _ = p (unK old)
val _ = print ((case K "new" of K s => s) ^ "\n")
fun h () = 1
val h1 = h
fun h () = 2
val _ = p (h1 () * 10 + h ())
