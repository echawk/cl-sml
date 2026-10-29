fun p n = print (Int.toString n ^ "\n")
infix 6 plus2
fun a plus2 b = a + b + 2
val _ = p (1 plus2 2)
infix 4 eqmod3
fun a eqmod3 b = a mod 3 = b mod 3
val _ = print (if 4 eqmod3 7 then "yes\n" else "no\n")
infix 7 times
fun a times b = a * b
val _ = p (1 plus2 2 times 3)
