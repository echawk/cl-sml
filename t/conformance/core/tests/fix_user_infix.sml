fun p n = print (Int.toString n ^ "\n")
infix 6 ++
fun a ++ b = a + b
val _ = p (1 ++ 2 * 3)
infix 7 **
fun a ** b = a * b
val _ = p (2 + 3 ** 4)
val _ = p (2 ** 3 + 4)
infix 1 <<<
fun a <<< b = a - b
val _ = p (10 <<< 2 + 3)
infix 9 ^^
fun a ^^ b = a * 100 + b
val _ = p (1 + 2 ^^ 3)
