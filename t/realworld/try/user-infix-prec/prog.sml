infix 6 ++
infix 7 **
fun a ++ b = a + b : int
fun a ** b = a * b : int
val _ = print (Int.toString (1 ++ 2 ** 3) ^ " " ^ Int.toString (2 ** 3 ++ 1) ^ "\n")
