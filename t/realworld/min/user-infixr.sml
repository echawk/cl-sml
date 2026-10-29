infixr 5 +++
fun a +++ b = a - b : int
val _ = print (Int.toString (10 +++ 5 +++ 2) ^ "\n")
