infixr 5 +++
fun a +++ b = a - b
val _ = print (Int.toString (10 +++ 5 +++ 2) ^ "\n")
