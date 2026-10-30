datatype t = Nil' | ::: of int * t
infixr 5 :::
fun sum Nil' = 0 | sum (x ::: r) = x + sum r
val _ = print (Int.toString (sum (1 ::: 2 ::: Nil')) ^ "\n")
