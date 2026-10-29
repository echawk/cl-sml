fun p n = print (Int.toString n ^ "\n")
datatype t = Nil' | ::: of int * t
infixr 5 :::
val l = 1 ::: 2 ::: 3 ::: Nil'
fun sum Nil' = 0
  | sum (x ::: r) = x + sum r
val _ = p (sum l)
fun second (_ ::: y ::: _) = y
  | second _ = ~1
val _ = p (second l)
infix 4 <=>
datatype pair = <=> of int * int
fun diff (a <=> b) = a - b
val _ = p (diff (9 <=> 4))
val _ = p (case 3 <=> 1 of x <=> y => x * y)
