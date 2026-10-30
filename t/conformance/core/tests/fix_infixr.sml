fun p n = print (Int.toString n ^ "\n")
infixr 5 +++
fun a +++ b = a - b
val _ = p (10 +++ 5 +++ 2)
infix 5 ---
fun a --- b = a - b
val _ = p (10 --- 5 --- 2)
infixr 2 $
fun f $ x = f x
val _ = p ((fn x => x + 1) $ (fn x => x * 2) $ 5)
