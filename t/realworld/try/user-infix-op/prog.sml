infix 4 <$>
fun f <$> x = f x
val _ = print (Int.toString ((fn x => x + 1) <$> 41) ^ "\n")
