fun p n = print (Int.toString n ^ "\n")
val _ = p (op + (1, 2))
val _ = p (op * (3, 4))
val _ = p (foldl op + 0 [1, 2, 3, 4])
val _ = p (length (op :: (1, [2, 3])))
val _ = p (op - (10, 3))
nonfix +
val _ = p (+ (5, 6))
infix 6 +
val _ = p (7 + 8)
infix 6 plus
fun x plus y = x + y
val _ = p (1 plus 2)
val _ = p (op plus (3, 4))
nonfix plus
val _ = p (plus (5, 6))
