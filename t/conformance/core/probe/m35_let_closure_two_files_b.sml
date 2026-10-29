val f = let val y = 20 in fn () => y end
val _ = print (Int.toString (f ()) ^ "\n")
