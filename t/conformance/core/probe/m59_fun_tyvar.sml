fun 'a twice (f : 'a -> 'a) (x : 'a) = f (f x)
val _ = print (Int.toString (twice (fn x => x + 1) 0) ^ "\n")
