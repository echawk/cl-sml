fun f () = let val M = 5 in M + 1 end
val _ = print (Int.toString (f ()) ^ "\n")
