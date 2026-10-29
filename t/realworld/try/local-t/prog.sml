fun f () = let val T = 5 in T + 1 end
val _ = print (Int.toString (f ()) ^ "\n")
