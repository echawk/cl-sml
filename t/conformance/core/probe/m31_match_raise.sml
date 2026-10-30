val _ = print (Int.toString ((fn 0 => 1) 2 handle Match => 7) ^ "\n")
val _ = print ((let val SOME y = NONE : int option in "no" end) handle Bind => "Bind\n")
