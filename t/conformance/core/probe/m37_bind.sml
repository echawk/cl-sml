val _ = print ((let val SOME y = (NONE : int option) in "no" end) handle Bind => "Bind\n")
