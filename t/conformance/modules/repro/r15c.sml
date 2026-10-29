functor Unit () = struct val u = "unit functor" end
structure U = Unit ()
val _ = print (U.u ^ "\n")
