val i = ref 0
val _ = while !i < 3 do (print (Int.toString (!i)); i := !i + 1)
val _ = print "\n"
