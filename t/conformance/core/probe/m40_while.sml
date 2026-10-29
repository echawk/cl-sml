val i = ref 0
val _ = while !i < 3 do i := !i + 1
val _ = print (Int.toString (!i) ^ "\n")
