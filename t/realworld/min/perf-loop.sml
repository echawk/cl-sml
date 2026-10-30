fun loop (i, acc) = if i = 0 then acc else loop (i - 1, acc + i * 2)
val _ = print (Int.toString (loop (10, 0)) ^ "\n")
