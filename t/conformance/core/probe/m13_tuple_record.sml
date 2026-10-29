val _ = print (if {1 = 1, 2 = 2} = (1, 2) then "true\n" else "false\n")
val {1 = x, 2 = y} = (7, 8)
val _ = print (Int.toString (x + y) ^ "\n")
