fun s "a" = 1 | s "bc" = 2 | s _ = 3
val _ = print (Int.toString (s "a" + s "bc" * 10 + s "zz" * 100) ^ "\n")
