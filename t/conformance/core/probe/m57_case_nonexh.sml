val r = (case [1] of [] => "empty") handle Match => "Match"
val _ = print (r ^ "\n")
