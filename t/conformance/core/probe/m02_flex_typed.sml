fun getb ({b, ...} : {a : int, b : int}) = b
val _ = print (Int.toString (getb {a = 1, b = 2}) ^ "\n")
