fun k (x : int list as _ :: _) = length x | k _ = 0
val _ = print (Int.toString (k [1, 2, 3]) ^ "\n")
