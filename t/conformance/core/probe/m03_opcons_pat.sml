fun hd' (op :: (h, _)) = h | hd' [] = 0
val _ = print (Int.toString (hd' [7, 8]) ^ "\n")
