fun p n = print (Int.toString n ^ "\n")
val _ = p ~5
val _ = p 0x1F
val _ = p ~0x1F
val _ = p 0xff
val _ = p 007
val _ = print (if 0w7 = 0w7 then "w1\n" else "bad\n")
val _ = print (if 0wx1F = 0w31 then "w2\n" else "bad\n")
val _ = print (if 0wxff = 0w255 then "w3\n" else "bad\n")
val _ = print (if 0w3 + 0w4 = 0w7 then "w4\n" else "bad\n")
val _ = p (~ 3)
val _ = p (2 - ~3)
