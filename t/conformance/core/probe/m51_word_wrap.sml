val w = 0w0 - 0w1
val _ = print (if w > 0w0 then "wraps\n" else "negative\n")
val _ = print (if w + 0w1 = 0w0 then "zero\n" else "nonzero\n")
