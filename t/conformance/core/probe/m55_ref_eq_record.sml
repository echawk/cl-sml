val _ = print (if {a = ref 1} = {a = ref 1} then "eq\n" else "ne\n")
val r = ref 0
val _ = print (if (r, [r]) = (r, [r]) then "eq\n" else "ne\n")
