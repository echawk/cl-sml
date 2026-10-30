fun abs_ { num, den } = { num = Int.abs num, den = den : int }
val r = abs_ {num = ~3, den = 4}
val _ = print (Int.toString (#num r) ^ "/" ^ Int.toString (#den r) ^ "\n")
