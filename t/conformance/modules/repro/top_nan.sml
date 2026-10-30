val x = 0.0 / 0.0
val _ = print (if Real.isNan x then "nan\n" else "not nan\n")
val _ = print (if x > 1.0 then "gt\n" else "not gt\n")
