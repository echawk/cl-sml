val m = valOf Int.maxInt
val _ = print ((Int.toString (m + 1)) handle Overflow => "Overflow\n")
val _ = print (case Int.precision of NONE => "NONE\n" | SOME p => Int.toString p ^ "\n")
