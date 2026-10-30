infix 5 !!
fun a !! b = (print "side\n"; if a > b then raise Fail "big" else a + b)
val _ = print ((Int.toString (3 !! 2)) handle Fail m => m ^ "\n")
