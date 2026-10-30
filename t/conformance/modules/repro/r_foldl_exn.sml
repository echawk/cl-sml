val count = ref 0
fun f (x, acc) = (count := !count + 1; if x = 2 then raise Fail "two" else x + acc)
val r = (foldl f 0 [1, 2, 3]) handle Fail s => ~1
val _ = print (Int.toString r ^ " calls=" ^ Int.toString (!count) ^ "\n")
