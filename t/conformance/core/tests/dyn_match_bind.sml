fun p s = print (s ^ "\n")
val _ = p (Int.toString ((fn 0 => 1) 2 handle Match => 7))
val _ = p (Int.toString (let val (x :: _) = [] : int list in x end handle Bind => 0))
fun partial (SOME x) = x
val _ = p (Int.toString (partial NONE handle Match => ~1))
val _ = p ((case 3 of 1 => "one" | 2 => "two") handle Match => "match")
val _ = p (let val SOME y = SOME 5 in Int.toString y end)
val _ = p ((let val 1 = 2 in "no" end) handle Bind => "bind")
val _ = p ((let val (a, 0) = (1, 1) in "no" end) handle Bind => "bind2")
fun f x = case x of [] => "empty"
val _ = p ((f [1]) handle Match => "match2")
val _ = p ((raise Match) handle Bind => "wrong" | Match => "explicit match")
