fun p s = print (s ^ "\n")
fun pairs l = String.concatWith "," (map (fn (a, b) => Int.toString a ^ ":" ^ b) l)
val z = ListPair.zip ([1, 2, 3], ["a", "b"])
val _ = p (pairs z)
val (xs, ys) = ListPair.unzip [(1, "x"), (2, "y")]
val _ = p (String.concatWith "," (map Int.toString xs) ^ " " ^ String.concat ys)
val _ = p (String.concatWith "," (ListPair.map (fn (a, b) => Int.toString a ^ b) ([1, 2], ["p", "q"])))
val _ = ListPair.app (fn (a, b) => print (Int.toString (a + b) ^ " ")) ([1, 2], [10, 20])
val _ = p ""
val _ = p (ListPair.foldl (fn (a, b, acc) => acc ^ Int.toString a ^ b) "" ([1, 2], ["x", "y"]))
val _ = p (ListPair.foldr (fn (a, b, acc) => acc ^ Int.toString a ^ b) "" ([1, 2], ["x", "y"]))
val _ = p (if ListPair.all (fn (a, b) => a < b) ([1, 2], [2, 3]) then "all" else "not all")
val _ = p (if ListPair.exists (fn (a, b) => a = b) ([1, 2], [0, 2]) then "exists" else "none")
val _ = p ((pairs (ListPair.zipEq ([1], ["a", "b"]))) handle ListPair.UnequalLengths => "UnequalLengths")
