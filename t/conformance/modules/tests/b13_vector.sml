fun p s = print (s ^ "\n")
fun vs v = String.concatWith "," (Vector.foldr (fn (x, acc) => Int.toString x :: acc) [] v)
val v = Vector.fromList [10, 20, 30]
val _ = p (Int.toString (Vector.length v) ^ " " ^ Int.toString (Vector.sub (v, 1)))
val _ = p ((Int.toString (Vector.sub (v, 3))) handle Subscript => "sub Subscript")
val _ = p ((Int.toString (Vector.sub (v, ~1))) handle Subscript => "sub neg Subscript")
val _ = p (vs (Vector.tabulate (4, fn i => i * 3)))
val _ = p (Int.toString (Vector.foldl (op +) 0 v))
val _ = p (Vector.foldl (fn (x, acc) => acc ^ Int.toString x) "" v)
val _ = p (Vector.foldr (fn (x, acc) => acc ^ Int.toString x) "" v)
val _ = Vector.app (fn x => print (Int.toString x ^ ";")) v
val _ = p ""
val _ = Vector.appi (fn (i, x) => print (Int.toString i ^ "=" ^ Int.toString x ^ ";")) v
val _ = p ""
val _ = p (vs (Vector.map (fn x => x + 1) v))
val _ = p (vs (Vector.update (v, 0, 99)) ^ " / " ^ vs v)
val _ = p (vs (Vector.concat [v, Vector.fromList [1, 2]]))
val _ = p (case Vector.find (fn x => x > 15) v of SOME x => Int.toString x | NONE => "NONE")
val _ = p (if Vector.exists (fn x => x = 20) v andalso Vector.all (fn x => x > 5) v then "ok" else "bad")
val _ = p (vs (Vector.mapi (fn (i, x) => i + x) v))
