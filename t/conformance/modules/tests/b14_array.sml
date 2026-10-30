fun p s = print (s ^ "\n")
fun as2s a = String.concatWith "," (Array.foldr (fn (x, acc) => Int.toString x :: acc) [] a)
val a = Array.array (3, 0)
val _ = Array.update (a, 1, 5)
val _ = p (as2s a ^ " len=" ^ Int.toString (Array.length a))
val _ = p ((Array.update (a, 3, 1); "no exn") handle Subscript => "update Subscript")
val _ = p ((Int.toString (Array.sub (a, 3))) handle Subscript => "sub Subscript")
val b = Array.fromList [1, 2, 3, 4]
val _ = p (Int.toString (Array.sub (b, 2)))
val _ = p (as2s (Array.tabulate (5, fn i => i * i)))
val _ = p (Int.toString (Array.foldl (op +) 0 b))
val _ = Array.app (fn x => print (Int.toString x)) b
val _ = p ""
val _ = Array.modify (fn x => x * 10) b
val _ = p (as2s b)
val _ = Array.modifyi (fn (i, x) => x + i) b
val _ = p (as2s b)
val c = Array.array (6, ~1)
val _ = Array.copy {src = b, dst = c, di = 1}
val _ = p (as2s c)
val _ = p ((Array.copy {src = b, dst = c, di = 3}; "copied") handle Subscript => "copy Subscript")
val v = Array.vector b
val _ = p (Int.toString (Vector.length v) ^ " " ^ Int.toString (Vector.sub (v, 0)))
val _ = p ((as2s (Array.array (~1, 0))) handle Size => "array Size")
val _ = p (case Array.findi (fn (i, x) => x > 20) b of SOME (i, x) => Int.toString i ^ ":" ^ Int.toString x | NONE => "NONE")
(* arrays have identity equality *)
val _ = p (if Array.fromList [1] = Array.fromList [1] then "equal" else "distinct")
val _ = p (if a = a then "self equal" else "self distinct")
