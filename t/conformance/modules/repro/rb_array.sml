val b = Array.fromList [1, 2, 3, 4]
val _ = Array.modify (fn x => x * 10) b
val _ = Array.modifyi (fn (i, x) => x + i) b
val _ = print (Int.toString (Array.sub (b, 3)) ^ "\n")
val v = Array.vector b
val _ = print (Int.toString (Vector.length v) ^ " " ^ Int.toString (Vector.sub (v, 0)) ^ "\n")
