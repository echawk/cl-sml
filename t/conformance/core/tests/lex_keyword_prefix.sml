(* identifiers that start with a reserved word are ordinary identifiers *)
fun p n = print (Int.toString n ^ "\n")
fun raiseIt () = 5
val _ = p (raiseIt ())
val raised = 13
val _ = p (raised + 1)
val recx = 6
val _ = p recx
val rec recf = fn 0 => 0 | n => n + recf (n - 1)
val _ = p (recf 3)
val fnx = 7
val _ = p fnx
val ifx = 8
val _ = p (ifx)
val casex = 9
val _ = p casex
val letx = 10
val _ = p letx
val opx = 11
val _ = p opx
val whilex = 12
val _ = p whilex
