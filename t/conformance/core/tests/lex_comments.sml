(* Definition 2.7: comments nest *)
(* outer (* inner *) still outer *)
val a = 1 (* trailing *) + (* mid (* nested *) *) 2
(**)
(***)
val s = "(* not a comment *)"
val _ = print (Int.toString a ^ "\n")
val _ = print (s ^ "\n")
(* multi
   line
   (* nested
      (* deep *) *)
   comment *)
val _ = print "end\n"
