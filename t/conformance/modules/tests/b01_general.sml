(* General: o, before, ignore, exnName, exnMessage, !, := *)
fun p s = print (s ^ "\n")
val f = (fn x => x + 1) o (fn x => x * 2)
val _ = p (Int.toString (f 5))
val r = ref 0
val v = (r := 1; 10) before r := 2
val _ = p (Int.toString v ^ " " ^ Int.toString (!r))
val _ = ignore (3 + 4)
val _ = p (General.exnName Div)
val _ = p (exnName (Fail "msg"))
val _ = p (exnName Subscript)
exception MyExn of int
val _ = p (exnName (MyExn 3))
val _ = p ((Int.toString (1 div 0)) handle Div => "Div")
val _ = p ((Int.toString (valOf NONE)) handle Option => "Option")
val _ = p ((hd []) handle Empty => "Empty")
val _ = p (case General.LESS of LESS => "LESS" | _ => "?")
val _ = p (Int.toString (!(ref 5)))
