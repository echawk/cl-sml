val a = (String.sub ("abc", 5); "no") handle Subscript => "sub"
val b = (Vector.sub (vector [1], 3); "no") handle Subscript => "vsub" | _ => "other"
val c = (Char.chr 300; "no") handle Chr => "chr"
val d = (String.sub ("abc", 5); "no") handle e => exnName e
val _ = print (a ^ " " ^ b ^ " " ^ c ^ " " ^ d ^ "\n")
