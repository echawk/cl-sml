(* @basis *)
fun codes s = String.concatWith "," (map (Int.toString o ord) (explode s))
val _ = print (codes "\n\t\\\"" ^ "\n")
val _ = print (codes "\a\b\v\f\r" ^ "\n")
val _ = print (codes "\^@\^A\^C\^Z\^[\^_" ^ "\n")
val _ = print (codes "\065\066\000\255" ^ "\n")
val _ = print ("\u0041\u0062" ^ "\n")
val _ = print (codes "\u00E9\u00FF" ^ "\n")
val _ = print ("ab\   \cd" ^ "\n")
val _ = print ("gap\
   \ across \
       \lines" ^ "\n")
val _ = print (Int.toString (size "\t\n\\") ^ "\n")
