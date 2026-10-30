(* same as lex_string_escapes but only top-level basis identifiers *)
fun codes [] = "" | codes [c] = Int.toString (ord c) | codes (c::cs) = Int.toString (ord c) ^ "," ^ codes cs
fun p s = print (codes (explode s) ^ "\n")
val _ = p "\n\t\\\""
val _ = p "\a\b\v\f\r"
val _ = p "\^@\^A\^C\^Z\^[\^_"
val _ = p "\065\066\000\255"
val _ = p "\u0041\u0062"
val _ = p "\u00E9\u00FF"
val _ = print ("ab\   \cd" ^ "\n")
val _ = print ("gap\
   \ across \
       \lines" ^ "\n")
