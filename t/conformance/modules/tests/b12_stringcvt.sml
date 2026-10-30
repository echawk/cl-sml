fun p s = print (s ^ "\n")
fun showOpt NONE = "NONE" | showOpt (SOME n) = "SOME " ^ Int.toString n
val _ = p ("[" ^ StringCvt.padLeft #"0" 5 "42" ^ "] [" ^ StringCvt.padRight #"." 5 "ab" ^ "] [" ^ StringCvt.padLeft #" " 2 "toolong" ^ "]")
val _ = p (showOpt (StringCvt.scanString (Int.scan StringCvt.DEC) "  77 rest"))
val _ = p (showOpt (Int.fromString " 42 "))
(* scanning from a substring with getc *)
val src = Substring.full "123abc"
val _ = p (case Int.scan StringCvt.DEC Substring.getc src of
               SOME (n, rest) => Int.toString n ^ " rest=" ^ Substring.string rest
             | NONE => "NONE")
(* scanning from a list of chars with a custom reader *)
fun listGetc [] = NONE | listGetc (c :: cs) = SOME (c, cs)
val _ = p (case Int.scan StringCvt.DEC listGetc (explode "~15!") of
               SOME (n, rest) => Int.toString n ^ " rest=" ^ implode rest
             | NONE => "NONE")
val _ = p (case Bool.scan listGetc (explode "true!") of SOME (b, _) => Bool.toString b | NONE => "NONE")
val _ = p (let val (s, r) = StringCvt.splitl Char.isDigit listGetc (explode "12ab") in s ^ "|" ^ implode r end)
val _ = p (implode (StringCvt.skipWS listGetc (explode "   x")))
val _ = p (case StringCvt.scanString Real.scan "2.55e1" of SOME r => Real.toString r | NONE => "NONE")
