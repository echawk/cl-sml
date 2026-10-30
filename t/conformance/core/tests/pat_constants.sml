fun p s = print (s ^ "\n")
fun i 0 = "zero" | i ~1 = "minus one" | i 0x10 = "sixteen" | i _ = "other"
val _ = p (i 0) val _ = p (i ~1) val _ = p (i 16) val _ = p (i 5)
fun s "a" = 1 | s "bc" = 2 | s "" = 3 | s _ = 4
val _ = p (Int.toString (s "a" + s "bc" * 10 + s "" * 100 + s "zz" * 1000))
fun c #"x" = "ex" | c #"\n" = "newline" | c _ = "?"
val _ = p (c #"x") val _ = p (c #"\n") val _ = p (c #"y")
fun w 0w0 = "w0" | w 0wx10 = "w16" | w _ = "w?"
val _ = p (w 0w0) val _ = p (w 0w16) val _ = p (w 0w3)
fun bb (true, false) = "tf" | bb (_, true) = "_t" | bb _ = "ff"
val _ = p (bb (true, false)) val _ = p (bb (false, true)) val _ = p (bb (false, false))
val _ = p (case () of () => "unit")
fun u () = "unit fn"
val _ = p (u ())
