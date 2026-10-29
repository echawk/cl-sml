structure S = struct val v = 10 fun get () = v end
val g = S.get
structure S = struct val v = 20 fun get () = v end
val _ = print (Int.toString (g ()) ^ "\n")
