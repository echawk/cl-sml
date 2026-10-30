fun f () = 1
fun g () = f ()
fun f () = 2
val _ = print (Int.toString (g ()) ^ "\n")
