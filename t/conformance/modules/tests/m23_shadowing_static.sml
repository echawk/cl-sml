(* static scoping across redefinitions at top level *)
val x = 1
fun getX () = x
val x = 2
val _ = print (Int.toString (getX ()) ^ "\n")
val _ = print (Int.toString x ^ "\n")

structure S = struct val v = 10 end
fun getV () = S.v
structure S = struct val v = 20 end
val _ = print (Int.toString (getV ()) ^ "\n")
val _ = print (Int.toString S.v ^ "\n")

fun f n = n + 1
fun g n = f n * 10
fun f n = n + 100
val _ = print (Int.toString (g 1) ^ "\n")
