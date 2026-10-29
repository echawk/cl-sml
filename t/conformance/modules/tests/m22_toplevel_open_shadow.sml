(* top-level open shadows earlier bindings; later bindings shadow opened ones *)
val x = 1
val y = "outer y"
structure S = struct val x = 2 fun f () = x end
open S
val _ = print (Int.toString x ^ "\n")
val _ = print (y ^ "\n")
val x = 3
val _ = print (Int.toString x ^ "\n")
val _ = print (Int.toString S.x ^ "\n")
val _ = print (Int.toString (f ()) ^ "\n")
