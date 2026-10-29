(* @basis *)
structure S = struct
  val x = 10
  fun f y = y + x
  structure T = struct val z = 7 end
  datatype t = K of int
end
val _ = print (Int.toString (S.f S.x) ^ "\n")
val _ = print (Int.toString S.T.z ^ "\n")
val _ = print (Int.toString (case S.K 3 of S.K n => n) ^ "\n")
val _ = print (Int.toString (Int.max (2, 9)) ^ "\n")
val _ = print (String.concat ["a", "b"] ^ "\n")
