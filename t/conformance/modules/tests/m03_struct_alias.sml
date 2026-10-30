(* structure aliases, including aliases of nested structures *)
structure S =
struct
  val x = 3
  structure T = struct val y = 4 fun h z = z * 10 end
end

structure A = S
structure U = S.T
structure V = A.T

val _ = print (Int.toString A.x ^ "\n")
val _ = print (Int.toString A.T.y ^ "\n")
val _ = print (Int.toString U.y ^ "\n")
val _ = print (Int.toString (V.h 5) ^ "\n")

structure W = struct structure Q = S end
val _ = print (Int.toString W.Q.T.y ^ "\n")
