(* identifiers that are ordinary nonfix variables in SML97 *)
val plus = 3
val sub = 4
val oplus = 5
fun id x = x
val _ = print (Int.toString (id plus) ^ "\n")
val _ = print (Int.toString (id sub) ^ "\n")
val _ = print (Int.toString (id oplus + id plus) ^ "\n")
val l = [plus, sub]
val _ = print (Int.toString (length l) ^ "\n")
