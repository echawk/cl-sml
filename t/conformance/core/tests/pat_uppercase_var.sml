(* an uppercase identifier that is not a constructor is a variable *)
fun f X = X + 1
val _ = print (Int.toString (f 41) ^ "\n")
val Y = 5
val _ = print (Int.toString (Y * 2) ^ "\n")
fun g (A, B) = A - B
val _ = print (Int.toString (g (10, 3)) ^ "\n")
val h = fn N => N * N
val _ = print (Int.toString (h 7) ^ "\n")
val Z :: _ = [8, 9]
val _ = print (Int.toString Z ^ "\n")
fun first (K :: _) = K | first [] = 0
val _ = print (Int.toString (first [3]) ^ "\n")
