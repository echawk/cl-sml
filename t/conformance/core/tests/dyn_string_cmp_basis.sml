(* @basis *)
fun s LESS = "LESS" | s EQUAL = "EQUAL" | s GREATER = "GREATER"
val _ = print (s (String.compare ("abc", "abd")) ^ "\n")
val _ = print (s (String.compare ("b", "abc")) ^ "\n")
val _ = print (s (String.compare ("", "")) ^ "\n")
val _ = print (s (Int.compare (3, 2)) ^ "\n")
val _ = print (s (Char.compare (#"a", #"b")) ^ "\n")
