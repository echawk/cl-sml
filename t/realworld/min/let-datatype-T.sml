val f = fn x => let datatype t = T of int in case T x of T y => y end
val _ = print (Int.toString (f 42) ^ "\n")
