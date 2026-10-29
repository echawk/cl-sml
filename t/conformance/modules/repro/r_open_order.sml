structure A = struct val c = "A.c" end
structure B = struct val c = "B.c" end
open A B
val _ = print (c ^ "\n")
