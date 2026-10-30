structure A = struct val c = "A.c" end
structure C = struct open A val d = c end
val _ = print (C.d ^ "\n")
