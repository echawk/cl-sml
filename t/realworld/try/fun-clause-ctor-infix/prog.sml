infix D
fun (A :: C) D E = A ^ E
  | nil D E = E
val _ = print (op D (["Hello "], "world!\n"))
