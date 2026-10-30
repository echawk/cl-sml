fun b x = print ((if x then "true" else "false") ^ "\n")
infix 0 ==>
fun a ==> c = not a orelse c
val _ = b (true ==> false = false)
val _ = b (false ==> 1 = 2)
val _ = b ((true ==> false) = false)
