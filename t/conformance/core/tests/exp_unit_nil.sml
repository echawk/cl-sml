fun b x = print ((if x then "true" else "false") ^ "\n")
val _ = b (() = ())
val _ = b ([] = nil)
val _ = b (([] : int list) = [])
val () = print "unit pattern\n"
val u = print "returns unit\n"
val _ = b (u = ())
val _ = b (ignore 5 = ())
val _ = b (null [])
val _ = b (null [()])
val _ = b (length [(), ()] = 2)
