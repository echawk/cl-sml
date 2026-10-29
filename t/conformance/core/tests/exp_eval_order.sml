fun t s v = (print s; v)
val _ = (t "a" 1, t "b" 2, t "c" 3)
val _ = print "\n"
val _ = {zed = t "1" 1, alpha = t "2" 2, mid = t "3" 3}
val _ = print "\n"
val _ = {2 = t "x" 0, 1 = t "y" 0}
val _ = print "\n"
val _ = (t "f" (fn x => x)) (t "g" 0)
val _ = print "\n"
val _ = t "l" 1 + t "r" 2
val _ = print "\n"
val _ = [t "p" 1, t "q" 2, t "s" 3]
val _ = print "\n"
val _ = t "h" 1 :: t "i" []
val _ = print "\n"
fun f x y = x + y
val _ = f (t "u" 1) (t "v" 2)
val _ = print "\n"
val _ = SOME (t "w" 1)
val _ = print "\n"
val _ = (t "m" 1; t "n" 2)
val _ = print "\n"
