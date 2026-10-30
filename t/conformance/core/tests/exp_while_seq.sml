fun p n = print (Int.toString n ^ "\n")
val i = ref 0
val s = ref 0
val _ = while !i < 5 do (s := !s + !i; i := !i + 1)
val _ = p (!s)
val _ = p (!i)
val v = (print "one "; print "two "; 3)
val _ = p v
val w = let val a = 1 in print "x "; print "y "; a + 1 end
val _ = p w
val u = (while false do (); ())
val _ = print (case u of () => "unit\n")
val cnt = ref 10
val _ = while (cnt := !cnt - 1; !cnt > 5) do print "."
val _ = p (!cnt)
