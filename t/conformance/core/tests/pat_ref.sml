fun p n = print (Int.toString n ^ "\n")
fun deref (ref x) = x
val _ = p (deref (ref 5))
val ref y = ref 6
val _ = p y
val r = ref 0
val _ = p (case r of ref 0 => 100 | ref n => n)
val _ = r := 9
val _ = p (case r of ref 0 => 100 | ref n => n)
val (ref a, ref b) = (ref 1, ref 2)
val _ = p (a + b)
fun incr (c as ref n) = c := n + 1
val _ = incr r
val _ = p (!r)
val _ = p (case SOME (ref 3) of SOME (ref k) => k | NONE => 0)
