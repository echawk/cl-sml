(* @basis *)
(* when Int is bounded, overflow of every arithmetic operator raises Overflow *)
fun ok f = (ignore (f ()); false) handle Overflow => true
val _ =
  case Int.maxInt of
      NONE => print "ok\nok\nok\nok\n"
    | SOME mx =>
        let val mn = valOf Int.minInt
            fun r b = print ((if b then "ok" else "no-overflow") ^ "\n")
        in r (ok (fn () => mx + 1));
           r (ok (fn () => mn - 1));
           r (ok (fn () => mx * mx));
           r (ok (fn () => mn div ~1))
        end
