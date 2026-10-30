(* exceptions declared in a functor body are fresh per application *)
functor F () = struct exception E of int fun raiseIt n = raise E n end

structure A = F ()
structure B = F ()

fun test f =
  (f (); "no exception")
  handle B.E n => "B.E " ^ Int.toString n
       | A.E n => "A.E " ^ Int.toString n
       | _ => "other"

val _ = print (test (fn () => A.raiseIt 1) ^ "\n")
val _ = print (test (fn () => B.raiseIt 2) ^ "\n")
val _ = print (test (fn () => raise A.E 3) ^ "\n")
val _ = print (test (fn () => raise B.E 4) ^ "\n")

(* nullary exception *)
functor G () = struct exception X fun boom () = raise X end
structure G1 = G ()
structure G2 = G ()
val _ = print ((G1.boom (); "none") handle G2.X => "G2.X" | G1.X => "G1.X")
val _ = print "\n"
