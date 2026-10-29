fun p s = print (s ^ "\n")
fun mk () =
  let exception E
  in ((fn () => raise E) : unit -> unit, fn f => ((f (); "none") handle E => "caught")) end
val (r1, h1) = mk ()
val (r2, h2) = mk ()
val _ = p (h1 r1)
val _ = p ((h1 r2) handle _ => "escaped")
val _ = p ((h2 r1) handle _ => "escaped")
val _ = p (h2 r2)
exception T
val oldT = (fn () => raise T) : unit -> string
exception T
val _ = p ((oldT ()) handle T => "new" | _ => "old")
val _ = p ((raise T) handle T => "new" | _ => "old")
fun gen n = let exception X of int in if n = 0 then raise X 0 else (gen (n - 1) handle X k => raise X (k + 1)) end
val _ = p ((gen 3; "none") handle _ => "outer")
