fun mk () = let exception E in ((fn () => raise E) : unit -> unit, fn f => ((f (); "none") handle E => "caught")) end
val (r1, h1) = mk ()
val (r2, h2) = mk ()
val _ = print (h1 r1 ^ "\n")
val _ = print ((h1 r2) handle _ => "escaped\n")
