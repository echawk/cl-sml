exception E
val raiseOldE = fn () => raise E
exception E
val _ = print ((raiseOldE (); "none\n") handle E => "new\n" | _ => "old\n")
