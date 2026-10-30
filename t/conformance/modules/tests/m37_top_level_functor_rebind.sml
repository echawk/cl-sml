(* rebinding a functor name does not affect structures built earlier *)
functor F (X : sig val n : int end) = struct fun get () = X.n end
structure A = F (struct val n = 1 end)
functor F (X : sig val n : int end) = struct fun get () = X.n * 100 end
structure B = F (struct val n = 2 end)
val _ = print (Int.toString (A.get ()) ^ " " ^ Int.toString (B.get ()) ^ "\n")
