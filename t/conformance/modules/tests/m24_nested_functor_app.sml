(* functor applied to the result of another functor application *)
functor Inc (X : sig val n : int end) = struct val n = X.n + 1 end
structure Zero = struct val n = 0 end
structure Two = Inc (Inc (Zero))
val _ = print (Int.toString Two.n ^ "\n")
structure Three = Inc (struct val n = Two.n end)
val _ = print (Int.toString Three.n ^ "\n")
