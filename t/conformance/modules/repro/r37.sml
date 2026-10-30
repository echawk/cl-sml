functor F (X : sig val n : int end) =
struct
  fun get () = X.n
end
structure One = struct val n = 1 end
structure Two = struct val n = 2 end
structure A = F (One)
functor F (X : sig val n : int end) =
struct
  fun get () = X.n * 100
end
structure B = F (Two)
val _ = print (Int.toString (A.get ()) ^ " " ^ Int.toString (B.get ()) ^ "\n")
