functor WithSub (structure S : sig val v : int end) =
struct
  val w = S.v * 3
end
structure V = struct val v = 14 end
structure WS = WithSub (structure S = V)
val _ = print (Int.toString WS.w ^ "\n")
