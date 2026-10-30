functor Double (S : sig type t val f : t -> t end) = struct
  val ff = fn x => S.f (S.f x)
end
structure F = Double (struct type t = int fun f x = x + x end)
val _ = print (Int.toString (F.ff 42) ^ "\n")
