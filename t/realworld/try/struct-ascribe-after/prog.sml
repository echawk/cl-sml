signature S = sig type t val f : t -> int val mk : int -> t end
structure X = struct
  type t = int
  fun f x = x + 1
  fun mk x = x
end : S
val _ = print (Int.toString (X.f (X.mk 1)) ^ "\n")
