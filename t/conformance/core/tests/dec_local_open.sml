fun p n = print (Int.toString n ^ "\n")
local
  val h = 1
  fun helper x = x + h
in
  val v = helper 10
  fun pub x = helper x * 2
end
val _ = p v
val _ = p (pub 1)
val h = 50
val _ = p (pub 1)
structure S = struct
  val a = 2
  fun f x = x + a
  datatype d = D of int
end
open S
val _ = p (f a)
val _ = p (case D 5 of D n => n)
val r = let open S in f 100 end
val _ = p r
val q = 1; val q = q + 1; val _ = p q
