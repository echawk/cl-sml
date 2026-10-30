functor F (X : sig val k : int end) =
struct
  fun f n = n + X.k
end
structure A = F (struct val k = 1 end)
val _ = print (Int.toString (A.f 10) ^ "\n")
