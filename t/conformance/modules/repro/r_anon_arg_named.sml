functor F (X : sig val k : int end) =
struct
  fun f n = n + X.k
end
structure K = struct val k = 1 end
structure A = F (K)
val _ = print (Int.toString (A.f 10) ^ "\n")
