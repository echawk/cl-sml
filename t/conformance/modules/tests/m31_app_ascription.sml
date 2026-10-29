(* ascription on a structure bound to a functor application *)
signature S = sig val f : int -> int end
functor F (X : sig val k : int end) = struct fun f n = n + X.k val private = 0 end
structure A : S = F (struct val k = 1 end)
structure B :> S = F (struct val k = 2 end)
structure C = F (struct val k = 3 end) : S
val _ = print (Int.toString (A.f 10) ^ " " ^ Int.toString (B.f 10) ^ " " ^ Int.toString (C.f 10) ^ "\n")
val private = 99
open A
val _ = print (Int.toString private ^ "\n")
