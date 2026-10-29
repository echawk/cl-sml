(* signature bindings with and; structure and functor bindings with and *)
signature A = sig val a : int end
      and B = sig val b : int end

structure SA : A = struct val a = 1 end
      and SB : B = struct val b = 2 end

val _ = print (Int.toString (SA.a + SB.b) ^ "\n")

functor F (X : A) = struct val fa = X.a + 10 end
    and G (Y : B) = struct val gb = Y.b + 20 end

structure FA = F (SA)
structure GB = G (SB)
val _ = print (Int.toString FA.fa ^ "\n")
val _ = print (Int.toString GB.gb ^ "\n")
