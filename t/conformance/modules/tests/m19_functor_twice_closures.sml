(* functions from an earlier application still see their own instance *)
signature V = sig val v : int end

functor Mk (X : V) =
struct
  val base = X.v * 10
  fun get () = base
  fun viaHelper () = helper () + 1
  and helper () = base
  fun arg () = X.v
end

structure P = Mk (struct val v = 1 end)
structure Q = Mk (struct val v = 2 end)

val _ = print (Int.toString (P.get ()) ^ "\n")
val _ = print (Int.toString (Q.get ()) ^ "\n")
val _ = print (Int.toString (P.viaHelper ()) ^ "\n")
val _ = print (Int.toString (P.arg ()) ^ "\n")
val _ = print (Int.toString P.base ^ "\n")
