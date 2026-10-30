(* state created in a functor body belongs to each application *)
functor Counter (val start : int) =
struct
  val r = ref start
  fun next () = (r := !r + 1; !r)
  fun get () = !r
end

structure C1 = Counter (val start = 0)
structure C2 = Counter (val start = 100)

val _ = print (Int.toString (C1.next ()) ^ "\n")
val _ = print (Int.toString (C1.next ()) ^ "\n")
val _ = print (Int.toString (C2.next ()) ^ "\n")
val _ = print (Int.toString (C1.get ()) ^ "\n")
val _ = print (Int.toString (!C1.r) ^ "\n")
val _ = print (Int.toString (!C2.r) ^ "\n")
