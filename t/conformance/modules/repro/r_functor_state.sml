functor Counter (X : sig val start : int end) =
struct
  val r = ref X.start
  fun next () = (r := !r + 1; !r)
end
structure C1 = Counter (struct val start = 0 end)
structure C2 = Counter (struct val start = 100 end)
val _ = print (Int.toString (C1.next ()) ^ "\n")
