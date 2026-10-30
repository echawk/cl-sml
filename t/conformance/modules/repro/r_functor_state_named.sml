functor Counter (X : sig val start : int end) =
struct
  val r = ref X.start
  fun next () = (r := !r + 1; !r)
end
structure Z = struct val start = 0 end
structure H = struct val start = 100 end
structure C1 = Counter (Z)
structure C2 = Counter (H)
val _ = print (Int.toString (C1.next ()) ^ "\n")
