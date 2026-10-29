(* open inside structures, let and local *)
structure A =
struct
  val x = 10
  fun double n = 2 * n
end

structure B =
struct
  open A
  val y = double x + 1
end

val _ = print (Int.toString B.y ^ "\n")
(* members brought in by open are members of B *)
val _ = print (Int.toString B.x ^ "\n")
val _ = print (Int.toString (B.double 21) ^ "\n")

val z = let open A in double (x + 5) end
val _ = print (Int.toString z ^ "\n")

local
  open A
in
  val w = double 100
end
val _ = print (Int.toString w ^ "\n")

(* open inside a let inside a function body *)
fun g n = let open A in double n + x end
val _ = print (Int.toString (g 7) ^ "\n")
