(* Nested structures and long identifiers S.T.x *)
structure Outer =
struct
  val a = 1
  structure Inner =
  struct
    val b = a + 10
    structure Deep =
    struct
      val c = b + 100
      fun f x = x + c
    end
    val d = Deep.c + 1
  end
  val e = Inner.Deep.f 1000
end

val _ = print (Int.toString Outer.a ^ "\n")
val _ = print (Int.toString Outer.Inner.b ^ "\n")
val _ = print (Int.toString Outer.Inner.Deep.c ^ "\n")
val _ = print (Int.toString (Outer.Inner.Deep.f 5) ^ "\n")
val _ = print (Int.toString Outer.Inner.d ^ "\n")
val _ = print (Int.toString Outer.e ^ "\n")
