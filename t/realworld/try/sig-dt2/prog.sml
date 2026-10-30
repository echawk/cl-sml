structure S : sig
    datatype p = A | B
    val f : p -> int
  end = struct
    datatype p = A | B
    fun f A = 1 | f B = 2
  end
val _ = print (Int.toString (S.f S.B) ^ "\n")
