structure S : sig
    datatype p = X | O
    val f : p -> int
  end = struct
    datatype p = X | O
    fun f X = 1 | f O = 2
  end
val _ = print (Int.toString (S.f S.O) ^ "\n")
