structure C : sig
    type t
    val fromInt : int -> t
    val toInt : t -> int
  end = struct
    type t = int
    fun fromInt x = x
    fun toInt x = x
  end
val _ = print (Int.toString (C.toInt (C.fromInt 7)) ^ "\n")
