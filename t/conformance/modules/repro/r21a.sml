signature X = sig val y : int end
structure T = struct
  val y = 5
end : X
val _ = print (Int.toString T.y ^ "\n")
