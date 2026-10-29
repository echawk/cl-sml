structure U = struct
  structure Array = struct val sub = Array.sub end
end
val a = Array.fromList [1,2,3]
val _ = print (Int.toString (U.Array.sub (a, 1)) ^ "\n")
