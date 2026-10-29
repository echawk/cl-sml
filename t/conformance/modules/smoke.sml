structure S = struct type t = int fun f x = x + 1 end
val _ = print (Int.toString (S.f 41) ^ "\n")
