structure S = struct
infix 9 over
val op over = (fn (a, b) => a * 10 + b : int)
val x = ~2 over 1
end
val _ = print (Int.toString S.x ^ "\n")
