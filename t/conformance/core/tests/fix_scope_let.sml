fun p n = print (Int.toString n ^ "\n")
fun xx (a, b) = a - b
val v = let infix 7 xx in 2 xx 3 end
val _ = p v
val _ = p (xx (10, 4))
local
  infix 5 ##
  fun a ## b = a * b
in
  val w = 6 ## 7
end
fun ## (a, b) = a + b
val _ = p w
val _ = p (## (1, 2))
structure S = struct infix 5 %% fun a %% b = a + b + 1000 val y = 1 %% 2 end
fun %% (a, b) = a + b
val _ = p S.y
val _ = p (%% (1, 2))
