fun b x = print ((if x then "true" else "false") ^ "\n")
val _ = b ([1, 2, 3] = [1, 2, 3])
val _ = b ([1, 2] = [1, 2, 3])
val _ = b ((1, "a", #"c") = (1, "a", #"c"))
val _ = b ({x = 1, y = [true]} = {y = [true], x = 1})
datatype t = A | B of int * t
val _ = b (B (1, B (2, A)) = B (1, B (2, A)))
val _ = b (B (1, A) = B (2, A))
val _ = b (SOME (SOME [A]) = SOME (SOME [A]))
val r1 = ref 1
val r2 = ref 1
val _ = b (r1 = r2)
val _ = b (r1 = r1)
val _ = b ([r1] = [r1])
val _ = b ((r1, 1) = (r2, 1))
val _ = b (ref 0 = ref 0)
datatype u = U of int ref
val _ = b (U r1 = U r1)
val _ = b (U r1 = U r2)
val _ = r2 := 5
val _ = b (!r1 = !r2)
val _ = b ("abc" <> "abd")
val _ = b (() = ())
val _ = b ([] = ([] : int list))
val _ = b (NONE = (NONE : int option))
val _ = b (0w3 = 0w3)
val _ = b (#"a" = #"a")
val _ = b (true = (1 < 2))
