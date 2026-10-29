fun p n = print (Int.toString n ^ "\n")
val 'a id = fn (x : 'a) => x
val _ = p (id 3)
val _ = print (id "s\n")
fun 'a twice (f : 'a -> 'a) (x : 'a) = f (f x)
val _ = p (twice (fn x => x + 1) 0)
fun ('a, 'b) mkpair (x : 'a) (y : 'b) = (x, y)
val _ = p (#1 (mkpair 4 "x"))
fun len (l : 'a list) = case l of [] => 0 | _ :: r => 1 + len r
val _ = p (len [1, 2] + len ["a"])
val idPoly = fn x => x
val _ = p (idPoly 5 + size (idPoly "ab"))
val _ = p (let val g = idPoly idPoly in g 3 end)
