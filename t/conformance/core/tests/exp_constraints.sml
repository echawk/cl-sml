fun p n = print (Int.toString n ^ "\n")
val _ = p (1 : int)
val f = (fn x => x) : int -> int
val _ = p (f 3)
val l = [] : int list
val _ = p (length l)
val _ = p (let val x : int = 3 in x end)
val _ = p ((4 : int) + 1)
val _ = p (length ([1, 2] : int list) + (2 : int))
fun g (x : int) : int = x : int
val _ = p (g 6)
val _ = p ((fn x => x : int) 7)
val r = {a = 1 : int, b = "x" : string}
val _ = p (#a r)
