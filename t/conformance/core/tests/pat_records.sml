fun p n = print (Int.toString n ^ "\n")
val r = {a = 1, b = 2, c = 3}
val {a, b, c} = r
val _ = p (a + b + c)
val {a = x, ...} = r
val _ = p x
fun getb ({b, ...} : {a : int, b : int, c : int}) = b
val _ = p (getb r)
fun sumab {a, b, c = _} = a + b
val _ = p (sumab r)
val {inner = {deep}, outer} = {outer = 4, inner = {deep = 5}}
val _ = p (deep * outer)
val _ = p (#c r)
val _ = p (#b {b = 9, a = "s"})
val _ = print (if {b = 1, a = 2} = {a = 2, b = 1} then "eq\n" else "ne\n")
val {x : int, y as (1, z)} = {x = 5, y = (1, 2)}
val _ = p (x + z + #1 y)
fun kv {key = k : string, value = v : int} = k ^ Int.toString v
val _ = print (kv {value = 3, key = "k"} ^ "\n")
