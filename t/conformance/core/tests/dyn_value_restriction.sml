fun p n = print (Int.toString n ^ "\n")
val id = fn x => x
val _ = p (id 1 + size (id "ab"))
val r = ref ([] : int list)
val _ = r := [1, 2]
val _ = p (length (!r))
val _ = p (let val f = (fn x => x) (fn y => y) in f 3 end)
val _ = p (let val g = id id in g 4 end)
fun mkRef () = ref []
val r2 : string list ref = mkRef ()
val _ = r2 := ["a"]
val _ = p (length (!r2))
val pair = (fn x => x, 5)
val _ = p (#1 pair (#2 pair))
