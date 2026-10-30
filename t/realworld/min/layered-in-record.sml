datatype s = S of {c : int, r : int}
datatype v = V of {h : int, s : s}
fun f (V{h, s as S{c, ...}}) = h + c
val _ = print (Int.toString (f (V{h = 1, s = S{c = 2, r = 3}})) ^ "\n")
