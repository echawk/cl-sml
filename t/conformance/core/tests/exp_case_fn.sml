fun p s = print (s ^ "\n")
val classify = fn 0 => "zero" | 1 => "one" | n => if n < 0 then "neg" else "many"
val _ = p (classify 0 ^ classify 1 ^ classify ~5 ^ classify 9)
fun g x = case x of
    (0, _) => "a"
  | (_, 0) => "b"
  | (m, n) => (case m + n of 2 => "c" | _ => "d")
val _ = p (g (0, 1) ^ g (1, 0) ^ g (1, 1) ^ g (2, 2))
val h = fn x => fn y => x * 10 + y
val _ = p (Int.toString (h 3 4))
val compose = fn (f, g) => fn x => f (g x)
val _ = p (Int.toString (compose (fn x => x + 1, fn x => x * 2) 10))
val _ = p (case SOME 3 of NONE => "none" | SOME 3 => "three" | SOME _ => "other")
val k = fn (a, b) => a ^ b
val _ = p (k ("x", "y"))
val _ = p ((fn {l, ...} : {l : string, m : int} => l) {l = "lab", m = 0})
