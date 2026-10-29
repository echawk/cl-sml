fun p n = print (Int.toString n ^ "\n")
fun adder n = fn x => x + n
val add5 = adder 5
val add10 = adder 10
val _ = p (add5 1 + add10 1)
fun counter () = let val c = ref 0 in fn () => (c := !c + 1; !c) end
val c1 = counter ()
val c2 = counter ()
val _ = (c1 (); c1 (); c2 ())
val _ = p (c1 () * 10 + c2 ())
fun curry f x y = f (x, y)
fun uncurry f (x, y) = f x y
val _ = p (curry op + 2 3)
val _ = p (uncurry (fn a => fn b => a * b) (4, 5))
val fs = map (fn n => fn x => x * n) [1, 2, 3]
val _ = p (foldl (fn (f, acc) => f acc) 1 fs)
val rec fact = fn 0 => 1 | n => n * fact (n - 1)
val _ = p (fact 10)
fun twice f = f o f
val _ = p (twice (twice (fn x => x + 1)) 0)
