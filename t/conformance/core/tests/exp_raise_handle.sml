fun p s = print (s ^ "\n")
exception A
exception B of int
exception C of string * int
fun thrower 0 = raise A
  | thrower 1 = raise B 7
  | thrower 2 = raise C ("c", 3)
  | thrower n = n
fun catch n = Int.toString (thrower n)
              handle A => "A"
                   | B k => "B" ^ Int.toString k
                   | C (s, k) => s ^ Int.toString k
val _ = p (catch 0 ^ " " ^ catch 1 ^ " " ^ catch 2 ^ " " ^ catch 3)
fun inner n = (thrower n; "none") handle A => "inner-A"
val _ = p ((inner 1) handle B k => "outer-B" ^ Int.toString k)
val _ = p (inner 0)
fun rethrow n = (thrower n; "none") handle e => (p "saw"; raise e)
val _ = p ((rethrow 1) handle B k => "re-B" ^ Int.toString k)
val _ = p ((raise A) handle A => "raised in handler arg" | B _ => "no")
val _ = p (((raise B 1) handle B n => raise B (n + 1)) handle B n => "chain" ^ Int.toString n)
val e = B 42
val _ = p ((raise e) handle B n => Int.toString n)
fun classify ex = case ex of A => "isA" | B _ => "isB" | _ => "other"
val _ = p (classify (C ("x", 1)) ^ classify A ^ classify (B 0))
val _ = p (Int.toString (1 + ((raise A) handle A => 2)))
val _ = p (Int.toString ((fn x => x) 5 handle A => 0))
