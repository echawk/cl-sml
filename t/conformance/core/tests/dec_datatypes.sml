fun p n = print (Int.toString n ^ "\n")
type point = int * int
type 'a pair = 'a * 'a
fun addp ((a, b) : point) = a + b
val _ = p (addp (3, 4))
val pp : string pair = ("x", "y")
val _ = print (#1 pp ^ #2 pp ^ "\n")
datatype 'a list' = Nil | Cons of 'a * 'a list'
fun len Nil = 0 | len (Cons (_, r)) = 1 + len r
val _ = p (len (Cons (1, Cons (2, Nil))))
datatype ('a, 'b) either = Left of 'a | Right of 'b
fun pick (Left a) = a | pick (Right b) = size b
val _ = p (pick (Left 4) + pick (Right "abc"))
datatype expr = Num of int | Add of expr * expr | Neg of expr | Let of decl * expr | Var
and decl = Bind of expr
fun ev env (Num n) = n
  | ev env (Add (a, b)) = ev env a + ev env b
  | ev env (Neg a) = ~ (ev env a)
  | ev env (Let (Bind e, body)) = ev (ev env e) body
  | ev env Var = env
val _ = p (ev 0 (Let (Bind (Num 5), Add (Var, Neg (Num 2)))))
datatype t = T | NIL | F
fun tn T = 1 | tn NIL = 2 | tn F = 3
val _ = p (tn T + tn NIL * 10 + tn F * 100)
