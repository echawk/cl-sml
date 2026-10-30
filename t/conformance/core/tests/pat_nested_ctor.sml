(* @basis *)
fun p n = print (Int.toString n ^ "\n")
datatype t = A | B of int | C of t * t
fun eval A = 0
  | eval (B n) = n
  | eval (C (B 1, C (A, B x))) = 1000 + x
  | eval (C (l, r)) = eval l + eval r
val _ = p (eval (C (B 1, C (A, B 5))))
val _ = p (eval (C (B 2, C (A, B 5))))
fun oo (SOME (SOME x)) = x | oo (SOME NONE) = ~1 | oo NONE = ~2
val _ = p (oo (SOME (SOME 3)) + oo (SOME NONE) + oo NONE)
fun sl (SOME [x]) = x | sl (SOME (x :: y :: _)) = x + y | sl _ = 0
val _ = p (sl (SOME [4]) + sl (SOME [1, 2, 3]) + sl (SOME []) + sl NONE)
datatype 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
fun depth Leaf = 0
  | depth (Node (l, _, r)) = 1 + Int.max (depth l, depth r)
fun insert (x, Leaf) = Node (Leaf, x, Leaf)
  | insert (x, t as Node (l, y, r)) =
      if x < y then Node (insert (x, l), y, r)
      else if x > y then Node (l, y, insert (x, r))
      else t
val tr = foldl insert Leaf [5, 3, 8, 1, 4, 9, 10]
val _ = p (depth tr)
fun inorder Leaf = [] | inorder (Node (l, x, r)) = inorder l @ [x] @ inorder r
val _ = print (String.concatWith " " (map Int.toString (inorder tr)) ^ "\n")
