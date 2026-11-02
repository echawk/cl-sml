datatype tree = Leaf | Node of tree * int * tree;
fun insert (Leaf, k) = Node (Leaf, k, Leaf)
  | insert (t as Node (l, x, r), k) =
      if k = x then t
      else if k < x then Node (insert (l, k), x, r)
      else Node (l, x, insert (r, k));
fun member (Leaf, k) = false
  | member (Node (l, x, r), k) = k = x orelse (if k < x then member (l, k) else member (r, k));
fun size Leaf = 0 | size (Node (l, _, r)) = size l + 1 + size r;
fun build 0 seed t = t
  | build n seed t = build (n - 1) ((seed * 1103515245 + 12345) mod 1000003) (insert (t, seed));
fun count t 0 seed acc = acc
  | count t n seed acc =
      count t (n - 1) ((seed * 69069 + 1) mod 1000003) (if member (t, seed) then acc + 1 else acc);
fun leaves Leaf = 1 | leaves (Node (l, _, r)) = leaves l + leaves r;
fun benchmark () = let val t = build 200000 1 Leaf in size t + count t 300000 7 0 + leaves t end;
