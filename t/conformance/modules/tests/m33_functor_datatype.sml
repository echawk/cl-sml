(* datatypes declared in a functor body; each application has its own *)
functor MkTree (X : sig type t val show : t -> string end) =
struct
  datatype tree = Leaf | Node of tree * X.t * tree
  fun insert lt (x, Leaf) = Node (Leaf, x, Leaf)
    | insert lt (x, t as Node (l, y, r)) =
        if lt (x, y) then Node (insert lt (x, l), y, r) else Node (l, y, insert lt (x, r))
  fun show Leaf = "."
    | show (Node (l, x, r)) = "(" ^ show l ^ X.show x ^ show r ^ ")"
end
structure IT = MkTree (struct type t = int val show = Int.toString end)
structure ST = MkTree (struct type t = string fun show s = s end)
val t1 = foldl (IT.insert (fn (a : int, b) => a < b)) IT.Leaf [2, 1, 3]
val t2 = foldl (ST.insert (fn (a : string, b) => a < b)) ST.Leaf ["b", "a", "c"]
val _ = print (IT.show t1 ^ "\n")
val _ = print (ST.show t2 ^ "\n")
val _ = print (case t1 of IT.Node (_, x, _) => "root " ^ Int.toString x ^ "\n" | IT.Leaf => "leaf\n")
