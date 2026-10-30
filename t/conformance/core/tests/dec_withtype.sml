(* @basis *)
fun p n = print (Int.toString n ^ "\n")
datatype 'a tree = Node of 'a * 'a forest
withtype 'a forest = 'a tree list
fun size (Node (_, kids)) = 1 + sizeF kids
and sizeF ([] : 'a forest) = 0
  | sizeF (t :: ts) = size t + sizeF ts
val t = Node (1, [Node (2, []), Node (3, [Node (4, [])])])
val _ = p (size t)
val f : int forest = [t, t]
val _ = p (sizeF f)
datatype a = A of b withtype b = int * string
val A (n, s) = A (3, "abc")
val _ = p (n + String.size s)
