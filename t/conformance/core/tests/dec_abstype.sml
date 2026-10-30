fun p n = print (Int.toString n ^ "\n")
abstype stack = S of int list
with
  val empty = S []
  fun push (x, S l) = S (x :: l)
  fun top (S (x :: _)) = x
    | top (S []) = ~1
  fun size (S l) = length l
end
val s = push (3, push (2, push (1, empty)))
val _ = p (top s)
val _ = p (size s)
val _ = p (top empty)
abstype 'a bag = Bag of 'a list * int
withtype counter = int
with
  fun mk (l : 'a list) = Bag (l, length l)
  fun count (Bag (_, n)) : counter = n
end
val _ = p (count (mk [1, 2, 3, 4]))
