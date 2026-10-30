datatype color = RED | BLACK
datatype tree = empty | tree of int * color * tree * tree
fun insert (key, t) =
    let fun f empty = tree(key,RED,empty,empty)
          | f (tree(k,BLACK,l,r)) =
              if key < k then tree(k,BLACK,f l,r) else tree(k,BLACK,l,f r)
          | f (tree(k,RED,l,r)) =
              if key < k then tree(k,RED,f l,r) else tree(k,RED,l,f r)
    in f t end
fun size empty = 0 | size (tree(_,_,l,r)) = 1 + size l + size r
val _ = print (Int.toString (size (insert (3, insert (1, insert (2, empty))))) ^ "\n")
