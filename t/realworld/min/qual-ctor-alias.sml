structure Tree = struct datatype tree = NULL | ND of {sz : int, next : tree ref} end
structure T = Tree
fun size T.NULL = 0
  | size (T.ND{sz, ...}) = sz
fun both (T.NULL, T.NULL) = "nulls"
  | both (l as T.ND{...}, T.NULL) = "left"
  | both _ = "other"
val _ = print (Int.toString (size T.NULL + size (T.ND{sz = 3, next = ref T.NULL})) ^ " " ^ both (T.NULL, T.NULL) ^ " " ^ both (T.ND{sz=1, next = ref T.NULL}, T.NULL) ^ "\n")
