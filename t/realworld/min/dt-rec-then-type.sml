datatype term
  = Var of int
  | Prop of { name: string } * term list

type head = { name: string }
val _ = print "ok\n"
