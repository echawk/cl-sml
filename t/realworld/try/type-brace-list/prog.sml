type rhsData = {rhs : int list, code : string} list
datatype rule = RULE of {lhs : int, rhs : int list}
val x : rhsData = [{rhs = [1], code = "c"}]
val _ = print (#code (hd x) ^ "\n")
