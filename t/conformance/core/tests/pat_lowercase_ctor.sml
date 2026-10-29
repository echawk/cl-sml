(* constructors and variables are distinguished by status, not by case *)
datatype color = red | green | blue
fun name red = "red" | name green = "green" | name blue = "blue"
val _ = print (name green ^ " " ^ name blue ^ " " ^ name red ^ "\n")
fun isRed red = true | isRed _ = false
val _ = print ((if isRed blue then "yes" else "no") ^ "\n")
exception oops of int
val _ = print ((raise oops 4) handle oops n => Int.toString n ^ "\n")
datatype shape = circle of int | square of int
fun area (circle r) = 3 * r * r | area (square s) = s * s
val _ = print (Int.toString (area (circle 2) + area (square 3)) ^ "\n")
