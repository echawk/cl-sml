(* datatypes in structures, long constructors in patterns, replication *)
structure Shapes =
struct
  datatype shape = Circle of int | Rect of int * int | Empty
  structure Colors = struct datatype color = Red | Green | Blue of int end
end

fun describe (Shapes.Circle r) = "circle " ^ Int.toString r
  | describe (Shapes.Rect (w, h)) = "rect " ^ Int.toString (w * h)
  | describe Shapes.Empty = "empty"

fun color Shapes.Colors.Red = "red"
  | color Shapes.Colors.Green = "green"
  | color (Shapes.Colors.Blue n) = "blue" ^ Int.toString n

val _ = print (describe (Shapes.Circle 3) ^ "\n")
val _ = print (describe (Shapes.Rect (2, 5)) ^ "\n")
val _ = print (describe Shapes.Empty ^ "\n")
val _ = print (color (Shapes.Colors.Blue 9) ^ " " ^ color Shapes.Colors.Green ^ "\n")

structure R = struct datatype t = datatype Shapes.shape end
val _ = print (describe (R.Rect (3, 3)) ^ "\n")
val _ = print (case R.Circle 1 of Shapes.Circle n => "same ctor " ^ Int.toString n | _ => "different" )
val _ = print "\n"

datatype c = datatype Shapes.Colors.color
val _ = print (color (Blue 1) ^ "\n")
val _ = print (case Red of Red => "Red ok\n" | _ => "bad\n")

(* constructors used as functions *)
val circles = map Shapes.Circle [1, 2]
val _ = print (concat (map describe circles) ^ "\n")
