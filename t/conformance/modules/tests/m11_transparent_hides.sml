(* transparent ascription hides members the signature does not list *)
signature S = sig type t val x : t val show : t -> string end

val hidden = "outer hidden"
fun helper () = "outer helper"

structure M : S =
struct
  type t = int
  val hidden = "inner hidden"
  fun helper () = "inner helper"
  val x = 42
  fun show n = Int.toString n ^ " " ^ hidden
end

(* transparent: t = int is visible *)
val _ = print (Int.toString (M.x + 1) ^ "\n")
val _ = print (M.show M.x ^ "\n")
open M
val _ = print (hidden ^ "\n")
val _ = print (helper () ^ "\n")
val _ = print (show x ^ "\n")
