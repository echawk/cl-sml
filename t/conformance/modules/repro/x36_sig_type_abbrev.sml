(* type abbreviations in signatures (derived form) and type specs with and *)
signature S =
sig
  type t = int
  type u and v
  val x : t
  val toU : t -> u
  val showU : u -> string
end
structure M :> S = struct
  type t = int
  type u = string
  type v = bool
  val x = 4
  fun toU n = Int.toString n
  fun showU s = "<" ^ s ^ ">"
  end
val _ = print (Int.toString (M.x + 1) ^ "\n")
val _ = print (M.showU (M.toU 12) ^ "\n")
