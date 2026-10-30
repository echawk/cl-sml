(* signature with val, type, eqtype, datatype, exception and structure specs *)
signature SIG =
sig
  type t
  eqtype e
  datatype shape = Circle of int | Square of int | Dot
  exception Bad of string
  structure Sub : sig val k : int end
  val make : int -> t
  val get : t -> int
  val area : shape -> int
  val ee : e
end

structure M : SIG =
struct
  type t = int
  type e = string
  datatype shape = Circle of int | Square of int | Dot
  exception Bad of string
  structure Sub = struct val k = 99 val hidden = 1 end
  fun make n = n * 2
  fun get n = n
  fun area (Circle r) = 3 * r * r
    | area (Square s) = s * s
    | area Dot = raise Bad "dot"
  val ee = "eq"
end

val _ = print (Int.toString (M.get (M.make 21)) ^ "\n")
val _ = print (Int.toString (M.area (M.Circle 2)) ^ "\n")
val _ = print (Int.toString (M.area (M.Square 5)) ^ "\n")
val _ = print ((Int.toString (M.area M.Dot)) handle M.Bad s => "Bad " ^ s ^ "\n")
val _ = print (Int.toString M.Sub.k ^ "\n")
val _ = print (if M.ee = M.ee then "eq ok\n" else "eq bad\n")
val _ = print (case M.Square 3 of M.Circle _ => "circle\n" | M.Square n => "square " ^ Int.toString n ^ "\n" | M.Dot => "dot\n")
