(* functor F (spec) derived form, applied as F (strdec) *)
functor Add (val n : int  type t  val conv : t -> int) =
struct
  fun add x = conv x + n
end

structure A5 = Add (val n = 5 type t = int fun conv x = x)
structure A100 = Add (val n = 100 type t = string fun conv s = size s)

val _ = print (Int.toString (A5.add 1) ^ "\n")
val _ = print (Int.toString (A100.add "abc") ^ "\n")
val _ = print (Int.toString (A5.add 2) ^ "\n")

(* empty spec and empty strdec *)
functor Unit () = struct val u = "unit functor" end
structure U = Unit ()
val _ = print (U.u ^ "\n")

(* structure spec inside the derived-form argument *)
functor WithSub (structure S : sig val v : int end) = struct val w = S.v * 3 end
structure WS = WithSub (structure S = struct val v = 14 end)
val _ = print (Int.toString WS.w ^ "\n")
