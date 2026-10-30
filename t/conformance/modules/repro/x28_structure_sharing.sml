(* structure sharing (derived form of type sharing) *)
signature T = sig
  type t
  val v : t
  val show : t -> string
  end

functor Pair (structure A : T
              structure B : T
              sharing A = B) =
struct
  fun both () = A.show A.v ^ "," ^ B.show B.v ^ "," ^ A.show B.v
end

structure X = struct
  type t = int
  val v = 1
  fun show n = Int.toString n
  end
structure Y = struct
  type t = int
  val v = 2
  fun show n = "y" ^ Int.toString n
  end
structure P = Pair (structure A = X
  structure B = Y)
val _ = print (P.both () ^ "\n")
