(* functors whose argument has nested substructures *)
signature ORD = sig
  type t
  val compare : t * t -> order
  end
signature KEYED =
sig
  structure Key : ORD
  val name : string
  val default : Key.t
end

functor Lookup (K : KEYED) =
struct
  fun find (k, []) = K.default
    | find (k, (k', v) :: rest) =
        (case K.Key.compare (k, k') of EQUAL => v | _ => find (k, rest))
  val label = K.name
end

structure IntKeyed =
struct
 
  structure Key = struct
  type t = int
  fun compare (a : int, b) = if a < b then LESS else if a > b then GREATER else EQUAL
  end
  val name = "int-keyed"
  val default = ~1
end

structure L = Lookup (IntKeyed)
val _ = print (L.label ^ "\n")
val _ = print (Int.toString (L.find (2, [(1, 10), (2, 20), (3, 30)])) ^ "\n")
val _ = print (Int.toString (L.find (9, [(1, 10)])) ^ "\n")

(* functor result containing a substructure *)
functor Wrap (X : sig
  val n : int end) =
struct
 
  structure Inner = struct
  val m = X.n + 1
  end
  val outer = Inner.m * 2
end
structure W = Wrap (struct
  val n = 4 end)
val _ = print (Int.toString W.Inner.m ^ "\n")
val _ = print (Int.toString W.outer ^ "\n")
