(* sharing type constraints in functor arguments *)
signature ELEM = sig type t val toString : t -> string end
signature CONTAINER =
sig
  type elem
  type c
  val empty : c
  val add : elem * c -> c
  val toList : c -> elem list
end

functor Show (structure E : ELEM
              structure C : CONTAINER
              sharing type E.t = C.elem) =
struct
  fun show c = concat (map (fn x => E.toString x ^ ";") (C.toList c))
end

structure IntElem = struct type t = int fun toString n = Int.toString n end
structure ListC = struct
  type elem = int
  type c = int list
  val empty = []
  fun add (x, c) = x :: c
  fun toList c = rev c
end

structure S = Show (structure E = IntElem structure C = ListC)
val _ = print (S.show (ListC.add (3, ListC.add (2, ListC.add (1, ListC.empty)))) ^ "\n")

(* sharing type inside a signature *)
signature TWO =
sig
  type a
  type b
  sharing type a = b
  val f : a -> b
end
structure Two : TWO = struct type a = int type b = int fun f x = x + 1 end
val _ = print (Int.toString (Two.f 1) ^ "\n")
