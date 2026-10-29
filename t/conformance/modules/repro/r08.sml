signature ELEM = sig type t val toString : t -> string end
functor Show (structure E : ELEM
              structure F : ELEM) =
struct
  fun show x = E.toString x ^ F.toString x
end
structure IntElem = struct
  type t = int
  fun toString n = Int.toString n
end
structure S = Show (structure E = IntElem
                    structure F = IntElem)
val _ = print (S.show 7 ^ "\n")
