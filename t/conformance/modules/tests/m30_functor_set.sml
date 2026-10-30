(* a classic set functor with an abstract result *)
signature ORD = sig type t val compare : t * t -> order end
signature SET =
sig
  type elem
  type set
  val empty : set
  val insert : elem * set -> set
  val member : elem * set -> bool
  val toList : set -> elem list
end

functor ListSet (O : ORD) :> SET where type elem = O.t =
struct
  type elem = O.t
  type set = elem list
  val empty = []
  fun member (x, []) = false
    | member (x, y :: ys) = (case O.compare (x, y) of EQUAL => true | _ => member (x, ys))
  fun insert (x, s) =
    let
      fun ins [] = [x]
        | ins (l as y :: ys) =
            (case O.compare (x, y) of
                 LESS => x :: l
               | EQUAL => l
               | GREATER => y :: ins ys)
    in ins s end
  fun toList s = s
end

structure IntSet = ListSet (struct type t = int fun compare (a : int, b) = if a < b then LESS else if a > b then GREATER else EQUAL end)
structure StrSet = ListSet (struct type t = string fun compare (a : string, b) = if a < b then LESS else if a > b then GREATER else EQUAL end)

val s = foldl IntSet.insert IntSet.empty [5, 3, 8, 3, 1]
val _ = print (concat (map (fn n => Int.toString n ^ " ") (IntSet.toList s)) ^ "\n")
val _ = print (if IntSet.member (8, s) then "8 in\n" else "8 out\n")
val t = foldl StrSet.insert StrSet.empty ["pear", "apple", "fig", "apple"]
val _ = print (concat (map (fn n => n ^ " ") (StrSet.toList t)) ^ "\n")
val s2 = IntSet.insert (4, s)
val _ = print (concat (map (fn n => Int.toString n ^ " ") (IntSet.toList s2)) ^ "\n")
