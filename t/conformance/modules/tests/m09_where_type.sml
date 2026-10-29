(* where type *)
signature T = sig type t val v : t val show : t -> string end

structure A : T where type t = int = struct type t = int val v = 5 fun show n = Int.toString n end
val _ = print (Int.toString (A.v + 1) ^ "\n")

structure B :> T where type t = string = struct type t = string val v = "bee" fun show s = s end
val _ = print (B.v ^ "\n")

signature T2 = T where type t = bool
structure C : T2 = struct type t = bool val v = true fun show b = if b then "yes" else "no" end
val _ = print (C.show (not C.v) ^ "\n")

(* where type on a parameterised type *)
signature P = sig type 'a box val wrap : 'a -> 'a box end
structure D :> P where type 'a box = 'a list = struct type 'a box = 'a list fun wrap x = [x] end
val _ = print (Int.toString (length (D.wrap 1 @ [2, 3])) ^ "\n")
