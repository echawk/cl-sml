(* one-line structures *)
structure S = struct type t = int fun f (x : t) = x + 1 end
val _ = print (Int.toString (S.f 41) ^ "\n")

structure P = struct val x = 1 val y = 2 end
val _ = print (Int.toString (P.x + P.y) ^ "\n")

structure D = struct datatype d = A | B fun g A = 1 | g B = 2 end
val _ = print (Int.toString (D.g D.B) ^ "\n")

structure E = struct exception Boom of int fun go n = raise Boom n end
val _ = print ((Int.toString (E.go 5) handle E.Boom n => "boom " ^ Int.toString n) ^ "\n")

structure T = struct type 'a pair = 'a * 'a fun swap ((a, b) : int pair) = (b, a) end
val _ = print (Int.toString (#1 (T.swap (1, 2))) ^ "\n")
