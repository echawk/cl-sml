(* open with several structures; later ones shadow earlier ones *)
structure A = struct val a = "A.a" val c = "A.c" end
structure B = struct val b = "B.b" val c = "B.c" end
open A B
val _ = print (a ^ " " ^ b ^ " " ^ c ^ "\n")
structure C = struct open B A val d = c end
val _ = print (C.d ^ " " ^ C.c ^ "\n")
