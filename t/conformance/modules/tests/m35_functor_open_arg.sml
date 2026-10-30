(* functor body that opens its argument *)
signature ARG = sig val scale : int val name : string end
functor Scaler (A : ARG) =
struct
  open A
  fun apply n = n * scale
  val label = name ^ "!"
end
structure S3 = Scaler (struct val scale = 3 val name = "three" end)
structure S5 = Scaler (struct val scale = 5 val name = "five" end)
val _ = print (Int.toString (S3.apply 7) ^ " " ^ S3.label ^ "\n")
val _ = print (Int.toString (S5.apply 7) ^ " " ^ S5.label ^ "\n")
val _ = print (Int.toString (S3.apply 1) ^ " " ^ S3.name ^ " " ^ Int.toString S3.scale ^ "\n")
