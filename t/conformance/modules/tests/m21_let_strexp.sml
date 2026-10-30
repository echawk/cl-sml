(* let in structure expressions; struct ... end : SIG postfix ascription *)
structure S =
  let
    structure Helper = struct val k = 7 end
  in
    struct
      val x = Helper.k * 6
    end
  end

val _ = print (Int.toString S.x ^ "\n")

signature X = sig val y : int end
structure T = struct val y = 5 val z = 6 end : X
val _ = print (Int.toString T.y ^ "\n")
structure U = struct val y = 8 end :> X
val _ = print (Int.toString U.y ^ "\n")
