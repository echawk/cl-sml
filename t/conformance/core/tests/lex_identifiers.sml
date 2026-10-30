val x' = 1
val x'' = 2
val a_b = 3
val A'b_C = 4
val iff = 5
val ending = 6
val dot = 7
val asx = 8
val inx = 9
val opx = 10
val handler = 11
val withx = 12
val raised = 13
val letter = 14
val local1 = 15
val ofs = 16
val fnord = 17
val thenx = 18
val elsewhere = 19
val doit = 20
val andalso' = 21
val orelse1 = 22
val sum = x' + x'' + a_b + A'b_C + iff + ending + dot + asx + inx + opx + handler
          + withx + raised + letter + local1 + ofs + fnord + thenx + elsewhere + doit
          + andalso' + orelse1
val _ = print (Int.toString sum ^ "\n")
val op ++ = fn (a, b) => a * 10 + b
val _ = print (Int.toString (++ (1, 2)) ^ "\n")
val <*> = fn x => x + 100
val _ = print (Int.toString (<*> 1) ^ "\n")
fun |=| (a, b) = a = b
val _ = print (if |=| (3, 3) then "eq\n" else "ne\n")
