(* Loops written as self tail calls: they must run in constant stack on
   every Common Lisp, also when compiled through an emitted artifact. *)

fun count 0 acc = acc
  | count n acc = count (n - 1) (acc + 1);
val counted = count 1000000 0;

fun sumTo (0, acc) = acc
  | sumTo (n, acc) = sumTo (n - 1, acc + n);
val summed = sumTo (1000000, 0);

fun countdown n =
    let fun go k = if k = 0 then "done"
                   else case k mod 2 of
                            0 => go (k - 1)
                          | _ => let val j = k - 1 in go j end
    in go n end;
val countedDown = countdown 1000000;
