(* @basis *)
(* Int precision is implementation-defined, but must be self-consistent *)
val consistent =
  case (Int.precision, Int.maxInt, Int.minInt) of
      (NONE, NONE, NONE) => true
    | (SOME _, SOME mx, SOME mn) =>
        ((mx + 1; false) handle Overflow => true)
        andalso ((mn - 1; false) handle Overflow => true)
        andalso ((~ mn; false) handle Overflow => true)
        andalso ((mx * 2; false) handle Overflow => true)
        andalso (mn + mx = ~1)
    | _ => false
val _ = print (if consistent then "consistent\n" else "inconsistent\n")
val _ = print (Int.toString (1000000 * 1000) ^ "\n")
