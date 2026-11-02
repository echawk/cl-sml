fun hash (0, w) = w
  | hash (n, w) =
      let val w = Word.xorb (w, Word.<< (w, 0w13))
          val w = Word.xorb (w, Word.>> (w, 0w7))
          val w = w * 0w31 + Word.fromInt n
      in hash (n - 1, w) end;
fun benchmark () = Word.toInt (Word.andb (hash (3000000, 0w88172645463325), 0wxFFFFFF));
