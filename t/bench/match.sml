(* Pattern matching (docs/ROADMAP.md item 11): red-black tree insertion,
   whose balance has nested constructor patterns, a lexer matching on
   characters and keywords, and a fold over tokens. *)
datatype color = R | B;
datatype tree = E | T of color * tree * int * tree;
fun balance (B, T (R, T (R, a, x, b), y, c), z, d) = T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, T (R, a, x, T (R, b, y, c)), z, d) = T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, a, x, T (R, T (R, b, y, c), z, d)) = T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, a, x, T (R, b, y, T (R, c, z, d))) = T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (color, a, x, b) = T (color, a, x, b);
fun insert (t, k) =
  let fun ins E = T (R, E, k, E)
        | ins (s as T (color, a, y, b)) =
            if k < y then balance (color, ins a, y, b)
            else if k > y then balance (color, a, y, ins b)
            else s
  in case ins t of T (_, a, y, b) => T (B, a, y, b) | E => E end;
fun depth E = 0
  | depth (T (B, a, _, _)) = 1 + depth a
  | depth (T (R, a, _, _)) = depth a;
fun size E = 0 | size (T (_, a, _, b)) = size a + 1 + size b;
fun build 0 seed t = t
  | build n seed t = build (n - 1) ((seed * 1103515245 + 12345) mod 1000003) (insert (t, seed));

datatype token = Num of int | Id of string | Kw of string | Op of char | Sep;
fun keyword "let" = true | keyword "in" = true | keyword "end" = true
  | keyword "fun" = true | keyword "val" = true | keyword _ = false;
fun digit c = ord c - ord #"0";
fun lex ([], acc) = rev acc
  | lex (#" " :: cs, acc) = lex (cs, acc)
  | lex (#"\n" :: cs, acc) = lex (cs, Sep :: acc)
  | lex (#";" :: cs, acc) = lex (cs, Sep :: acc)
  | lex (#"+" :: cs, acc) = lex (cs, Op #"+" :: acc)
  | lex (#"-" :: cs, acc) = lex (cs, Op #"-" :: acc)
  | lex ((c as #"*") :: cs, acc) = lex (cs, Op c :: acc)
  | lex ((c as #"=") :: cs, acc) = lex (cs, Op c :: acc)
  | lex (#"(" :: cs, acc) = lex (cs, Op #"(" :: acc)
  | lex (#")" :: cs, acc) = lex (cs, Op #")" :: acc)
  | lex (c :: cs, acc) =
      if Char.isDigit c then number (cs, digit c, acc)
      else word (cs, [c], acc)
and number (c :: cs, n, acc) =
      if Char.isDigit c then number (cs, n * 10 + digit c, acc) else lex (c :: cs, Num n :: acc)
  | number ([], n, acc) = rev (Num n :: acc)
and word (c :: cs, w, acc) =
      if Char.isAlpha c then word (cs, c :: w, acc) else lex (c :: cs, ident w :: acc)
  | word ([], w, acc) = rev (ident w :: acc)
and ident w = let val s = implode (rev w) in if keyword s then Kw s else Id s end;
fun score [] n = n
  | score (Kw "let" :: Id _ :: Op #"=" :: ts) n = score ts (n + 7)
  | score (Kw _ :: ts) n = score ts (n + 3)
  | score (Id _ :: Op #"(" :: ts) n = score ts (n + 5)
  | score (Num a :: Op #"+" :: Num b :: ts) n = score ts (n + a + b)
  | score (Num a :: ts) n = score ts (n + a mod 7)
  | score (Op _ :: ts) n = score ts (n + 1)
  | score (_ :: ts) n = score ts n;
val program = explode
  "let val x = 12 + 30 in f (x) * g (y - 4) end;\nfun h a = a + 1;\nval y = 7 * (x + 2)\n";
fun lexing 0 acc = acc
  | lexing n acc = lexing (n - 1) (score (lex (program, [])) acc);
fun benchmark () =
  let val t = build 300000 1 E in size t + depth t + lexing 400000 0 end;
