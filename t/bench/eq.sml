datatype color = Red | Green | Blue;
fun nextColor Red = Green | nextColor Green = Blue | nextColor Blue = Red;
fun countEq (s, c) =
  let val n = size s
      fun go (i, acc) = if i >= n then acc else go (i + 1, if String.sub (s, i) = c then acc + 1 else acc)
  in go (0, 0) end;
val text = String.concat (List.tabulate (2000, fn i => "abcab" ^ Int.toString i));
fun reps 0 a = a | reps k a = reps (k - 1) (a + countEq (text, #"a"));
fun strs 0 acc = acc
  | strs n acc = strs (n - 1) (if Int.toString (n mod 100) = "42" then acc + 1 else acc);
fun colors 0 c acc = acc
  | colors n c acc = colors (n - 1) (nextColor c) (if c = Blue then acc + 1 else acc);
fun mem (x, []) = false | mem (x, y :: ys) = x = y orelse mem (x, ys);
val ints = List.tabulate (1000, fn i => i * 3);
fun mems 0 acc = acc | mems n acc = mems (n - 1) (if mem (n mod 3000, ints) then acc + 1 else acc);
fun benchmark () = reps 100 0 + strs 300000 0 + colors 3000000 Red 0 + mems 20000 0;
