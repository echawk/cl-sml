(* Records, tuples and selectors: record patterns, #lab, a flexible
   pattern, and a function returning a pair. *)
type point = {x : int, y : int, z : int};
fun add ({x = x1, y = y1, z = z1} : point, {x = x2, y = y2, z = z2} : point) =
  {x = x1 + x2, y = y1 + y2, z = z1 + z2};
fun norm1 (p : point) = abs (#x p) + abs (#y p) + abs (#z p);
fun step ({x, y, z} : point) = {x = y mod 1000, y = z mod 1000, z = (x + y + z) mod 1000};
fun getx ({x, ...} : point) = x;
fun divmod (a, b) = (a div b, a mod b);
fun loop 0 p acc = acc
  | loop n p acc =
      let val q = step (add (p, {x = n, y = 1, z = 2}))
          val (d, m) = divmod (norm1 q, 7)
      in loop (n - 1) q (acc + d + m + getx q) end;
fun benchmark () = loop 2000000 {x = 1, y = 2, z = 3} 0;
