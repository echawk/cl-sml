fun mandel (cr, ci) =
  let fun go (zr, zi, i) =
        if i >= 200 then i
        else if zr * zr + zi * zi > 4.0 then i
        else go (zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, i + 1)
  in go (0.0, 0.0, 0) end;
fun rows y acc =
  if y >= 200 then acc
  else let fun cols x acc =
             if x >= 200 then acc
             else cols (x + 1) (acc + mandel (real x / 100.0 - 1.5, real y / 100.0 - 1.0))
       in rows (y + 1) (cols 0 acc) end;
fun benchmark () = rows 0 0;
