fun tak (x, y, z) = if y < x then tak (tak (x - 1, y, z), tak (y - 1, z, x), tak (z - 1, x, y)) else z;
fun loop 0 acc = acc | loop n acc = loop (n - 1) (acc + tak (18, 12, 6));
fun benchmark () = loop 20 0;
