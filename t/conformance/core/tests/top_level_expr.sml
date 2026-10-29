print "first\n";
val x = 1;
print (Int.toString x ^ "\n");
(fn () => print "anon\n") ();
let val y = 2 in print (Int.toString y ^ "\n") end;
