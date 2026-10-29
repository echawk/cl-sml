functor F () = struct exception E fun f () = raise E end
structure S = F ()
structure T = F ()
val _ = (T.f () handle S.E => print "Bad\n" | T.E => print "Good\n")
