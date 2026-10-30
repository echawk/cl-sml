fun p s = print (s ^ "\n")
val t = Time.fromReal 1.5
val _ = p (Real.toString (Time.toReal t))
val _ = p (IntInf.toString (Time.toMilliseconds t))
val _ = p (Time.toString (Time.fromSeconds 3))
val _ = p (if Time.< (Time.zeroTime, t) then "lt" else "ge")
val now = Time.now ()
val _ = p (if Time.> (now, Time.zeroTime) then "now ok" else "now bad")
val ct = Timer.startCPUTimer ()
val rt = Timer.startRealTimer ()
val {usr, sys} = Timer.checkCPUTimer ct
val _ = p (if Time.>= (usr, Time.zeroTime) then "cpu ok" else "cpu bad")
val _ = p (if Time.>= (Timer.checkRealTimer rt, Time.zeroTime) then "real ok" else "real bad")
