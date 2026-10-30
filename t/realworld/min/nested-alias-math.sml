structure Real64 = Real
structure M = struct
 structure R = Real64
 val x = R.Math.pi
end
val _ = print (Real.toString M.x ^ "\n")
