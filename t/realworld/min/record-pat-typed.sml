fun failure {module : string, func : string, msg : string} = module ^ "." ^ func ^ ": " ^ msg
val _ = print (failure {module = "M", func = "f", msg = "boom"} ^ "\n")
