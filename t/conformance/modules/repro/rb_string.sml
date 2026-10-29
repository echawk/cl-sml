fun b true = "T" | b false = "F"
fun t name f = print (name ^ "=" ^ ((f ()) handle Subscript => "Subscript" | e => "exn " ^ exnName e) ^ "\n")
val s = "hello world"
val _ = t "isPrefix hel" (fn () => b (String.isPrefix "hel" s))
val _ = t "isPrefix world" (fn () => b (String.isPrefix "world" s))
val _ = t "isSuffix world" (fn () => b (String.isSuffix "world" s))
val _ = t "isSubstring" (fn () => b (String.isSubstring "lo w" s))
val _ = t "isPrefix empty" (fn () => b (String.isPrefix "" s))
val _ = t "isPrefix long" (fn () => b (String.isPrefix "hello world!!" s))
val _ = t "andalso-orelse" (fn () => b (true orelse false andalso false))
val _ = t "andalso-orelse2" (fn () => b (false andalso true orelse true))
