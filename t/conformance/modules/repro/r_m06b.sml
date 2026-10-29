structure M =
struct
  exception Bad of string
  fun f n = raise Bad "dot"
end
val _ = print ((Int.toString (M.f 1)) handle M.Bad s => "Bad " ^ s ^ "\n")
