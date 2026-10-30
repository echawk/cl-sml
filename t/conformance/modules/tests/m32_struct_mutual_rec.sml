(* mutually recursive functions and val rec inside structures *)
structure P =
struct
  fun even 0 = true
    | even n = odd (n - 1)
  and odd 0 = false
    | odd n = even (n - 1)
  val rec fact = fn 0 => 1 | n => n * fact (n - 1)
end
val _ = print ((if P.even 10 then "even" else "odd") ^ " " ^ (if P.odd 7 then "odd" else "even") ^ "\n")
val _ = print (Int.toString (P.fact 10) ^ "\n")
