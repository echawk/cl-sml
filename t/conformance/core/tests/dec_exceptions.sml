fun p s = print (s ^ "\n")
exception E1
exception E2 of int
exception E3 = E1
exception E4 = E2
val _ = p ((raise E3) handle E1 => "E3 caught as E1")
val _ = p ((raise E1) handle E3 => "E1 caught as E3")
val _ = p ((raise E4 5) handle E2 n => "E4 as E2 " ^ Int.toString n)
local exception Hidden in
  fun hide () = raise Hidden
  fun seek f = (f (); "none") handle Hidden => "hidden"
end
val _ = p (seek hide)
val _ = p (let exception L of string in (raise L "let") handle L s => s end)
exception Pair of int * int
val _ = p ((raise Pair (1, 2)) handle Pair (a, b) => Int.toString (a + b))
exception Fn of int -> int
val _ = p ((raise Fn (fn x => x * 3)) handle Fn f => Int.toString (f 4))
val _ = p ((raise Fail "msg") handle Fail m => "Fail " ^ m)
