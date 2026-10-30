val v = let
  exception L of string
in
  (raise L "let") handle L s => s
end
val _ = print (v ^ "\n")
exception E1
exception E3 = E1
val _ = print ((raise E3) handle E1 => "alias ok\n")
