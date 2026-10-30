functor Add (val n : int) =
struct
  fun add x = x + n
end
structure A5 = Add (val n = 5)
val _ = print (Int.toString (A5.add 1) ^ "\n")
