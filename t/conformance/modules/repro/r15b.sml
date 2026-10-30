functor Add (val n : int
             val m : int) =
struct
  fun add x = x + n + m
end
structure A5 = Add (val n = 5
                    val m = 10)
val _ = print (Int.toString (A5.add 1) ^ "\n")
