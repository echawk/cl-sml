(* opaque ascription: abstract types, runtime behaviour unchanged *)
signature COUNTER =
sig
  type t
  val zero : t
  val inc : t -> t
  val value : t -> int
end

structure C :> COUNTER =
struct
  type t = int
  val zero = 0
  fun inc n = n + 1
  fun value n = n
  val extra = 1000
end

val _ = print (Int.toString (C.value (C.inc (C.inc C.zero))) ^ "\n")

val extra = 5
open C
val _ = print (Int.toString extra ^ "\n")
val _ = print (Int.toString (value (inc zero)) ^ "\n")

(* opaque ascription of a stateful structure *)
signature STACK = sig type 'a stack val empty : 'a stack val push : 'a * 'a stack -> 'a stack
                      val pop : 'a stack -> ('a * 'a stack) option end
structure St :> STACK =
struct
  type 'a stack = 'a list
  val empty = []
  fun push (x, s) = x :: s
  fun pop [] = NONE
    | pop (x :: s) = SOME (x, s)
end
val s = St.push (3, St.push (2, St.push (1, St.empty)))
val _ = case St.pop s of
            SOME (x, s') => (print (Int.toString x ^ "\n");
                             case St.pop s' of SOME (y, _) => print (Int.toString y ^ "\n") | NONE => ())
          | NONE => print "empty\n"
