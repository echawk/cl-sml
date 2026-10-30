(* local within structures *)
structure S =
struct
  local
    val secret = 41
    fun helper n = n + 1
  in
    val answer = helper secret
    fun twice n = helper (helper n)
  end
  val other = answer * 2
end

val _ = print (Int.toString S.answer ^ "\n")
val _ = print (Int.toString (S.twice 0) ^ "\n")
val _ = print (Int.toString S.other ^ "\n")

(* a hidden local name must not shadow an outer binding when opened *)
val secret = 7
open S
val _ = print (Int.toString secret ^ "\n")
