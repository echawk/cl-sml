(*
 * cl-sml: Standard ML Basis Library, structure IntInf
 * (not part of HaMLet's basis).
 *
 * Notes:
 * - IntInf.int is the run time's integers.  Its own operations never
 *   overflow; the overloaded ones (+ etc.) only apply to it unchecked, and
 *   then raise Overflow when both operands are in Int's range and the
 *   result is not (HaMLet's overloading classes do not include it).
 * - LargeInt is IntInf (as in Poly/ML and MLton).  IntInf is loaded before
 *   INTEGER, so it is first ascribed INT_INF_CORE; LargeInt.sml adds
 *   toLarge and fromLarge and ascribes INT_INF.
 *)

structure IntInf :> INT_INF_CORE =
struct
  type int          = Int.int (* the run time's integers, unbounded *)

  val precision     = NONE : Int.int option
  val minInt        = NONE : int option
  val maxInt        = NONE : int option

  val toInt         = use{b = "IntInf.toInt"} : int -> Int.int
  val fromInt       = use{b = "IntInf.fromInt"} : Int.int -> int

  val ~             = use{b = "IntInf.~"} : int -> int
  val abs           = use{b = "IntInf.abs"} : int -> int
  val op+           = use{b = "IntInf.+"} : int * int -> int
  val op-           = use{b = "IntInf.-"} : int * int -> int
  val op*           = use{b = "IntInf.*"} : int * int -> int
  val op div        = use{b = "IntInf.div"} : int * int -> int
  val op mod        = use{b = "IntInf.mod"} : int * int -> int
  val quot          = use{b = "IntInf.quot"} : int * int -> int
  val rem           = use{b = "IntInf.rem"} : int * int -> int
  fun divMod(i, j)  = (i div j, i mod j)
  fun quotRem(i, j) = (quot(i, j), rem(i, j))

  val pow           = use{b = "IntInf.pow"} : int * Int.int -> int
  val log2          = use{b = "IntInf.log2"} : int -> Int.int
  val orb           = use{b = "IntInf.orb"} : int * int -> int
  val xorb          = use{b = "IntInf.xorb"} : int * int -> int
  val andb          = use{b = "IntInf.andb"} : int * int -> int
  val notb          = use{b = "IntInf.notb"} : int -> int
  val <<            = use{b = "IntInf.<<"} : int * word -> int
  val ~>>           = use{b = "IntInf.~>>"} : int * word -> int

  fun compare(i, j) = if i < j then LESS else if i = j then EQUAL else GREATER
  fun min(i, j)     = if i < j then i else j
  fun max(i, j)     = if i > j then i else j
  fun sign i        = if i < 0 then ~1 else if i > 0 then 1 else 0
  fun sameSign(i, j) = sign i = sign j

  val fmt           = use{b = "IntInf.fmt"} : StringCvt.radix -> int -> string
  val toString      = fmt StringCvt.DEC

  local
    open StringCvt

    fun base BIN = 2
      | base OCT = 8
      | base DEC = 10
      | base HEX = 16

    fun isDigit BIN = (fn c => #"0" <= c andalso c <= #"1")
      | isDigit OCT = (fn c => #"0" <= c andalso c <= #"7")
      | isDigit DEC = Char.isDigit
      | isDigit HEX = Char.isHexDigit

    fun value c =
        Char.ord(Char.toUpper c) -
          (if c < #"A" then Char.ord #"0" else Char.ord #"A" - 10)

    fun digits (radix, n, k) getc src =
        case getc src of
          SOME(c, src') =>
            if isDigit radix c
            then digits (radix, base radix * n + value c, k + 1) getc src'
            else (n, k, src)
        | NONE => (n, k, src)

    fun hexPrefix getc src =
        case getc src of
          SOME(#"0", src1) =>
            (case getc src1 of
               SOME(c, src2) => if c = #"x" orelse c = #"X" then src2 else src
             | NONE => src)
        | _ => src
  in
    fun scan radix getc src =
        let
          val src = skipWS getc src
          val (negative, src1) =
              case getc src of
                SOME(#"-", src') => (true, src')
              | SOME(#"~", src') => (true, src')
              | SOME(#"+", src') => (false, src')
              | _ => (false, src)
          val src2 = if radix = HEX then hexPrefix getc src1 else src1
          val (n, k, src3) =
              case digits (radix, 0, 0) getc src2 of
                (_, 0, _) => digits (radix, 0, 0) getc src1
              | result => result
        in
          if k = 0 then NONE else SOME(if negative then ~n else n, src3)
        end
  end

  val fromString    = StringCvt.scanString(scan StringCvt.DEC)

  val op<           = op< : int * int -> bool
  val op<=          = op<= : int * int -> bool
  val op>           = op> : int * int -> bool
  val op>=          = op>= : int * int -> bool
end;
