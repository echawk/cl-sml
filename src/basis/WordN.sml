(*
 * cl-sml: Standard ML Basis Library, structures Word64, Word32 and Word16
 * (not part of HaMLet's basis).
 *
 * Notes:
 * - The values are the run time's integers; the structures' operations
 *   reduce modulo 2^wordSize.  The overloaded operators (+ etc.) only
 *   apply to them unchecked (HaMLet's overloading classes do not include
 *   them), and then do not reduce.
 * - LargeWord is Word (wordSize 62), so Word64.toLarge loses the top bits.
 *)

local
  (* The value of the unsigned number scan reads (with Word.scan's syntax),
   * which fromIntInf reduces or rejects with Overflow. *)
  structure WordScan =
  struct
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
        IntInf.fromInt(Char.ord(Char.toUpper c) -
          (if c < #"A" then Char.ord #"0" else Char.ord #"A" - 10))

    fun digits (radix, n, k) getc src =
        case getc src of
          SOME(c, src') =>
            if isDigit radix c
            then digits (radix, IntInf.+(IntInf.*(IntInf.fromInt(base radix), n),
                                         value c),
                         k + 1) getc src'
            else (n, k, src)
        | NONE => (n, k, src)

    (* 0w, 0wx, 0wX, and for HEX also 0x and 0X *)
    fun prefix radix getc src =
        case getc src of
          SOME(#"0", src1) =>
            (case getc src1 of
               SOME(#"w", src2) =>
                 (case getc src2 of
                    SOME(c, src3) =>
                      if radix = HEX andalso (c = #"x" orelse c = #"X")
                      then src3 else src2
                  | NONE => src2)
             | SOME(c, src2) =>
                 if radix = HEX andalso (c = #"x" orelse c = #"X")
                 then src2 else src
             | NONE => src)
        | _ => src

    fun scan radix getc src =
        let
          val src1 = skipWS getc src
          val zero = IntInf.fromInt 0
          val (n, k, src2) =
              case digits (radix, zero, 0) getc (prefix radix getc src1) of
                (_, 0, _) => digits (radix, zero, 0) getc src1
              | result => result
        in
          if k = 0 then NONE else SOME(n, src2)
        end
  end
in

structure Word64 :> WORD =
struct
  type word         = Word.word (* the run time's integers, below 2^64 *)

  val wordSize      = 64

  val toLarge       = use{b = "Word64.toLarge"} : word -> LargeWord.word
  val toLargeX      = use{b = "Word64.toLargeX"} : word -> LargeWord.word
  val fromLarge     = use{b = "Word64.fromLarge"} : LargeWord.word -> word
  val toInt         = use{b = "Word64.toInt"} : word -> Int.int
  val toIntX        = use{b = "Word64.toIntX"} : word -> Int.int
  val fromInt       = use{b = "Word64.fromInt"} : Int.int -> word
  val toLargeInt    = use{b = "Word64.toLargeInt"} : word -> LargeInt.int
  val toLargeIntX   = use{b = "Word64.toLargeIntX"} : word -> LargeInt.int
  val fromLargeInt  = use{b = "Word64.fromLargeInt"} : LargeInt.int -> word
  val fromIntInf    = use{b = "Word64.fromIntInf"} : IntInf.int -> word

  val notb          = use{b = "Word64.notb"} : word -> word
  val orb           = use{b = "Word64.orb"} : word * word -> word
  val xorb          = use{b = "Word64.xorb"} : word * word -> word
  val andb          = use{b = "Word64.andb"} : word * word -> word
  val <<            = use{b = "Word64.<<"} : word * Word.word -> word
  val >>            = use{b = "Word64.>>"} : word * Word.word -> word
  val ~>>           = use{b = "Word64.~>>"} : word * Word.word -> word
  val op+           = use{b = "Word64.+"} : word * word -> word
  val op-           = use{b = "Word64.-"} : word * word -> word
  val op*           = use{b = "Word64.*"} : word * word -> word
  val op div        = use{b = "Word64.div"} : word * word -> word
  val op mod        = use{b = "Word64.mod"} : word * word -> word
  fun ~w            = fromInt 0 - w

  fun compare(i : word, j) =
      if i < j then LESS else if i = j then EQUAL else GREATER
  fun min(i : word, j) = if i < j then i else j
  fun max(i : word, j) = if i > j then i else j

  val fmt           = use{b = "Word64.fmt"} : StringCvt.radix -> word -> string
  val toString      = fmt StringCvt.HEX
  fun scan radix getc src =
      case WordScan.scan radix getc src of
        SOME(i, src') => SOME(fromIntInf i, src')
      | NONE => NONE
  val fromString    = StringCvt.scanString(scan StringCvt.HEX)

  val op<           = op< : word * word -> bool
  val op<=          = op<= : word * word -> bool
  val op>           = op> : word * word -> bool
  val op>=          = op>= : word * word -> bool
end;

structure Word32 :> WORD =
struct
  type word         = Word.word (* the run time's integers, below 2^32 *)

  val wordSize      = 32

  val toLarge       = use{b = "Word32.toLarge"} : word -> LargeWord.word
  val toLargeX      = use{b = "Word32.toLargeX"} : word -> LargeWord.word
  val fromLarge     = use{b = "Word32.fromLarge"} : LargeWord.word -> word
  val toInt         = use{b = "Word32.toInt"} : word -> Int.int
  val toIntX        = use{b = "Word32.toIntX"} : word -> Int.int
  val fromInt       = use{b = "Word32.fromInt"} : Int.int -> word
  val toLargeInt    = use{b = "Word32.toLargeInt"} : word -> LargeInt.int
  val toLargeIntX   = use{b = "Word32.toLargeIntX"} : word -> LargeInt.int
  val fromLargeInt  = use{b = "Word32.fromLargeInt"} : LargeInt.int -> word
  val fromIntInf    = use{b = "Word32.fromIntInf"} : IntInf.int -> word

  val notb          = use{b = "Word32.notb"} : word -> word
  val orb           = use{b = "Word32.orb"} : word * word -> word
  val xorb          = use{b = "Word32.xorb"} : word * word -> word
  val andb          = use{b = "Word32.andb"} : word * word -> word
  val <<            = use{b = "Word32.<<"} : word * Word.word -> word
  val >>            = use{b = "Word32.>>"} : word * Word.word -> word
  val ~>>           = use{b = "Word32.~>>"} : word * Word.word -> word
  val op+           = use{b = "Word32.+"} : word * word -> word
  val op-           = use{b = "Word32.-"} : word * word -> word
  val op*           = use{b = "Word32.*"} : word * word -> word
  val op div        = use{b = "Word32.div"} : word * word -> word
  val op mod        = use{b = "Word32.mod"} : word * word -> word
  fun ~w            = fromInt 0 - w

  fun compare(i : word, j) =
      if i < j then LESS else if i = j then EQUAL else GREATER
  fun min(i : word, j) = if i < j then i else j
  fun max(i : word, j) = if i > j then i else j

  val fmt           = use{b = "Word32.fmt"} : StringCvt.radix -> word -> string
  val toString      = fmt StringCvt.HEX
  fun scan radix getc src =
      case WordScan.scan radix getc src of
        SOME(i, src') => SOME(fromIntInf i, src')
      | NONE => NONE
  val fromString    = StringCvt.scanString(scan StringCvt.HEX)

  val op<           = op< : word * word -> bool
  val op<=          = op<= : word * word -> bool
  val op>           = op> : word * word -> bool
  val op>=          = op>= : word * word -> bool
end;

structure Word16 :> WORD =
struct
  type word         = Word.word (* the run time's integers, below 2^16 *)

  val wordSize      = 16

  val toLarge       = use{b = "Word16.toLarge"} : word -> LargeWord.word
  val toLargeX      = use{b = "Word16.toLargeX"} : word -> LargeWord.word
  val fromLarge     = use{b = "Word16.fromLarge"} : LargeWord.word -> word
  val toInt         = use{b = "Word16.toInt"} : word -> Int.int
  val toIntX        = use{b = "Word16.toIntX"} : word -> Int.int
  val fromInt       = use{b = "Word16.fromInt"} : Int.int -> word
  val toLargeInt    = use{b = "Word16.toLargeInt"} : word -> LargeInt.int
  val toLargeIntX   = use{b = "Word16.toLargeIntX"} : word -> LargeInt.int
  val fromLargeInt  = use{b = "Word16.fromLargeInt"} : LargeInt.int -> word
  val fromIntInf    = use{b = "Word16.fromIntInf"} : IntInf.int -> word

  val notb          = use{b = "Word16.notb"} : word -> word
  val orb           = use{b = "Word16.orb"} : word * word -> word
  val xorb          = use{b = "Word16.xorb"} : word * word -> word
  val andb          = use{b = "Word16.andb"} : word * word -> word
  val <<            = use{b = "Word16.<<"} : word * Word.word -> word
  val >>            = use{b = "Word16.>>"} : word * Word.word -> word
  val ~>>           = use{b = "Word16.~>>"} : word * Word.word -> word
  val op+           = use{b = "Word16.+"} : word * word -> word
  val op-           = use{b = "Word16.-"} : word * word -> word
  val op*           = use{b = "Word16.*"} : word * word -> word
  val op div        = use{b = "Word16.div"} : word * word -> word
  val op mod        = use{b = "Word16.mod"} : word * word -> word
  fun ~w            = fromInt 0 - w

  fun compare(i : word, j) =
      if i < j then LESS else if i = j then EQUAL else GREATER
  fun min(i : word, j) = if i < j then i else j
  fun max(i : word, j) = if i > j then i else j

  val fmt           = use{b = "Word16.fmt"} : StringCvt.radix -> word -> string
  val toString      = fmt StringCvt.HEX
  fun scan radix getc src =
      case WordScan.scan radix getc src of
        SOME(i, src') => SOME(fromIntInf i, src')
      | NONE => NONE
  val fromString    = StringCvt.scanString(scan StringCvt.HEX)

  val op<           = op< : word * word -> bool
  val op<=          = op<= : word * word -> bool
  val op>           = op> : word * word -> bool
  val op>=          = op>= : word * word -> bool
end;
end;
