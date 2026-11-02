(*
 * cl-sml: Standard ML Basis Library, structures Int64, Int32, Int16 and Int8
 * (not part of HaMLet's basis).
 *
 * Notes:
 * - The values are the run time's integers; the structures' operations
 *   raise Overflow outside their ranges.  The overloaded operators (+ etc.)
 *   only apply to them unchecked (HaMLet's overloading classes do not
 *   include them), and then check Int's range, not theirs.
 * - LargeInt is IntInf.
 *)

structure Int64 :> INTEGER =
struct
  type int          = Int.int (* the run time's integers, in 64 bits *)

  val precision     = use{b = "Int64.precision"} () : Int.int option
  val minInt        = use{b = "Int64.minInt"} () : int option
  val maxInt        = use{b = "Int64.maxInt"} () : int option

  val toLarge       = use{b = "Int64.toLarge"} : int -> LargeInt.int
  val fromLarge     = use{b = "Int64.fromLarge"} : LargeInt.int -> int
  val toInt         = use{b = "Int64.toInt"} : int -> Int.int
  val fromInt       = use{b = "Int64.fromInt"} : Int.int -> int
  val fromIntInf    = use{b = "Int64.fromLarge"} : IntInf.int -> int

  val ~             = use{b = "Int64.~"} : int -> int
  val abs           = use{b = "Int64.abs"} : int -> int
  val op+           = use{b = "Int64.+"} : int * int -> int
  val op-           = use{b = "Int64.-"} : int * int -> int
  val op*           = use{b = "Int64.*"} : int * int -> int
  val op div        = use{b = "Int64.div"} : int * int -> int
  val op mod        = use{b = "Int64.mod"} : int * int -> int
  val quot          = use{b = "Int64.quot"} : int * int -> int
  val rem           = use{b = "Int64.rem"} : int * int -> int

  fun compare(i, j) = if i < j then LESS else if i = j then EQUAL else GREATER
  fun min(i, j)     = if i < j then i else j
  fun max(i, j)     = if i > j then i else j
  fun sign i        = if i < 0 then ~1 else if i > 0 then 1 else 0
  fun sameSign(i, j) = sign i = sign j

  val fmt           = use{b = "Int64.fmt"} : StringCvt.radix -> int -> string
  val toString      = fmt StringCvt.DEC
  fun scan radix getc src =
      case IntInf.scan radix getc src of
        SOME(i, src') => SOME(fromIntInf i, src')
      | NONE => NONE
  val fromString    = StringCvt.scanString(scan StringCvt.DEC)

  val op<           = op< : int * int -> bool
  val op<=          = op<= : int * int -> bool
  val op>           = op> : int * int -> bool
  val op>=          = op>= : int * int -> bool
end;

structure Int32 :> INTEGER =
struct
  type int          = Int.int (* the run time's integers, in 32 bits *)

  val precision     = use{b = "Int32.precision"} () : Int.int option
  val minInt        = use{b = "Int32.minInt"} () : int option
  val maxInt        = use{b = "Int32.maxInt"} () : int option

  val toLarge       = use{b = "Int32.toLarge"} : int -> LargeInt.int
  val fromLarge     = use{b = "Int32.fromLarge"} : LargeInt.int -> int
  val toInt         = use{b = "Int32.toInt"} : int -> Int.int
  val fromInt       = use{b = "Int32.fromInt"} : Int.int -> int
  val fromIntInf    = use{b = "Int32.fromLarge"} : IntInf.int -> int

  val ~             = use{b = "Int32.~"} : int -> int
  val abs           = use{b = "Int32.abs"} : int -> int
  val op+           = use{b = "Int32.+"} : int * int -> int
  val op-           = use{b = "Int32.-"} : int * int -> int
  val op*           = use{b = "Int32.*"} : int * int -> int
  val op div        = use{b = "Int32.div"} : int * int -> int
  val op mod        = use{b = "Int32.mod"} : int * int -> int
  val quot          = use{b = "Int32.quot"} : int * int -> int
  val rem           = use{b = "Int32.rem"} : int * int -> int

  fun compare(i, j) = if i < j then LESS else if i = j then EQUAL else GREATER
  fun min(i, j)     = if i < j then i else j
  fun max(i, j)     = if i > j then i else j
  fun sign i        = if i < 0 then ~1 else if i > 0 then 1 else 0
  fun sameSign(i, j) = sign i = sign j

  val fmt           = use{b = "Int32.fmt"} : StringCvt.radix -> int -> string
  val toString      = fmt StringCvt.DEC
  fun scan radix getc src =
      case IntInf.scan radix getc src of
        SOME(i, src') => SOME(fromIntInf i, src')
      | NONE => NONE
  val fromString    = StringCvt.scanString(scan StringCvt.DEC)

  val op<           = op< : int * int -> bool
  val op<=          = op<= : int * int -> bool
  val op>           = op> : int * int -> bool
  val op>=          = op>= : int * int -> bool
end;

structure Int16 :> INTEGER =
struct
  type int          = Int.int (* the run time's integers, in 16 bits *)

  val precision     = use{b = "Int16.precision"} () : Int.int option
  val minInt        = use{b = "Int16.minInt"} () : int option
  val maxInt        = use{b = "Int16.maxInt"} () : int option

  val toLarge       = use{b = "Int16.toLarge"} : int -> LargeInt.int
  val fromLarge     = use{b = "Int16.fromLarge"} : LargeInt.int -> int
  val toInt         = use{b = "Int16.toInt"} : int -> Int.int
  val fromInt       = use{b = "Int16.fromInt"} : Int.int -> int
  val fromIntInf    = use{b = "Int16.fromLarge"} : IntInf.int -> int

  val ~             = use{b = "Int16.~"} : int -> int
  val abs           = use{b = "Int16.abs"} : int -> int
  val op+           = use{b = "Int16.+"} : int * int -> int
  val op-           = use{b = "Int16.-"} : int * int -> int
  val op*           = use{b = "Int16.*"} : int * int -> int
  val op div        = use{b = "Int16.div"} : int * int -> int
  val op mod        = use{b = "Int16.mod"} : int * int -> int
  val quot          = use{b = "Int16.quot"} : int * int -> int
  val rem           = use{b = "Int16.rem"} : int * int -> int

  fun compare(i, j) = if i < j then LESS else if i = j then EQUAL else GREATER
  fun min(i, j)     = if i < j then i else j
  fun max(i, j)     = if i > j then i else j
  fun sign i        = if i < 0 then ~1 else if i > 0 then 1 else 0
  fun sameSign(i, j) = sign i = sign j

  val fmt           = use{b = "Int16.fmt"} : StringCvt.radix -> int -> string
  val toString      = fmt StringCvt.DEC
  fun scan radix getc src =
      case IntInf.scan radix getc src of
        SOME(i, src') => SOME(fromIntInf i, src')
      | NONE => NONE
  val fromString    = StringCvt.scanString(scan StringCvt.DEC)

  val op<           = op< : int * int -> bool
  val op<=          = op<= : int * int -> bool
  val op>           = op> : int * int -> bool
  val op>=          = op>= : int * int -> bool
end;

structure Int8 :> INTEGER =
struct
  type int          = Int.int (* the run time's integers, in 8 bits *)

  val precision     = use{b = "Int8.precision"} () : Int.int option
  val minInt        = use{b = "Int8.minInt"} () : int option
  val maxInt        = use{b = "Int8.maxInt"} () : int option

  val toLarge       = use{b = "Int8.toLarge"} : int -> LargeInt.int
  val fromLarge     = use{b = "Int8.fromLarge"} : LargeInt.int -> int
  val toInt         = use{b = "Int8.toInt"} : int -> Int.int
  val fromInt       = use{b = "Int8.fromInt"} : Int.int -> int
  val fromIntInf    = use{b = "Int8.fromLarge"} : IntInf.int -> int

  val ~             = use{b = "Int8.~"} : int -> int
  val abs           = use{b = "Int8.abs"} : int -> int
  val op+           = use{b = "Int8.+"} : int * int -> int
  val op-           = use{b = "Int8.-"} : int * int -> int
  val op*           = use{b = "Int8.*"} : int * int -> int
  val op div        = use{b = "Int8.div"} : int * int -> int
  val op mod        = use{b = "Int8.mod"} : int * int -> int
  val quot          = use{b = "Int8.quot"} : int * int -> int
  val rem           = use{b = "Int8.rem"} : int * int -> int

  fun compare(i, j) = if i < j then LESS else if i = j then EQUAL else GREATER
  fun min(i, j)     = if i < j then i else j
  fun max(i, j)     = if i > j then i else j
  fun sign i        = if i < 0 then ~1 else if i > 0 then 1 else 0
  fun sameSign(i, j) = sign i = sign j

  val fmt           = use{b = "Int8.fmt"} : StringCvt.radix -> int -> string
  val toString      = fmt StringCvt.DEC
  fun scan radix getc src =
      case IntInf.scan radix getc src of
        SOME(i, src') => SOME(fromIntInf i, src')
      | NONE => NONE
  val fromString    = StringCvt.scanString(scan StringCvt.DEC)

  val op<           = op< : int * int -> bool
  val op<=          = op<= : int * int -> bool
  val op>           = op> : int * int -> bool
  val op>=          = op>= : int * int -> bool
end;
