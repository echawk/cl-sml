(*
 * cl-sml: Standard ML Basis Library, structure Time
 * (not part of HaMLet's basis).
 *
 * Note: a time is a number of nanoseconds (since the Unix epoch for now()),
 * a LargeInt.int (that is, an IntInf.int).
 *)

structure Time :> TIME =
struct
  type time         = LargeInt.int

  exception Time

  val zeroTime      = LargeInt.fromInt 0

  fun fromReal r    = Real.toLargeInt IEEEReal.TO_NEAREST (r * 1.0E9)
                      handle Overflow => raise Time | Domain => raise Time
  fun toReal t      = Real.fromLargeInt t / 1.0E9

  fun toSeconds t      = LargeInt.quot(t, LargeInt.fromInt 1000000000)
  fun toMilliseconds t = LargeInt.quot(t, LargeInt.fromInt 1000000)
  fun toMicroseconds t = LargeInt.quot(t, LargeInt.fromInt 1000)
  fun toNanoseconds t  = t
  fun fromSeconds s      = LargeInt.*(s, LargeInt.fromInt 1000000000)
  fun fromMilliseconds s = LargeInt.*(s, LargeInt.fromInt 1000000)
  fun fromMicroseconds s = LargeInt.*(s, LargeInt.fromInt 1000)
  fun fromNanoseconds s  = s

  fun t1 + t2       = LargeInt.+(t1, t2) handle Overflow => raise Time
  fun t1 - t2       = LargeInt.-(t1, t2) handle Overflow => raise Time
  val compare       = LargeInt.compare

  val now           = use{b = "Time.now"} : unit -> time

  fun fmt n t       = Real.fmt (StringCvt.FIX(SOME(if n < 0 then 0 else n)))
                               (toReal t)
  val toString      = fmt 3
  fun scan getc src = case Real.scan getc src of
                        SOME(r, src') => SOME(fromReal r, src')
                      | NONE => NONE
  val fromString    = StringCvt.scanString scan

  val op<           = LargeInt.<
  val op<=          = LargeInt.<=
  val op>           = LargeInt.>
  val op>=          = LargeInt.>=
end;
