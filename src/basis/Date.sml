(*
 * cl-sml: Standard ML Basis Library, structure Date
 * (not part of HaMLet's basis).
 *
 * Notes:
 * - A date is kept as its wall-clock time in seconds since 1970-01-01
 *   00:00:00 (as if it were UTC), its offset (NONE for the local time zone,
 *   else SOME t, t west of UTC: local time = UTC - t, as MLton has it) and
 *   whether daylight saving time is in effect (known for local dates only).
 * - The calendar is the proleptic Gregorian one, with Howard Hinnant's
 *   days-from-civil algorithms.
 * - The local time zone comes from the host Lisp (decode-universal-time),
 *   through Date.localOffset and Date.isDst; %Z prints "UTC" for a zero
 *   offset and nothing otherwise, as no time zone names are known.
 * - fmt knows %a %A %b %B %c %d %H %I %j %m %M %p %S %U %w %W %x %X %y %Y
 *   %Z %% of the Basis and C's %D %e %F %h %n %R %t %T %u %z.
 *)

structure Date :> DATE =
struct
  datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  datatype month = Jan | Feb | Mar | Apr | May | Jun
                 | Jul | Aug | Sep | Oct | Nov | Dec

  type date = {wall : int, offset : Time.time option, dst : bool option}

  exception Date

  infix 6 ^
  val op^ = String.^
  val rev = List.rev

  (* The offset west of UTC, in seconds, and daylight saving time, of the
   * local time zone at the UTC time given in seconds since the epoch. *)
  val localOffsetAt = use{b = "Date.localOffset"} : int -> int
  val isDstAt       = use{b = "Date.isDst"} : int -> bool

  val months = [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]
  val monthNames = ["January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November",
                    "December"]
  (* Sunday first, as weekday numbers %w count. *)
  val weekdays = [Sun, Mon, Tue, Wed, Thu, Fri, Sat]
  val weekdayNames = ["Sunday", "Monday", "Tuesday", "Wednesday",
                      "Thursday", "Friday", "Saturday"]

  fun index (x, l) =
      let fun go (_, []) = raise Date
            | go (i, y :: ys) = if x = y then i else go (i + 1, ys)
      in go (0, l) end

  fun monthNumber m = index (m, months) + 1     (* 1 .. 12 *)

  fun daysFromCivil (y, m, d) =
      let val y   = if m <= 2 then y - 1 else y
          val era = y div 400
          val yoe = y - era * 400
          val mp  = (m + 9) mod 12
          val doy = (153 * mp + 2) div 5 + d - 1
          val doe = yoe * 365 + yoe div 4 - yoe div 100 + doy
      in era * 146097 + doe - 719468 end

  fun civilFromDays z =
      let val z   = z + 719468
          val era = z div 146097
          val doe = z - era * 146097
          val yoe = (doe - doe div 1460 + doe div 36524 - doe div 146096) div 365
          val y   = yoe + era * 400
          val doy = doe - (365 * yoe + yoe div 4 - yoe div 100)
          val mp  = (5 * doy + 2) div 153
          val d   = doy - (153 * mp + 2) div 5 + 1
          val m   = if mp < 10 then mp + 3 else mp - 9
      in (if m <= 2 then y + 1 else y, m, d) end

  fun timeToSeconds t = Int.fromLarge (LargeInt.div (Time.toNanoseconds t,
                                                     LargeInt.fromInt 1000000000))
  fun secondsToTime s = Time.fromSeconds (Int.toLarge s)

  val day0 = 86400

  fun fields ({wall, ...} : date) =
      let val days = wall div day0
          val secs = wall mod day0
          val (y, m, d) = civilFromDays days
      in {year = y, month = m, day = d, days = days,
          hour = secs div 3600, minute = secs mod 3600 div 60,
          second = secs mod 60}
      end

  fun normaliseOffset t =
      (* Into (-24h, 24h), as the Basis asks. *)
      let val s = timeToSeconds t
      in secondsToTime (Int.rem (s, day0)) end

  fun date {year, month, day, hour, minute, second, offset} =
      let val wall = (daysFromCivil (year, monthNumber month, 1) + day - 1) * day0
                     + hour * 3600 + minute * 60 + second
      in {wall = wall, offset = Option.map normaliseOffset offset, dst = NONE}
      end
      handle Overflow => raise Date

  fun year d    = #year (fields d)
  fun month d   = List.nth (months, #month (fields d) - 1)
  fun day d     = #day (fields d)
  fun hour d    = #hour (fields d)
  fun minute d  = #minute (fields d)
  fun second d  = #second (fields d)
  (* 1970-01-01 was a Thursday. *)
  fun weekNumber d = (#days (fields d) + 4) mod 7       (* Sunday = 0 *)
  fun weekDay d = List.nth (weekdays, weekNumber d)
  fun yearDay d =
      let val {year, days, ...} = fields d
      in days - daysFromCivil (year, 1, 1) end
  fun offset ({offset, ...} : date) = offset
  fun isDst ({dst, ...} : date) = dst

  fun localOffset () = secondsToTime (localOffsetAt (timeToSeconds (Time.now ())))

  fun fromTimeLocal t =
      let val utc = timeToSeconds t
      in {wall = utc - localOffsetAt utc, offset = NONE, dst = SOME (isDstAt utc)}
      end

  fun fromTimeUniv t =
      {wall = timeToSeconds t, offset = SOME Time.zeroTime, dst = NONE}

  fun toTime ({wall, offset = SOME t, ...} : date) =
        secondsToTime (wall + timeToSeconds t)
    | toTime {wall, offset = NONE, ...} =
        let val guess = wall + localOffsetAt wall
        in secondsToTime (wall + localOffsetAt guess) end

  fun compare ({wall = w1, ...} : date, {wall = w2, ...} : date) =
      Int.compare (w1, w2)

  (* Formatting *)

  fun pad (width, c) n =
      StringCvt.padLeft c width (Int.toString n)
  val two = pad (2, #"0")
  val three = pad (3, #"0")
  fun abbrev s = String.substring (s, 0, 3)

  fun directive (d : date) c =
      let val {year = y, month = m, day = dd, hour = h, minute = mi,
               second = s, ...} = fields d
          val wd = weekNumber d
          val yd = yearDay d
          val f = directive d
      in
        case c of
          #"a" => abbrev (List.nth (weekdayNames, wd))
        | #"A" => List.nth (weekdayNames, wd)
        | #"b" => abbrev (List.nth (monthNames, m - 1))
        | #"h" => abbrev (List.nth (monthNames, m - 1))
        | #"B" => List.nth (monthNames, m - 1)
        | #"c" => String.concatWith " " [f #"a", f #"b", f #"d", f #"X", f #"Y"]
        | #"d" => two dd
        | #"e" => pad (2, #" ") dd
        | #"D" => String.concatWith "/" [f #"m", f #"d", f #"y"]
        | #"F" => String.concatWith "-" [f #"Y", f #"m", f #"d"]
        | #"H" => two h
        | #"I" => two (if h mod 12 = 0 then 12 else h mod 12)
        | #"j" => three (yd + 1)
        | #"m" => two m
        | #"M" => two mi
        | #"n" => "\n"
        | #"p" => if h < 12 then "AM" else "PM"
        | #"R" => f #"H" ^ ":" ^ f #"M"
        | #"S" => two s
        | #"t" => "\t"
        | #"T" => f #"X"
        | #"u" => Int.toString (if wd = 0 then 7 else wd)
        | #"U" => two ((yd + 7 - wd) div 7)
        | #"w" => Int.toString wd
        | #"W" => two ((yd + 7 - (wd + 6) mod 7) div 7)
        | #"x" => f #"D"
        | #"X" => String.concatWith ":" [f #"H", f #"M", f #"S"]
        | #"y" => two (y mod 100)
        | #"Y" => Int.toString y
        | #"z" =>
            (case offset d of
               NONE => ""
             | SOME t =>
                 let val east = ~ (timeToSeconds t)
                     val a = Int.abs east
                 in (if east < 0 then "-" else "+") ^ two (a div 3600)
                    ^ two (a mod 3600 div 60)
                 end)
        | #"Z" => (case offset d of
                     SOME t => if timeToSeconds t = 0 then "UTC" else ""
                   | NONE => "")
        | c => String.str c
      end

  fun fmt format d =
      let fun go ([], acc) = String.concat (rev acc)
            | go (#"%" :: c :: rest, acc) = go (rest, directive d c :: acc)
            | go (c :: rest, acc) = go (rest, String.str c :: acc)
      in go (String.explode format, []) end

  val toString = fmt "%a %b %d %H:%M:%S %Y"

  (* Scanning: the format of toString. *)

  fun scan getc src =
      let
        fun word (src, n) =
            let fun go (0, src, acc) = SOME (String.implode (rev acc), src)
                  | go (k, src, acc) =
                      case getc src of
                        SOME (c, src') => if Char.isAlpha c
                                          then go (k - 1, src', c :: acc)
                                          else NONE
                      | NONE => NONE
            in go (n, src, []) end
        fun number (src, min, max) =
            let fun go (k, n, src) =
                    if k = max then SOME (n, src)
                    else case getc src of
                           SOME (c, src') =>
                             if Char.isDigit c
                             then go (k + 1, n * 10 + (Char.ord c - Char.ord #"0"), src')
                             else if k >= min then SOME (n, src) else NONE
                         | NONE => if k >= min then SOME (n, src) else NONE
            in go (0, 0, src) end
        fun char (src, c) =
            case getc src of
              SOME (c', src') => if c = c' then SOME src' else NONE
            | NONE => NONE
        fun spaces src = StringCvt.skipWS getc src
        fun find (name, names) =
            let fun go (_, []) = NONE
                  | go (i, n :: ns) = if abbrev n = name then SOME i else go (i + 1, ns)
            in go (0, names) end
        fun >>= (NONE, _) = NONE
          | >>= (SOME x, f) = f x
        infix >>=
      in
        word (spaces src, 3) >>= (fn (wname, src) =>
        find (wname, weekdayNames) >>= (fn _ =>
        word (spaces src, 3) >>= (fn (mname, src) =>
        find (mname, monthNames) >>= (fn m =>
        number (spaces src, 1, 2) >>= (fn (dd, src) =>
        number (spaces src, 2, 2) >>= (fn (h, src) =>
        char (src, #":") >>= (fn src =>
        number (src, 2, 2) >>= (fn (mi, src) =>
        char (src, #":") >>= (fn src =>
        number (src, 2, 2) >>= (fn (s, src) =>
        number (spaces src, 1, 9) >>= (fn (y, src) =>
          if dd < 1 orelse dd > 31 orelse h > 23 orelse mi > 59 orelse s > 61
          then NONE
          else SOME (date {year = y, month = List.nth (months, m), day = dd,
                           hour = h, minute = mi, second = s, offset = NONE},
                     src))))))))))))
      end

  val fromString = StringCvt.scanString scan
end;
