(* shim.sml -- loaded before the program in the *-shim modes only.
 * HaMLet's basis (hamlet/basis/all.sml) leaves BinIO out, and util/log.sml
 * (every SML/NJ benchmark) mentions it, so in checked mode every benchmark
 * stops at "unknown type BinIO.outstream".  This minimal BinIO over TextIO
 * lets the *-shim runs measure what blocks the programs next. *)
structure BinIO = struct
  type outstream = TextIO.outstream
  val openOut = TextIO.openOut
  val closeOut = TextIO.closeOut
  fun output (s, v) = TextIO.output (s, Byte.bytesToString v)
  fun output1 (s, b) = TextIO.output1 (s, Byte.byteToChar b)
  val flushOut = TextIO.flushOut
end
