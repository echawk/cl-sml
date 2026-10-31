(*
 * cl-sml: Standard ML Basis Library, structure BinIO
 * (not part of HaMLet's basis).
 *
 * Note: Incomplete, like TextIO.  Streams are the run time's octet streams.
 *)

structure BinIO :> BIN_IO =
struct
  structure StreamIO =
  struct
    type vector   = Word8Vector.vector
    type elem     = Word8.word
  end

  type vector     = StreamIO.vector
  type elem       = StreamIO.elem
  type instream   = int
  type outstream  = int

  val openIn      = use{b = "BinIO.openIn"} : string -> instream
  val openOut     = use{b = "BinIO.openOut"} : string -> outstream
  val openAppend  = use{b = "BinIO.openAppend"} : string -> outstream
  val closeIn     = use{b = "BinIO.closeIn"} : instream -> unit
  val closeOut    = use{b = "BinIO.closeOut"} : outstream -> unit

  val input       = use{b = "BinIO.input"} : instream -> vector
  val input1      = use{b = "BinIO.input1"} : instream -> elem option
  val inputN      = use{b = "BinIO.inputN"} : instream * int -> vector
  val inputAll    = use{b = "BinIO.inputAll"} : instream -> vector
  val lookahead   = use{b = "BinIO.lookahead"} : instream -> elem option
  val endOfStream = use{b = "BinIO.endOfStream"} : instream -> bool

  val output      = use{b = "BinIO.output"} : outstream * vector -> unit
  val output1     = use{b = "BinIO.output1"} : outstream * elem -> unit
  val flushOut    = use{b = "BinIO.flushOut"} : outstream -> unit
end;
