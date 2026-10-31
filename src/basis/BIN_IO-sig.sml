(*
 * cl-sml: Standard ML Basis Library, signature BIN_IO
 * (not part of HaMLet's basis).
 *
 * Note: Incomplete, like TEXT_IO: no stream-level (StreamIO) operations.
 *)

signature BIN_IO =
sig
  structure StreamIO : STREAM_IO
    where type vector = Word8Vector.vector
    where type elem = Word8.word

  (* include IMPERATIVE_IO *)
  type vector = StreamIO.vector
  type elem = StreamIO.elem
  type instream
  type outstream
  val input : instream -> vector
  val input1 : instream -> elem option
  val inputN : instream * int -> vector
  val inputAll : instream -> vector
  val lookahead : instream -> elem option
  val closeIn : instream -> unit
  val endOfStream : instream -> bool
  val output : outstream * vector -> unit
  val output1 : outstream * elem -> unit
  val flushOut : outstream -> unit
  val closeOut : outstream -> unit

  val openIn : string -> instream
  val openOut : string -> outstream
  val openAppend : string -> outstream
end;
