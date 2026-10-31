(*
 * (c) Andreas Rossberg 2001-2025
 *
 * Standard ML Basis Library
 *
 * Notes:
 * - We only implement required structures (and their signatures).
 * - Modules commented out are not yet implemented.
 *
 * cl-sml: this directory is cl-sml's Basis Library.  It is derived from
 * HaMLet's (https://github.com/rossberg/hamlet, directory basis/, commit
 * 37275b9), under HaMLet's license (LICENSE.txt here), with cl-sml's fixes
 * and additions.  cl-sml compiles these files once into the package
 * SML.BASIS, which every SML package sees (src/basis.lisp), and HaMLet's
 * checker elaborates the same files as the initial basis of the programs it
 * checks (src/type-checker.lisp).  `use{b = "Name"}' is the primitive Name
 * of cl-sml's run time (SML-BASIS-PRIMITIVE in src/runtime.lisp).  The
 * checker only elaborates these files (in the order of the uses below),
 * so HaMLet's evaluator need not know cl-sml's primitives.
 *)

local val use : string -> unit = use in val _ = let in

use "infix.sml";
use "types.sml";
use "exceptions.sml";

use "GENERAL-sig.sml";
use "General.sml";
use "OPTION-sig.sml";
use "Option.sml";
use "BOOL-sig.sml";
use "Bool.sml";

use "LIST-sig.sml";
use "List.sml";
use "LIST_PAIR-sig.sml";
use "ListPair.sml";

use "CHAR-sig.sml";
use "Char.sml";
use "STRING-sig.sml";
use "String.sml";
use "SUBSTRING-sig.sml";
use "Substring.sml";
use "STRING_CVT-sig.sml";
use "StringCvt.sml";

use "INTEGER-sig.sml";
use "Int.sml";
use "LargeInt.sml";
use "Position.sml";
use "WORD-sig.sml";
use "Word.sml";
use "Word8.sml";
use "LargeWord.sml";
use "INT_INF-sig.sml";      (* cl-sml *)
use "IntInf.sml";           (* cl-sml *)
use "IntN.sml";             (* cl-sml: Int64, Int32, Int16, Int8 *)
use "WordN.sml";            (* cl-sml: Word64, Word32, Word16 *)

use "IEEE_REAL-sig.sml";
use "IEEEReal.sml";
use "MATH-sig.sml";
use "Math.sml";
use "REAL-sig.sml";
use "Real.sml";
use "LargeReal.sml";

use "VECTOR-sig.sml";
use "Vector.sml";
use "MONO_VECTOR-sig.sml";
use "Word8Vector.sml";
use "CharVector.sml";
use "VECTOR_SLICE-sig.sml";
use "VectorSlice.sml";
use "MONO_VECTOR_SLICE-sig.sml";
use "Word8VectorSlice.sml";
use "CharVectorSlice.sml";

use "ARRAY-sig.sml";
use "Array.sml";
use "MONO_ARRAY-sig.sml";
use "Word8Array.sml";
use "CharArray.sml";
use "ARRAY_SLICE-sig.sml";
use "ArraySlice.sml";
use "MONO_ARRAY_SLICE-sig.sml";
use "Word8ArraySlice.sml";
use "CharArraySlice.sml";

use "BYTE-sig.sml";
use "Byte.sml";
use "TEXT-sig.sml";
use "Text.sml";

use "IO-sig.sml";
use "IO.sml";
(*use "PRIM_IO-sig.sml";*)
(*use "BinPrimIO.sml";*)
(*use "TextPrimIO.sml";*)
use "STREAM_IO-sig.sml";
use "IMPERATIVE_IO-sig.sml";
use "TEXT_STREAM_IO-sig.sml";
use "TEXT_IO-sig.sml";
use "TextIO.sml";
use "BIN_IO-sig.sml";
use "BinIO.sml";

use "TIME-sig.sml";         (* cl-sml *)
use "Time.sml";             (* cl-sml *)
use "TIMER-sig.sml";        (* cl-sml *)
use "Timer.sml";            (* cl-sml *)

use "OS_FILE_SYS-sig.sml";
use "OS_PATH-sig.sml";
use "OS_PROCESS-sig.sml";
(*use "OS_IO-sig.sml";*)
use "OS-sig.sml";
use "OS_Path.sml";
use "OS_FileSys.sml";
use "OS_Process.sml";
(*use "OS_IO.sml";*)
use "OS.sml";
use "COMMAND_LINE-sig.sml";
use "CommandLine.sml";
(*use "DATE-sig.sml";*)
(*use "Date.sml";*)

use "values.sml";

() end end;
