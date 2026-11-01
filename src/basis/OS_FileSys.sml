(*
 * (c) Andreas Rossberg 2001-2025
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

structure OS =
struct
  open OS

  structure FileSys : OS_FILE_SYS =
  struct
    val getDir = use{b = "OS.FileSys.getDir"} : unit -> string
    val chDir  = use{b = "OS.FileSys.chDir"} : string -> unit
    val mkDir  = use{b = "OS.FileSys.mkDir"} : string -> unit
    val rmDir  = use{b = "OS.FileSys.rmDir"} : string -> unit
    val isDir  = use{b = "OS.FileSys.isDir"} : string -> bool
    val isLink = use{b = "OS.FileSys.isLink"} : string -> bool

    (* A directory stream reads the entries listed when it was opened. *)
    type dirstream = string list * string list ref
    val listDir = use{b = "OS.FileSys.listDir"} : string -> string list
    fun openDir name = let val names = listDir name in (names, ref names) end
    fun readDir (_, rest) =
        case !rest of [] => NONE | n :: ns => (rest := ns; SOME n)
    fun rewindDir (names, rest) = rest := names
    fun closeDir (_, rest) = rest := []

    (* cl-sml *)
    val fullPath = use{b = "OS.FileSys.fullPath"} : string -> string
    val realPath = fullPath
    val modTime  = use{b = "OS.FileSys.modTime"} : string -> Time.time
    (* The run time cannot set a file's times. *)
    val setTime = use{b = "OS.FileSys.setTime"} : string * Time.time option -> unit
    val fileSize = use{b = "OS.FileSys.fileSize"} : string -> Position.int
    val remove   = use{b = "OS.FileSys.remove"} : string -> unit
    val rename   = use{b = "OS.FileSys.rename"}
                     : {old : string, new : string} -> unit
    datatype access_mode = A_READ | A_WRITE | A_EXEC
    (* The run time cannot test permissions: a file is accessible in every
     * mode if it exists. *)
    val exists   = use{b = "OS.FileSys.exists"} : string -> bool
    fun access(name, modes : access_mode list) = exists name
    val tmpName  = use{b = "OS.FileSys.tmpName"} : unit -> string
  end
end;
