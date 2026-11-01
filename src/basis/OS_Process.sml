(*
 * (c) Andreas Rossberg 2013-2025
 *
 * Standard ML Basis Library
 *)

structure OS =
struct
  open OS

  structure Process :> OS_PROCESS =
  struct
    type status = int

    val success = 0
    val failure = 1

    fun isSuccess st = st = 0
    fun terminate st = (use{b = "OS.Process.terminate"} : status -> 'a) st
    val exit = terminate
    fun getEnv name = (use{b = "OS.Process.getEnv"} : string -> string option) name
    fun system command = (use{b = "OS.Process.system"} : string -> status) command
  end
end;
