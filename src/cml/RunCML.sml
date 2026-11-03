(*
 * cl-sml: Concurrent ML, structure RunCML (docs/DESIGN-CML.md).  doit runs
 * its function as the first thread and returns the status given to
 * shutdown, or OS.Process.failure when no thread can run any more
 * (deadlock, or every thread ended).  The time slice is ignored: the
 * threads are OS threads, which the OS preempts.
 *)

structure RunCML :> RUN_CML =
struct
  (* The run time passes a status as 0 (success) or 1 (failure). *)
  val isRunning   = use{b = "RunCML.isRunning"} : unit -> bool
  val run         = use{b = "RunCML.doit"} : (unit -> unit) -> int
  fun doit (f, _) = if run f = 0 then OS.Process.success else OS.Process.failure
  fun shutdown st = (use{b = "RunCML.shutdown"} : int -> 'a)
                      (if OS.Process.isSuccess st then 0 else 1)
end;
