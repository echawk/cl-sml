(*
 * cl-sml: Concurrent ML, structure RunCML (docs/DESIGN-CML.md).  doit runs
 * its function as the first thread and returns the status given to
 * shutdown, or OS.Process.failure when no thread can run any more
 * (deadlock, or every thread ended).  The time slice is ignored: the
 * threads are OS threads, which the OS preempts.
 *)

structure RunCML :> RUN_CML =
struct
  val isRunning   = use{b = "RunCML.isRunning"} : unit -> bool
  val run         = use{b = "RunCML.doit"} : (unit -> unit) -> OS.Process.status
  fun doit (f, _) = run f
  fun shutdown st = (use{b = "RunCML.shutdown"} : OS.Process.status -> 'a) st
end;
