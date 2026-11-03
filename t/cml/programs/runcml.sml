(* RunCML: doit's status, deadlock detection, isRunning.  (docs/DESIGN-CML.md,
   section 4, item 14.) *)

open CML

fun say s = print (s ^ "\n")
fun status st = if OS.Process.isSuccess st then "success" else "failure"

val _ = say ("isRunning outside doit: " ^ Bool.toString (RunCML.isRunning ()))

val st = RunCML.doit (fn () => (say ("isRunning inside: " ^ Bool.toString (RunCML.isRunning ()));
                                RunCML.shutdown OS.Process.success),
                      SOME (Time.fromMilliseconds 10))
val _ = say ("shutdown success: " ^ status st)

(* shutdown from another thread, while others are blocked. *)
val st = RunCML.doit (fn () =>
                         let val ch = channel ()
                         in
                           List.app (fn _ => ignore (spawn (fn () => recv ch))) [1, 2, 3];
                           ignore (spawn (fn () => RunCML.shutdown OS.Process.failure));
                           recv ch
                         end,
                      NONE)
val _ = say ("shutdown failure from another thread: " ^ status st)

(* Deadlock: two threads each wait for the other. *)
val st = RunCML.doit (fn () =>
                         let val a = channel () and b = channel ()
                         in
                           ignore (spawn (fn () => (recv a; send (b, ()))));
                           recv b; send (a, ())
                         end,
                      NONE)
val _ = say ("deadlock: " ^ status st)

(* Every thread ends without shutdown. *)
val st = RunCML.doit (fn () => ignore (spawn (fn () => ())), NONE)
val _ = say ("all threads ended: " ^ status st)

(* A deadlock while one thread waits for a time-out is detected once it
   has timed out. *)
val st = RunCML.doit (fn () =>
                         (ignore (spawn (fn () => sync (timeOutEvt (Time.fromMilliseconds 30))));
                          sync never),
                      NONE)
val _ = say ("deadlock after a time-out: " ^ status st)

(* A thread computing without synchronising does not keep doit from
   returning after shutdown. *)
fun spin n = if n < 0 then n else spin (n + 1)
val st = RunCML.doit (fn () => (ignore (spawn (fn () => ignore (spin 0)));
                                RunCML.shutdown OS.Process.success),
                      NONE)
val _ = say ("busy thread stopped: " ^ status st)

(* CML operations outside doit. *)
val _ = say ((ignore (CML.sync (CML.alwaysEvt 1)); "sync outside doit: no exception")
             handle Fail _ => "sync outside doit: Fail")
val _ = say ((RunCML.shutdown OS.Process.success; "shutdown outside doit: no exception")
             handle Fail _ => "shutdown outside doit: Fail")
val _ = say ("isRunning after: " ^ Bool.toString (RunCML.isRunning ()))
val st = RunCML.doit (fn () => RunCML.shutdown OS.Process.success, NONE)
val _ = say ("doit again: " ^ status st)

(* EXPECT
isRunning outside doit: false
isRunning inside: true
shutdown success: success
shutdown failure from another thread: failure
deadlock: failure
all threads ended: failure
deadlock after a time-out: failure
busy thread stopped: success
sync outside doit: Fail
shutdown outside doit: Fail
isRunning after: false
doit again: success
*)
