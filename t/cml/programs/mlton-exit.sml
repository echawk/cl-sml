(* Ported from MLton's lib/cml/tests/exit.sml (https://github.com/MLton/mlton,
   commit 5fe943391e539a502699327a6266b6e8e97939e1): the first thread
   exits at once, so no thread is left and doit returns failure.  Also
   the uncaught exceptions of MLton's print-style tests: one thread's
   exception does not stop the others. *)

structure Main =
struct
   open CML

   fun doit _ =
      CML.exit ()
end

val st = RunCML.doit (fn () => Main.doit (), NONE)
val _ = print (if OS.Process.isSuccess st then "success\n" else "failure\n")

val st = RunCML.doit (fn () =>
                         let val ch = CML.channel ()
                         in
                           ignore (CML.spawn (fn () => raise Fail "uncaught"));
                           ignore (CML.spawn (fn () => CML.send (ch, "still running")));
                           print (CML.recv ch ^ "\n");
                           RunCML.shutdown OS.Process.success
                         end,
                      NONE)
val _ = print (if OS.Process.isSuccess st then "success\n" else "failure\n")

(* EXPECT
failure
still running
success
*)
