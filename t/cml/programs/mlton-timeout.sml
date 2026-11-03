(* After MLton's lib/cml/tests/timeout.sml (https://github.com/MLton/mlton,
   commit 5fe943391e539a502699327a6266b6e8e97939e1): threads that sleep
   for different times with timeOutEvt.  Upstream sleeps for seconds and
   prints thread ids; here the delays are milliseconds, and the threads
   report in the order they wake, which the delays fix (200 ms apart). *)

structure Main =
struct
   open CML

   fun doit' n =
      RunCML.doit
      (fn () =>
       let
          val woken = channel ()
          fun make m =
             if m <= 0 then ()
             else (ignore (spawn (fn () =>
                                  (sync (timeOutEvt (Time.fromMilliseconds (Int.toLarge (200 * m))));
                                   send (woken, m))));
                   make (m - 1))
          val () = make n
          val order = List.tabulate (n, fn _ => recv woken)
       in
          print ("woke: " ^ String.concatWith " " (map Int.toString order) ^ "\n");
          RunCML.shutdown OS.Process.success
       end,
       SOME (Time.fromMilliseconds 10))

   fun doit n = doit' n
end

val _ = print (if OS.Process.isSuccess (Main.doit 5) then "success\n" else "failure\n")

(* EXPECT
woke: 1 2 3 4 5
success
*)
