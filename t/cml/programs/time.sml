(* Time-outs.  (docs/DESIGN-CML.md, section 4, item 9.)  The bounds are
   lower bounds only, so a loaded machine cannot fail them. *)

open CML

fun say s = print (s ^ "\n")

fun main () =
    let
      val start = Time.now ()
      val _ = sync (timeOutEvt (Time.fromMilliseconds 100))
      val elapsed = Time.toMilliseconds (Time.- (Time.now (), start))
      val _ = say ("timeOutEvt waited long enough: " ^ Bool.toString (elapsed >= 99))

      val _ = sync (atTimeEvt (Time.- (Time.now (), Time.fromSeconds 10)))
      val _ = say "atTimeEvt in the past: fired"
      val start = Time.now ()
      val _ = sync (atTimeEvt (Time.+ (start, Time.fromMilliseconds 50)))
      val elapsed = Time.toMilliseconds (Time.- (Time.now (), start))
      val _ = say ("atTimeEvt waited long enough: " ^ Bool.toString (elapsed >= 49))
      val _ = sync (timeOutEvt Time.zeroTime)
      val _ = say "timeOutEvt 0: fired"

      (* A message that is ready wins over a time-out. *)
      val ch = channel ()
      val _ = spawn (fn () => send (ch, "message"))
      val r = select [wrap (timeOutEvt (Time.fromSeconds 30), fn () => "time-out"), recvEvt ch]
      val _ = say ("message against a time-out: " ^ r)

      (* The earlier of two time-outs fires. *)
      val r = select [wrap (timeOutEvt (Time.fromSeconds 30), fn () => "late"),
                      wrap (timeOutEvt (Time.fromMilliseconds 20), fn () => "early")]
      val _ = say ("two time-outs: " ^ r)

      (* A time-out starts when it is synchronised on, not when made. *)
      val e = timeOutEvt (Time.fromMilliseconds 100)
      val _ = sync (timeOutEvt (Time.fromMilliseconds 120))
      val start = Time.now ()
      val _ = sync e
      val elapsed = Time.toMilliseconds (Time.- (Time.now (), start))
      val _ = say ("time-out starts at sync: " ^ Bool.toString (elapsed >= 99))

      (* Threads that only wait for time-outs are not deadlocked. *)
      val done = channel ()
      val _ = spawn (fn () => (sync (timeOutEvt (Time.fromMilliseconds 30)); send (done, ())))
      val _ = recv done
      val _ = say "a timed wait is not a deadlock"
    in
      RunCML.shutdown OS.Process.success
    end

val st = RunCML.doit (main, NONE)
val _ = say (if OS.Process.isSuccess st then "success" else "failure")

(* EXPECT
timeOutEvt waited long enough: true
atTimeEvt in the past: fired
atTimeEvt waited long enough: true
timeOutEvt 0: fired
message against a time-out: message
two time-outs: early
time-out starts at sync: true
a timed wait is not a deadlock
success
*)
