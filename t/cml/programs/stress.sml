(* Stress (docs/DESIGN-CML.md, section 4, item 15): a ring of 1000 threads
   passing a token 100 times round, and two threads printing 10000 lines
   each (the harness checks that no line is corrupted). *)

open CML

fun say s = print (s ^ "\n")

val threads = 1000
val laps = 100

fun ring () =
    let
      val chans = Vector.tabulate (threads, fn _ => channel ())
      fun node i =
          let
            val inCh = Vector.sub (chans, i)
            val outCh = Vector.sub (chans, (i + 1) mod threads)
            fun loop () = (send (outCh, recv inCh + 1); loop ())
          in
            loop ()
          end
      fun spawnNodes i = if i < threads then (ignore (spawn (fn () => node i)); spawnNodes (i + 1))
                         else ()
      val _ = spawnNodes 1
      val first = Vector.sub (chans, 1)
      val last = Vector.sub (chans, 0)
      fun lap (0, token) = token
        | lap (n, token) = (send (first, token); lap (n - 1, recv last + 1))
    in
      lap (laps, 0)
    end

fun printers () =
    let
      val done = channel ()
      (* A loop, not List.app over a list: ECL's threads have small stacks. *)
      fun lines (s, 0) = ()
        | lines (s, n) = (print s; lines (s, n - 1))
      fun printer s = spawn (fn () => (lines (s, 10000); send (done, ())))
    in
      printer "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n";
      printer "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\n";
      recv done; recv done
    end

val st = RunCML.doit (fn () =>
                         (say ("ring: " ^ Int.toString (ring ()));
                          printers ();
                          RunCML.shutdown OS.Process.success),
                      NONE)
val _ = say (if OS.Process.isSuccess st then "success" else "failure")
