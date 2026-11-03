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
      val _ = List.app (fn i => ignore (spawn (fn () => node i))) (List.tabulate (threads - 1, fn i => i + 1))
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
      fun printer s = spawn (fn () => (List.app (fn _ => print s) (List.tabulate (10000, fn i => i));
                                       send (done, ())))
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
