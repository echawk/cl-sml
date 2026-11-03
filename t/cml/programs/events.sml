(* Event combinators: wrap, guard, withNack, wrapHandler, always, never.
   (docs/DESIGN-CML.md, section 4, items 4-8.) *)

open CML

fun say s = print (s ^ "\n")

fun main () =
    let
      (* 4. Wraps run after the commit, in the synchronising thread, and
         compose in order. *)
      val me = getTid ()
      val e = wrap (wrap (alwaysEvt 1, fn x => (if sameTid (getTid (), me) then x * 10 else 0)),
                    fn x => x + 2)
      val _ = say ("nested wraps: " ^ Int.toString (sync e))
      val ch = channel ()
      val _ = spawn (fn () => send (ch, 7))
      val _ = say ("wrap of recv: " ^ sync (wrap (recvEvt ch, Int.toString)))

      (* 5. A guard runs once per synchronisation, before polling. *)
      val count = ref 0
      val g = guard (fn () => (count := !count + 1; alwaysEvt (!count)))
      val a = sync g
      val b = sync g
      val _ = say ("guard: " ^ Int.toString a ^ " " ^ Int.toString b ^ " " ^ Int.toString (!count))
      val g2 = guard (fn () => let val c = channel ()
                               in ignore (spawn (fn () => send (c, "from guard"))); recvEvt c end)
      val _ = say (sync g2)

      (* 6. withNack: the nack is enabled iff another branch is chosen. *)
      val nacked = channel ()
      val r = select [withNack (fn nack => (ignore (spawn (fn () => (sync nack; send (nacked, "nack"))));
                                            never)),
                      alwaysEvt "other branch"]
      val _ = say (r ^ ", then " ^ recv nacked)
      val flag = ref false
      val watcher = ref (getTid ())
      val r = select [withNack (fn nack => (watcher := spawn (fn () => (sync nack; flag := true));
                                            alwaysEvt "own branch")),
                      never]
      val w = select [wrap (joinEvt (!watcher), fn () => "nack fired (wrong)"),
                      wrap (timeOutEvt (Time.fromMilliseconds 200), fn () => "no nack")]
      val _ = say (r ^ ": " ^ w)
      (* Of two enabled branches, the losing one's nack fires. *)
      val losers = channel ()
      fun branch name = withNack (fn nack => (ignore (spawn (fn () => (sync nack; send (losers, name))));
                                              alwaysEvt name))
      val winner = select [branch "left", branch "right"]
      val loser = recv losers
      val _ = say ("nack of the loser: " ^ Bool.toString (winner <> loser))
      (* Nested withNacks, and a nack chosen by a guard around a choice. *)
      val nested = channel ()
      val r = select [withNack (fn outer =>
                        (ignore (spawn (fn () => (sync outer; send (nested, "outer nack"))));
                         choose [withNack (fn inner =>
                                   (ignore (spawn (fn () => (sync inner; send (nested, "inner nack"))));
                                    never)),
                                 never])),
                      alwaysEvt "outside"]
      val n1 = recv nested
      val n2 = recv nested
      val _ = say (r ^ ": " ^ (if n1 < n2 then n1 ^ ", " ^ n2 else n2 ^ ", " ^ n1))

      (* 7. wrapHandler handles exceptions of the wrap functions only. *)
      val h = sync (wrapHandler (wrap (alwaysEvt 1, fn _ => raise Fail "in wrap"),
                                 fn Fail m => String.size m | e => raise e))
      val _ = say ("wrapHandler: " ^ Int.toString h)
      val outer = (sync (wrap (wrapHandler (alwaysEvt 1, fn _ => 0), fn _ => raise Fail "outer"));
                   "not raised")
                  handle Fail m => "raised " ^ m
      val _ = say ("outside the handler: " ^ outer)
      val g = (sync (guard (fn () => raise Fail "guard")); "no") handle Fail m => m
      val _ = say ("exception in a guard: " ^ g)

      (* 8. never is never enabled, alwaysEvt always. *)
      val r = select [wrap (never, fn () => "never"),
                      wrap (timeOutEvt (Time.fromMilliseconds 10), fn () => "time-out")]
      val _ = say ("never: " ^ r)
      val _ = say ("alwaysEvt: " ^ sync (alwaysEvt "always"))
      val _ = say ("choose of one: " ^ Int.toString (select [alwaysEvt 3]))
    in
      RunCML.shutdown OS.Process.success
    end

val st = RunCML.doit (main, NONE)
val _ = say (if OS.Process.isSuccess st then "success" else "failure")

(* select [] blocks forever: with no other thread, a deadlock. *)
val st = RunCML.doit (fn () => select [], NONE)
val _ = say ("select []: " ^ (if OS.Process.isSuccess st then "success" else "failure"))

(* EXPECT
nested wraps: 12
wrap of recv: 7
guard: 1 2 2
from guard
other branch, then nack
own branch: no nack
nack of the loser: true
outside: inner nack, outer nack
wrapHandler: 7
outside the handler: raised outer
exception in a guard: guard
never: time-out
alwaysEvt: always
choose of one: 3
success
select []: failure
*)
