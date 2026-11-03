(* Threads: joinEvt, uncaught exceptions, exit, thread ids and properties.
   (docs/DESIGN-CML.md, section 4, item 10.) *)

open CML

fun say s = print (s ^ "\n")

fun main () =
    let
      val ch = channel ()
      val t = spawn (fn () => send (ch, 1))
      val _ = recv ch
      val _ = sync (joinEvt t)
      val _ = say "joinEvt after a normal exit"
      val _ = sync (joinEvt t)
      val _ = say "joinEvt again: enabled"

      (* An uncaught exception ends only its thread. *)
      val t = spawn (fn () => raise Fail "boom")
      val _ = sync (joinEvt t)
      val _ = say "joinEvt after an uncaught exception"
      val t = spawn (fn () => (exit (); say "after exit (wrong)"))
      val _ = sync (joinEvt t)
      val _ = say "joinEvt after exit"
      val sc = channel ()
      val t = spawnc (fn s => send (sc, s)) "spawnc"
      val _ = say (recv sc)
      val _ = sync (joinEvt t)

      (* Thread ids. *)
      val me = getTid ()
      val tc = channel ()
      val other = spawn (fn () => send (tc, getTid ()))
      val reported = recv tc
      val _ = say ("sameTid: " ^ Bool.toString (sameTid (other, reported))
                   ^ " " ^ Bool.toString (sameTid (me, other)))
      val _ = say ("compareTid: " ^ (case compareTid (me, other) of
                                       LESS => "LESS" | EQUAL => "EQUAL" | GREATER => "GREATER")
                   ^ " " ^ (case compareTid (me, me) of EQUAL => "EQUAL" | _ => "?"))
      val _ = say ("hashTid differs: " ^ Bool.toString (hashTid me <> hashTid other))
      val _ = say ("tidToString: " ^ Bool.toString (String.isPrefix "[" (tidToString me)))

      (* Thread properties are per thread. *)
      val counter = ref 0
      val {getFn, setFn, peekFn, clrFn} = newThreadProp (fn () => (counter := !counter + 1; !counter))
      val _ = say ("peek before: " ^ (case peekFn () of NONE => "NONE" | SOME v => Int.toString v))
      val v1 = getFn ()
      val _ = setFn 42
      val _ = spawn (fn () => send (ch, getFn ()))
      val vOther = recv ch
      val _ = say ("prop: " ^ Int.toString v1 ^ " " ^ Int.toString (getFn ()) ^ " "
                   ^ Int.toString vOther)
      val _ = clrFn ()
      val _ = say ("peek after clear: " ^ (case peekFn () of NONE => "NONE" | SOME v => Int.toString v))
      val {getFn = getFlag, setFn = setFlag} = newThreadFlag ()
      val _ = setFlag true
      val f = channel ()
      val _ = spawn (fn () => send (f, getFlag ()))
      val _ = say ("flag: " ^ Bool.toString (getFlag ()) ^ " " ^ Bool.toString (recv f))

      (* The other threads keep running while one computes. *)
      val results = channel ()
      fun busy n = if n = 0 then 0 else 1 + busy (n - 1)
      val _ = List.app (fn i => ignore (spawn (fn () => send (results, busy (i * 1000)))))
                       [1, 2, 3, 4]
      val total = List.foldl (fn (_, acc) => acc + recv results) 0 [1, 2, 3, 4]
      val _ = say ("computing threads: " ^ Int.toString total)
      val _ = yield ()
      val _ = say "yield"
    in
      RunCML.shutdown OS.Process.success
    end

val st = RunCML.doit (main, NONE)
val _ = say (if OS.Process.isSuccess st then "success" else "failure")

(* EXPECT
joinEvt after a normal exit
joinEvt again: enabled
joinEvt after an uncaught exception
joinEvt after exit
spawnc
sameTid: true false
compareTid: LESS EQUAL
hashTid differs: true
tidToString: true
peek before: NONE
prop: 1 42 2
peek after clear: NONE
flag: true false
computing threads: 10000
yield
success
*)
