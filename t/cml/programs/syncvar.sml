(* SyncVar: I-variable and M-variable semantics.  (docs/DESIGN-CML.md,
   section 4, item 11.) *)

open CML
structure SV = SyncVar

fun say s = print (s ^ "\n")
fun show NONE = "NONE"
  | show (SOME v) = "SOME " ^ Int.toString v

fun main () =
    let
      (* I-variables: written once, read by everyone. *)
      val iv = SV.iVar ()
      val readers = List.tabulate (3, fn _ => let val c = channel ()
                                              in ignore (spawn (fn () => send (c, SV.iGet iv))); c end)
      val _ = say ("iGetPoll empty: " ^ show (SV.iGetPoll iv))
      val _ = SV.iPut (iv, 5)
      val _ = say ("readers: " ^ String.concatWith " " (map (Int.toString o recv) readers))
      val _ = say ("iGet again: " ^ Int.toString (SV.iGet iv) ^ ", poll: " ^ show (SV.iGetPoll iv))
      val _ = say ((SV.iPut (iv, 6); "second iPut: no exception")
                   handle SV.Put => "second iPut: Put")
      val _ = say ("sameIVar: " ^ Bool.toString (SV.sameIVar (iv, iv))
                   ^ " " ^ Bool.toString (SV.sameIVar (iv, SV.iVar ())))

      (* M-variables: take empties, put to a full one raises Put. *)
      val mv = SV.mVarInit 1
      val _ = say ((SV.mPut (mv, 2); "mPut to full: no exception") handle SV.Put => "mPut to full: Put")
      val _ = say ("mGet: " ^ Int.toString (SV.mGet mv) ^ ", mTake: " ^ Int.toString (SV.mTake mv))
      val _ = say ("mTakePoll empty: " ^ show (SV.mTakePoll mv) ^ ", mGetPoll: " ^ show (SV.mGetPoll mv))
      val taker = channel ()
      val _ = spawn (fn () => send (taker, SV.mTake mv))
      val _ = SV.mPut (mv, 3)
      val _ = say ("blocked mTake gets: " ^ Int.toString (recv taker))
      val _ = say ("then empty: " ^ show (SV.mTakePoll mv))
      val _ = SV.mPut (mv, 10)
      val old = SV.mSwap (mv, 20)
      val _ = say ("mSwap: " ^ Int.toString old ^ " -> " ^ Int.toString (SV.mGet mv))
      (* A blocked mSwap, then a blocked reader of the new value. *)
      val ev = SV.mVar ()
      val swapped = channel ()
      val _ = spawn (fn () => send (swapped, SV.mSwap (ev, 100)))
      val _ = sync (timeOutEvt (Time.fromMilliseconds 20))
      val _ = SV.mPut (ev, 99)
      val _ = say ("blocked mSwap: " ^ Int.toString (recv swapped) ^ " -> " ^ Int.toString (SV.mGet ev))

      (* mTake and mPut as a lock: no update is lost. *)
      val counter = SV.mVarInit 0
      val done = channel ()
      fun work 0 = send (done, ())
        | work n = (SV.mPut (counter, SV.mTake counter + 1); work (n - 1))
      val _ = List.app (fn _ => ignore (spawn (fn () => work 200))) [1, 2, 3, 4, 5]
      val _ = List.app (fn _ => recv done) [1, 2, 3, 4, 5]
      val _ = say ("counter: " ^ Int.toString (SV.mGet counter))
      val _ = say ("sameMVar: " ^ Bool.toString (SV.sameMVar (mv, mv)))
      val r = select [wrap (SV.mTakeEvt counter, fn v => "took " ^ Int.toString v),
                      wrap (timeOutEvt (Time.fromSeconds 30), fn () => "time-out")]
      val _ = say ("mTakeEvt in a choice: " ^ r)
    in
      RunCML.shutdown OS.Process.success
    end

val st = RunCML.doit (main, NONE)
val _ = say (if OS.Process.isSuccess st then "success" else "failure")

(* EXPECT
iGetPoll empty: NONE
readers: 5 5 5
iGet again: 5, poll: SOME 5
second iPut: Put
sameIVar: true false
mPut to full: Put
mGet: 1, mTake: 1
mTakePoll empty: NONE, mGetPoll: NONE
blocked mTake gets: 3
then empty: NONE
mSwap: 10 -> 20
blocked mSwap: 99 -> 100
counter: 1000
sameMVar: true
mTakeEvt in a choice: took 1000
success
*)
