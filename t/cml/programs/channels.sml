(* Channels: rendezvous, polls, choose.  (docs/DESIGN-CML.md, section 4,
   items 1-3.) *)

open CML

fun say s = print (s ^ "\n")

fun main () =
    let
      (* 1. send returns only once a receiver has arrived, and the other
         way round. *)
      val ch = channel ()
      val receiving = ref false
      val t = spawn (fn () => (send (ch, 1);
                               say (if !receiving then "send after recv: ok"
                                    else "send returned early")))
      val _ = sync (timeOutEvt (Time.fromMilliseconds 50))
      val _ = receiving := true
      val v = recv ch
      val _ = sync (joinEvt t)
      val _ = say ("received " ^ Int.toString v)

      val sending = ref false
      val ch2 = channel ()
      val t = spawn (fn () => (recv ch2;
                               say (if !sending then "recv after send: ok"
                                    else "recv returned early")))
      val _ = sync (timeOutEvt (Time.fromMilliseconds 50))
      val _ = sending := true
      val _ = send (ch2, 2)
      val _ = sync (joinEvt t)

      (* 2. Polls succeed only with a partner waiting. *)
      val ch3 : int chan = channel ()
      val _ = say ("sendPoll alone: " ^ Bool.toString (sendPoll (ch3, 3)))
      val _ = say ("recvPoll alone: " ^ (case recvPoll ch3 of NONE => "NONE" | SOME _ => "SOME"))
      val got = ref 0
      val t = spawn (fn () => got := recv ch3)
      fun pollSend () = if sendPoll (ch3, 33) then () else (yield (); pollSend ())
      val _ = pollSend ()
      val _ = sync (joinEvt t)
      val _ = say ("sendPoll with a receiver: " ^ Int.toString (!got))
      val t = spawn (fn () => send (ch3, 44))
      fun pollRecv () = case recvPoll ch3 of SOME v => v | NONE => (yield (); pollRecv ())
      val _ = say ("recvPoll with a sender: " ^ Int.toString (pollRecv ()))
      val _ = sync (joinEvt t)

      (* 3. choose commits to exactly one; the other message stays. *)
      val a = channel ()
      val b = channel ()
      val ta = spawn (fn () => send (a, 10))
      val tb = spawn (fn () => send (b, 20))
      val first = select [recvEvt a, recvEvt b]
      val second = if first = 10 then recv b else recv a
      val _ = sync (joinEvt ta)
      val _ = sync (joinEvt tb)
      val _ = say ("choose: " ^ Int.toString (first + second))

      (* send and recv in one choice, against partners of both kinds. *)
      val c = channel ()
      val _ = spawn (fn () => send (c, 5))
      val r = select [wrap (recvEvt c, fn v => v), wrap (sendEvt (c, 6), fn () => 0)]
      val _ = if r = 5 then () else say "send/recv choice: wrong"
      val _ = say "send/recv choice: ok"
      val _ = say ("sameChannel: " ^ Bool.toString (sameChannel (a, a))
                   ^ " " ^ Bool.toString (sameChannel (a, b)))
    in
      RunCML.shutdown OS.Process.success
    end

val st = RunCML.doit (main, NONE)
val _ = say (if OS.Process.isSuccess st then "success" else "failure")

(* EXPECT
send after recv: ok
received 1
recv after send: ok
sendPoll alone: false
recvPoll alone: NONE
sendPoll with a receiver: 33
recvPoll with a sender: 44
choose: 30
send/recv choice: ok
sameChannel: true false
success
*)
