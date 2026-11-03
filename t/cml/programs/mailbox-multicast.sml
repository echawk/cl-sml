(* Mailbox and Multicast.  (docs/DESIGN-CML.md, section 4, items 12-13.) *)

open CML

fun say s = print (s ^ "\n")
fun ints l = String.concatWith " " (map Int.toString l)

fun main () =
    let
      (* Mailbox: send never blocks; messages arrive in FIFO order. *)
      val mb = Mailbox.mailbox ()
      val _ = List.app (fn i => Mailbox.send (mb, i)) (List.tabulate (100, fn i => i))
      val got = List.tabulate (100, fn _ => Mailbox.recv mb)
      val _ = say ("mailbox in order: " ^ Bool.toString (got = List.tabulate (100, fn i => i)))
      val _ = say ("recvPoll empty: " ^ (case Mailbox.recvPoll mb of NONE => "NONE" | SOME _ => "SOME"))
      val _ = Mailbox.send (mb, 7)
      val _ = say ("recvPoll: " ^ (case Mailbox.recvPoll mb of NONE => "NONE" | SOME v => Int.toString v))
      (* A blocked receiver, and two senders' messages each in order. *)
      val out = channel ()
      val _ = spawn (fn () => send (out, Mailbox.recv mb))
      val _ = Mailbox.send (mb, 8)
      val _ = say ("blocked receiver: " ^ Int.toString (recv out))
      val done = channel ()
      fun sender base = spawn (fn () => (List.app (fn i => Mailbox.send (mb, base + i))
                                                  (List.tabulate (50, fn i => i));
                                         send (done, ())))
      val _ = (sender 1000; sender 2000; recv done; recv done)
      val all = List.tabulate (100, fn _ => Mailbox.recv mb)
      val fromA = List.filter (fn v => v < 2000) all
      val fromB = List.filter (fn v => v >= 2000) all
      val _ = say ("per-sender order: "
                   ^ Bool.toString (fromA = List.tabulate (50, fn i => 1000 + i)
                                    andalso fromB = List.tabulate (50, fn i => 2000 + i)))
      val _ = say ("sameMailbox: " ^ Bool.toString (Mailbox.sameMailbox (mb, mb)))

      (* Multicast: every port gets every message sent after it was made,
         in order; a copy starts where its original is. *)
      val mc = Multicast.mChannel ()
      val p1 = Multicast.port mc
      val p2 = Multicast.port mc
      val _ = List.app (fn i => Multicast.multicast (mc, i)) [1, 2, 3]
      val p3 = Multicast.port mc
      val _ = Multicast.multicast (mc, 4)
      val first = Multicast.recv p1
      val p1copy = Multicast.copy p1
      val rest1 = List.tabulate (3, fn _ => Multicast.recv p1)
      val _ = say ("port 1: " ^ ints (first :: rest1))
      val _ = say ("port 2: " ^ ints (List.tabulate (4, fn _ => Multicast.recv p2)))
      val _ = say ("port made later: " ^ Int.toString (Multicast.recv p3))
      val _ = say ("copy: " ^ ints (List.tabulate (3, fn _ => Multicast.recv p1copy)))
      (* Readers in other threads. *)
      val results = channel ()
      val ports = List.tabulate (3, fn _ => Multicast.port mc)
      val _ = List.app (fn p => ignore (spawn (fn () =>
                 send (results, ints (List.tabulate (3, fn _ => sync (Multicast.recvEvt p)))))))
                       ports
      val _ = List.app (fn i => Multicast.multicast (mc, i)) [10, 20, 30]
      val _ = say ("threads: " ^ String.concatWith ", " (List.tabulate (3, fn _ => recv results)))
    in
      RunCML.shutdown OS.Process.success
    end

val st = RunCML.doit (main, NONE)
val _ = say (if OS.Process.isSuccess st then "success" else "failure")

(* EXPECT
mailbox in order: true
recvPoll empty: NONE
recvPoll: 7
blocked receiver: 8
per-sender order: true
sameMailbox: true
port 1: 1 2 3 4
port 2: 1 2 3 4
port made later: 4
copy: 2 3 4
threads: 10 20 30, 10 20 30, 10 20 30
success
*)
