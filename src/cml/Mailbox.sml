(*
 * cl-sml: Concurrent ML, structure Mailbox (docs/DESIGN-CML.md).
 *)

structure Mailbox :> MAILBOX =
struct
  (* A run-time object (mailbox). *)
  type obj = int

  type 'a mbox = obj

  val mailbox     = use{b = "Mailbox.new"} : unit -> obj
  val sameMailbox = use{b = "CML.same"} : obj * obj -> bool
  fun send x      = (use{b = "Mailbox.send"} : obj * 'a -> unit) x
  fun recvEvt m   = (use{b = "Mailbox.recvEvt"} : obj -> 'a CML.event) m
  fun recv m      = CML.sync (recvEvt m)
  fun recvPoll m  = (use{b = "CML.poll"} : 'a CML.event * ('a -> 'a option) * 'a option
                                           -> 'a option) (recvEvt m, SOME, NONE)
end;
