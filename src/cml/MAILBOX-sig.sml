(*
 * cl-sml: Concurrent ML, signature MAILBOX (docs/DESIGN-CML.md):
 * asynchronous channels with an unbounded buffer.
 *)

signature MAILBOX =
sig
  type 'a mbox
  val mailbox     : unit -> 'a mbox
  val sameMailbox : 'a mbox * 'a mbox -> bool
  val send        : 'a mbox * 'a -> unit
  val recv        : 'a mbox -> 'a
  val recvEvt     : 'a mbox -> 'a CML.event
  val recvPoll    : 'a mbox -> 'a option
end;
