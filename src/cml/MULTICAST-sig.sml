(*
 * cl-sml: Concurrent ML, signature MULTICAST (docs/DESIGN-CML.md):
 * asynchronous multicast channels.
 *)

signature MULTICAST =
sig
  type 'a mchan
  type 'a port
  type 'a event = 'a CML.event
  val mChannel  : unit -> 'a mchan
  val port      : 'a mchan -> 'a port
  val copy      : 'a port -> 'a port
  val recv      : 'a port -> 'a
  val recvEvt   : 'a port -> 'a event
  val multicast : 'a mchan * 'a -> unit
end;
