(*
 * cl-sml: Concurrent ML, structure Multicast (docs/DESIGN-CML.md), in SML
 * over the core as upstream's, after Reppy's book: the messages form a
 * stream of I-variables, each holding a message and the next one; a
 * server thread appends to it, and a port is a position in it.
 *)

structure Multicast :> MULTICAST =
struct
  structure SV = SyncVar

  type 'a event = 'a CML.event

  datatype 'a state = State of 'a * 'a state SV.ivar

  datatype 'a port = Port of 'a state SV.ivar ref

  datatype 'a request = Message of 'a | NewPort

  datatype 'a mchan = MChan of 'a request CML.chan * 'a port CML.chan

  fun mChannel () =
      let
        val requests = CML.channel ()
        val replies = CML.channel ()
        fun server tail =
            case CML.recv requests of
              NewPort => (CML.send (replies, Port (ref tail)); server tail)
            | Message m =>
                let val next = SV.iVar ()
                in SV.iPut (tail, State (m, next)); server next end
      in
        ignore (CML.spawn (fn () => server (SV.iVar ())));
        MChan (requests, replies)
      end

  fun port (MChan (requests, replies)) =
      (CML.send (requests, NewPort); CML.recv replies)

  fun copy (Port position) = Port (ref (!position))

  fun recvEvt (Port position) =
      CML.guard (fn () =>
        CML.wrap (SV.iGetEvt (!position),
                  fn State (m, next) => (position := next; m)))

  fun recv p = CML.sync (recvEvt p)

  fun multicast (MChan (requests, _), m) = CML.send (requests, Message m)
end;
