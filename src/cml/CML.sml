(*
 * cl-sml: Concurrent ML, structure CML (docs/DESIGN-CML.md).  The values
 * are cl-sml/cml's run-time primitives (src/cml/primitives.lisp); the
 * polymorphic ones are eta-expanded so that they stay polymorphic.
 *)

structure CML :> CML =
struct
  (* A run-time object (thread, channel, event). *)
  type obj = int

  type thread_id = obj
  type 'a chan   = obj
  type 'a event  = obj

  val version = {system = "cl-sml CML", version_id = [1, 0], date = "2026-11-03"}
  val banner  = "cl-sml CML, version 1.0"

  val same = use{b = "CML.same"} : obj * obj -> bool

  (* Threads *)
  val getTid      = use{b = "CML.getTid"} : unit -> thread_id
  val sameTid     = same
  val tidId       = use{b = "CML.tidId"} : thread_id -> int
  fun compareTid (t1, t2) = Int.compare (tidId t1, tidId t2)
  fun hashTid t   = Word.fromInt (tidId t)
  val tidToString = use{b = "CML.tidToString"} : thread_id -> string
  val spawn       = use{b = "CML.spawn"} : (unit -> unit) -> thread_id
  fun spawnc f x  = spawn (fn () => f x)
  fun exit ()     = (use{b = "CML.exit"} : unit -> 'a) ()
  val joinEvt     = use{b = "CML.joinEvt"} : thread_id -> obj
  val yield       = use{b = "CML.yield"} : unit -> unit

  fun newThreadProp (init : unit -> 'b) =
      let
        val key = (use{b = "CML.newPropKey"} : unit -> int) ()
      in
        {clrFn = fn () => (use{b = "CML.propClr"} : int -> unit) key,
         getFn = fn () => (use{b = "CML.propGet"} : int * (unit -> 'b) -> 'b) (key, init),
         peekFn = fn () => (use{b = "CML.propPeek"}
                            : int * ('b -> 'b option) * 'b option -> 'b option)
                             (key, SOME, NONE),
         setFn = fn v => (use{b = "CML.propSet"} : int * 'b -> unit) (key, v)}
      end

  fun newThreadFlag () =
      let
        val {getFn, setFn, ...} = newThreadProp (fn () => false)
      in
        {getFn = getFn, setFn = setFn}
      end

  (* Events *)
  val never          = (use{b = "CML.never"} : unit -> obj) ()
  fun alwaysEvt x    = (use{b = "CML.alwaysEvt"} : 'a -> obj) x
  fun wrap x         = (use{b = "CML.wrap"} : obj * ('a -> 'b) -> obj) x
  fun wrapHandler x  = (use{b = "CML.wrapHandler"} : obj * (exn -> 'a) -> obj) x
  val guard          = use{b = "CML.guard"} : (unit -> obj) -> obj
  val withNack       = use{b = "CML.withNack"} : (obj -> obj) -> obj
  val choose         = use{b = "CML.choose"} : obj list -> obj
  fun sync e         = (use{b = "CML.sync"} : obj -> 'a) e
  fun select es      = sync (choose es)
  fun poll e         = (use{b = "CML.poll"} : obj * ('a -> 'a option) * 'a option
                                              -> 'a option) (e, SOME, NONE)

  (* Channels *)
  val channel        = use{b = "CML.channel"} : unit -> obj
  val sameChannel    = same
  fun sendEvt x      = (use{b = "CML.sendEvt"} : obj * 'a -> obj) x
  val recvEvt        = use{b = "CML.recvEvt"} : obj -> obj
  fun send x         = sync (sendEvt x) : unit
  fun recv ch        = sync (recvEvt ch)
  fun sendPoll x     = Option.isSome (poll (sendEvt x) : unit option)
  fun recvPoll ch    = poll (recvEvt ch)

  (* Time-outs *)
  val timeOutEvt     = use{b = "CML.timeOutEvt"} : Time.time -> obj
  val atTimeEvt      = use{b = "CML.atTimeEvt"} : Time.time -> obj
end;
