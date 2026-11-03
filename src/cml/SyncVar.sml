(*
 * cl-sml: Concurrent ML, structure SyncVar (docs/DESIGN-CML.md).  Both
 * kinds of variable are the run time's synchronous variables, whose
 * events are base events (src/cml/core.lisp).
 *)

structure SyncVar :> SYNC_VAR =
struct
  (* A run-time object (synchronous variable). *)
  type obj = int

  exception Put

  type 'a ivar = obj
  type 'a mvar = obj

  val newVar    = use{b = "SyncVar.new"} : unit -> obj
  val same      = use{b = "CML.same"} : obj * obj -> bool
  fun put x     = if (use{b = "SyncVar.put"} : obj * 'a -> bool) x then () else raise Put
  fun readEvt v = (use{b = "SyncVar.readEvt"} : obj -> 'a CML.event) v
  fun takeEvt v = (use{b = "SyncVar.takeEvt"} : obj -> 'a CML.event) v
  fun swapEvt x = (use{b = "SyncVar.swapEvt"} : obj * 'a -> 'a CML.event) x
  fun poll e    = (use{b = "CML.poll"} : 'a CML.event * ('a -> 'a option) * 'a option
                                         -> 'a option) (e, SOME, NONE)

  val iVar       = newVar
  val iPut       = put
  val iGetEvt    = readEvt
  fun iGet v     = CML.sync (readEvt v)
  fun iGetPoll v = poll (readEvt v)
  val sameIVar   = same

  val mVar        = newVar
  fun mVarInit x  = let val v = newVar () in put (v, x); v end
  val mPut        = put
  val mTakeEvt    = takeEvt
  fun mTake v     = CML.sync (takeEvt v)
  fun mTakePoll v = poll (takeEvt v)
  val mGetEvt     = readEvt
  fun mGet v      = CML.sync (readEvt v)
  fun mGetPoll v  = poll (readEvt v)
  val mSwapEvt    = swapEvt
  fun mSwap x     = CML.sync (swapEvt x)
  val sameMVar    = same
end;
