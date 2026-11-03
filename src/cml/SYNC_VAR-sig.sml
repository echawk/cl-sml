(*
 * cl-sml: Concurrent ML, signature SYNC_VAR (docs/DESIGN-CML.md):
 * I-variables (written once) and M-variables.
 *)

signature SYNC_VAR =
sig
  exception Put

  type 'a ivar
  val iVar     : unit -> 'a ivar
  val iPut     : 'a ivar * 'a -> unit
  val iGet     : 'a ivar -> 'a
  val iGetEvt  : 'a ivar -> 'a CML.event
  val iGetPoll : 'a ivar -> 'a option
  val sameIVar : 'a ivar * 'a ivar -> bool

  type 'a mvar
  val mVar      : unit -> 'a mvar
  val mVarInit  : 'a -> 'a mvar
  val mPut      : 'a mvar * 'a -> unit
  val mTake     : 'a mvar -> 'a
  val mTakeEvt  : 'a mvar -> 'a CML.event
  val mTakePoll : 'a mvar -> 'a option
  val mGet      : 'a mvar -> 'a
  val mGetEvt   : 'a mvar -> 'a CML.event
  val mGetPoll  : 'a mvar -> 'a option
  val mSwap     : 'a mvar * 'a -> 'a
  val mSwapEvt  : 'a mvar * 'a -> 'a CML.event
  val sameMVar  : 'a mvar * 'a mvar -> bool
end;
