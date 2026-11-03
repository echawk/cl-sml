(*
 * cl-sml: Concurrent ML, signature CML (docs/DESIGN-CML.md): the core
 * that SML/NJ's and MLton's CML share (their signatures VERSION, THREAD,
 * CHANNEL, EVENT and TIME_OUT, here in one).
 *)

signature CML =
sig
  val version : {system : string, version_id : int list, date : string}
  val banner  : string

  type thread_id
  type 'a chan
  type 'a event

  (* Threads *)
  val getTid        : unit -> thread_id
  val sameTid       : thread_id * thread_id -> bool
  val compareTid    : thread_id * thread_id -> order
  val hashTid       : thread_id -> word
  val tidToString   : thread_id -> string
  val spawnc        : ('a -> unit) -> 'a -> thread_id
  val spawn         : (unit -> unit) -> thread_id
  val exit          : unit -> 'a
  val joinEvt       : thread_id -> unit event
  val yield         : unit -> unit
  val newThreadProp : (unit -> 'b) -> {clrFn : unit -> unit,
                                       getFn : unit -> 'b,
                                       peekFn : unit -> 'b option,
                                       setFn : 'b -> unit}
  val newThreadFlag : unit -> {getFn : unit -> bool, setFn : bool -> unit}

  (* Channels *)
  val channel     : unit -> 'a chan
  val sameChannel : 'a chan * 'a chan -> bool
  val send        : 'a chan * 'a -> unit
  val recv        : 'a chan -> 'a
  val sendEvt     : 'a chan * 'a -> unit event
  val recvEvt     : 'a chan -> 'a event
  val sendPoll    : 'a chan * 'a -> bool
  val recvPoll    : 'a chan -> 'a option

  (* Events *)
  val never       : 'a event
  val alwaysEvt   : 'a -> 'a event
  val wrap        : 'a event * ('a -> 'b) -> 'b event
  val wrapHandler : 'a event * (exn -> 'a) -> 'a event
  val guard       : (unit -> 'a event) -> 'a event
  val withNack    : (unit event -> 'a event) -> 'a event
  val choose      : 'a event list -> 'a event
  val sync        : 'a event -> 'a
  val select      : 'a event list -> 'a

  (* Time-outs *)
  val timeOutEvt : Time.time -> unit event
  val atTimeEvt  : Time.time -> unit event
end;
