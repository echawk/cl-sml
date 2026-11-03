(*
 * cl-sml: Concurrent ML, signature RUN_CML (docs/DESIGN-CML.md).
 *)

signature RUN_CML =
sig
  val isRunning : unit -> bool
  val doit      : (unit -> unit) * Time.time option -> OS.Process.status
  val shutdown  : OS.Process.status -> 'a
end;
