(*
 * cl-sml: Standard ML Basis Library, structure Timer
 * (not part of HaMLet's basis).
 *
 * Note: the run time does not split CPU time into user, system and GC
 * time; all of it is user time.  The total timers count from the start of
 * the Lisp image.
 *)

structure Timer :> TIMER =
struct
  (* A timer is the time it was started at. *)
  type cpu_timer         = Time.time
  type real_timer        = Time.time

  val cpuTime            = use{b = "Timer.cpuTime"} : unit -> Time.time
  val realTime           = use{b = "Timer.realTime"} : unit -> Time.time

  fun startCPUTimer ()   = cpuTime ()
  fun checkCPUTimer t    = {usr = Time.-(cpuTime (), t), sys = Time.zeroTime}
  fun checkCPUTimes t    = {nongc = checkCPUTimer t,
                            gc = {usr = Time.zeroTime, sys = Time.zeroTime}}
  fun checkGCTime t      = Time.zeroTime
  fun totalCPUTimer ()   = Time.zeroTime

  fun startRealTimer ()  = realTime ()
  fun checkRealTimer t   = Time.-(realTime (), t)
  fun totalRealTimer ()  = Time.zeroTime
end;
