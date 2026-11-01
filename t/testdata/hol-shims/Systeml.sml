(* Stands in for the Systeml structure HOL's configuration generates. *)
structure Systeml = struct
  val release = "cl-sml"
  val version = 0
  val HOLDIR = case OS.Process.getEnv "HOLDIR" of SOME d => d | NONE => "HOL"
end
