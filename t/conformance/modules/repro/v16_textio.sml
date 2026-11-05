fun p s = print (s ^ "\n")
(* The runner's TMPDIR (run.sh sets it to t/conformance/modules/tmp/). *)
val dir = case OS.Process.getEnv "TMPDIR" of
              SOME d => if String.isSuffix "/" d then d else d ^ "/"
            | NONE => "/tmp/"
val file = dir ^ "v16.txt"
val out = TextIO.openOut file
val _ = TextIO.output (out, "a\nlast")
val _ = TextIO.closeOut out
val ins = TextIO.openIn file
fun showLine NONE = "NONE" | showLine (SOME l) = "SOME [" ^ String.toString l ^ "]"
val _ = p (showLine (TextIO.inputLine ins))
val _ = p (showLine (TextIO.inputLine ins))
val _ = p (showLine (TextIO.inputLine ins))
val _ = TextIO.closeIn ins
val _ = p ((TextIO.closeIn (TextIO.openIn "/nonexistent/x"); "opened") handle IO.Io {name, function, ...} => "Io " ^ function)
