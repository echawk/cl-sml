fun p s = print (s ^ "\n")
(* The runner's TMPDIR (run.sh sets it to t/conformance/modules/tmp/). *)
val dir = case OS.Process.getEnv "TMPDIR" of
              SOME d => if String.isSuffix "/" d then d else d ^ "/"
            | NONE => "/tmp/"
val file = dir ^ "conf_textio_tmp.txt"
val out = TextIO.openOut file
val _ = TextIO.output (out, "line one\nline two\n")
val _ = TextIO.output1 (out, #"x")
val _ = TextIO.closeOut out
val ins = TextIO.openIn file
fun showLine NONE = "NONE" | showLine (SOME l) = "SOME [" ^ String.toString l ^ "]"
val _ = p (showLine (TextIO.inputLine ins))
val _ = p (showLine (TextIO.inputLine ins))
val _ = p (showLine (TextIO.inputLine ins))
val _ = p (showLine (TextIO.inputLine ins))
val _ = p (if TextIO.endOfStream ins then "eof" else "not eof")
val _ = TextIO.closeIn ins
val ins2 = TextIO.openIn file
val all = TextIO.inputAll ins2
val _ = TextIO.closeIn ins2
val _ = p (Int.toString (size all))
val ins3 = TextIO.openIn file
val _ = p (TextIO.inputN (ins3, 4))
val _ = p (case TextIO.input1 ins3 of SOME c => "[" ^ str c ^ "]" | NONE => "NONE")
val _ = TextIO.closeIn ins3
val app = TextIO.openAppend file
val _ = TextIO.output (app, "\nappended\n")
val _ = TextIO.closeOut app
val ins4 = TextIO.openIn file
val _ = p (Int.toString (size (TextIO.inputAll ins4)))
val _ = TextIO.closeIn ins4
val _ = TextIO.output (TextIO.stdOut, "to stdOut\n")
val _ = TextIO.output (TextIO.stdErr, "to stdErr\n")
val _ = TextIO.flushOut TextIO.stdOut
val _ = TextIO.print "TextIO.print\n"
val _ = p ((TextIO.openIn "/nonexistent/dir/file"; "opened") handle IO.Io _ => "Io raised")
val _ = p "done"
