fun p s = print (s ^ "\n")
val file = "/private/tmp/claude-501/-Users-ethan-sml-cl-sml/ea664555-fe04-4ec2-b261-892e363ba3ab/scratchpad/conf-modules/tmp/v16.txt"
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
