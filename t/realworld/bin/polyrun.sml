(* polyrun.sml -- run $POLYRUN_FILE with Poly/ML, sending compiler warnings
 * and errors to stderr so stdout holds only the program's own output.
 * usage: POLYRUN_FILE=ref.sml poly --script polyrun.sml *)
local
  val file = valOf (OS.Process.getEnv "POLYRUN_FILE")
  val ins = TextIO.openIn file
  val err = TextIO.stdErr
  val hadError = ref false
  fun msg {message, hard, location : PolyML.location, context = _} =
        (if hard then hadError := true else ();
         TextIO.output (err, #file location ^ ":" ^ FixedInt.toString (#startLine location)
                              ^ (if hard then ": error: " else ": warning: "));
         PolyML.prettyPrint (fn s => TextIO.output (err, s), 100) message;
         TextIO.output (err, "\n"))
  val params = [PolyML.Compiler.CPErrorMessageProc msg,
                PolyML.Compiler.CPOutStream (fn s => TextIO.output (err, s)),
                PolyML.Compiler.CPFileName file]
  fun loop () =
        if TextIO.endOfStream ins then ()
        else (PolyML.compiler (fn () => TextIO.input1 ins, params) (); loop ())
in
  val () = (loop (); TextIO.flushOut TextIO.stdOut)
           handle e => (TextIO.flushOut TextIO.stdOut;
                        TextIO.output (err, "Exception- " ^ General.exnMessage e ^ " raised\n");
                        OS.Process.exit OS.Process.failure)
end
