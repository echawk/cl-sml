(*
 * cl-sml's entry point into HaMLet's static semantics.
 *
 * Sml.elab and Program.elabProgram report elaboration errors on stderr and
 * then carry on with the previous basis, so a caller cannot tell that a
 * program was rejected.  This structure performs the same parse, syntactic
 * restriction check and elaboration, but lets Error.Error escape, and returns
 * HaMLet's rendering of the newly elaborated bindings.
 *)

structure ClSmlHamlet =
struct
  open SyntaxProgram
  open AnnotationProgram

  infix @@

  val width = 79

  fun initial () = Sml.elabArg (Sml.lib ())

  fun elabProgram (B, Program(topdec, NONE)@@_) =
      let
        val B1 = ElabModule.elabTopDec(B, topdec)
      in
        (StaticBasis.plus(B, B1), [B1])
      end
    | elabProgram (B, Program(topdec, SOME program)@@_) =
      let
        val B1        = ElabModule.elabTopDec(B, topdec)
        val (B', Bs)  = elabProgram (StaticBasis.plus(B, B1), program)
      in
        (B', B1 :: Bs)
      end

  fun describe Bs =
      String.concat
        (List.map (fn B => PrettyPrint.toString(PPStaticBasis.ppBasis B, width)) Bs)

  fun elab ((J, B_BIND, B_STAT), (filenameOpt, source)) =
      let
        val (J', program) = Parse.parse(J, source, filenameOpt)
        val B_BIND'       = SyntacticRestrictionsProgram.checkProgram(B_BIND, program)
        val (B_STAT', Bs) = elabProgram (B_STAT, program)
      in
        ((J', B_BIND', B_STAT'), describe Bs)
      end
end;
