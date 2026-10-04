(*
 * cl-sml's entry point into HaMLet's static semantics.
 *
 * Sml.elab and Program.elabProgram report elaboration errors on stderr and
 * then carry on with the previous basis, so a caller cannot tell that a
 * program was rejected.  This structure performs the same parse, syntactic
 * restriction check and elaboration, but lets Error.Error escape, and returns
 * HaMLet's rendering of the newly elaborated bindings.
 *
 * It also returns facts that cl-sml's code generator needs from the
 * elaborated program, keyed by source position (line, column) of the phrase
 * they describe; see ClSmlFacts below.
 *)

(*
 * Facts are tuples (kind, line, column, name, info):
 *
 *   ("id",  l, c, longvid, is)   identifier occurrence in an expression
 *   ("pat", l, c, longvid, is)   identifier in a pattern (atomic or applied)
 *   ("ov",  l, c, vid, tyname)   overloaded identifier resolved at tyname
 *   ("scon", l, c, text, tyname) special constant resolved at tyname
 *   ("str", l, c, strid, members) structure binding and its value members
 *
 * where is is "v", "c" or "e" for nullary constructors and exceptions, and
 * "c1" or "e1" for ones carrying an argument.  Structure members are a
 * space-separated list of entries "is:longvid".
 *)
structure ClSmlFacts =
struct
  open SyntaxCore
  open AnnotationCore

  infix @@

  type fact = string * int * int * string * string

  val facts : fact list ref = ref []

  fun add (kind, A, name, info) =
      let
        val {file, region} = loc A
        val (line, col) = #1 region
      in
        facts := (kind, line, col, name, info) :: !facts
      end

  fun statusString (sigma, is) =
      let
        val (alphas, tau) = sigma
        val arity =
            case Type.determined tau of
              StaticObjectsCore.FunType _ => "1"
            | _ => ""
      in
        case is of
          IdStatus.v => "v"
        | IdStatus.c => "c" ^ arity
        | IdStatus.e => "e" ^ arity
      end

  fun tynameString tau =
      SOME (TyName.toString (Type.tyname tau)) handle _ => NONE

  fun overloadedInstance (sigma, tau) =
      case sigma of
        ([alpha], _) =>
          if Option.isSome (TyVar.overloadingClass alpha) then
            let
              val (taus, tau') = TypeScheme.instance sigma
            in
              (Type.unify (tau', tau); tynameString (List.hd taus))
              handle _ => NONE
            end
          else NONE
      | _ => NONE

  fun longVIdString longvid = LongVId.toString longvid

  fun whenSome f NONE = ()
    | whenSome f (SOME x) = f x

  fun scon (sc@@A) =
      case Prop.try (elab A) of
        SOME t => add ("scon", A, SCon.toString sc, TyName.toString t)
      | NONE => ()

  fun longVIdExp (longvid@@A', A) =
      case Prop.try (elab A') of
        SOME valstr =>
          let
            val name = longVIdString longvid
          in
            add ("id", A', name, statusString valstr);
            (case Prop.try (elab A) of
               SOME tau =>
                 whenSome (fn t => add ("ov", A', name, t))
                          (overloadedInstance (#1 valstr, tau))
             | NONE => ())
          end
      | NONE => ()

  fun longVIdPat (longvid@@A') =
      let
        val name = longVIdString longvid
      in
        case Prop.try (elab A') of
          SOME valstr => add ("pat", A', name, statusString valstr)
        | NONE => add ("pat", A', name, "v")
      end

  fun atExp (SCONAtExp sc@@A) = scon sc
    | atExp (IDAtExp (_, longvid)@@A) = longVIdExp (longvid, A)
    | atExp (RECORDAtExp NONE@@A) = ()
    | atExp (RECORDAtExp (SOME row)@@A) = expRow row
    | atExp (LETAtExp (d, e)@@A) = (dec d; exp e)
    | atExp (PARAtExp e@@A) = exp e

  and expRow (ExpRow (_, e, rest)@@A) = (exp e; whenSome expRow rest)

  and exp (ATExp a@@A) = atExp a
    | exp (APPExp (e, a)@@A) = (exp e; atExp a)
    | exp (COLONExp (e, _)@@A) = exp e
    | exp (HANDLEExp (e, m)@@A) = (exp e; match m)
    | exp (RAISEExp e@@A) = exp e
    | exp (FNExp m@@A) = match m

  and match (Match (r, rest)@@A) = (mrule r; whenSome match rest)

  and mrule (Mrule (p, e)@@A) = (pat p; exp e)

  and dec (VALDec (_, vb)@@A) = valBind vb
    | dec (TYPEDec _@@A) = ()
    | dec (DATATYPEDec _@@A) = ()
    | dec (DATATYPE2Dec _@@A) = ()
    | dec (ABSTYPEDec (_, d)@@A) = dec d
    | dec (EXCEPTIONDec eb@@A) = exBind eb
    | dec (LOCALDec (d1, d2)@@A) = (dec d1; dec d2)
    | dec (OPENDec _@@A) = ()
    | dec (EMPTYDec@@A) = ()
    | dec (SEQDec (d1, d2)@@A) = (dec d1; dec d2)

  and valBind (PLAINValBind (p, e, rest)@@A) =
      (pat p; exp e; whenSome valBind rest)
    | valBind (RECValBind vb@@A) = valBind vb

  and exBind (NEWExBind (_, _, _, rest)@@A) = whenSome exBind rest
    | exBind (EQUALExBind (_, _, _, longvid, rest)@@A) =
      (longVIdPat longvid; whenSome exBind rest)

  and atPat (WILDCARDAtPat@@A) = ()
    | atPat (SCONAtPat sc@@A) = scon sc
    | atPat (IDAtPat (_, longvid)@@A) = longVIdPat longvid
    | atPat (RECORDAtPat NONE@@A) = ()
    | atPat (RECORDAtPat (SOME row)@@A) = patRow row
    | atPat (PARAtPat p@@A) = pat p

  and patRow (DOTSPatRow@@A) = ()
    | patRow (FIELDPatRow (_, p, rest)@@A) = (pat p; whenSome patRow rest)

  and pat (ATPat a@@A) = atPat a
    | pat (CONPat (_, longvid, a)@@A) = (longVIdPat longvid; atPat a)
    | pat (COLONPat (p, _)@@A) = pat p
    | pat (ASPat (_, _, _, p)@@A) = pat p

  (* Modules *)

  structure M = SyntaxModule

  fun envMembers (prefix, StaticObjectsCore.Env (SE, TE, VE)) =
      VIdMap.foldri
        (fn (vid, valstr, acc) =>
           (statusString valstr ^ ":" ^ prefix ^ VId.toString vid) :: acc)
        (StrIdMap.foldri
           (fn (strid, E, acc) =>
              envMembers (prefix ^ StrId.toString strid ^ ".", E) @ acc)
           [] SE)
        VE

  fun strExp (M.STRUCTStrExp d@@A) = strDec d
    | strExp (M.IDStrExp _@@A) = ()
    | strExp (M.COLONStrExp (s, _)@@A) = strExp s
    | strExp (M.SEALStrExp (s, _)@@A) = strExp s
    | strExp (M.APPStrExp (_, s)@@A) = strExp s
    | strExp (M.LETStrExp (d, s)@@A) = (strDec d; strExp s)

  and strDec (M.DECStrDec d@@A) = dec d
    | strDec (M.STRUCTUREStrDec sb@@A) = strBind sb
    | strDec (M.LOCALStrDec (d1, d2)@@A) = (strDec d1; strDec d2)
    | strDec (M.EMPTYStrDec@@A) = ()
    | strDec (M.SEQStrDec (d1, d2)@@A) = (strDec d1; strDec d2)

  and strBind (M.StrBind (strid@@A', s, rest)@@A) =
      ( strExp s;
        (case Prop.try (elab (annotation s)) of
           SOME E =>
             add ("str", A', StrId.toString strid,
                  String.concatWith " " (envMembers ("", E)))
         | NONE => ());
        whenSome strBind rest )

  fun funBind (M.FunBind (_, _, _, s, rest)@@A) =
      (strExp s; whenSome funBind rest)

  fun topDec (M.STRDECTopDec (d, rest)@@A) = (strDec d; whenSome topDec rest)
    | topDec (M.SIGDECTopDec (_, rest)@@A) = whenSome topDec rest
    | topDec (M.FUNDECTopDec (M.FunDec fb@@_, rest)@@A) =
      (funBind fb; whenSome topDec rest)

  fun collect topdec =
      ( facts := [];
        topDec topdec;
        let val result = List.rev (!facts) in facts := []; result end )
end;

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
        (StaticBasis.plus(B, B1), [B1], ClSmlFacts.collect topdec)
      end
    | elabProgram (B, Program(topdec, SOME program)@@_) =
      let
        val B1             = ElabModule.elabTopDec(B, topdec)
        val facts          = ClSmlFacts.collect topdec
        val (B', Bs, rest) = elabProgram (StaticBasis.plus(B, B1), program)
      in
        (B', B1 :: Bs, facts @ rest)
      end

  fun describe Bs =
      String.concat
        (List.map (fn B => PrettyPrint.toString(PPStaticBasis.ppBasis B, width)) Bs)

  fun elab ((J, B_BIND, B_STAT), (filenameOpt, source)) =
      let
        val (J', program)        = Parse.parse(J, source, filenameOpt)
        val B_BIND'              = SyntacticRestrictionsProgram.checkProgram(B_BIND, program)
        val (B_STAT', Bs, facts) = elabProgram (B_STAT, program)
      in
        ((J', B_BIND', B_STAT'), describe Bs, facts)
      end
end;
