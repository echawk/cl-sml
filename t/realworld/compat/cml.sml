(* compat/cml.sml: replaces the SML/NJ benchmarks' programs/common/cml.sml
   (https://github.com/smlnj/benchmarks, commit
   75ee8bee6fbd38af68a549bc6ca091f15fb8d3f0), a small CML of their own
   built on SMLofNJ.Cont, by the same signature over cl-sml's CML
   (system cl-sml/cml, docs/DESIGN-CML.md).  run runs its function as the
   first thread of RunCML.doit and shuts down when it returns, which ends
   the threads still blocked (the sieve's filters).  exit ends the calling
   thread.  The Basis's CML is reached through BasisCML: within the new
   structure, cl-sml would take CML.spawn for the new structure's own. *)

structure BasisCML = CML

structure CML : sig

    val spawn : (unit -> unit) -> unit

    val yield : unit -> unit

    val exit : unit -> 'a

    val run : ('a -> 'b) * 'a -> 'b

    type 'a chan

    val channel : unit -> 'a chan
    val send : 'a chan * 'a -> unit
    val recv : 'a chan -> 'a

  end = struct

    datatype 'a result = Value of 'a | Raised of exn

    fun spawn f = ignore (BasisCML.spawn f)
    val yield = BasisCML.yield
    fun exit () = BasisCML.exit ()

    fun run (f, arg) = let
          val result = ref NONE
          fun main () = (
                result := SOME (Value (f arg) handle e => Raised e);
                RunCML.shutdown OS.Process.success)
          in
            ignore (RunCML.doit (main, NONE));
            case !result
             of SOME (Value v) => v
              | SOME (Raised e) => raise e
              | NONE => raise Fail "deadlock"
          end

    type 'a chan = 'a BasisCML.chan
    val channel = BasisCML.channel
    val send = BasisCML.send
    val recv = BasisCML.recv

  end
