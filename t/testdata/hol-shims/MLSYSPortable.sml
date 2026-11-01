(* Stands in for HOL's src/portableML/{poly,mosml}/MLSYSPortable.sml, which
   use Poly/ML and Moscow ML internals.  pointer_eq is conservative (never
   true); md5sum and save_heap fail. *)
structure MLSYSPortable =
struct

exception Interrupt

fun listDir s =
   let
      val ds = OS.FileSys.openDir s
      fun recurse acc =
         case OS.FileSys.readDir ds of
            NONE => acc
          | SOME f => recurse (f :: acc)
   in
      recurse [] before OS.FileSys.closeDir ds
   end

fun pointer_eq (x : 'a, y : 'a) = false
fun ref_to_int (x : 'a ref) = 0
fun catch_SIGINT () = ()
fun md5sum (s : string) : string = raise Fail "MLSYSPortable.md5sum"
fun time f x = f x

structure HOLSusp = Susp

fun reraise e = raise e
fun display_exn f e = reraise e

fun make_counter {inc, init} =
  let val counter = ref init
  in fn () => let val i = !counter in counter := i + inc; i end
  end

fun syncref init =
  let val v = ref init
  in {get = fn () => !v,
      upd = fn f => let val (r, x) = f (!v) in v := x; r end}
  end

fun unique_tmp_suffix () = "0"
fun save_heap (file : string) : unit = raise Fail "MLSYSPortable.save_heap"

end
