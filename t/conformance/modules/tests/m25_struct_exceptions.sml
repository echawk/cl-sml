(* exceptions in structures: long names in raise, handle and aliases *)
structure Err =
struct
  exception NotFound
  exception Invalid of string
  structure Deep = struct exception Code of int end
end

exception Alias = Err.Invalid
exception Missing = Err.NotFound

fun try f = (f (); "ok")
  handle Err.NotFound => "NotFound"
       | Err.Invalid s => "Invalid " ^ s
       | Err.Deep.Code n => "Code " ^ Int.toString n

val _ = print (try (fn () => raise Err.NotFound) ^ "\n")
val _ = print (try (fn () => raise Err.Invalid "x") ^ "\n")
val _ = print (try (fn () => raise Err.Deep.Code 42) ^ "\n")
val _ = print (try (fn () => raise Alias "via alias") ^ "\n")
val _ = print (try (fn () => raise Missing) ^ "\n")
val _ = print ((raise Err.Invalid "y") handle Alias s => "alias caught " ^ s ^ "\n")

structure E2 = struct exception Local = Err.Deep.Code end
val _ = print (try (fn () => raise E2.Local 7) ^ "\n")
