(*
 * (c) Andreas Rossberg 2001-2025
 *
 * Standard ML Basis Library
 *
 * Note:
 * - We must keep the array type transparent in order to keep its
 *   special equality property.
 *
 * cl-sml: the type stays HaMLet's, 'a ref vector ref, for its equality,
 * but an array is the run time's (a simple-vector of its elements after
 * the marker :array; src/runtime.lisp), and the compiler opens sub, update
 * and length.  HaMLet's version, on refs, is src/coalton/Array.sml, which
 * the Coalton backend uses.
 *)

structure Array : ARRAY =
struct
  type 'a array               = 'a ref vector ref
  type 'a vector              = 'a vector

  val maxLen                  = use{b = "Array.maxLen"} () : int

  fun array x    = (use{b = "Array.array"} : int * 'a -> 'a array) x
  fun fromList x = (use{b = "Array.fromList"} : 'a list -> 'a array) x
  fun length x   = (use{b = "Array.length"} : 'a array -> int) x
  fun sub x      = (use{b = "Array.sub"} : 'a array * int -> 'a) x
  fun update x   = (use{b = "Array.update"} : 'a array * int * 'a -> unit) x
  fun vector x   = (use{b = "Array.vector"} : 'a array -> 'a vector) x
  fun copy{src, dst, di} =
      (use{b = "Array.copy"} : 'a array * 'a array * int -> unit) (src, dst, di)
  fun copyVec{src, dst, di} =
      (use{b = "Array.copyVec"} : 'a vector * 'a array * int -> unit) (src, dst, di)

  fun tabulate(n, f) =
      if n < 0 orelse n > maxLen then raise Size
      else if n = 0 then fromList nil
      else let val arr = array(n, f 0)
               fun fill i = if i = n then () else (update(arr, i, f i); fill(i + 1))
           in fill 1; arr end

  fun appi f arr =
      let val n = length arr
          fun loop i = if i = n then () else (f(i, sub(arr, i)); loop(i + 1))
      in loop 0 end
  fun app f arr =
      let val n = length arr
          fun loop i = if i = n then () else (f(sub(arr, i)); loop(i + 1))
      in loop 0 end
  fun modifyi f arr =
      let val n = length arr
          fun loop i = if i = n then ()
                       else (update(arr, i, f(i, sub(arr, i))); loop(i + 1))
      in loop 0 end
  fun modify f arr =
      let val n = length arr
          fun loop i = if i = n then ()
                       else (update(arr, i, f(sub(arr, i))); loop(i + 1))
      in loop 0 end
  fun foldli f init arr =
      let val n = length arr
          fun loop(i, x) = if i = n then x else loop(i + 1, f(i, sub(arr, i), x))
      in loop(0, init) end
  fun foldri f init arr =
      let fun loop(i, x) = if i = 0 then x else loop(i - 1, f(i - 1, sub(arr, i - 1), x))
      in loop(length arr, init) end
  fun foldl f init arr =
      let val n = length arr
          fun loop(i, x) = if i = n then x else loop(i + 1, f(sub(arr, i), x))
      in loop(0, init) end
  fun foldr f init arr =
      let fun loop(i, x) = if i = 0 then x else loop(i - 1, f(sub(arr, i - 1), x))
      in loop(length arr, init) end
  fun findi f arr =
      let val n = length arr
          fun loop i = if i = n then NONE
                       else let val x = sub(arr, i)
                            in if f(i, x) then SOME(i, x) else loop(i + 1) end
      in loop 0 end
  fun find f arr =
      let val n = length arr
          fun loop i = if i = n then NONE
                       else let val x = sub(arr, i)
                            in if f x then SOME x else loop(i + 1) end
      in loop 0 end
  fun exists f arr            = Option.isSome(find f arr)
  fun all f arr               = Bool.not(exists (Bool.not o f) arr)

  fun collate  f (a1, a2)     = collate'(f, a1, a2, 0)
  and collate'(f, a1, a2, i)  =
      case (i = length a1, i = length a2) of
        (true,  true ) => EQUAL
      | (true,  false) => LESS
      | (false, true ) => GREATER
      | (false, false) =>
      case f(sub(a1, i), sub(a2, i)) of
        EQUAL => collate'(f, a1, a2, i+1)
      | other => other
end

type 'a array = 'a Array.array;
