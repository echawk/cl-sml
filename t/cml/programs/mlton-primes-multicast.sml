(* After MLton's lib/cml/tests/primes-multicast.sml
   (https://github.com/MLton/mlton, commit
   5fe943391e539a502699327a6266b6e8e97939e1): a prime sieve whose source
   of natural numbers is a multicast channel; a filter thread per prime;
   the printer shuts down after n primes.  Rewritten from upstream's
   description, without its debugging output. *)

structure Main =
struct
   open CML

   fun makeNatStream c =
      let
         val ch = Multicast.mChannel ()
         fun count i = (Multicast.multicast (ch, i); count (i + 1))
      in
         ignore (spawn (fn () => count c));
         ch
      end

   fun makeFilter (p, inCh) =
      let
         val outCh = channel ()
         fun loop () =
            let val i = recv inCh
            in if i mod p <> 0 then send (outCh, i) else (); loop ()
            end
      in
         ignore (spawn loop);
         outCh
      end

   fun makePrimes () =
      let
         val primes = channel ()
         val nats = makeNatStream 2
         val port = Multicast.port nats
         val first = channel ()
         val _ = spawn (fn () => let fun loop () = (send (first, Multicast.recv port); loop ())
                                 in loop () end)
         fun head ch =
            let val p = recv ch
            in send (primes, p); head (makeFilter (p, ch))
            end
      in
         ignore (spawn (fn () => head first));
         primes
      end

   fun makeNatPrinter (ch, n) =
      let
         fun loop (i, acc) =
            if i > n then (print (String.concatWith " " (rev acc) ^ "\n");
                           RunCML.shutdown OS.Process.success)
            else loop (i + 1, Int.toString (recv ch) :: acc)
      in
         loop (1, [])
      end

   fun doit' n =
      RunCML.doit (fn () => makeNatPrinter (makePrimes (), n), SOME (Time.fromMilliseconds 10))

   fun doit n = doit' n
end

val _ = print (if OS.Process.isSuccess (Main.doit 25) then "success\n" else "failure\n")

(* EXPECT
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
success
*)
