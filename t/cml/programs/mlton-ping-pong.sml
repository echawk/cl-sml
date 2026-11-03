(* Ported from MLton's lib/cml/tests/ping-pong.sml
   (https://github.com/MLton/mlton, commit
   5fe943391e539a502699327a6266b6e8e97939e1), with a driver in place of
   run-main.sml. *)

structure Main =
struct
   open CML

   fun pong ch =
      let
         fun loop () =
            let
               val () = recv ch
            in
               loop ()
            end
         val _ = spawn (fn () => loop ())
      in
         ()
      end

   fun ping ch n =
      let
         fun loop i =
            if i > n then RunCML.shutdown OS.Process.success
               else let
                       val () = send (ch, ())
                    in
                       loop (i + 1)
                    end
         val _ = spawn (fn () => loop 0)
      in
         ()
      end

   fun doit n =
      RunCML.doit
      (fn () =>
       let
          val ch = channel ()
          val () = pong ch
          val () = ping ch n
       in
          ()
       end,
       SOME (Time.fromMilliseconds 10))
end

val _ = print (if OS.Process.isSuccess (Main.doit 10000) then "success\n" else "failure\n")

(* EXPECT
success
*)
