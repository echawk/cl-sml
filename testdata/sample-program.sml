(* real file loading smoke test *)
val (file_x, file_y) = (11, 31);

fun fileFact 0 = 1
  | fileFact n = n * fileFact (n - 1);

val file_result =
  let
    val cell = ref 0;
  in
    cell := fileFact 5;
    !cell
  end;

val file_comment_ok = "done";
