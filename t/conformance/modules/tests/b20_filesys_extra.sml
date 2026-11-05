fun p s = print (s ^ "\n")
(* The runner's TMPDIR (run.sh sets it to t/conformance/modules/tmp/). *)
val dir = case OS.Process.getEnv "TMPDIR" of
              SOME d => if String.isSuffix "/" d then d else d ^ "/"
            | NONE => "/tmp/"
val file = dir ^ "conf_fs_tmp.txt"
val out = TextIO.openOut file
val _ = TextIO.output (out, "12345")
val _ = TextIO.closeOut out
val _ = p (Position.toString (OS.FileSys.fileSize file))
val _ = p (if OS.FileSys.access (file, []) then "exists" else "missing")
val _ = OS.FileSys.remove file
val _ = p (if OS.FileSys.access (file, []) then "exists" else "missing")
val sub = dir ^ "conf_fs_dir"
val _ = OS.FileSys.mkDir sub
val _ = p (if OS.FileSys.isDir sub then "mkDir ok" else "mkDir bad")
val _ = OS.FileSys.rmDir sub
val _ = p (if OS.FileSys.access (sub, []) then "still there" else "rmDir ok")
