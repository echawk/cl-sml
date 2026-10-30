structure Err =
struct
  exception NotFound
  exception Other
end
val _ = print ((raise Err.Other) handle Err.NotFound => "wrong: NotFound\n" | Err.Other => "Other\n")
