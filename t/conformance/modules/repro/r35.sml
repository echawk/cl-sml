functor Scaler (A : sig val scale : int end) =
struct
  open A
  fun apply n = n * scale
end
structure Three = struct val scale = 3 end
structure S3 = Scaler (Three)
val _ = print (Int.toString (S3.apply 7) ^ "\n")
