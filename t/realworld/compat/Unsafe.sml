(* compat: the pieces of SML/NJ's Unsafe used by the benchmarks *)
structure Unsafe = struct
  structure Array = struct val sub = Array.sub val update = Array.update end
  structure Real64 = struct
    fun castFromWord (w : Word64.word) = let
          val e = Word64.toInt (Word64.andb (Word64.>> (w, 0w52), 0wx7ff))
          val m = Word64.toLargeInt (Word64.andb (w, 0wxFFFFFFFFFFFFF))
          val s = if Word64.andb (Word64.>> (w, 0w63), 0w1) = 0w1 then ~1.0 else 1.0
          in
            s * (1.0 + Real.fromLargeInt m / 4503599627370496.0)
              * Math.pow (2.0, Real.fromInt (e - 1023))
          end
  end
end
