(*
 * cl-sml: Standard ML Basis Library, structures RealVector and RealArray
 * (not part of HaMLet's basis), like Word8Vector and Word8Array.
 *)

structure RealVector : MONO_VECTOR
  where type elem = real =
struct
  open Vector

  type elem   = real
  type vector = elem vector
end;

structure RealArray : MONO_ARRAY
  where type vector = RealVector.vector
  where type elem = real =
struct
  open Array

  type elem   = real
  type vector = RealVector.vector
  type array  = elem array
end;
