structure String = struct
  open String
  fun concatWithMap sep f xs = concatWith sep (List.map f xs)
end
