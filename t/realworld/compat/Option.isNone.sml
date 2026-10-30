structure Option = struct
  open Option
  fun isNone NONE = true
    | isNone (SOME _) = false
end
