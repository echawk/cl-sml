structure LibBase = struct
  exception NotFound
  exception Unimplemented of string
  exception Impossible of string
  fun failure {module : string, func : string, msg : string} =
        raise Fail (module ^ "." ^ func ^ ": " ^ msg)
end
