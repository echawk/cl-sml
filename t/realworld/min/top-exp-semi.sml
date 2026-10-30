functor F () = struct val x = ref 0 end
structure F = F ()
print (Bool.toString (F.x = F.x) ^ "\n");
