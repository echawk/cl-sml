structure S = struct
    type head = { name: string, n: int }
    val l = ref ([] : head list)
end
val _ = print "ok\n"
