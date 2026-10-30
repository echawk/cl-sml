structure B = struct
    datatype term
      = Var of int
      | Prop of { name: string, props: (term * term) list ref } * term list

    type head = { name: string, props: (term * term) list ref }

    val lemmas = ref ([] : head list)
    fun headname {name = n, props=p} = n
end
val _ = print (B.headname {name = "x", props = ref []} ^ "\n")
