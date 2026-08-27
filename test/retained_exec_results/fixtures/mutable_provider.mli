type result = { mutable value : int }
val make : int -> result

type shared_result = { shared_value : int @@ aliased }
val make_shared : int -> shared_result

val pass_foreign : Hidden_provider.result -> Hidden_provider.result
