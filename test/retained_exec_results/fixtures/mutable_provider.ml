[@@@verocaml.verify]

type result = { mutable value : int }
let make value = { value }

type shared_result = { shared_value : int @@ aliased }
let make_shared shared_value = { shared_value }

let pass_foreign (value : Hidden_provider.result) = value
