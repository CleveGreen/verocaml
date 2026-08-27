[@@@verocaml.verify]

let use_mutable value = (Mutable_provider.make value).Mutable_provider.value
let use_shared value = (Shared_provider.make_shared value).Shared_provider.shared_value
let use_foreign (value : Hidden_provider.result) = Foreign_provider.pass_foreign value
