[@@@verocaml.verify]

let use_monomorphic value = (Provider.proof_record value).Provider.value
let use_generic value = (Generic_proof_provider.proof_box value).Generic_proof_provider.box_value
