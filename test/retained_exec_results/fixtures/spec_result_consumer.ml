[@@@verocaml.verify]

let use_monomorphic value = (Provider.spec_record value).Provider.value
let use_generic value = (Generic_spec_provider.spec_box value).Generic_spec_provider.box_value
