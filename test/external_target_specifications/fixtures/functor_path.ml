[@@@verocaml.verify]
module Make (Target : module type of Legacy) = Target
module Generated = Make (Legacy)
let promised_specification value = Generated.promised value
[@@verocaml.external_specification]
