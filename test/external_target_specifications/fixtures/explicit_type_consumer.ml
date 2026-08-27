[@@@verocaml.verify]

let promised_specification : int -> int = fun value -> Legacy.promised value
[@@verocaml.external_specification]

let promised value = Legacy.promised value
