[@@@verocaml.verify]

let apply callback value = callback value
let use value = apply Provider.make_record value
