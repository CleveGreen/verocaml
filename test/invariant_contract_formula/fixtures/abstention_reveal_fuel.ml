type chain = End | Next of chain

let rec count value =
  [%verocaml.decreases value];
  match value with End -> 0 | Next rest -> 1 + count rest
[@@verocaml.spec] [@@verocaml.opaque]

let reveal_fuel value =
  [%verocaml.reveal_with_fuel (count, 1)];
  value
[@@verocaml.proof]
