type 'a chain = Empty | Link of 'a * 'a chain
let rec bad (xs : int chain ref) : int =
  [%verocaml.decreases xs];
  bad xs
[@@verocaml.spec] [@@verocaml.opaque]
