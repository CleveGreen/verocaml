type 'a chain = Empty | Link of 'a * 'a chain

let rec bad (xs : string chain) : int =
  [%verocaml.decreases xs];
  match xs with Empty -> 0 | Link (_, tail) -> bad tail
[@@verocaml.spec] [@@verocaml.opaque]
