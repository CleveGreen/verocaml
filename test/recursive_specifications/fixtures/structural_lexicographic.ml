type 'a chain = Empty | Link of 'a * 'a chain
let rec bad (xs : int chain) (n : int) : int =
  [%verocaml.decreases (xs, n)];
  match xs with Empty -> n | Link (_, tail) -> bad tail (n - 1)
[@@verocaml.spec] [@@verocaml.opaque]
