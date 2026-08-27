type 'a seq = Nil | Cons of 'a * 'a seq

let rec child (xs : 'a seq [@finite]) : unit =
  [%verocaml.decreases xs];
  match xs with Nil -> () | Cons (_, tail) -> child tail
[@@verocaml.proof]
