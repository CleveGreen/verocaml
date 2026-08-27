type 'a seq = Nil | Cons of 'a * 'a seq

let rec open_theorem (xs : 'a seq [@finite]) : unit =
  [%verocaml.decreases xs];
  match xs with Nil -> () | Cons (_, tail) -> open_theorem tail
[@@verocaml.proof]
