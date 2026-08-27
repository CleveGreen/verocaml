type 'a seq = Nil | Cons of 'a * 'a seq

let rec observe (xs : 'a seq [@finite]) : unit =
  [%verocaml.decreases xs];
  match xs with Nil -> () | Cons (_, tail) -> observe tail
[@@verocaml.proof]

let instantiate (xs : string seq [@finite]) : unit = observe xs
[@@verocaml.proof]
