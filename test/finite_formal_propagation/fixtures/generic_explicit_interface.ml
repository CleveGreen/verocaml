type 'a seq = Nil | Cons of 'a * 'a seq

let rec seq_reflexive (xs : 'a seq [@finite]) : unit =
  [%verocaml.assert xs = xs];
  [%verocaml.decreases xs];
  match xs with Nil -> () | Cons (_, tail) -> seq_reflexive tail
[@@verocaml.proof]

let int_seq_reflexive (seed : int) : unit =
  seq_reflexive (Cons (seed, Nil))
[@@verocaml.proof]

let bool_seq_reflexive (seed : bool) : unit =
  seq_reflexive (Cons (seed, Nil))
[@@verocaml.proof]
