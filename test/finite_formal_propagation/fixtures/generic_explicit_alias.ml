type 'a seq = Nil | Cons of 'a * 'a seq
type 'a public_seq = 'a seq

let rec alias_reflexive (xs : 'a public_seq [@finite]) : unit =
  [%verocaml.assert xs = xs];
  [%verocaml.decreases xs];
  match xs with Nil -> () | Cons (_, tail) -> alias_reflexive tail
[@@verocaml.proof]

let int_alias_reflexive (seed : int) : unit =
  alias_reflexive (Cons (seed, Nil))
[@@verocaml.proof]

let bool_alias_reflexive (seed : bool) : unit =
  alias_reflexive (Cons (seed, Nil))
[@@verocaml.proof]
