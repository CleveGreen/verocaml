type 'a seq = Nil | Cons of 'a * 'a seq

let rec spec_length (xs : 'a seq) : int =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (_, tail) -> 1 + spec_length tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec lemma_equal_refl (xs : 'a seq [@finite]) : unit =
  [%verocaml.assert xs = xs];
  [%verocaml.decreases xs];
  match xs with Nil -> () | Cons (_, tail) -> lemma_equal_refl tail
[@@verocaml.proof]

let int_length_control (xs : int seq) : int = spec_length xs
[@@verocaml.spec]

let bool_length_control (xs : bool seq) : int = spec_length xs
[@@verocaml.spec]

let int_instantiation (xs : int seq [@finite]) : unit = lemma_equal_refl xs
[@@verocaml.proof]

let bool_instantiation (xs : bool seq [@finite]) : unit = lemma_equal_refl xs
[@@verocaml.proof]
