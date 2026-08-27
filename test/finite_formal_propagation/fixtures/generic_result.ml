type 'a seq = Nil | Cons of 'a * 'a seq

let first (xs : 'a seq) : 'a seq = xs
[@@verocaml.spec]

let instantiate (xs : int seq) : int seq = first xs
[@@verocaml.spec]
