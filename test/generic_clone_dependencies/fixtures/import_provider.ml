type 'a seq = Nil | Cons of 'a * 'a seq

let same_shape (_xs : 'a seq) : bool = true
[@@verocaml.spec]
