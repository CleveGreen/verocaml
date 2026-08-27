type 'a seq = Nil | Cons of 'a * 'a seq

let unused (_xs : 'a seq) : bool = true
[@@verocaml.spec]

let control (_xs : int seq) : bool = true
[@@verocaml.spec]
