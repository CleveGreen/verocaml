type 'a seq = Nil | Cons of 'a * 'a seq

let open_theorem (_xs : 'a seq [@finite]) : unit = ()
[@@verocaml.proof]
