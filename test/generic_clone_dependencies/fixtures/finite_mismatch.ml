type 'a seq = Nil | Cons of 'a * 'a seq

let consume (_xs : 'a seq [@finite]) : unit = ()
[@@verocaml.proof]

let forward (xs : 'a seq) : unit = consume xs
[@@verocaml.proof]

let int_control (xs : int seq [@finite]) : unit = forward xs
[@@verocaml.proof]

let bool_control (xs : bool seq [@finite]) : unit = forward xs
[@@verocaml.proof]
