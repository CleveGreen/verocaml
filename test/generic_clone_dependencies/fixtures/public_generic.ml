type 'a seq = Nil | Cons of 'a * 'a seq

let observe (_xs : 'a seq [@finite]) : unit = ()
[@@verocaml.proof]

let int_control (xs : int seq [@finite]) : unit = observe xs
[@@verocaml.proof]

let bool_control (xs : bool seq [@finite]) : unit = observe xs
[@@verocaml.proof]
