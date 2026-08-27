type 'a seq = Nil | Cons of 'a * 'a seq

let theorem (_xs : 'a seq [@finite]) : unit = ()
[@@verocaml.proof]

let invalid (xs : 'a seq [@finite]) : bool =
  theorem xs;
  true
[@@verocaml.spec]

let int_control (xs : int seq [@finite]) : bool = invalid xs
[@@verocaml.spec]

let bool_control (xs : bool seq [@finite]) : bool = invalid xs
[@@verocaml.spec]
