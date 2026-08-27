type 'a seq = Nil | Cons of 'a * 'a seq

let same_shape (_left : 'a seq) (_right : 'a seq) : bool = true
[@@verocaml.spec]

let observe (xs : 'a seq [@finite]) : unit =
  [%verocaml.ensures
    fun _result -> same_shape xs xs = same_shape xs xs];
  ()
[@@verocaml.proof]

let int_control (xs : int seq [@finite]) : unit = observe xs
[@@verocaml.proof]

let bool_control (xs : bool seq [@finite]) : unit = observe xs
[@@verocaml.proof]
