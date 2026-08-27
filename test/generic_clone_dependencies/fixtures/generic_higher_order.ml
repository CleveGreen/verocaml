type 'a seq = Nil | Cons of 'a * 'a seq

let observe (_predicate : 'a -> bool) (_xs : 'a seq) : unit = ()
[@@verocaml.proof]

let instantiate (xs : int seq) : unit =
  observe (fun value -> value >= 0) xs
[@@verocaml.proof]
