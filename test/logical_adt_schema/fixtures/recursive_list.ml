type 'a chain = Nil | Cons of 'a * 'a chain

let rebuild value =
  match value with Nil -> Nil | Cons (head, tail) -> Cons (head, tail)
[@@verocaml.spec]

let same left right = left = right [@@verocaml.spec]

let reconstruction (value : 'a chain) : unit =
  [%verocaml.assert same (rebuild value) value];
  ()
[@@verocaml.proof]
