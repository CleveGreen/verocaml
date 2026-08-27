type _ t = Int : int t | Next : 'a t -> 'a t
let depth (_xs : int t) = 0 [@@verocaml.spec]
