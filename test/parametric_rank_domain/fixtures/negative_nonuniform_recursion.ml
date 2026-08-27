type _ nest = Z : 'a nest | S : ('a * 'a) nest -> 'a nest
let depth (_xs : int nest) = 0 [@@verocaml.spec]
