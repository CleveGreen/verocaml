let bad x : unit = let mutable y = x in y <- y + 1 [@@verocaml.proof]
