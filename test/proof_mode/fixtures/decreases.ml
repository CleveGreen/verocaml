let bad x : unit = [%verocaml.decreases x]; () [@@verocaml.proof]
