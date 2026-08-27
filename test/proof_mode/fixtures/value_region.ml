let lemma (x:int) : unit = () [@@verocaml.proof]
let bad x = let proof_value = [%verocaml.proof lemma x] in x
