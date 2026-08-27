let lemma (x:int) : unit = () [@@verocaml.proof]
let bad x = lemma x; x
