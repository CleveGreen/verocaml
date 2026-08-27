let exec x = x + 1
let bad x : unit = exec x; () [@@verocaml.proof]
