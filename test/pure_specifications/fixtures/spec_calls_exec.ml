let executable (x : int) : int = x + 1
let bad (x : int) : int = executable x [@@verocaml.spec]
