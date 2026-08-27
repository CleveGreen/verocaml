let math_succ (x : int) : int = x + 1 [@@verocaml.spec]
let runtime_use (x : int) : int = math_succ x
