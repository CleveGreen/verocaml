let observe x = x + 1 [@@verocaml.spec]
let tracked_id (x : int [@tracked]) : (int [@tracked]) = (x [@tracked])
[@@verocaml.proof]
let erased_exec (x : int [@ghost]) : (int [@ghost]) = (x [@ghost])
type packet = { run : int; ghost : int [@ghost] }
type choice = C of int * (int [@tracked])
