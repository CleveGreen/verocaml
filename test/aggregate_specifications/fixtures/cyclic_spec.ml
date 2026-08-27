type chain = End | Link of chain

let expose (chain : chain) = chain [@@verocaml.spec]
