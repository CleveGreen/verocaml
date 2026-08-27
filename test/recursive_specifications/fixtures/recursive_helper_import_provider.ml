type foreign_chain = Foreign_empty | Foreign_link of int * foreign_chain

let imported_nonpositive (value : int) : bool = value <= 0
[@@verocaml.spec]
