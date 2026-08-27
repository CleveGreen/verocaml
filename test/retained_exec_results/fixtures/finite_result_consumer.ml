[@@@verocaml.verify]

let consume (_chain : Provider.chain [@finite]) = ()
let use count =
  [%verocaml.requires count >= 0];
  consume (Provider.make_chain count)
