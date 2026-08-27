type tree = Leaf | Pair of tree * tree

let rec build count (leaf : tree [@finite]) : tree =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then leaf
  else
    let child = build (count - 1) leaf in
    Pair (child, child)

let consume (_tree : tree [@finite]) : unit = ()

let use count (leaf : tree [@finite]) =
  [%verocaml.requires count >= 0];
  consume (build count leaf)
