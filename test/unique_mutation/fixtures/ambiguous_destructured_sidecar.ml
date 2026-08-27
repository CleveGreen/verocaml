type box = { mutable value : int }

let ambiguous (box : box @ unique) ((box, ()) : box * unit @ unique) :
    box @ unique =
  [%verocaml.requires box.value >= 0];
  box
