type box = { mutable value : int }

type holder = {
  box : box;
  delta : int;
}

type packed = Packed of box * int

let record_destructured ({ box; delta } : holder @ unique) : box @ unique =
  [%verocaml.requires delta = 0];
  [%verocaml.ensures fun result ->
    result.value = [%verocaml.old box.value]];
  box.value <- box.value + delta;
  box

let constructor_destructured (Packed (box, delta) : packed @ unique) :
    box @ unique =
  [%verocaml.requires delta = 0];
  [%verocaml.ensures fun result ->
    result.value = [%verocaml.old box.value]];
  box.value <- box.value + delta;
  box
