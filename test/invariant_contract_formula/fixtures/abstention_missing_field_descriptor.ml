type box = { value : int }
let missing_field_descriptor (box : box) =
  [%verocaml.ensures fun result -> result = box.value];
  box.value
