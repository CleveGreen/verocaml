type box = { value : int }
let missing_aggregate_descriptor value =
  [%verocaml.ensures fun result -> result.value = value];
  { value }
