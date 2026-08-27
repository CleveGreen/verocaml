let local_specification (value : int) = value [@@verocaml.spec]

let imported_identity (value : int) =
  [%verocaml.ensures fun result -> local_specification value = result];
  value
