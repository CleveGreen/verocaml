let local_nonpositive (value : int) : bool =
  Recursive_helper_import_provider.imported_nonpositive value
[@@verocaml.spec]

let rec transitive_imported_count (value : int) : int =
  [%verocaml.decreases value];
  if local_nonpositive value then 0
  else transitive_imported_count (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
