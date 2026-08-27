let rec imported_count (value : int) : int =
  [%verocaml.decreases value];
  if Recursive_helper_import_provider.imported_nonpositive value then 0
  else imported_count (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
