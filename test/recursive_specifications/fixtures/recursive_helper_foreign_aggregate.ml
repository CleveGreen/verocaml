let foreign_is_empty
    (value : Recursive_helper_import_provider.foreign_chain) : bool =
  match value with
  | Recursive_helper_import_provider.Foreign_empty -> true
  | Recursive_helper_import_provider.Foreign_link _ -> false
[@@verocaml.spec]

let rec invalid_foreign_aggregate (value : int) : int =
  [%verocaml.decreases value];
  if
    foreign_is_empty Recursive_helper_import_provider.Foreign_empty
    || value <= 0
  then 0
  else invalid_foreign_aggregate (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
