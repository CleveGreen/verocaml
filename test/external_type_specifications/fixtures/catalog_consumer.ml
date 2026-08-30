let has_box (box : 'a External_types.box) : bool =
  match box with External_types.Empty -> false | External_types.Box _ -> true
[@@verocaml.spec]
