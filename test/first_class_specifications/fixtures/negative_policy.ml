let optional_partial ?(base = 0) (value : int) : int = base + value
[@@verocaml.spec]

let use_optional (value : int) : int =
  [%verocaml.assert optional_partial ~base:value 2 = value + 2];
  value
