[%%verocaml.symbolic val int_image : int -> int]
[%%verocaml.symbolic val int_predicate : int -> bool]
[%%verocaml.symbolic val mixed_image : int -> bool -> int]
[%%verocaml.symbolic val mixed_predicate : bool -> int -> bool]

let unary_integer (value : int) : int =
  [%verocaml.ensures fun _ -> int_image value = int_image value];
  value

let unary_boolean (value : int) : int =
  [%verocaml.ensures fun _ -> int_predicate value = int_predicate value];
  value

let ordered_integer (number : int) (flag : bool) : int =
  [%verocaml.ensures fun _ ->
    mixed_image number flag = mixed_image number flag];
  number

let ordered_boolean (flag : bool) (number : int) : int =
  [%verocaml.ensures fun _ ->
    mixed_predicate flag number = mixed_predicate flag number];
  number
