let choose (condition : bool) (left : bool) (right : bool) =
  if condition then left else right
[@@verocaml.spec]

let nested_choose (first : bool) (second : bool) (value : bool) =
  choose first (choose second value (not value)) false
[@@verocaml.spec]

let matched (condition : bool) =
  match condition with
  | false -> nested_choose true false true
  | true -> nested_choose false true false
[@@verocaml.spec]

let nested_path_control (condition : bool) =
  [%verocaml.ensures fun result ->
    result =
      choose condition (matched condition) (nested_choose false true true)];
  if condition then false else false

let unsupported_integer_choice (condition : bool) =
  if condition then 1 else 2
[@@verocaml.spec]

let unsupported_form_falls_back (condition : bool) =
  [%verocaml.ensures fun result ->
    result = unsupported_integer_choice condition];
  if condition then 1 else 2

let wrong_result_never_verifies (condition : bool) =
  [%verocaml.ensures fun result ->
    result = choose condition false true];
  if condition then true else false
