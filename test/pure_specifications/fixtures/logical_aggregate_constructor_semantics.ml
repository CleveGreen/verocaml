type 'a option = None | Some of 'a

let spec_opt_eq (left : int option) (right : int option) : bool =
  match left with
  | None -> (match right with None -> true | Some _ -> false)
  | Some left_value ->
      (match right with
       | None -> false
       | Some right_value -> left_value = right_value)
[@@verocaml.spec]

let option_tag_payload_law (value : int) : bool =
  match Some value with
  | None -> false
  | Some observed -> observed = value
[@@verocaml.spec]

let option_disjoint_law (value : int) : bool =
  match Some value with None -> true | Some _ -> false
[@@verocaml.spec]

let option_injective_law (left : int) (right : int) : bool =
  spec_opt_eq (Some left) (Some right) = (left = right)
[@@verocaml.spec]

let choose_option (condition : bool) (value : int) : int option =
  if condition then Some value else None
[@@verocaml.spec]

let rewrite_option (condition : bool) (value : int option) : int option =
  match value with
  | None -> if condition then Some 0 else None
  | Some observed -> if condition then None else Some observed
[@@verocaml.spec]

let nested_option (first : bool) (second : bool) (value : int) : int option =
  let chosen = choose_option first value in
  match chosen with
  | None -> rewrite_option second None
  | Some observed ->
      if second then rewrite_option first (Some observed)
      else choose_option first observed
[@@verocaml.spec]

let direct_constructor_match (value : int) : unit =
  [%verocaml.ensures fun _ -> option_tag_payload_law value];
  [%verocaml.ensures fun _ -> not (option_disjoint_law value)];
  ()

let direct_injectivity (left : int) (right : int) : unit =
  [%verocaml.ensures fun _ -> option_injective_law left right];
  ()

let nested_helper_composition (first : bool) (second : bool) (value : int) : int option =
  [%verocaml.ensures fun result ->
    spec_opt_eq result (nested_option first second value)];
  let chosen = if first then Some value else None in
  match chosen with
  | None -> if second then Some 0 else None
  | Some observed ->
      if second then
        if first then None else Some observed
      else if first then Some observed else None

let wrong_nested_result (first : bool) (second : bool) (value : int) : int option =
  [%verocaml.ensures fun result ->
    spec_opt_eq result (nested_option first second value)];
  None
