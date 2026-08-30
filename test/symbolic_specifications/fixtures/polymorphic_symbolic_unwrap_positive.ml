type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let opt_is_some (o : 'a option) : bool =
  match o with None -> false | Some _ -> true
[@@verocaml.spec]

[%%verocaml.symbolic val unbound_value : 'a]

let spec_unwrap (o : 'a option) : 'a =
  match o with None -> (unbound_value : 'a) | Some value -> value
[@@verocaml.spec]

let lemma_unwrap (o : 'a option) =
  [%verocaml.requires opt_is_some o];
  [%verocaml.ensures fun _ ->
    match o with None -> false | Some value -> value = spec_unwrap o];
  ()
[@@verocaml.proof]

let lemma_some (value : 'a) =
  [%verocaml.ensures fun _ -> spec_unwrap (Some value) = value];
  ()
[@@verocaml.proof]

let lemma_none (_witness : 'a) =
  [%verocaml.ensures fun _ ->
    spec_unwrap None = (unbound_value : 'a)];
  ()
[@@verocaml.proof]

let spec_choose (condition : bool) (value : 'a) : 'a =
  if condition then value else (unbound_value : 'a)
[@@verocaml.spec]

let lemma_choose_true (value : 'a) =
  [%verocaml.ensures fun _ -> spec_choose true value = value];
  ()
[@@verocaml.proof]

let lemma_choose_false (value : 'a) =
  [%verocaml.ensures fun _ ->
    spec_choose false value = (unbound_value : 'a)];
  ()
[@@verocaml.proof]

let spec_choose_nested (first : bool) (second : bool) (value : 'a) : 'a =
  if first then value
  else if second then (unbound_value : 'a)
  else value
[@@verocaml.spec]

let lemma_nested_first (value : 'a) =
  [%verocaml.ensures fun _ -> spec_choose_nested true false value = value];
  ()
[@@verocaml.proof]

let lemma_nested_second (value : 'a) =
  [%verocaml.ensures fun _ ->
    spec_choose_nested false true value = (unbound_value : 'a)];
  ()
[@@verocaml.proof]

let lemma_nested_last (value : 'a) =
  [%verocaml.ensures fun _ -> spec_choose_nested false false value = value];
  ()
[@@verocaml.proof]

[%%verocaml.symbolic val spec_unwrap_sym : 'a option -> 'a]

let axiom_unwrap_alt (o : 'a option) =
  [%verocaml.requires opt_is_some o];
  [%verocaml.ensures fun _ ->
    match o with None -> false | Some value -> value = spec_unwrap_sym o];
  ()
[@@verocaml.axiom]

let lemma_axiomatized (o : 'a option) =
  [%verocaml.requires opt_is_some o];
  [%verocaml.ensures fun _ ->
    match o with None -> false | Some value -> value = spec_unwrap_sym o];
  axiom_unwrap_alt o
[@@verocaml.proof]

let lemma_axiomatized_int (value : int) =
  [%verocaml.ensures fun _ -> value = spec_unwrap_sym (Some value)];
  axiom_unwrap_alt (Some value)
[@@verocaml.proof]

let lemma_axiomatized_bool (value : bool) =
  [%verocaml.ensures fun _ -> value = spec_unwrap_sym (Some value)];
  axiom_unwrap_alt (Some value)
[@@verocaml.proof]
