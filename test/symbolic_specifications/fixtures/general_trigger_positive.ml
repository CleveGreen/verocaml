type 'a box = Box of 'a

[%%verocaml.symbolic val integer_head : int -> int]
[%%verocaml.symbolic val boolean_head : bool -> bool]
[%%verocaml.symbolic val aggregate_head : 'a box -> 'a box]
[%%verocaml.symbolic val parametric_head : 'a -> 'a]

let integer_trigger (value : int) : int =
  [%verocaml.requires
    forall (fun (candidate : int) ->
      ((integer_head candidate) [@trigger]) = integer_head candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let boolean_trigger (value : bool) : bool =
  [%verocaml.requires
    forall (fun (candidate : bool) ->
      ((boolean_head candidate) [@trigger]) || not (boolean_head candidate))];
  [%verocaml.ensures fun result -> result = value];
  value

let aggregate_trigger (value : int box) : int box =
  [%verocaml.requires
    forall (fun (candidate : int box) ->
      ((aggregate_head candidate) [@trigger]) = aggregate_head candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let parametric_trigger (value : 'a) : 'a =
  [%verocaml.requires
    forall (fun (candidate : 'a) ->
      ((parametric_head candidate) [@trigger]) = parametric_head candidate)];
  [%verocaml.ensures fun result -> result = value];
  value
