open Vstd

type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a box = Box of 'a
type 'a sequence = Empty | Item of 'a * 'a sequence
type ('a, 'b) pair = { left : 'a; right : 'b }

[%%verocaml.symbolic val integer_image : Int.t -> Int.t]
[%%verocaml.symbolic val integer_predicate : Int.t -> bool]
[%%verocaml.symbolic val boolean_image : bool -> bool]
[%%verocaml.symbolic val parametric_image : 'a -> 'a]
[%%verocaml.symbolic val option_image : 'a option -> 'a option]
[%%verocaml.symbolic val box_image : 'a box -> 'a box]
[%%verocaml.symbolic val sequence_image : 'a sequence -> 'a sequence]
[%%verocaml.symbolic val pair_image : ('a, 'b) pair -> ('a, 'b) pair]
[%%verocaml.symbolic val combined_image : bool -> Int.t -> Int.t]
[%%verocaml.symbolic val symbolic_apply : ('a -> 'b) -> 'a -> 'b]

let successor (value : Int.t) : Int.t = value + 1 [@@verocaml.spec]

let named_integer_image (value : Int.t) : Int.t = integer_image value
[@@verocaml.spec]

let named_box_image (value : 'a box) : 'a box = box_image value
[@@verocaml.spec]

let maps_to_self (f : 'a -> 'a) : bool =
  forall (fun (candidate : 'a) -> ((f candidate) [@trigger]) = candidate)
[@@verocaml.spec]

let has_value_witness (f : 'a -> 'b) (value : 'a) : bool =
  exists (fun (candidate : 'a) ->
    candidate = value && f candidate = f value)
[@@verocaml.spec]

let symbolic_integer_instantiation (value : int) : int =
  [%verocaml.requires
    forall (fun (candidate : Int.t) ->
      ((integer_image candidate) [@trigger]) = candidate + 1)];
  [%verocaml.requires
    forall (fun (candidate : Int.t) ->
      ((integer_predicate candidate) [@trigger]) = (candidate >= 0))];
  [%verocaml.assert integer_image value = value + 1];
  [%verocaml.assert integer_predicate value = (value >= 0)];
  [%verocaml.assert
    exists (fun (candidate : Int.t) ->
      candidate = value && integer_image candidate = value + 1)];
  [%verocaml.ensures fun result -> result = value];
  value

let symbolic_boolean_instantiation (value : bool) : bool =
  [%verocaml.requires
    forall (fun (candidate : bool) ->
      ((boolean_image candidate) [@trigger]) = not candidate)];
  [%verocaml.assert boolean_image value = not value];
  [%verocaml.assert
    exists (fun (candidate : bool) ->
      candidate = value && boolean_image candidate = not value)];
  [%verocaml.ensures fun result -> result = value];
  value

let symbolic_multiargument_instantiation (flag : bool) (value : int) : int =
  [%verocaml.requires
    forall (fun (candidate : Int.t) ->
      ((combined_image flag candidate) [@trigger]) = candidate)];
  [%verocaml.assert combined_image flag value = value];
  [%verocaml.ensures fun result -> result = value];
  value

let direct_spec_triggers (value : int) (boxed : 'a box) : int =
  [%verocaml.requires
    forall (fun (candidate : Int.t) ->
      ((named_integer_image candidate) [@trigger]) = candidate)];
  [%verocaml.requires
    forall (fun (candidate : 'a box) ->
      ((named_box_image candidate) [@trigger]) = candidate)];
  [%verocaml.assert integer_image value = value];
  [%verocaml.assert box_image boxed = boxed];
  [%verocaml.ensures fun result -> result = value];
  value

let symbolic_parametric_instantiation (value : 'a) : 'a =
  [%verocaml.requires
    forall (fun (candidate : 'a) ->
      ((parametric_image candidate) [@trigger]) = candidate)];
  [%verocaml.assert parametric_image value = value];
  [%verocaml.assert
    exists (fun (candidate : 'a) ->
      candidate = value && parametric_image candidate = value)];
  [%verocaml.ensures fun result -> result = value];
  value

let symbolic_aggregate_instantiation
    (optional : 'a option)
    (boxed : 'a box)
    (sequence : 'a sequence) : 'a option =
  [%verocaml.requires
    forall (fun (candidate : 'a option) ->
      ((option_image candidate) [@trigger]) = candidate)];
  [%verocaml.requires
    forall (fun (candidate : 'a box) ->
      ((box_image candidate) [@trigger]) = candidate)];
  [%verocaml.requires
    forall (fun (candidate : 'a sequence) ->
      ((sequence_image candidate) [@trigger]) = candidate)];
  [%verocaml.assert option_image optional = optional];
  [%verocaml.assert box_image boxed = boxed];
  [%verocaml.assert sequence_image sequence = sequence];
  [%verocaml.assert
    exists (fun (candidate : 'a option) -> candidate = optional)];
  [%verocaml.assert exists (fun (candidate : 'a box) -> candidate = boxed)];
  [%verocaml.assert
    exists (fun (candidate : 'a sequence) -> candidate = sequence)];
  [%verocaml.ensures fun result -> result = optional];
  optional

let symbolic_record_instantiation (value : ('a, 'b) pair) : ('a, 'b) pair =
  [%verocaml.requires
    forall (fun (candidate : ('a, 'b) pair) ->
      ((pair_image candidate) [@trigger]) = candidate)];
  [%verocaml.assert pair_image value = value];
  [%verocaml.assert
    exists (fun (candidate : ('a, 'b) pair) -> candidate = value)];
  [%verocaml.ensures fun result -> result = value];
  value

let nested_quantifiers (value : int) : int =
  [%verocaml.requires
    forall (fun (outer : Int.t) ->
      ((integer_image outer) [@trigger]) = outer)];
  [%verocaml.assert
    forall (fun (outer : Int.t) ->
      ((integer_image outer) [@trigger]) = outer
      && exists (fun (inner : Int.t) ->
           inner = outer && integer_image inner = inner))];
  [%verocaml.assert
    exists (fun (outer : Int.t) ->
      outer = value
      && forall (fun (inner : Int.t) ->
           ((integer_image inner) [@trigger]) = inner))];
  [%verocaml.ensures fun result -> result = value];
  value

let quantified_postcondition (value : int) : int =
  [%verocaml.requires
    forall (fun (candidate : Int.t) ->
      ((integer_image candidate) [@trigger]) = candidate)];
  [%verocaml.ensures fun result ->
    result = value
    && forall (fun (candidate : Int.t) ->
         ((integer_image candidate) [@trigger]) = candidate)];
  value

let spec_function_quantifiers (value : int) : int =
  [%verocaml.requires maps_to_self integer_image];
  [%verocaml.assert integer_image value = value];
  [%verocaml.assert has_value_witness integer_image value];
  [%verocaml.assert has_value_witness successor value];
  [%verocaml.assert
    symbolic_apply successor value = symbolic_apply successor value];
  [%verocaml.ensures fun result -> result = value];
  value

let quantified_function_binder (value : int) : int =
  [%verocaml.requires
    forall (fun (f : Int.t -> Int.t) -> (f value [@trigger]) = f value)];
  [%verocaml.requires
    forall (fun (predicate : Int.t -> bool) ->
      ((predicate value) [@trigger]) = predicate value)];
  [%verocaml.requires
    forall (fun (project : Int.t -> Int.t option) ->
      ((project value) [@trigger]) = project value)];
  [%verocaml.requires
    forall (fun (staged : Int.t -> Int.t -> Int.t) ->
      ((staged value) [@trigger]) = staged value)];
  [%verocaml.requires
    forall (fun (labelled : base:Int.t -> Int.t) ->
      ((labelled ~base:value) [@trigger]) = labelled ~base:value)];
  [%verocaml.requires
    forall (fun (f : Int.t -> Int.t) ->
      ((symbolic_apply f value) [@trigger]) = f value)];
  [%verocaml.assert symbolic_apply successor value = value + 1];
  [%verocaml.assert
    exists (fun (f : Int.t -> Int.t) -> f value = f value)];
  [%verocaml.ensures fun result -> result = value];
  value

let quantified_proof (value : Int.t) : unit =
  [%verocaml.requires
    forall (fun (candidate : Int.t) ->
      ((integer_image candidate) [@trigger]) = candidate)];
  [%verocaml.assert integer_image value = value];
  [%verocaml.assert
    exists (fun (candidate : Int.t) -> candidate = value)];
  ()
[@@verocaml.proof]
