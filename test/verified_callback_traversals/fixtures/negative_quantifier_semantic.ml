type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let observed (_value : 'a) : bool = true [@@verocaml.spec]
type 'a seq = Nil | Cons of 'a * 'a seq
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let false_universal (value : int) : int =
  [%verocaml.ensures fun result ->
    forall (fun (candidate : int) ->
      ((observed candidate) [@trigger])
      && candidate <> candidate)]
  ;
  value

let false_existential (value : int) : int =
  [%verocaml.ensures fun _ ->
    exists (fun (candidate : int) -> candidate <> candidate)]
  ;
  value

let false_nested (value : int) : int =
  [%verocaml.ensures fun _ ->
    forall (fun (outer : int) ->
      ((observed outer) [@trigger])
      && exists (fun (inner : int) -> inner <> inner))]
  ;
  value

let false_abstract (abstract : 'a) : int =
  [%verocaml.ensures fun _ ->
    forall (fun (candidate : 'a) ->
      ((observed candidate) [@trigger]) && candidate <> candidate)]
  ;
  0

let false_option (optional : 'a option) : int =
  [%verocaml.ensures fun _ ->
    forall (fun (candidate : 'a option) ->
      ((observed candidate) [@trigger]) && candidate <> candidate)]
  ;
  0

let false_sequence (sequence : 'a seq) : int =
  [%verocaml.ensures fun _ ->
    forall (fun (candidate : 'a seq) ->
      ((observed candidate) [@trigger]) && candidate <> candidate)]
  ;
  0

let false_tree (tree : 'a tree) : int =
  [%verocaml.ensures fun _ ->
    forall (fun (candidate : 'a tree) ->
      ((observed candidate) [@trigger]) && candidate <> candidate)]
  ;
  0
