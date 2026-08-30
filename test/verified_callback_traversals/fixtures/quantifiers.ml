type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a seq = Nil | Cons of 'a * 'a seq
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let observed (_value : 'a) : bool = false [@@verocaml.spec]
let observed_scalar (_value : int) : bool = false [@@verocaml.spec]

let rec observed_int (value : int) : bool =
  [%verocaml.decreases value];
  forall (fun (candidate : int) ->
    ((observed_scalar candidate) [@trigger])
    = if value <= 0 then true else observed_int (value - 1))
[@@verocaml.spec]
[@@verocaml.revealed]

let quantifiers
    (abstract : 'a)
    (optional : 'a option)
    (sequence : 'a seq)
    (tree : 'a tree)
    (number : int)
    (flag : bool) : int =
  [%verocaml.requires
    forall (fun (n : int) -> ((observed n) [@trigger]) || n = n)];
  [%verocaml.requires
    forall (fun (b : bool) ->
      ((observed b) [@trigger]) || b = b)];
  [%verocaml.requires
    forall (fun (x : 'a) ->
      ((observed x) [@trigger]) || x = x)];
  [%verocaml.requires
    forall (fun (x : 'a option) ->
      ((observed x) [@trigger])
      ||
      match x with
      | None -> x = None
      | Some payload -> x = Some payload)];
  [%verocaml.requires
    forall (fun (x : 'a seq) ->
      ((observed x) [@trigger])
      ||
      match x with
      | Nil -> x = Nil
      | Cons (head, tail) -> x = Cons (head, tail) && tail = tail)];
  [%verocaml.requires
    forall (fun (x : 'a tree) ->
      ((observed x) [@trigger])
      ||
      match x with
      | Leaf -> x = Leaf
      | Node (value, left, right) ->
          x = Node (value, left, right) && left = left && right = right)];
  [%verocaml.requires exists (fun (n : int) -> n = number)];
  [%verocaml.requires exists (fun (b : bool) -> b = flag)];
  [%verocaml.requires exists (fun (x : 'a) -> x = abstract)];
  [%verocaml.requires exists (fun (x : 'a option) -> x = optional)];
  [%verocaml.requires exists (fun (x : 'a seq) -> x = sequence)];
  [%verocaml.requires exists (fun (x : 'a tree) -> x = tree)];
  [%verocaml.requires
    forall (fun (outer : int) ->
      ((observed outer) [@trigger])
      || exists (fun (inner : int) ->
           inner = outer
           && forall (fun (nested : int) ->
                ((observed nested) [@trigger]) || nested = nested)))];
  [%verocaml.ensures fun result -> exists (fun (n : int) -> n = result)];
  [%verocaml.ensures fun result ->
    forall (fun (candidate : int) ->
      ((observed candidate) [@trigger]) || result = result)];
  [%verocaml.assert
    forall (fun (n : int) -> ((observed n) [@trigger]) || n = n)];
  [%verocaml.proof ()];
  number

let proof_quantifier (value : int) : unit =
  [%verocaml.assert
    forall (fun (candidate : int) ->
      ((observed candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.assert exists (fun (witness : int) -> witness = value)];
  ()
[@@verocaml.proof]

let callback_quantifier (callback : 'a -> bool) (value : 'a) : bool =
  [%verocaml.requires call_requires (callback value)];
  [%verocaml.requires
    forall (fun (candidate : 'a) ->
      ((observed candidate) [@trigger])
      || (candidate = candidate && call_requires (callback candidate)))];
  [%verocaml.ensures fun result -> call_ensures (callback value) result];
  callback value
