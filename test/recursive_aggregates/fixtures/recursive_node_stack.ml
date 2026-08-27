type node =
  | Empty
  | Node of {
      value : int;
      mutable next : node;
    }

type stack = {
  mutable top : node;
  mutable length : int;
}

let make_stack n =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result.length = n];
  { top = Empty; length = n }

let call_make_stack n =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result = n];
  let stack = make_stack n in
  stack.length

let construct_and_match value next =
  [%verocaml.ensures fun result -> result = value];
  let node = Node { value; next } in
  match node with
  | Empty -> 0
  | Node { value; next = _ } -> value

let classify_node = function
  | Empty -> 0
  | Node { value = _; next = _ } -> 1

let classify_nested_node = function
  | Empty -> 0
  | Node { value = _; next = Empty } -> 1
  | Node { value = _; next = Node { value = _; next = _ } } -> 2

let record_pattern = function
  | { top = _; length } -> length

let tuple_with_record n =
  let stack = { top = Empty; length = n } in
  (stack, stack.length)
