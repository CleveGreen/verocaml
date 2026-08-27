type snapshot = End | More of int * snapshot

module type STACK = sig
  type t

  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val preserve : int -> int -> t @ unique
  val refresh : t @ unique -> t @ unique
  val length : t @ read -> int
end

module Stack : STACK = struct
  type node =
    | Empty
    | Node of { mutable value : int; mutable next : node }

  type t = { mutable top : node; mutable length : int }

  let rec contents_node (node : node) : snapshot =
    [%verocaml.decreases node];
    match node with
    | Empty -> End
    | Node { value; next } -> More (value, contents_node next)
  [@@verocaml.spec]
  [@@verocaml.opaque]

  let model (stack : t @ read) : snapshot @ immutable =
    contents_node stack.top
  [@@verocaml.spec]

  let invariant (stack : t @ read) =
    let contents = model stack in
    contents = contents
  [@@verocaml.type_invariant]

  let preserve first second : t @ unique =
    [%verocaml.ensures fun result -> model result = More (first, End)];
    let stack =
      {
        top =
          Node
            {
              value = first;
              next = Node { value = second; next = Empty };
            };
        length = 2;
      }
    in
    (match stack.top with
    | Empty -> ()
    | Node record -> record.next <- Empty);
    stack

  let refresh (stack : t @ unique) : t @ unique =
    if false then
      match stack.top with
      | Empty -> stack
      | Node record ->
          record.value <- 0;
          stack
    else stack

  let length (stack : t @ read) = stack.length
end

let run value =
  let stack = Stack.preserve value value in
  Stack.refresh stack
