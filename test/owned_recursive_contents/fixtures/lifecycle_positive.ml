type snapshot = End | More of int * snapshot

module type STACK = sig
  type t

  val model : t @ read -> snapshot @ immutable
  val empty : unit -> t @ unique
  val singleton : int -> t @ unique
  val deep : int -> int -> int -> t @ unique
  val pushed : int -> int -> int -> int -> t @ unique
  val nested_drop : int -> int -> int -> t @ unique
  val reroot_drop : int -> int -> int -> t @ unique
  val keep : t @ unique -> t @ unique
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

  let empty () : t @ unique =
    [%verocaml.ensures fun result -> model result = End];
    { top = Empty; length = 0 }

  let singleton value : t @ unique =
    [%verocaml.ensures fun result -> model result = More (value, End)];
    { top = Node { value; next = Empty }; length = 1 }

  let deep a b c : t @ unique =
    [%verocaml.ensures fun result ->
      model result = More (a, More (b, More (c, End)))];
    {
      top =
        Node
          {
            value = a;
            next =
              Node
                {
                  value = b;
                  next = Node { value = c; next = Empty };
                };
          };
      length = 3;
    }

  let pushed a b c d : t @ unique =
    [%verocaml.ensures fun result ->
      model result = More (d, More (a, More (b, More (c, End))))];
    let stack =
      {
        top =
          Node
            {
              value = a;
              next =
                Node
                  {
                    value = b;
                    next = Node { value = c; next = Empty };
                  };
            };
        length = 3;
      }
    in
    [%verocaml.proof
      [%verocaml.assert
        model stack = More (a, More (b, More (c, End)))];
      ()];
    stack.top <- Node { value = d; next = stack.top };
    stack

  let nested_drop a b c : t @ unique =
    [%verocaml.ensures fun result ->
      model result = More (a, More (b, End))];
    let stack =
      {
        top =
          Node
            {
              value = a;
              next =
                Node
                  {
                    value = b;
                    next = Node { value = c; next = Empty };
                  };
            };
        length = 3;
      }
    in
    [%verocaml.proof
      [%verocaml.assert
        model stack = More (a, More (b, More (c, End)))];
      ()];
    (match stack.top with
    | Empty -> ()
    | Node { value = _; next = Empty } -> ()
    | Node { value = _; next = Node second } -> second.next <- Empty);
    stack

  let reroot_drop a b c : t @ unique =
    [%verocaml.ensures fun result -> model result = More (b, More (c, End))];
    let stack =
      {
        top =
          Node
            {
              value = a;
              next =
                Node
                  {
                    value = b;
                    next = Node { value = c; next = Empty };
                  };
            };
        length = 3;
      }
    in
    [%verocaml.proof
      [%verocaml.assert
        model stack = More (a, More (b, More (c, End)))];
      ()];
    (match stack.top with
    | Empty -> ()
    | Node { value = _; next } -> stack.top <- next);
    stack

  let keep (stack : t @ unique) : t @ unique = stack
  let length (stack : t @ read) = stack.length
end
