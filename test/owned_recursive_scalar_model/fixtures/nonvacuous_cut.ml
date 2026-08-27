type snapshot = { length : int; head : int; is_empty : bool }
type shape = { has_two_nodes : bool }

module type STACK = sig
  type t
  val model : t @ read -> snapshot @ immutable
  val has_two_nodes : t @ read -> shape @ immutable
  val two_nodes : int -> int -> t @ unique
  val length : t @ read -> int
  val cut : t @ unique -> t @ unique
end

module Stack : STACK = struct
  type node =
    | Empty
    | Node of { mutable value : int; mutable next : node }

  type t = { mutable top : node; mutable length : int }

  let model (stack : t @ read) : snapshot @ immutable =
    match stack.top with
    | Empty ->
        { length = stack.length; head = 0; is_empty = true }
    | Node record ->
        { length = stack.length; head = record.value; is_empty = false }
  [@@verocaml.spec]

  let has_two_nodes (stack : t @ read) : shape @ immutable =
    match stack.top with
    | Empty -> { has_two_nodes = false }
    | Node record ->
        (match record.next with
        | Empty -> { has_two_nodes = false }
        | Node _ -> { has_two_nodes = true })
  [@@verocaml.spec]

  let two_nodes first second : t @ unique =
    [%verocaml.ensures fun result ->
      let view = model result in
      let shape = has_two_nodes result in
      view.length = 2
      && view.head = first
      && not view.is_empty
      && shape.has_two_nodes];
    {
      top =
        Node
          {
            value = first;
            next = Node { value = second; next = Empty };
          };
      length = 2;
    }

  let length (stack : t @ read) = stack.length

  let cut (stack : t @ unique) : t @ unique =
    [%verocaml.requires
      let shape = has_two_nodes stack in
      shape.has_two_nodes];
    [%verocaml.ensures fun result ->
      let view = model result in
      let shape = has_two_nodes result in
      view.length = 1
      && view.head = [%verocaml.old (model stack).head]
      && not view.is_empty
      && not shape.has_two_nodes];
    match stack.top with
    | Empty -> stack
    | Node record ->
        record.next <- Empty;
        stack.length <- 1;
        stack
end

let two_nodes_then_cut first second =
  [%verocaml.ensures fun result ->
    let view = Stack.model result in
    view.length = 1
    && view.head = first
    && not view.is_empty];
  let stack = Stack.two_nodes first second in
  Stack.cut stack
