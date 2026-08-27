type snapshot = { length : int; head : int; is_empty : bool }

module type STACK = sig
  type t
  val model : t @ read -> snapshot @ immutable
  val singleton : int -> t @ unique
  val length : t @ read -> int
  val zero_head : t @ unique -> t @ unique
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

  let singleton value : t @ unique =
    [%verocaml.ensures fun result ->
      let view = model result in
      view.length = 1
      && view.head = value
      && not view.is_empty];
    { top = Node { value; next = Empty }; length = 1 }

  let length (stack : t @ read) = stack.length

  let zero_head (stack : t @ unique) : t @ unique =
    [%verocaml.requires
      let view = model stack in
      view.length = 1 && not view.is_empty];
    [%verocaml.ensures fun result ->
      let view = model result in
      view.length = 1
      && view.head = 0
      && not view.is_empty];
    match stack.top with
    | Empty -> stack
    | Node record ->
        record.value <- 0;
        stack
end

let singleton_then_zero value =
  [%verocaml.ensures fun result ->
    let view = Stack.model result in
    view.length = 1
    && view.head = 0
    && not view.is_empty];
  let stack = Stack.singleton value in
  Stack.zero_head stack
