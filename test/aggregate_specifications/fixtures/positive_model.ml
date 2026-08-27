type snapshot = { length : int }

module type STACK = sig
  type t
  val singleton : int -> t @ unique
  val length : t @ read -> int
  val model : t @ read -> snapshot @ immutable
  val drop : t @ unique -> t @ unique
end

module Stack : STACK = struct
  type node = Empty | Node of { mutable value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }

  let singleton value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }

  let length (stack : t @ read) =
    [%verocaml.ensures fun result -> result = stack.length];
    stack.length

  let model (stack : t @ read) : snapshot @ immutable =
    { length = stack.length }
  [@@verocaml.spec]

  let drop (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node { value = _; next } ->
        stack.top <- next;
        stack.length <- 0;
        stack
end

let observe (stack : Stack.t @ read) =
  [%verocaml.requires (Stack.model stack).length >= 0];
  [%verocaml.ensures fun result -> result = (Stack.model stack).length];
  Stack.length stack
