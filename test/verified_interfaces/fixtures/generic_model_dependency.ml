type 'a snapshot = End | More of 'a * 'a snapshot

module type STACK = sig
  type t

  val model : t @ read -> int snapshot @ immutable
  val singleton : int -> t @ unique
  val keep : t @ unique -> t @ unique
  val length : t @ read -> int
end

module Stack : STACK = struct
  type node =
    | Empty
    | Node of { mutable value : int; mutable next : node }

  type t = { mutable top : node; mutable length : int }

  let rec contents_node (node : node) : int snapshot =
    [%verocaml.decreases node];
    match node with
    | Empty -> End
    | Node { value; next } -> More (value, contents_node next)
  [@@verocaml.spec]
  [@@verocaml.opaque]

  let model (stack : t @ read) : int snapshot @ immutable =
    contents_node stack.top
  [@@verocaml.spec]

  let singleton value : t @ unique =
    [%verocaml.ensures fun result -> model result = More (value, End)];
    { top = Node { value; next = Empty }; length = 1 }

  let keep (stack : t @ unique) : t @ unique = stack
  let length (stack : t @ read) = stack.length
end
