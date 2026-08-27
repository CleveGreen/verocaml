type snapshot =
  | End
  | More of { mutable value : int; mutable next : snapshot }

module type STACK = sig
  type t

  val model : t @ read -> snapshot
  val singleton : int -> t @ unique
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
    | Node { value; next } ->
        More { value; next = contents_node next }
  [@@verocaml.spec]
  [@@verocaml.opaque]

  let model (stack : t @ read) : snapshot = contents_node stack.top
  [@@verocaml.spec]

  let singleton value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }

  let keep (stack : t @ unique) : t @ unique = stack
  let length (stack : t @ read) = stack.length
end
