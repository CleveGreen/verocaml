type snapshot = End | More of int * snapshot

module type STACK = sig
  type t

  val model : t @ read -> snapshot @ immutable
  val singleton : int -> t @ unique
  val keep : t @ unique -> t @ unique
  val length : t @ read -> int
end

module Stack : STACK = struct
  type node =
    | Empty
    | Node of { mutable value : int; mutable next : node }

  type t = { mutable top : node; mutable length : int }

  let rec model_body (node : node) : snapshot =
    match node with
    | Empty -> End
    | Node { value; next } -> More (value, model_body next)

  let model (stack : t @ read) : snapshot @ immutable =
    model_body stack.top
  [@@verocaml.external_specification]

  let singleton value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }

  let keep (stack : t @ unique) : t @ unique = stack
  let length (stack : t @ read) = stack.length
end
