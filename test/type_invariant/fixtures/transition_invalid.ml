type snapshot = { length : int }

module type STACK = sig
  type t
  val singleton : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val snapshot : t @ read -> snapshot @ immutable
  val invalidate : t @ unique -> t @ unique
end

module Stack : STACK = struct
  type node = Empty | Node of { mutable value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }

  let singleton value : t @ unique =
    [%verocaml.requires value >= 0];
    { top = Node { value; next = Empty }; length = value }

  let model (stack : t @ read) : snapshot @ immutable =
    ({ length = stack.length } : snapshot)
  [@@verocaml.spec]

  let invariant (stack : t @ read) =
    (model stack).length >= 0
  [@@verocaml.type_invariant]

  let snapshot (stack : t @ read) : snapshot @ immutable =
    ({ length = stack.length } : snapshot)

  let invalidate (stack : t @ unique) : t @ unique =
    stack.length <- -1;
    stack
end
