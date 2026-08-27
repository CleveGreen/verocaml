type snapshot = { observed : int }

module type STACK = sig
  type t

  val singleton : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val set_length : t @ unique -> int -> t @ unique
end

module Stack : STACK = struct
  type t = { mutable length : int }

  let singleton length : t @ unique = { length }

  let model (stack : t @ read) : snapshot @ immutable =
    { observed = stack.length }
  [@@verocaml.spec]

  let invariant (stack : t @ read) = (model stack).observed >= 0
  [@@verocaml.type_invariant]

  let set_length (stack : t @ unique) length : t @ unique =
    stack.length <- length;
    stack
end
