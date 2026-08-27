type snapshot = { value : int }

module type BOX = sig
  type t
  val make : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val read : t @ read -> int
end

module Box : BOX = struct
  type t = { value : int }

  let make value : t @ unique =
    [%verocaml.requires value >= 0];
    { value }

  let model (box : t @ read) : snapshot @ immutable =
    ({ value = box.value } : snapshot)
  [@@verocaml.spec]

  let invariant (box : t @ read) =
    (model box).value >= 0
  [@@verocaml.type_invariant]

  let read (box : t @ read) = box.value
end

let trusted_read (box : Box.t @ read) =
  [%verocaml.ensures fun result -> result >= 0];
  Box.read box
[@@verocaml.external_body]

let run (box : Box.t @ read) =
  trusted_read box
