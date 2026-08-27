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
  let invariant (box : t @ read) = (model box).value >= 0
  [@@verocaml.type_invariant]
  let read (box : t @ read) = box.value
end

let left value =
  [%verocaml.requires value >= 0];
  Box.make value

let right value =
  [%verocaml.requires value >= 0];
  Box.make value

let independent value =
  [%verocaml.requires value >= 0];
  Box.make value

let top value =
  [%verocaml.requires value >= 0];
  let left_box = left value in
  let _right_box = right value in
  left_box

let independent_root value =
  [%verocaml.requires value >= 0];
  independent value

let unrelated value = value + 1
