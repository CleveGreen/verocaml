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

  let invariant (box : t @ read) : bool =
    (model box).value >= 0
  [@@verocaml.type_invariant]

  let read (box : t @ read) = box.value
end

let prove_model_nonnegative (box : Box.t @ read) : unit =
  [%verocaml.ensures fun result -> (Box.model box).value >= 0];
  [%verocaml.use_type_invariant box];
  ()
[@@verocaml.proof]

let probe_locality (box : Box.t @ read) : unit =
  [%verocaml.use_type_invariant box];
  ()
[@@verocaml.proof]

let run value =
  [%verocaml.requires value >= 0];
  let box = Box.make value in
  [%verocaml.proof prove_model_nonnegative box];
  Box.read box
