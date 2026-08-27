let observed (_value : int) : bool = true [@@verocaml.spec]
type nested_snapshot = { nested_value : int }

let lemma_scope (value : int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger]) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

let need (value : int) : unit =
  [%verocaml.requires
    forall (fun candidate ->
      ((observed candidate) [@trigger]) || candidate = candidate)];
  ()
[@@verocaml.proof]

let before_activation (value : int) : unit =
  [%verocaml.requires observed value];
  need value
[@@verocaml.proof]

[@@@verocaml.activate [lemma_scope]]

module type NESTED = sig
  type t
  val make : int -> t @ unique
  val model : t @ read -> nested_snapshot @ immutable
  val invariant : t @ read -> bool
  val read : t @ read -> int
  val nested_lemma : int -> unit
  val nested_before : int -> unit
  val nested_after : int -> unit
end

module Nested : NESTED = struct
  type t = { nested_value : int }

  let make nested_value : t @ unique =
    [%verocaml.requires nested_value >= 0];
    { nested_value }

  let model (value : t @ read) : nested_snapshot @ immutable =
    ({ nested_value = value.nested_value } : nested_snapshot)
  [@@verocaml.spec]

  let invariant (value : t @ read) : bool =
    (model value).nested_value >= 0
  [@@verocaml.type_invariant]

  let read (value : t @ read) = value.nested_value

  let nested_lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      ((observed value) [@trigger]) || value = value];
    ()
  [@@verocaml.proof]
  [@@verocaml.broadcast]

  let nested_before (_value : int) : unit =
    [%verocaml.assert true];
    ()
  [@@verocaml.proof]

  [@@@verocaml.activate [nested_lemma]]

  let nested_after (value : int) : unit =
    need value
  [@@verocaml.proof]
end

let parent_after_nested (value : int) : unit =
  [%verocaml.assert observed value];
  ()
[@@verocaml.proof]

let structure_scope (value : int) : unit =
  [%verocaml.requires observed value];
  need value
[@@verocaml.proof]

let expression_scope (value : int) : unit =
  [%verocaml.activate [lemma_scope]
    ([%verocaml.activate [lemma_scope] (need value)])]
[@@verocaml.proof]
