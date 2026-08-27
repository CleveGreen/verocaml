(* COMMON BEGIN *)
let observed (_value : 'a) : bool = true [@@verocaml.spec]
(* COMMON END *)

(* CASE raw BEGIN *)
let raw value =
  [%verocaml.requires
    let raw_forall predicate = predicate value in
    raw_forall (fun (x : int) -> observed x)];
  value
(* CASE raw END *)

(* CASE shadowed BEGIN *)
let shadowed value =
  [%verocaml.requires
    let forall predicate = predicate value in
    forall (fun (x : int) -> observed x)];
  value
(* CASE shadowed END *)

(* CASE qualified BEGIN *)
module Qualified = struct
  let forall predicate = predicate 0
end
let qualified value =
  [%verocaml.requires Qualified.forall (fun (x : int) -> x = value)];
  value
(* CASE qualified END *)

(* CASE function-binder BEGIN *)
let function_binder value =
  [%verocaml.requires
    forall (fun (callback : int -> int) ->
      ((observed callback) [@trigger]) || callback value = value)];
  value
(* CASE function-binder END *)

(* CASE reference-binder BEGIN *)
let reference_binder value =
  [%verocaml.requires
    forall (fun (cell : int ref) ->
      ((observed cell) [@trigger]) || !cell = value)];
  value
(* CASE reference-binder END *)

(* CASE array-binder BEGIN *)
let array_binder value =
  [%verocaml.requires
    forall (fun (items : int array) ->
      ((observed items) [@trigger]) || Array.length items = value)];
  value
(* CASE array-binder END *)

(* CASE object-binder BEGIN *)
let object_binder value =
  [%verocaml.requires
    forall (fun (item : < get : int >) ->
      ((observed item) [@trigger]) || item#get = value)];
  value
(* CASE object-binder END *)

(* CASE mutable-binder BEGIN *)
type mutable_box = { mutable item : int }
let mutable_binder value =
  [%verocaml.requires
    forall (fun (box : mutable_box) ->
      ((observed box) [@trigger]) || box.item = value)];
  value
(* CASE mutable-binder END *)

(* CASE cyclic-binder BEGIN *)
type cyclic = Cycle of cyclic
let cyclic_binder value =
  [%verocaml.requires
    forall (fun (cycle : cyclic) ->
      ((observed cycle) [@trigger]) || value = value)];
  value
(* CASE cyclic-binder END *)

(* CASE open-binder BEGIN *)
let open_binder value =
  [%verocaml.requires
    forall (fun (item : [> `Item of int ]) ->
      ((observed item) [@trigger]) || value = value)];
  value
(* CASE open-binder END *)

(* CASE grouped-binder BEGIN *)
let grouped value =
  [%verocaml.requires
    forall (fun ((left, right) : int * int) ->
      ((observed left) [@trigger]) || left = right)];
  value
(* CASE grouped-binder END *)

(* CASE nested-wrong-owner BEGIN *)
let nested_wrong_owner value =
  [%verocaml.requires
    forall (fun (outer : int) ->
      exists (fun (inner : int) ->
        ((observed inner) [@trigger]) || outer = value))];
  value
(* CASE nested-wrong-owner END *)

(* Forged, stale, copied-marker, and wrong-program carrier controls are made
   from the authenticated positive CMT by the focused structural tool. *)
