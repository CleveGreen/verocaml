(* COMMON BEGIN *)
let observed (_value : 'a) : bool = true [@@verocaml.spec]
type box = { item : int }
let nullary () = true [@@verocaml.spec]
(* COMMON END *)

(* CASE missing BEGIN *)
let missing value =
  [%verocaml.requires forall (fun (x : int) -> observed x)];
  value
(* CASE missing END *)

(* CASE duplicate BEGIN *)
let duplicate value =
  [%verocaml.requires
    forall (fun (x : int) ->
      ((observed x) [@trigger]) && ((observed x) [@trigger]))];
  value
(* CASE duplicate END *)

(* CASE grouped-payload BEGIN *)
let grouped_payload value =
  [%verocaml.requires
    forall (fun (x : int) -> (observed x [@trigger observed x]))];
  value
(* CASE grouped-payload END *)

(* CASE misplaced-pattern BEGIN *)
let misplaced_pattern value =
  [%verocaml.requires
    forall (fun ((x [@trigger]) : int) -> observed x)];
  value
(* CASE misplaced-pattern END *)

(* CASE misplaced-quantifier BEGIN *)
let misplaced_quantifier value =
  [%verocaml.requires
    (forall (fun (x : int) -> observed x) [@trigger])];
  value
(* CASE misplaced-quantifier END *)

(* CASE nonapplication BEGIN *)
let nonapplication value =
  [%verocaml.requires
    forall (fun (x : int) -> ((true [@trigger]) && observed x))];
  value
(* CASE nonapplication END *)

(* CASE equality BEGIN *)
let equality value =
  [%verocaml.requires
    forall (fun (x : int) -> (((x = value) [@trigger]) || observed x))];
  value
(* CASE equality END *)

(* CASE selector BEGIN *)
let selector value =
  [%verocaml.requires
    forall (fun (x : box) -> (((x.item [@trigger]) = value) || true))];
  value
(* CASE selector END *)

(* CASE nullary BEGIN *)
let nullary_trigger value =
  [%verocaml.requires
    forall (fun (x : int) -> ((nullary () [@trigger]) || x = value))];
  value
(* CASE nullary END *)

(* CASE incomplete BEGIN *)
let incomplete value =
  [%verocaml.requires
    forall (fun (x : int) -> ((observed value) [@trigger]) || x = value)];
  value
(* CASE incomplete END *)

(* CASE wrong-binder BEGIN *)
let wrong_binder value =
  [%verocaml.requires
    forall (fun (x : int) ->
      exists (fun (other : int) -> other = x)
      || ((observed value) [@trigger]))];
  value
(* CASE wrong-binder END *)

(* CASE nested-owner BEGIN *)
let nested_owner value =
  [%verocaml.requires
    forall (fun (outer : int) ->
      exists (fun (inner : int) ->
        ((observed inner) [@trigger]) || outer = value))];
  value
(* CASE nested-owner END *)

(* CASE existential-trigger BEGIN *)
let existential_trigger value =
  [%verocaml.requires
    exists (fun (x : int) -> ((observed x) [@trigger]) || x = value)];
  value
(* CASE existential-trigger END *)
