(** IR-agnostic rewriting with explicit traversal, bounded repetition, and
    phase-level telemetry.

    A rule is responsible for semantic preservation. In a repeated phase it
    must also return [None] when it has no real change to make; otherwise the
    phase can consume all of its pass or rewrite budget without stabilizing. *)

type ('node, 'replacement) rule = 'node -> 'replacement option

type direction = Bottom_up | Top_down

type repetition =
  | Once
  | Until_stable of { max_passes : int }

type ('node, 'replacement) phase = {
  direction : direction;
  repetition : repetition;
  (** Optional traversal bound for each complete phase, shared across repeated
      passes. Top-down phases require a finite bound because a rule may expose
      fresh descendants indefinitely within one pass. *)
  max_nodes : int option;
  (** [None] is unlimited. [Some n] bounds successful replacements across all
      passes in this phase. This is a resource guard, not a proof that an
      unfolding rule is semantically terminating. *)
  max_rewrites : int option;
  rules : ('node, 'replacement) rule list;
}

type phase_stats = {
  passes : int;
  nodes_visited : int;
  rule_attempts : int;
  rewrites : int;
  stabilized : bool;
  (** True when a changing pass consumed the configured [max_passes], so the
      phase stopped without demonstrating a fixpoint. *)
  pass_limit_reached : bool;
  (** True only when work was skipped because [max_rewrites] was exhausted. A
      phase that performs exactly the last permitted rewrite at its final node
      need not report exhaustion. *)
  rewrite_budget_exhausted : bool;
  (** True only when traversal attempted to visit a node after consuming the
      configured [max_nodes]. *)
  node_budget_exhausted : bool;
}

(** Apply every rule once, in declaration order. A successful replacement is
    passed to the remaining rules. *)
val apply_rules :
  replace:('node -> 'replacement -> 'node) ->
  rules:('node, 'replacement) rule list ->
  'node ->
  'node

(** Rewrite children before their parent. *)
val bottom_up :
  map_children:(('node -> 'node) -> 'node -> 'node) ->
  replace:('node -> 'replacement -> 'node) ->
  rules:('node, 'replacement) rule list ->
  'node ->
  'node

(** Rewrite a parent before traversing the descendants of its replacement. *)
val top_down :
  max_nodes:int ->
  map_children:(('node -> 'node) -> 'node -> 'node) ->
  replace:('node -> 'replacement -> 'node) ->
  rules:('node, 'replacement) rule list ->
  'node ->
  'node

val apply_phase_with_stats :
  map_children:(('node -> 'node) -> 'node -> 'node) ->
  replace:('node -> 'replacement -> 'node) ->
  ('node, 'replacement) phase ->
  'node ->
  'node * phase_stats

val apply_phase :
  map_children:(('node -> 'node) -> 'node -> 'node) ->
  replace:('node -> 'replacement -> 'node) ->
  ('node, 'replacement) phase ->
  'node ->
  'node

val apply_phases_with_stats :
  map_children:(('node -> 'node) -> 'node -> 'node) ->
  replace:('node -> 'replacement -> 'node) ->
  ('node, 'replacement) phase list ->
  'node ->
  'node * phase_stats list

val apply_phases :
  map_children:(('node -> 'node) -> 'node -> 'node) ->
  replace:('node -> 'replacement -> 'node) ->
  ('node, 'replacement) phase list ->
  'node ->
  'node
