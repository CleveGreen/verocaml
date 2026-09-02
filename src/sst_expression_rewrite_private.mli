(** SST specialization of {!Rewrite_private}. Replacing an expression changes
    only its [expression_desc]; the authoritative root type and source span are
    retained by the adapter. Rules that synthesize descendants remain
    responsible for assigning valid types and spans to those descendants. *)

type rule =
  (Sst.expression, Sst.expression_desc) Rewrite_private.rule

(** SST phases add one solver-shape policy to the generic phase controls.
    Setting [rewrite_quantifier_triggers] to [false] prevents descendant
    traversal into explicit trigger expressions while still rewriting the
    quantifier body. Rules that replace the quantifier node itself remain
    responsible for preserving its trigger. This policy is useful when
    e-matching heads are more valuable than normalized trigger arithmetic. *)
type phase = {
  direction : Rewrite_private.direction;
  repetition : Rewrite_private.repetition;
  max_nodes : int option;
  max_rewrites : int option;
  rules : rule list;
  rewrite_quantifier_triggers : bool;
}

val apply_rules : rules:rule list -> Sst.expression -> Sst.expression
val bottom_up : rules:rule list -> Sst.expression -> Sst.expression
val top_down :
  max_nodes:int -> rules:rule list -> Sst.expression -> Sst.expression

val apply_phase_with_stats :
  phase -> Sst.expression -> Sst.expression * Rewrite_private.phase_stats

val apply_phase : phase -> Sst.expression -> Sst.expression

val apply_phases_with_stats :
  phase list ->
  Sst.expression ->
  Sst.expression * Rewrite_private.phase_stats list

val apply_phases : phase list -> Sst.expression -> Sst.expression
