(** Bounded, solver-oriented simplification for SST mathematical integers.

    The module deliberately does not rewrite [Sst.Int]. Runtime OCaml integers
    have target-width overflow and checked-arithmetic behaviour; [Z.t] ring
    identities are applied only to [Sst.Mathematical_int], whose intended
    semantics are unbounded mathematical integers. *)

type ring_mode =
  | Ring_disabled
  (** Keep local identities and constant folding, but do not build a
      polynomial representation. *)
  | Ring_reduce_only
  (** Emit a polynomial form only when the solver-cost measure strictly
      improves, with nonlinear-to-linear reduction taking priority. *)
  | Ring_canonicalize_bounded
  (** Permit bounded distribution/canonicalization within [ring_output_slack]
      when polynomial collection actually cancels terms. Pure expansion, and
      expansion that merely combines coefficients, is rejected unless the
      solver-cost measure already strictly improves. *)

type application_policy =
  | Preserve_applications
  (** Treat calls and symbolic applications as ring barriers. This is the
      default when preserving e-matching shape is more valuable. *)
  | Treat_pure_specifications_as_atoms
  (** Admit specification calls and symbolic applications as pure ring atoms.
      Such atoms remain protected from complete elimination because their call
      sites may carry verifier obligations. This policy permits reordering and
      combining, and—when separately enabled—duplication. *)

type atom_class = Cheap_atom | Heavy_atom

type config = {
  ring_mode : ring_mode;
  application_policy : application_policy;
  (** Extends the pure/total assumption to recursive specification calls. Use
      only where the logical encoding gives those calls total semantics. *)
  allow_recursive_specification_atoms : bool;
  (** Permit emitted polynomial syntax to contain more occurrences of a
      heavy atom (calls, conditionals, absolute values) than the input. When
      false, nonlinear cancellation is still allowed as long as it neither
      duplicates nor completely eliminates those atoms. Complete elimination
      is rejected under either setting so call-site obligations remain visible.
      Disabled in both built-in profiles. *)
  allow_heavy_atom_duplication : bool;
  (** Extension hook for domain-specific atoms. Returning [Some _] certifies
      that the complete expression is pure and total; [Cheap_atom] also
      authorizes bounded duplication, whereas [Heavy_atom] is protected by the
      occurrence gate above. Custom atoms are never completely eliminated
      unless [additional_obligation_free_atom] separately authorizes it. The
      classifier must be deterministic and cheap.
      Physically distinct custom atoms are kept distinct unless built-in
      structural equality or [additional_semantic_atom_equal] recognizes them. *)
  additional_pure_total_atom : Sst.expression -> atom_class option;
  (** Optional trusted erasure authority for custom atoms admitted above.
      Returning [true] certifies that eliminating every occurrence cannot hide
      a call-site or proof obligation. The built-in default is always false. *)
  additional_obligation_free_atom : Sst.expression -> bool;
  (** Optional semantic equality for domain-specific admitted atoms. [Some
      true] is a trusted proof obligation: conflating unequal atoms can make
      cancellation unsound. The result must be deterministic, symmetric, and
      consistent with the pure/total classifier; [None] uses built-in matching. *)
  additional_semantic_atom_equal :
    Sst.expression -> Sst.expression -> bool option;
  (** Rewrite explicit quantifier triggers as well as quantifier bodies. The
      e-matching-friendly profile disables this; the aggressive profile
      enables it. *)
  rewrite_quantifier_triggers : bool;
  normalize_comparisons : bool;
  normalize_absolute_value : bool;
  (** Bound the legacy recursive standalone constant evaluator. The default
      bottom-up hot path evaluates only immediate literal operands. *)
  max_constant_nodes : int;
  (** Maximum bit width of a newly computed Zarith constant/coefficient. *)
  max_constant_bits : int;
  max_ring_atoms : int;
  (** Maximum mathematical-arithmetic syntax nodes parsed by one polynomial
      candidate. This caps repeated failed attempts on long arithmetic chains. *)
  max_ring_input_nodes : int;
  (** Shared fuel for purity certification and structural atom equality within
      one polynomial candidate. Standalone certifications use the same value
      as their individual cap. *)
  max_ring_inspections : int;
  max_ring_monomials : int;
  max_ring_degree : int;
  (** Maximum accumulated Cartesian-product work while multiplying
      polynomials. *)
  max_ring_products : int;
  (** Maximum estimated operator-plus-term nodes in emitted ring syntax. *)
  max_ring_output_nodes : int;
  (** Additional emitted nodes tolerated by bounded canonicalization when
      polynomial collection cancelled terms but the static solver cost measure
      does not yet show a strict improvement. *)
  ring_output_slack : int;
  (** Maximum SST nodes visited across all fixpoint passes in one phase. *)
  max_phase_nodes : int;
  max_fixpoint_passes : int;
  max_rewrites : int option;
}

val e_matching_friendly_config : config
val aggressive_config : config
val default_config : config

(** Exact recursive evaluation of a closed mathematical-integer arithmetic
    subtree. This compatibility API is intentionally unbounded; production
    simplification uses the configured bounded paths. *)
val constant_value : Sst.expression -> Z.t option

(** Configured bounded evaluation for clients that need resource-controlled
    standalone constant analysis. *)
val constant_value_with : config -> Sst.expression -> Z.t option

val fold_constant_arithmetic : Sst_expression_rewrite_private.rule
val normalize_constant_multiplication : Sst_expression_rewrite_private.rule
val simplify_arithmetic_identities : Sst_expression_rewrite_private.rule
val normalize_ring_arithmetic : Sst_expression_rewrite_private.rule
val fold_constant_comparison : Sst_expression_rewrite_private.rule
val normalize_mathematical_comparison : Sst_expression_rewrite_private.rule

(** Rule lists contain rewrite logic only; the quantifier-trigger traversal
    policy is carried by {!simplification_phase}. *)
val rules : config -> Sst_expression_rewrite_private.rule list
val default_rules : Sst_expression_rewrite_private.rule list

(** A composable bottom-up simplification phase. A client can place bounded
    top-down specification-unfolding phases before this phase. *)
val simplification_phase : config -> Sst_expression_rewrite_private.phase

val simplify_with_stats :
  config ->
  Sst.expression ->
  Sst.expression * Rewrite_private.phase_stats

val simplify_with : config -> Sst.expression -> Sst.expression
val simplify : Sst.expression -> Sst.expression
