type result_interpretation = Mathematical_result | Lifted_runtime_result | Boolean_result | Carrier_result

type unsigned_range = private {
  declaration : Numeric_semantics_correlation_private.t;
  target : Build_target_profile_private.instance;
  view : Sst.function_definition;
  result_interpretation : result_interpretation;
  parameter : Sst.binding;
  lower_bound : Sst.ensures_clause;
  upper_bound : Sst.ensures_clause;
  semantic_width : int;
  semantic_width_evidence : string;
}

(** Matches an unconditional [0 <= view x < 2^width] contract using the exact
    compiler-correlated callable and derives the semantic width from the
    authenticated mathematical upper-bound statement.
    This is statement matching, not independent law admission: the completed
    provider's proof/trust context is not discharged here. It grants no runtime
    refinement or lowering capability. Unmatched declarations retain their
    ordinary specification/proof-call behavior.

    The initial matcher handles local, monomorphic unary scalar specification
    views and nonrecursive unit proofs/axioms, conjunctive postconditions, and
    bounded mathematical constant evaluation. The view result must be either
    mathematical Int or an explicit logical lift of its runtime-int result;
    the interpretation remains part of the matched statement. *)
val unsigned_range :
  target:Build_target_profile_private.instance ->
  Numeric_semantics_correlation_private.t -> unsigned_range option

type signed_view = private {
  declaration : Numeric_semantics_correlation_private.t;
  unsigned : unsigned_range;
  view : Sst.function_definition;
  result_interpretation : result_interpretation;
  parameter : Sst.binding;
  relation : Sst.ensures_clause;
  semantic_width : int;
  semantic_width_evidence : string;
}

(** Matches the unconditional mathematical relation
    [s x = if u x < 2^(w-1) then u x else u x - 2^w], preserving the exact
    unsigned view and clause witness. This only matches a statement; it does
    not admit either law or a runtime implementation. *)
val signed_view :
  unsigned:unsigned_range ->
  Numeric_semantics_correlation_private.t -> signed_view option

type statement = Unsigned of unsigned_range | Signed of signed_view
val declaration : statement -> Numeric_semantics_correlation_private.t
val views : statement -> Sst.function_definition list
