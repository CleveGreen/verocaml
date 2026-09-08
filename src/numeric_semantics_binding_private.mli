type declaration_kind = Numeric_semantics_correlation_private.declaration_kind =
  | Completed_proof_declaration
  | Explicit_axiom_declaration
  | Ordinary_specification_declaration

type t = Numeric_semantics_correlation_private.t = private {
  role : Cmt_input.interface_numeric_role;
  definition : Sst.function_definition;
  callable_definition : Sst.function_definition option;
  kind : declaration_kind;
}

(** Correlates local semantic declarations with their exact completed provider.
    A checked declaration is not evidence that its contract establishes the
    advertised numeric role, nor evidence of runtime implementation refinement.
    Imported semantics must be correlated at their original completed owner. *)
val complete_local :
  completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  (t list, string) result

val selects_explicit_axiom : t list -> Sst.function_definition -> bool
