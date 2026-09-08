type declaration_kind =
  | Completed_proof_declaration
  | Explicit_axiom_declaration
  | Ordinary_specification_declaration

type t = private {
  role : Cmt_input.interface_numeric_role;
  definition : Sst.function_definition;
  callable_definition : Sst.function_definition option;
  kind : declaration_kind;
}

(** Correlates local semantic declarations with their exact retained source
    definitions. This is pre-solver source identity only: it establishes no
    proof success, axiom authority, runtime refinement, or lowering authority. *)
val correlate_local :
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  (t list, string) result

val selects_explicit_axiom : t list -> Sst.function_definition -> bool
