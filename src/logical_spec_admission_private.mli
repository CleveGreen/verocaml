val admit :
  validated:Sst_validation.validated_program ->
  type_definitions:Sst.type_definition list ->
  classify:(
    Sst.function_id -> Logical_spec_capability_private.call_target) ->
  root_identity:Logical_spec_capability_private.root_identity ->
  Sst.expression ->
  (Logical_spec_capability_private.permit, string) result

type disposition =
  | Eligible of Logical_spec_capability_private.permit
  | Abstain of string

type registry_entry = {
  root : Sst.expression;
  identity : Logical_spec_capability_private.root_identity;
  disposition : disposition;
}

val contract_identity :
  Sst.function_definition ->
  Logical_spec_capability_private.contract_clause_kind ->
  int ->
  Logical_spec_capability_private.root_identity

val prepare :
  validated:Sst_validation.validated_program ->
  type_definitions:Sst.type_definition list ->
  classify:(
    Sst.function_id -> Logical_spec_capability_private.call_target) ->
  excluded_contract:(Sst.function_definition -> bool) ->
  authority_snapshot:string ->
  invariant_roots:(
    Logical_spec_capability_private.root_identity
    * Sst.expression
    * string option)
    list ->
  registry_entry list

val model_permit : registry_entry list -> Logical_spec_capability_private.permit option
val run_capability_controls :
  registry_entry list -> Sst_validation.validated_program -> unit

module For_testing : sig
  val observe_program_candidates :
    validated:Sst_validation.validated_program ->
    type_definitions:Sst.type_definition list ->
    classify:(
      Sst.function_id -> Logical_spec_capability_private.call_target) ->
    unit
end
