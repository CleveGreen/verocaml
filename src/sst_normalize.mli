(* Authenticated callback bindings and certificates remain opaque and retain
   identity across every constructor below; only endpoint expressions are
   normalized. *)

val authenticated_checked_exec :
  source_file:string ->
  function_id:Sst.function_id ->
  recursive:bool ->
  parameters:Sst.parameter list ->
  contracts:Sst.contracts ->
  body:Sst.expression ->
  result_type:Sst.typ ->
  returns_unique_parameter:int option ->
  span:Sst.span ->
  Sst.function_definition

val checked_exec_raw :
  function_id:Sst.function_id ->
  recursive:bool ->
  parameters:Sst.parameter list ->
  contracts:Sst.contracts ->
  body:Sst.expression ->
  result_type:Sst.typ ->
  returns_unique_parameter:int option ->
  span:Sst.span ->
  Sst.function_definition

val authenticated_spec_definition :
  function_id:Sst.function_id ->
  parameters:Sst.parameter list ->
  body:Sst.expression ->
  result_type:Sst.typ ->
  span:Sst.span ->
  Sst.function_definition

val classify_typedtree_program : Sst.program -> Sst.program

val classify_expression :
  callee_mode:(Sst.function_id -> Sst.verification_mode option) ->
  Sst.expression_stage ->
  Sst.expression ->
  Sst.expression

val authenticated_proof_definition :
  source_file:string ->
  function_id:Sst.function_id ->
  parameters:Sst.parameter list ->
  contracts:Sst.contracts ->
  body:Sst.expression ->
  span:Sst.span ->
  Sst.function_definition

val authenticated_external_specification :
  wrapper_id:Sst.function_id ->
  target_id:Sst.function_id ->
  parameters:Sst.parameter list ->
  contracts:Sst.contracts ->
  result_type:Sst.typ ->
  wrapper_span:Sst.span ->
  witness_span:Sst.span ->
  target_span:Sst.span ->
  Sst.function_definition * Sst.function_definition

val authenticated_trusted_external_body :
  source_file:string ->
  function_id:Sst.function_id ->
  parameters:Sst.parameter list ->
  contracts:Sst.contracts ->
  result_type:Sst.typ ->
  returns_unique_parameter:int option ->
  declaration_span:Sst.span ->
  witness_span:Sst.span ->
  Sst.function_definition

val authenticated_trusted_external_body_in_mode :
  source_file:string ->
  function_id:Sst.function_id ->
  mode:Sst.verification_mode ->
  parameters:Sst.parameter list ->
  contracts:Sst.contracts ->
  result_type:Sst.typ ->
  returns_unique_parameter:int option ->
  declaration_span:Sst.span ->
  witness_span:Sst.span ->
  Sst.function_definition
