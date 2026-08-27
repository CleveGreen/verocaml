val signature :
  definition:Sst.function_definition ->
  parameter_kinds:Parametric_signature_private.parameter_kind list ->
  parameter_modes:Sst.instance_mode list ->
  result_mode:Sst.instance_mode ->
  provider_completion:Verified_provider_completion_private.t ->
  (Parametric_signature_private.t, string) result
