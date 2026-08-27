val obligation_definition :
  resolved_path:string ->
  opaque_binding_id:int ->
  Sst.function_definition ->
  (Sst.function_definition, string) result
