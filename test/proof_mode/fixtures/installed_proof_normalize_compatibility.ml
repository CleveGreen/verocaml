let span = Diagnostic.file_span "installed_proof_normalize_compatibility.ml"

let function_id =
  Sst.{ function_index = 0; function_name = "predecessor_call_shape" }

let body =
  Sst.{ expression_desc = Unit_constant; typ = Unit; span }

let definition =
  Sst_normalize.authenticated_proof_definition
    ~source_file:"installed_proof_normalize_compatibility.ml" ~function_id
    ~parameters:[] ~contracts:Sst.empty_contracts ~body ~span

let () =
  if definition.Sst.recursive then
    failwith "public proof normalizer changed its nonrecursive behavior"
