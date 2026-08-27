type activation = {
  function_id : Sst.function_id;
  depth : int;
  span : Diagnostic.span;
}

type proof_path = {
  proof_function : Sst.function_id;
  path_index : int;
  activations : activation list;
}

val literal_depth :
  span:Diagnostic.span -> string -> (int, string) result
