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

let literal_depth ~span:_ literal =
  let parsed =
    try Some (Z.of_string literal) with Invalid_argument _ -> None
  in
  match parsed with
  | Some depth
    when Z.sign depth >= 0 && Z.compare depth (Z.of_int 64) <= 0 ->
      Ok (Z.to_int depth)
  | Some _ | None ->
      Error "reveal_with_fuel depth must be a literal in 0..64"
