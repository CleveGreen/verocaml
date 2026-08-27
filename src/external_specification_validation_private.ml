type role = Wrapper | Target

type error = { span : Diagnostic.span; message : string }

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let validate ~external_specifications ~program ~functions ~definition ~role link =
  let invalid span message = Error { span; message } in
  match link with
  | Sst.Unresolved_target { witness_span; _ } ->
      invalid witness_span
        "external specifications require an authenticated same-unit target"
  | Sst.Imported_unverified_target { witness_span; _ } ->
      External_target_specification_private.validate_definition
        external_specifications ~program ~definition
        ~expected_wrapper:(role = Wrapper) link
      |> Result.map_error (fun message -> { span = witness_span; message })
  | Sst.Same_unit_target
      { wrapper; target; target_span; declaration_span; witness_span } ->
      let find id =
        List.find_opt
          (fun candidate -> candidate.Sst.function_id = id)
          functions
      in
      let* wrapper_definition =
        match find wrapper with
        | Some wrapper -> Ok wrapper
        | None -> invalid witness_span "external specification wrapper is absent"
      in
      let* target_definition =
        match find target with
        | Some target -> Ok target
        | None -> invalid witness_span "external specification target is absent"
      in
      let same_link = function
        | Sst.Same_unit_target
            {
              wrapper = candidate_wrapper;
              target = candidate_target;
              target_span = candidate_target_span;
              declaration_span = candidate_declaration_span;
              witness_span = candidate_witness_span;
            } ->
            candidate_wrapper = wrapper && candidate_target = target
            && candidate_target_span = target_span
            && candidate_declaration_span = declaration_span
            && candidate_witness_span = witness_span
        | Sst.Imported_unverified_target _ | Sst.Unresolved_target _ -> false
      in
      let wrapper_matches =
        wrapper_definition.function_id = wrapper
        && wrapper_definition.mode = Sst.Exec
        && wrapper_definition.span = declaration_span
        &&
        match wrapper_definition.body with
        | Sst.External_specification candidate -> same_link candidate
        | _ -> false
      and target_matches =
        target_definition.function_id = target
        && target_definition.mode = Sst.Exec
        && target_definition.span = target_span
        &&
        match target_definition.body with
        | Sst.Trusted_external_spec_target candidate -> same_link candidate
        | _ -> false
      in
      let role_matches =
        match role with
        | Wrapper -> definition.function_id = wrapper
        | Target -> definition.function_id = target
      and shared_signature =
        wrapper_definition.parameters = target_definition.parameters
        && wrapper_definition.result_type = target_definition.result_type
        && wrapper_definition.contracts = target_definition.contracts
      in
      if wrapper = target then
        invalid witness_span "external specification wrapper cannot specify itself"
      else if target.function_index >= wrapper.function_index then
        invalid declaration_span
          "same-unit external target must precede its specification wrapper"
      else if not role_matches then
        invalid definition.span
          "external specification link is attached to the wrong declaration"
      else if (not wrapper_matches) || not target_matches then
        invalid witness_span
          "external specification wrapper/target linkage is not symmetric"
      else if not shared_signature then
        invalid witness_span
          "external specification wrapper and target signatures/contracts differ"
      else Ok ()
