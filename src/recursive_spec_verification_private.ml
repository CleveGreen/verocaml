type error = {
  span : Diagnostic.span;
  message : string;
}
type prepared = Spec_unfolding_private.prepared
type verified = Verified of {
  prepared : Spec_unfolding_private.prepared;
  preservation_capabilities : Recursive_spec_preservation.capability list;
}
type solver_controls = { force_retry_unknown : bool; force_retry_counterexample : bool; suppress_original_activation : bool; suppress_reached_authority : bool }
let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error
let fail span format =
  Printf.ksprintf (fun message -> Error { span; message }) format
let of_logic = function
  | Ok value -> Ok value
  | Error error -> fail error.Logic_ir.span "%s" (Logic_ir.error_to_string error)
let of_backend span = function
  | Ok value -> Ok value
  | Error error -> fail span "%s" (Z3_bridge.error_to_string error)
let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name
let seal_verified prepared =
  let validated = Spec_unfolding_private.validated_program prepared in
  let program = Sst_validation.program validated in
  let rank_domains = Vir.rank_domains_of_validated validated in
  let obligations = Spec_unfolding_private.termination_obligations prepared in
  let* preservation_capabilities =
    List.fold_left
      (fun result prepared_definition ->
        let* capabilities = result in
        let id = Spec_unfolding_private.definition_id prepared_definition in
        let* definition =
          match
            List.find_opt
              (fun definition ->
                same_function_id definition.Sst.function_id id)
              program.Sst.functions
          with
          | Some definition -> Ok definition
          | None ->
              fail (Spec_unfolding_private.definition_span prepared_definition)
                "verified recursive definition lost exact program identity"
        in
        let* helper_closure_snapshot =
          match
            Typedtree_adapter_private.Public.recursive_helper_closure_snapshot
              ~program ~definition
          with
          | Ok snapshot -> Ok snapshot
          | Error message -> fail definition.span "%s" message
        in
        let termination_obligations =
          List.filter
            (fun (obligation : Vir.obligation) ->
              obligation.Vir.function_ref.function_index
                = id.function_index
              && String.equal obligation.function_ref.function_name
                   id.function_name)
            obligations
        in
        match
          Recursive_spec_preservation.analyze ~program ~definition
            ~expanded_body:
              (Spec_unfolding_private.definition_body prepared_definition)
            ~helper_closure_snapshot ~rank_domains ~termination_obligations
        with
        | Ok None -> Ok capabilities
        | Ok (Some capability) -> Ok (capability :: capabilities)
        | Error message -> fail definition.span "%s" message)
      (Ok []) (Spec_unfolding_private.definitions prepared)
  in
  Ok
    (Verified
       { prepared; preservation_capabilities = List.rev preservation_capabilities })
let prepare program =
  match
    Sst_validation_private.Public.with_owned_contents_helpers_hidden (fun () ->
        Spec_unfolding_private.prepare program)
  with
  | Ok prepared -> Ok prepared
  | Error error ->
      fail (Diagnostic.file_span "<recursive-spec>") "%s"
        (Spec_unfolding_private.error_to_string error)
let has_definitions prepared =
  Spec_unfolding_private.definitions prepared <> []
let termination_obligation_count prepared =
  List.length (Spec_unfolding_private.termination_obligations prepared)
let required_features =
  [
    Logic_ir.Named_sorts;
    Logic_ir.Uninterpreted_functions;
    Logic_ir.Linear_integer_arithmetic;
    Logic_ir.Quantifiers;
    Logic_ir.Explicit_patterns;
    Logic_ir.Quantifier_ids;
  ]
type verification_outcome =
  | Verification_verified of verified
  | Verification_inconclusive of Solver_backend.obligation_result
let solver_reason = function
  | Z3_bridge.Resource_exhausted -> Solver_backend.Resource_exhausted
  | Timed_out -> Timed_out
  | Backend_unknown reason -> Backend_unknown reason
let termination_inconclusive solver_policy obligation reason =
  Verification_inconclusive
    {
      Solver_backend.obligation;
      outcome =
        Inconclusive
          {
            configured_timeout_ms =
              Solver_policy_private.timeout_ms solver_policy;
            configured_rlimit = Solver_policy_private.rlimit solver_policy;
            reason = solver_reason reason;
          };
    }
let verify_with_policy_and_requirements solver_policy requirements prepared =
  let span =
    match Spec_unfolding_private.definitions prepared with
    | definition :: _ -> Spec_unfolding_private.definition_span definition
    | [] -> Diagnostic.file_span "<recursive-spec>"
  in
  let* () = of_backend span (Z3_bridge.preflight requirements) in
  let obligations =
    Spec_unfolding_private.termination_obligations prepared
  in
  if List.exists Vir.obligation_has_structural_rank obligations then
    let config =
      {
        Z3_bridge.timeout_ms =
          Solver_policy_private.timeout_ms solver_policy;
        model = true;
      }
    in
    let rec verify = function
      | [] -> Result.map (fun verified -> Verification_verified verified)
                  (seal_verified prepared)
      | obligation :: rest ->
          let* outcome =
            of_backend obligation.Vir.span
              (Z3_bridge.solve_vir
                 ~rlimit:(Solver_policy_private.rlimit solver_policy)
                 ~requires:requirements config obligation)
          in
          (match outcome with
          | Z3_bridge.Verified -> verify rest
          | Counterexample _ ->
              fail span
                "recursive specification termination obligations did not all \
                 verify"
          | Inconclusive reason ->
              Ok (termination_inconclusive solver_policy obligation reason))
    in
    verify obligations
  else
    let* config =
      match
        Solver_backend.config_with_rlimit
          ~rlimit:(Solver_policy_private.rlimit solver_policy)
          ~timeout_ms:(Solver_policy_private.timeout_ms solver_policy)
      with
      | Ok config -> Ok config
      | Error error -> fail span "%s" (Solver_backend.error_to_string error)
    in
    let* results =
      match Solver_backend.solve_in_order config obligations with
      | Ok results -> Ok results
      | Error error -> fail span "%s" (Solver_backend.error_to_string error)
    in
    match List.find_opt
      (fun result ->
        match result.Solver_backend.outcome with
        | Solver_backend.Verified -> false
        | Counterexample _ | Inconclusive _ -> true)
      results
    with
    | None ->
        Result.map (fun verified -> Verification_verified verified)
          (seal_verified prepared)
    | Some ({ outcome = Inconclusive _; _ } as result) ->
        Ok (Verification_inconclusive result)
    | Some { outcome = Counterexample _; _ } ->
        fail span
          "recursive specification termination obligations did not all verify"
    | Some { outcome = Verified; _ } -> assert false
let resolve_solver_policy span ?rlimit ~timeout_ms () =
  let result =
    match rlimit with
    | None -> Solver_policy_private.create_default ~timeout_ms
    | Some rlimit -> Solver_policy_private.create ~timeout_ms ~rlimit
  in
  match result with
  | Ok policy -> Ok policy
  | Error error ->
      fail span "%s" (Solver_policy_private.error_to_string error)
let verify ?rlimit ~timeout_ms prepared =
  let span =
    match Spec_unfolding_private.definitions prepared with
    | definition :: _ -> Spec_unfolding_private.definition_span definition
    | [] -> Diagnostic.file_span "<recursive-spec>"
  in
  let* solver_policy = resolve_solver_policy span ?rlimit ~timeout_ms () in
  let* outcome =
    verify_with_policy_and_requirements solver_policy required_features prepared
  in
  match outcome with
  | Verification_verified verified -> Ok verified
  | Verification_inconclusive _ ->
      fail span
        "recursive specification termination obligations did not all verify"
let verify_for_preflight ?rlimit ~timeout_ms prepared =
  let span =
    match Spec_unfolding_private.definitions prepared with
    | definition :: _ -> Spec_unfolding_private.definition_span definition
    | [] -> Diagnostic.file_span "<recursive-spec>"
  in
  let* solver_policy = resolve_solver_policy span ?rlimit ~timeout_ms () in
  verify_with_policy_and_requirements solver_policy required_features prepared
let verify_with_requirements ?rlimit ~timeout_ms requirements prepared =
  let span =
    match Spec_unfolding_private.definitions prepared with
    | definition :: _ -> Spec_unfolding_private.definition_span definition
    | [] -> Diagnostic.file_span "<recursive-spec>"
  in
  let* solver_policy = resolve_solver_policy span ?rlimit ~timeout_ms () in
  let* outcome =
    verify_with_policy_and_requirements solver_policy requirements prepared
  in
  match outcome with
  | Verification_verified verified -> Ok verified
  | Verification_inconclusive _ ->
      fail span
        "recursive specification termination obligations did not all verify"
let prepared (Verified verified) = verified.prepared
let preservation_capabilities (Verified verified) =
  verified.preservation_capabilities
let definition verified function_id =
  match
    List.find_opt
      (fun definition ->
        same_function_id (Spec_unfolding_private.definition_id definition) function_id)
      (Spec_unfolding_private.definitions (prepared verified))
  with
  | Some definition -> Ok definition
  | None ->
      fail (Diagnostic.file_span "<recursive-spec>")
        "recursive specification %s#%d is absent from verified authority"
        function_id.function_name function_id.function_index
let definition_ids verified =
  List.map Spec_unfolding_private.definition_id
    (Spec_unfolding_private.definitions (prepared verified))
