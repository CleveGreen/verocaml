type successful_function = {
  definition : Sst.function_definition;
  evidence : Verification_proof_evidence_private.function_evidence;
  commit_ordinal : int;
}

type coordinator = {
  implementation : Cmt_input.implementation;
  program : Sst.program;
  validated : Sst_validation.validated_program;
  mutable successful : successful_function list;
  mutable unsuccessful : Sst.function_definition list;
  mutable next_commit_ordinal : int;
  mutable active : bool;
}

type t = {
  owner_artifact_full_key : string;
  program : Sst.program;
  validated : Sst_validation.validated_program;
  successful : successful_function list;
  dependency_closure : Numeric_proof_closure_walk_private.t;
  full_key : string;
}

let ( let* ) = Result.bind

let artifact_key = Imported_callable.artifact_full_key

let closure_matches closure ~implementation ~validated =
  String.equal closure.owner_artifact_full_key (artifact_key implementation)
  && closure.program == Sst_validation.program validated
  && closure.validated == validated

let matches = closure_matches

let authorizes_definition closure definition =
  Numeric_proof_closure_walk_private.authorizes_definition
    closure.dependency_closure definition

let function_evidence closure definition =
  if authorizes_definition closure definition then
    closure.successful
    |> List.find_opt (fun item -> item.definition == definition)
    |> Option.map (fun item -> item.evidence)
  else None

let dependency_evidence closure =
  Numeric_proof_closure_walk_private.dependency_evidence
    closure.dependency_closure

let full_key closure = closure.full_key

let eligible_law_root (definition : Sst.function_definition) =
  match (definition.mode, definition.body) with
  | Sst.Proof, (Sst.Proof_body _ | Sst.Trusted_external_body
                                  (Sst.Authenticated_external_body _)) ->
      true
  | _ -> false

module For_pipeline = struct
  let create ~implementation ~program ~validated =
    if program != Sst_validation.program validated then
      invalid_arg "prior-law coordinator requires the exact validated program";
    { implementation; program; validated; successful = []; unsuccessful = [];
      next_commit_ordinal = 0; active = true }

  let observe coordinator ~definition ~execution results =
    if not coordinator.active then
      invalid_arg "prior-law coordinator is closed";
    if
      not
        (List.exists (fun candidate -> candidate == definition)
           coordinator.program.functions)
      || execution.Vir.function_ref.function_index
         <> definition.Sst.function_id.function_index
      || not
           (String.equal execution.function_ref.function_name
              definition.function_id.function_name)
    then invalid_arg "prior-law function identity changed at coordinator commit";
    let exact_results =
      List.length results = List.length execution.obligations
      && List.for_all2
           (fun (result : Solver_backend.obligation_result) obligation ->
             String.equal
               (Vir_identity_private.obligation result.obligation)
               (Vir_identity_private.obligation obligation))
           results execution.obligations
    in
    let complete =
      exact_results
      && List.for_all
           (fun (result : Solver_backend.obligation_result) ->
             result.outcome = Solver_backend.Verified)
           results
    in
    let commit_ordinal = coordinator.next_commit_ordinal in
    coordinator.next_commit_ordinal <- commit_ordinal + 1;
    if complete then (
      let vir =
        { Vir.policy = coordinator.program.policy;
          rank_domains = Vir.rank_domains_of_validated coordinator.validated;
          trusted_external_body_declarations = [];
          functions = [ execution ] }
      in
      let captured =
        Verification_proof_evidence_private.For_pipeline.capture
          ~program:coordinator.program vir
      in
      let evidence =
        match Verification_proof_evidence_private.functions captured with
        | [ evidence ] -> evidence
        | [] | _ :: _ :: _ ->
            invalid_arg "prior-law coordinator evidence cardinality changed"
      in
      coordinator.successful <-
        { definition; evidence; commit_ordinal }
        :: coordinator.successful;
      [%log.debug "recorded successful prior-law function evidence"
        ~stage:(Delator.Field.string "numeric-prior-law-closure")
        ~function_name:
          (Delator.Field.string definition.function_id.function_name)
        ~obligations:(Delator.Field.int (List.length execution.obligations))
        ~decision:(Delator.Field.string "successful-scoped-evidence")])
    else (
      coordinator.unsuccessful <- definition :: coordinator.unsuccessful;
      [%log.error "refused unsuccessful prior-law function evidence"
        ~stage:(Delator.Field.string "numeric-prior-law-closure")
        ~function_name:
          (Delator.Field.string definition.function_id.function_name)
        ~exact_results:(Delator.Field.bool exact_results)
        ~decision:(Delator.Field.string "not-authority")])
  [@@delator.instrument] [@@delator.level debug]

  let complete coordinator ~root =
    let result =
      if not (eligible_law_root root) then
        Error "prior-law root is not a proof or authenticated axiom"
      else if List.exists (fun failed -> failed == root) coordinator.unsuccessful then
        Error "prior-law root did not verify"
      else
        match List.find_opt (fun item -> item.definition == root) coordinator.successful with
        | None -> Error "prior-law root is pending or absent"
        | Some root_item ->
            let successful =
              coordinator.successful
              |> List.filter (fun item ->
                     item.commit_ordinal <= root_item.commit_ordinal)
              |> List.sort (fun left right ->
                     Int.compare left.commit_ordinal right.commit_ordinal)
            in
            let successful_definition definition =
              List.exists (fun item -> item.definition == definition) successful
            in
            let evidence_for definition =
              successful
              |> List.find_opt (fun item -> item.definition == definition)
              |> Option.map (fun item -> item.evidence)
            in
            (match
               Numeric_proof_closure_walk_private.complete
                 ~implementation:coordinator.implementation
                 ~validated:coordinator.validated
                 ~successful:successful_definition ~evidence_for root
             with
            | Error reason ->
                Error
                  ("prior-law dependency closure rejected: "
                  ^ Numeric_proof_closure_walk_private.reason_name reason)
            | Ok dependency_closure ->
                let native_dependency =
                  List.find_opt
                    (fun item ->
                      Numeric_proof_closure_walk_private.authorizes_definition
                        dependency_closure item.definition
                      && Vir.function_execution_has_native_bv_projection
                           item.evidence.execution)
                    successful
                in
                let* () =
                  match native_dependency with
                  | None -> Ok ()
                  | Some item ->
                      [%log.error "refused native-assisted prior-law authority"
                        ~stage:
                          (Delator.Field.string "numeric-prior-law-closure")
                        ~function_name:
                          (Delator.Field.string
                             item.definition.function_id.function_name)
                        ~decision:
                          (Delator.Field.string
                             "native-proof-is-not-bootstrap-authority")];
                      Error
                        "prior-law dependency closure used a native BV projection"
                in
                let full_key =
                  Numeric_receipt_private.encode
                    ~schema:"verocaml.numeric-prior-law-closure.v2"
                    [ artifact_key coordinator.implementation;
                      string_of_int root.function_id.function_index;
                      root.function_id.function_name;
                      Numeric_proof_closure_walk_private.full_key
                        dependency_closure ]
                in
                Ok
                  { owner_artifact_full_key =
                      artifact_key coordinator.implementation;
                    program = coordinator.program;
                    validated = coordinator.validated;
                    successful;
                    dependency_closure;
                    full_key })
    in
    [%log.debug "completed scoped prior-law authority request"
      ~stage:(Delator.Field.string "numeric-prior-law-closure")
      ~function_name:(Delator.Field.string root.function_id.function_name)
      ~successful_functions:
        (Delator.Field.int
           (match result with Ok closure -> List.length closure.successful | Error _ -> 0))
      ~dependency_functions:
        (Delator.Field.int
           (match result with
           | Ok closure ->
               Numeric_proof_closure_walk_private.dependency_count
                 closure.dependency_closure
           | Error _ -> 0))
      ~trusted_dependencies:
        (Delator.Field.int
           (match result with
           | Ok closure ->
               Numeric_proof_closure_walk_private.trusted_dependency_count
                 closure.dependency_closure
           | Error _ -> 0))
      ~whole_program_completion:(Delator.Field.bool false)
      ~decision:
        (Delator.Field.string
           (match result with Ok _ -> "issued" | Error _ -> "rejected"))];
    result
  [@@delator.instrument] [@@delator.level debug]

  let close coordinator = coordinator.active <- false
end
