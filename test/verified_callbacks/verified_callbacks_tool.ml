let () = ignore Verified_callbacks_prerequisites.ready

let fail format = Printf.ksprintf (fun message -> failwith message) format

let load_implementation filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s at %s" diagnostic.Diagnostic.code diagnostic.span.file

let configuration threads =
  match
    Verifier_service.configuration ~threads ~timeout_ms:5_000
      ~rlimit:(Some 100_000)
  with
  | Ok configuration -> configuration
  | Error error ->
      fail "%s" (Verifier_service.configuration_error_message error)

let verify filename threads =
  Verifier_service.request ~configuration:(configuration threads)
    ~consumer:(load_implementation filename) ~dependencies:[]
  |> Verifier_service.verify

let require_result filename = function
  | Ok result -> result
  | Error error ->
      fail "%s: %s" filename (Verifier_service.error_message error)

let status = function
  | Verifier_service.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete-source"

let count_substring needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec count total offset =
    if offset + needle_length > text_length then total
    else if String.sub text offset needle_length = needle then
      count (total + 1) (offset + needle_length)
    else count total (offset + 1)
  in
  if needle_length = 0 then 0 else count 0 0

let inspect filename =
  let result = require_result filename (verify filename 1) in
  let vir = Verifier_service.vir result in
  let rendered_sst = Verifier_service.semantic_sst result in
  let rendered_vir = Vir.to_string vir in
  let reached =
    List.fold_left
      (fun total execution ->
        total + List.length execution.Vir.reached_callback_calls)
      0 vir.functions
  in
  let callback_preconditions =
    count_substring "callback-precondition" rendered_vir
  in
  let abstract_relations = count_substring "callback-requires" rendered_vir in
  let cloned_apply =
    List.fold_left
      (fun total execution ->
        if
          String.equal execution.Vir.function_ref.function_name "apply"
          || String.equal execution.function_ref.function_name "relay"
        then total + 1
        else total)
      0 vir.functions
  in
  Printf.printf
    "callbacks reached=%d preconditions=%d relations=%d helpers=%d \
     function-sort=%d body-equation=%d\n"
    reached callback_preconditions abstract_relations cloned_apply
    (count_substring "function-sort" rendered_sst
    + count_substring "function-sort" rendered_vir)
    (count_substring "callback-body-equation" rendered_vir)

let rec result_symbols = function
  | Vir.Unit_result -> []
  | Integer_result symbol | Boolean_result symbol | Aggregate_result symbol
  | Parametric_result symbol ->
      [ symbol ]
  | Tuple_result values -> List.concat_map result_symbols values

let same_application left right =
  Sst_callback_private.same_binding left.Vir.callback right.Vir.callback
  && left.arguments = right.arguments

let same_schema_application left right =
  left.Vir.callback.callback_id = right.Vir.callback.callback_id
  && String.equal left.callback.callback_name right.callback.callback_name
  && Callback_shape_private.same_identity left.callback.callback_shape
       right.callback.callback_shape
  && Callback_certificate_private.same_origin
       left.callback.callback_certificate right.callback.callback_certificate
  && left.arguments = right.arguments

let result_matches_argument result argument =
  match result, argument with
  | Vir.Integer_result expected, Vir.Recursive_integer_argument (Vir.Integer_symbol actual)
  | Boolean_result expected, Recursive_boolean_argument (Boolean_symbol actual)
  | Aggregate_result expected,
    Recursive_aggregate_argument
      { aggregate_desc = Aggregate_symbol actual; _ }
  | Parametric_result expected,
    Recursive_parametric_argument
      { parametric_desc = Parametric_symbol actual; _ } ->
      expected.symbol_id = actual.symbol_id
      && expected.role = Vir.Result && actual.role = Vir.Result
  | Unit_result, Recursive_boolean_argument (Boolean_constant true) -> true
  | Tuple_result _, _
  | Unit_result, _
  | Integer_result _, _
  | Boolean_result _, _
  | Aggregate_result _, _
  | Parametric_result _, _ ->
      false

let structural_evidence filename =
  let result = require_result filename (verify filename 1) in
  let functions = (Verifier_service.vir result).Vir.functions in
  let reached =
    List.concat_map
      (fun execution -> execution.Vir.reached_callback_calls)
      functions
  in
  let preconditions =
    List.concat_map
      (fun execution ->
        List.filter_map
          (fun obligation ->
            match obligation.Vir.kind with
            | Vir.Callback_precondition { callback; call_span } ->
                Some (callback, call_span)
            | Arithmetic_safety _ | Assertion _ | Local_assertion _
            | Postcondition _ | Call_precondition _ | Invariant_validity _
            | Entry_measure_nonnegative _ | Recursive_call_measure_nonnegative _
            | Recursive_call_strict_descent _ ->
                None)
          execution.Vir.obligations)
      functions
  in
  let exact_preconditions =
    List.for_all
      (fun call ->
        List.length
          (List.filter
             (fun (candidate, span) ->
               Sst_callback_private.same_binding call.Vir.application.callback
                 candidate
               && call.application.call_span = span)
             preconditions)
        = 1)
      reached
  in
  let exact_relations =
    List.for_all
      (fun call ->
        match call.Vir.ensures with
        | Vir.Callback_ensures { application; result } ->
            same_application call.application application
            && result_matches_argument call.result result
        | Forall_term _ | Exists_term _
        | Boolean_constant _ | Boolean_symbol _ | Boolean_not _
        | Boolean_and _ | Boolean_or _ | Integer_compare _ | Boolean_equal _
        | Boolean_not_equal _ | Boolean_selector _ | Aggregate_equal _
        | Parametric_equal _ | Boolean_invariant_application _
        | Logical_adt_schema _
        | Boolean_recursive_spec_application _
        | Boolean_specification_application _
        | Boolean_symbolic_application _
        | Callback_requires _ ->
            false)
      reached
  in
  let symbols =
    List.concat_map
      (fun (call : Vir.reached_callback_call) ->
        result_symbols call.Vir.result)
      reached
  in
  let ids = List.map (fun symbol -> symbol.Vir.symbol_id) symbols in
  let fresh =
    List.for_all (fun symbol -> symbol.Vir.role = Vir.Result) symbols
    && List.length ids = List.length (List.sort_uniq Int.compare ids)
  in
  let max_vcs =
    List.fold_left
      (fun maximum execution ->
        max maximum (List.length execution.Vir.obligations))
      0 functions
  in
  Printf.printf
    "structural reached=%d preconditions=%d exact=%b fresh-results=%b \
     exact-ensures=%b max-vcs=%d\n"
    (List.length reached) (List.length preconditions) exact_preconditions fresh
    exact_relations max_vcs

let repeated_call_evidence filename =
  let result = require_result filename (verify filename 1) in
  let reached =
    (Verifier_service.vir result).functions
    |> List.find_map (fun execution ->
           if
             String.equal execution.Vir.function_ref.function_name
               "identical_calls"
           then Some execution.reached_callback_calls
           else None)
    |> Option.value ~default:[]
  in
  let sites =
    List.fold_left
      (fun sites (call : Vir.reached_callback_call) ->
        match
          List.find_opt
            (fun (application, _) ->
              same_schema_application application call.application
              && application.call_span = call.application.call_span)
            sites
        with
        | None -> (call.application, [ call.result ]) :: sites
        | Some (application, results) ->
            (application, call.result :: results)
            :: List.filter
                 (fun (candidate, _) ->
                   not
                     (same_schema_application candidate call.application
                     && candidate.call_span = call.application.call_span))
                 sites)
      [] reached
  in
  let duplicate_pairs =
    List.fold_left
      (fun total (left, _) ->
        total
        + List.length
            (List.filter
               (fun (right, _) ->
                 left.Vir.call_span <> right.Vir.call_span
                 && same_schema_application left right)
               sites))
      0 sites
    / 2
  in
  let site_ids =
    List.map
      (fun (_, results) ->
        results
        |> List.concat_map result_symbols
        |> List.map (fun symbol -> symbol.Vir.symbol_id)
        |> List.sort_uniq Int.compare)
      sites
  in
  let ids = List.concat site_ids in
  Printf.printf
    "repeated-calls sites=%d path-facts=%d pairs=%d results=%d distinct=%b\n"
    (List.length sites) (List.length reached) duplicate_pairs
    (List.length ids)
    (List.for_all (fun ids -> List.length ids = 1) site_ids
    && List.length ids = List.length (List.sort_uniq Int.compare ids))

let verify_command filename threads =
  let result = require_result filename (verify filename threads) in
  Printf.printf "status=%s functions=%d obligations=%d threads=%d\n"
    (status (Verifier_service.status result))
    (Verifier_service.functions result)
    (Verifier_service.obligations result) threads

let result_snapshot result =
  ( Verifier_service.status result,
    Verifier_service.semantic_sst result,
    Vir.to_string (Verifier_service.vir result),
    Verifier_service.functions result,
    Verifier_service.obligations result )

let parity filename =
  let first = require_result filename (verify filename 1) in
  let repeated = require_result filename (verify filename 1) in
  let threaded = require_result filename (verify filename 2) in
  if result_snapshot first <> result_snapshot repeated then
    fail "repeat result differs";
  if result_snapshot first <> result_snapshot threaded then
    fail "threads 1/2 result differs";
  Printf.printf
    "parity=repeat/threads status=%s functions=%d obligations=%d resources=100000 timeout-ms=5000\n"
    (status (Verifier_service.status first))
    (Verifier_service.functions first)
    (Verifier_service.obligations first)

let reject_zero_work filename =
  Verification_driver_private.For_testing.reset_driver_entries ();
  Verification_pipeline.For_testing.reset_validated_pipeline_entries ();
  Solver_backend_counter_private.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let rejection =
    match verify filename 1 with Error error -> error | Ok _ ->
      fail "%s unexpectedly verified" filename
  in
  let counters = Z3_bridge.counters () in
  if
    Solver_backend_counter_private.solver_creation_count () <> 0
    || counters.contexts_created <> 0 || counters.solvers_created <> 0
  then fail "%s performed solver work" filename;
  let code =
    match Verifier_service.error_diagnostic rejection with
    | Some diagnostic -> diagnostic.Diagnostic.code
    | None -> "VERO_SERVICE"
  in
  Printf.printf
    "rejected=pre-solver code=%s driver=%d pipeline=%d solver=0 z3=0/0\n"
    code
    (Verification_driver_private.For_testing.driver_entries ())
    (Verification_pipeline.For_testing.validated_pipeline_entries ())

let semantic_negative filename =
  let result = require_result filename (verify filename 1) in
  if Verifier_service.status result <> Verifier_service.Counterexample then
    fail "%s did not produce a counterexample" filename;
  let callback_ids =
    let implementation = load_implementation filename in
    match Typedtree_lowering.lower implementation with
    | Error diagnostic ->
        fail "%s [%s]: %s" filename diagnostic.code diagnostic.message
    | Ok program ->
        List.concat_map
          (fun definition ->
            Sst_callback_private.bindings_in_definition definition)
          program.Sst.functions
        |> List.filter_map (fun callback ->
               match
                 Callback_certificate_private.kind
                   callback.Sst.callback_certificate
               with
               | Callback_certificate_private.Top_level
               | Callback_certificate_private.Local ->
                   Some callback.callback_id
               | Callback_certificate_private.Formal -> None)
        |> List.sort_uniq Int.compare
  in
  let diagnostics = Verifier_service.diagnostics result in
  let kind diagnostic =
    match Verifier_service.diagnostic_kind diagnostic with
    | Verifier_service.Callback_precondition _ -> "callback-precondition"
    | Call_precondition _ -> "call-precondition"
    | Postcondition _ ->
        let function_ref =
          Verifier_service.diagnostic_function diagnostic
        in
        if
          List.mem
            (Verifier_service.function_index function_ref)
            callback_ids
        then
          "callback-closure-postcondition"
        else "stronger-than-callback-postcondition"
    | Arithmetic_safety _ | Assertion _ | Local_assertion _
    | Invariant_validity _ | Entry_measure_nonnegative
    | Recursive_call_measure_nonnegative _
    | Recursive_call_strict_descent _ ->
        "other"
  in
  match diagnostics with
  | diagnostic :: _ ->
      Printf.printf "semantic-negative=counterexample function=%s kind=%s diagnostics=%d\n"
        (Verifier_service.function_name
           (Verifier_service.diagnostic_function diagnostic))
        (kind diagnostic) (List.length diagnostics)
  | [] -> fail "%s has no counterexample diagnostic" filename

let resource_evidence filename =
  Solver_backend_counter_private.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let result = require_result filename (verify filename 1) in
  let counters = Z3_bridge.counters () in
  let created = Solver_backend_counter_private.solver_creation_count () in
  let resets = Solver_backend_counter_private.reset_count () in
  if
    counters.contexts_created <> counters.contexts_cleaned
    || counters.contexts_live <> 0
    || counters.solvers_created <> counters.solver_resets
    || created <> resets
  then fail "callback resource cleanup is unbalanced";
  Printf.printf
    "resources obligations=%d contexts=%d/%d live=%d solvers=%d/%d \
     max-live=%d policy=100000/5000\n"
    (Verifier_service.obligations result)
    counters.contexts_created counters.contexts_cleaned counters.contexts_live
    counters.solvers_created counters.solver_resets
    counters.maximum_contexts_live

let cmt_compilation_identity filename =
  match Cmt_input.load filename with
  | Error diagnostic ->
      fail "load %s [%s]: %s" filename diagnostic.code diagnostic.message
  | Ok implementation -> (
      match Callback_certificate_private.cmt_compilation_identity implementation with
      | Ok identity -> identity
      | Error message -> fail "identity %s: %s" filename message)

let lifecycle_unit filename foreign_filename =
  let compilation_identity = cmt_compilation_identity filename in
  let repeated_identity = cmt_compilation_identity filename in
  let foreign_identity = cmt_compilation_identity foreign_filename in
  let span = Diagnostic.file_span "callback-lifecycle.ml" in
  let shape =
    match
      Callback_shape_private.create
        ~endpoints:[ (Callback_shape_private.Unlabelled, Parametric_type.Int) ]
        ~result:Parametric_type.Int
    with
    | Ok shape -> shape
    | Error message -> fail "%s" message
  in
  let origin =
    match
      Callback_certificate_private.issue_origin
        ~kind:Callback_certificate_private.Local ~compilation_identity
        ~callable_identity:"callback" ~shape
        ~compiler_mode:"portable" ~contract_identity:"contract" ~captures:[]
        ~pure:true ~total:true ~complete:true
    with
    | Ok origin -> origin
    | Error message -> fail "%s" message
  in
  let certificate =
    Callback_certificate_private.seal_call_edge origin
      ~caller_identity:"caller" ~call_edge_identity:"edge"
  in
  let binding =
    {
      Sst.callback_id = 7;
      callback_name = "callback";
      callback_shape = shape;
      callback_certificate = certificate;
      callback_span = span;
    }
  in
  let before = Solver_backend_counter_private.solver_creation_count () in
  let session = ref () in
  let wrong_session = ref () in
  let bound =
    Callback_certificate_private.bind_session certificate ~compilation_identity
      ~session
  in
  let authenticated =
    Callback_certificate_private.authenticate certificate ~shape
      ~compilation_identity ~session ~caller_identity:(Some "caller")
      ~call_edge_identity:(Some "edge")
  in
  let copied =
    Callback_certificate_private.authenticate certificate ~shape
      ~compilation_identity ~session ~caller_identity:(Some "other")
      ~call_edge_identity:(Some "edge")
  in
  let stale =
    Callback_certificate_private.authenticate certificate ~shape
      ~compilation_identity:foreign_identity ~session ~caller_identity:(Some "caller")
      ~call_edge_identity:(Some "edge")
  in
  let replay =
    Callback_certificate_private.authenticate certificate ~shape
      ~compilation_identity ~session:wrong_session ~caller_identity:(Some "caller")
      ~call_edge_identity:(Some "edge")
  in
  let after = Solver_backend_counter_private.solver_creation_count () in
  Printf.printf
    "lifecycle binding=%s stable=%b bound=%b authenticated=%b copied=%b stale=%b \
     session=%b solver-work=%d\n"
    binding.callback_name
    (Callback_certificate_private.same_compilation compilation_identity
       repeated_identity)
    (Result.is_ok bound)
    (Result.is_ok authenticated)
    (Result.is_error copied)
    (Result.is_error stale)
    (Result.is_error replay) (after - before)

let authenticate_cmt filename foreign_filename =
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic ->
        fail "load %s [%s]: %s" filename diagnostic.code diagnostic.message
  in
  let program =
    match Typedtree_lowering.lower implementation with
    | Ok program -> program
    | Error diagnostic ->
        fail "lower %s [%s]: %s" filename diagnostic.code diagnostic.message
  in
  let identity = cmt_compilation_identity filename in
  let foreign_identity = cmt_compilation_identity foreign_filename in
  let session = ref () in
  let before = Solver_backend_counter_private.solver_creation_count () in
  let authenticated =
    Sst_callback_private.authenticate_program ~compilation_identity:identity
      ~session program
  in
  let cross_cmt =
    Sst_callback_private.authenticate_program ~compilation_identity:foreign_identity
      ~session program
  in
  let cross_session =
    Sst_callback_private.authenticate_program ~compilation_identity:identity
      ~session:(ref ()) program
  in
  let raw_source =
    Typedtree_adapter.lower ~source_file:implementation.source_file
      ~imports:implementation.imports implementation.structure
  in
  let after = Solver_backend_counter_private.solver_creation_count () in
  let result_message = function Ok () -> "ok" | Error message -> message in
  Printf.printf
    "cmt-auth callbacks=%b authenticated=%b(%s) cross-cmt=%b cross-session=%b \
     raw-source=%b solver-work=%d\n"
    (Sst_callback_private.has_callbacks program)
    (Result.is_ok authenticated) (result_message authenticated) (Result.is_error cross_cmt)
    (Result.is_error cross_session) (Result.is_error raw_source)
    (after - before)

let compare_cmt_identities left right =
  let left_identity = cmt_compilation_identity left
  and right_identity = cmt_compilation_identity right in
  Printf.printf "cmt-identity equivalent=%b\n"
    (Callback_certificate_private.same_compilation left_identity right_identity)

let callback_kinds filename =
  let implementation = load_implementation filename in
  let program =
    match Typedtree_lowering.lower implementation with
    | Ok program -> program
    | Error diagnostic ->
        fail "%s [%s]: %s" filename diagnostic.code diagnostic.message
  in
  List.iter
    (fun definition ->
      let seen = Hashtbl.create 8 in
      List.iter
        (fun callback ->
          let key = callback.Sst.callback_id, callback.callback_name in
          if not (Hashtbl.mem seen key) then (
            Hashtbl.add seen key ();
            let kind =
              match
                Callback_certificate_private.kind
                  callback.Sst.callback_certificate
              with
              | Callback_certificate_private.Formal -> "formal"
              | Top_level -> "top"
              | Local -> "local"
            in
            Printf.printf "%s:%s:%s\n"
              definition.Sst.function_id.function_name callback.callback_name
              kind))
        (Sst_callback_private.bindings_in_definition definition))
    program.Sst.functions

let () =
  match Array.to_list Sys.argv with
  | [ _; "inspect"; filename ] -> inspect filename
  | [ _; "structural-evidence"; filename ] -> structural_evidence filename
  | [ _; "repeated-call-evidence"; filename ] ->
      repeated_call_evidence filename
  | [ _; "verify"; filename; threads ] ->
      verify_command filename (int_of_string threads)
  | [ _; "parity"; filename ] -> parity filename
  | [ _; "resource-evidence"; filename ] -> resource_evidence filename
  | [ _; "reject-zero-work"; filename ] -> reject_zero_work filename
  | [ _; "semantic-negative"; filename ] -> semantic_negative filename
  | [ _; "dump-sst"; filename ] ->
      let result = require_result filename (verify filename 1) in
      print_string (Verifier_service.semantic_sst result)
  | [ _; "lifecycle-unit"; filename; foreign_filename ] ->
      lifecycle_unit filename foreign_filename
  | [ _; "authenticate-cmt"; filename; foreign_filename ] ->
      authenticate_cmt filename foreign_filename
  | [ _; "compare-cmt-identities"; left; right ] ->
      compare_cmt_identities left right
  | [ _; "callback-kinds"; filename ] -> callback_kinds filename
  | _ ->
      fail
        "usage: verified_callbacks_tool (inspect FILE.cmt|structural-evidence FILE.cmt|repeated-call-evidence FILE.cmt|verify FILE.cmt THREADS|parity FILE.cmt|resource-evidence FILE.cmt|reject-zero-work FILE.cmt|semantic-negative FILE.cmt|dump-sst FILE.cmt|lifecycle-unit FILE.cmt FOREIGN.cmt|authenticate-cmt FILE.cmt FOREIGN.cmt|compare-cmt-identities LEFT.cmt RIGHT.cmt)"
