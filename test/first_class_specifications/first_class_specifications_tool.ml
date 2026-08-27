let fail format = Printf.ksprintf failwith format
let () = ignore First_class_specifications_tool_prerequisites.ready

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let load_input filename =
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic ->
        fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message
  in
  let program =
    match Typedtree_lowering.lower implementation with
    | Ok program -> program
    | Error diagnostic ->
        fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message
  in
  let validated =
    match Sst_validation.validate program with
    | Ok validated -> validated
    | Error error -> fail "%s" (Sst_validation.error_to_string error)
  in
  let invariants =
    match Type_invariant.authenticate validated with
    | Ok invariants -> invariants
    | Error error -> fail "%s" (Type_invariant.error_to_string error)
  in
  (implementation, program, validated, invariants)

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let pipeline_error = function
  | Verification_pipeline.Engine_error error ->
      Symbolic_executor_private.error_to_string error
  | Solve_error message -> message
  | Setup_error (Internal_setup_error message) -> message
  | Setup_error (Solver_configuration_error error) ->
      Solver_backend.error_to_string error

let policy () =
  match Solver_policy_private.create ~timeout_ms:5_000 ~rlimit:100_000 with
  | Ok policy -> policy
  | Error error -> fail "%s" (Solver_policy_private.error_to_string error)

type observation = {
  function_name : string;
  obligation : Vir.obligation;
  report : Broadcast_vc_private.report option;
}

let run filename =
  let implementation, program, validated, invariants = load_input filename in
  let solver_policy = policy () in
  let prepared = ref None in
  let observations = ref [] in
  let preflight () =
    match Verification_solver_private.preflight ~solver_policy program with
    | Error error -> Error error
    | Ok value ->
        prepared := Some value;
        Ok (Verification_solver_private.termination_obligations value)
  in
  let configure_solver () =
    match !prepared with
    | None ->
        Error
          (Verification_pipeline.Internal_setup_error
             "first-class test lost solver preflight")
    | Some value -> (
        match Verification_solver_private.configure ~solver_policy value with
        | Error _ as error -> error
        | Ok solve ->
            Ok
              (fun (request : Verification_pipeline.solve_request) ->
                List.iter
                  (fun obligation ->
                    observations :=
                      {
                        function_name =
                          request.definition.Sst.function_id.function_name;
                        obligation;
                        report = Broadcast_vc_private.report obligation;
                      }
                      :: !observations)
                  request.execution.Vir.obligations;
                solve request))
  in
  let proof_entry_activations () =
    Option.fold ~none:(Fun.const [])
      ~some:Verification_solver_private.proof_entry_activations !prepared
  in
  let report =
    Verification_pipeline.run_validated ~imports:None ~implementation ~program
      ~validated ~invariants ~preflight ~proof_entry_activations
      ~configure_solver ~on_result:ignore
    |> function
    | Ok report -> report
    | Error message -> fail "%s" message
  in
  (report, List.rev !observations)

let structural filename =
  Z3_bridge.reset_counters ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  let report, observations = run filename in
  let completion =
    match report.Verification_pipeline.outcome with
    | Ok completion -> completion
    | Error error -> fail "%s" (pipeline_error error)
  in
  Printf.printf "status=%s functions=%d obligations=%d destroyed=%b\n"
    (status_name completion.status)
    completion.functions completion.obligations report.session_destroyed;
  List.iter
    (fun observation ->
      match observation.report with
      | None -> ()
      | Some report ->
          Printf.printf "vc function=%s index=%d active=%d inserted=%d\n"
            observation.function_name
            observation.obligation.Vir.obligation_index
            report.active_declarations
            (List.length report.inserted);
          List.iter
            (fun (inserted : Broadcast_vc_private.inserted) ->
              Printf.printf
                "insert id=%s vector=[%s] ordinal=%d trusted=%b qid=%s skid=%s\n"
                inserted.broadcast_id
                (String.concat ","
                   (List.map Parametric_type.to_string inserted.type_vector))
                inserted.insertion_ordinal inserted.trusted inserted.qid
                inserted.skid)
            report.inserted)
    observations;
  let counters = Z3_bridge.counters () in
  Printf.printf
    "resources backend=%d contexts=%d solvers=%d resets=%d cleaned=%d live=%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    counters.contexts_created counters.solvers_created counters.solver_resets
    counters.contexts_cleaned counters.contexts_live

let diagnostic filename =
  Z3_bridge.reset_counters ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  let stage, code =
    match Cmt_input.load filename with
    | Error diagnostic -> ("loader", diagnostic.Diagnostic.code)
    | Ok implementation -> (
        match Typedtree_lowering.lower implementation with
        | Error diagnostic -> ("adapter", diagnostic.Diagnostic.code)
        | Ok program -> (
            match Sst_validation.validate program with
            | Error _ -> ("sst", "semantic")
            | Ok _ -> (
                let report, _ = run filename in
                match report.Verification_pipeline.outcome with
                | Error (Verification_pipeline.Engine_error _) ->
                    ("engine", "semantic")
                | Error _ -> ("pipeline", "semantic")
                | Ok _ -> ("accepted", "none"))))
  in
  let counters = Z3_bridge.counters () in
  Printf.printf "stage=%s code=%s backend=%d contexts=%d solvers=%d live=%d\n"
    stage code
    (Solver_backend.For_testing.solver_creation_count ())
    counters.contexts_created counters.solvers_created counters.contexts_live

let callback_abi () =
  let span = Diagnostic.file_span "callback-abi.ml" in
  let arrow =
    Spec_function_type_private.make ~label:None ~domain:Sst.Int ~range:Sst.Int
  in
  let symbol =
    Vir.
      {
        symbol_id = 110;
        source_name = "f";
        sort = Parametric (Spec_function_logic_private.binder arrow);
        role = Local;
        span;
      }
  in
  let function_term =
    Spec_function_logic_private.of_symbol ~arrow symbol |> Result.get_ok
  in
  let value =
    Logical_spec_evaluation_private.Function_value
      {
        function_term;
        function_arrow = arrow;
        function_closure = Abstract_function;
      }
  in
  let rejected = function Error _ -> true | Ok _ -> false in
  Printf.printf "relation=%b result=%b\n"
    (rejected (Call_contract_execution_private.relation_argument value))
    (rejected (Call_contract_execution_private.callback_result value))

let () =
  match Array.to_list Sys.argv with
  | [ _; "sst"; filename ] -> print_string (Sst.to_string (load filename))
  | [ _; "rec-auth"; filename ] -> (
      let program = load filename in
      let definition =
        List.find (fun definition -> definition.Sst.recursive) program.functions
      in
      let descriptor = List.hd program.parametric_adts in
      let measure =
        (List.hd definition.contracts.decreases).predicate.expression
      in
      match
        Parametric_adt_lowering_private.authenticate_direct_recursion
          ~descriptor ~definition ~measure
      with
      | Ok () -> print_endline "ok"
      | Error message -> print_endline message)
  | [ _; "structural"; filename ] -> structural filename
  | [ _; "diagnostic"; filename ] -> diagnostic filename
  | [ _; "callback-abi" ] -> callback_abi ()
  | _ ->
      fail
        "usage: first_class_specifications_tool \
         (sst|rec-auth|structural|diagnostic|callback-abi) [FILE.cmt]"
