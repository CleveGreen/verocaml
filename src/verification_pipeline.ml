type solve_request = {
  definition : Sst.function_definition;
  receipt_source : bool;
  receipt_dependent : bool;
  proof_activation_routes : Verification_session.proof_activation_route list;
  execution : Vir.function_execution;
}

type solve =
  solve_request -> (Solver_backend.obligation_result list, string) result

type threaded_solve =
  | Threaded_solve : {
      prepare :
        source_ordinal:int -> solve_request -> ('prepared, string) result;
      worker_request :
        'prepared -> Function_vc_worker_private.request;
      commit :
        'prepared ->
        Function_vc_worker_private.result ->
        (Solver_backend.obligation_result list, string) result;
    }
      -> threaded_solve

type setup_error =
  | Internal_setup_error of string
  | Solver_configuration_error of Solver_backend.error

type status = Verified | Counterexample | Inconclusive | Incomplete_source

type completion = {
  status : status;
  vir : Vir.program;
  functions : int;
  obligations : int;
}

type error =
  | Setup_error of setup_error
  | Engine_error of Symbolic_executor_private.error
  | Solve_error of string

type post_validation_invariant_breach = Post_validation_invariant_breach

module Error_identity = struct
  type t = error

  let equal left right = left == right
  let hash error = Hashtbl.hash error
end

module Post_validation_invariant_errors = Ephemeron.K1.Make (Error_identity)

let post_validation_invariant_errors = Post_validation_invariant_errors.create 4
let post_validation_invariant_errors_lock = Mutex.create ()

let with_post_validation_invariant_errors action =
  Mutex.lock post_validation_invariant_errors_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock post_validation_invariant_errors_lock)
    action

let mark_post_validation_invariant_breach error =
  with_post_validation_invariant_errors (fun () ->
      Post_validation_invariant_errors.replace
        post_validation_invariant_errors error ())

let post_validation_invariant_breach error =
  with_post_validation_invariant_errors (fun () ->
      if Post_validation_invariant_errors.mem post_validation_invariant_errors error
      then Some Post_validation_invariant_breach
      else None)

let injected_invariant_breach () =
  let error = Solve_error "verification pipeline post-validation invariant breach" in
  mark_post_validation_invariant_breach error;
  error

type report = {
  outcome : (completion, error) result;
  counters : Verification_session.counters;
  session_destroyed : bool;
}

type frontier_event =
  | Materialized of int * string
  | Committed of int * string
  | Blocked of int * string

let validated_pipeline_entries = ref 0
let observed_active_functions = Portable.Atomic.make 0
let observed_peak_functions = Portable.Atomic.make 0
let observed_spin_iterations = Portable.Atomic.make 0
let observed_functions_enabled = Portable.Atomic.make false
let injected_materialization_error = ref None
let frontier_events_reversed = ref []
let capture_frontier_events = ref false

let update_observed_peak @ portable = fun active ->
  let rec loop () =
    let peak = Portable.Atomic.get observed_peak_functions in
    if active > peak then
      match
        Portable.Atomic.compare_and_set observed_peak_functions
          ~if_phys_equal_to:peak ~replace_with:active
      with
      | Portable.Atomic.Compare_failed_or_set_here.Set_here -> ()
      | Compare_failed -> loop ()
  in
  loop ()

let spin_observed_function @ portable = fun () ->
  let rec loop remaining value =
    if remaining <= 0 then value
    else loop (remaining - 1) ((value * 1_103_515_245 + 12_345) land max_int)
  in
  ignore
    (Sys.opaque_identity
       (loop (Portable.Atomic.get observed_spin_iterations) 1))

let begin_observed_function @ portable = fun () ->
  let active = Portable.Atomic.fetch_and_add observed_active_functions 1 + 1 in
  update_observed_peak active;
  spin_observed_function ()

let end_observed_function @ portable = fun () ->
  Portable.Atomic.decr observed_active_functions

let observe_serial_function solve request =
  if not (Portable.Atomic.get observed_functions_enabled) then solve request
  else (
    begin_observed_function ();
    Fun.protect
      ~finally:end_observed_function
      (fun () -> solve request))

let observe_threaded_function @ portable = fun request ->
  if not (Portable.Atomic.get observed_functions_enabled) then
    Function_vc_worker_private.run request
  else (
    begin_observed_function ();
    Fun.protect
      ~finally:end_observed_function
      (fun () -> Function_vc_worker_private.run request))

let note_frontier_event event =
  if !capture_frontier_events then
    frontier_events_reversed := event :: !frontier_events_reversed

let note_materialized source_ordinal definition =
  [%log.trace "observed lowered verification materialization"
    ~stage:(Delator.Field.string "materialization-frontier")
    ~source_ordinal:(Delator.Field.int source_ordinal)
    ~function_index:(Delator.Field.int definition.Sst.function_id.function_index)
    ~decision:(Delator.Field.string "materialized")];
  note_frontier_event
    (Materialized
       (source_ordinal, definition.Sst.function_id.function_name))

let outcome_flags results =
  let counterexample =
    List.exists
      (fun result ->
        match result.Solver_backend.outcome with
        | Counterexample _ -> true
        | Verified | Inconclusive _ -> false)
      results
  in
  let inconclusive =
    List.exists
      (fun result ->
        match result.Solver_backend.outcome with
        | Inconclusive _ -> true
        | Verified | Counterexample _ -> false)
      results
  in
  (counterexample, inconclusive)

let run session prepared ~initial_obligations ~solve ~on_result =
  let serial_source_ordinal = ref (-1) in
  let rec loop executions functions obligations saw_counterexample
      saw_inconclusive saw_incomplete blocked_invariant_callables
      blocked_finite_callables blocked_frozen_callables
      frozen_constructor_failed = function
    | [] ->
        let status =
          if saw_counterexample then Counterexample
          else if saw_inconclusive then Inconclusive
          else if saw_incomplete then Incomplete_source
          else Verified
        in
        Ok
          {
            status;
            vir =
              Symbolic_executor_private.staged_program prepared
                (List.rev executions);
            functions;
            obligations;
          }
    | scheduled :: rest ->
        incr serial_source_ordinal;
        let source_ordinal = !serial_source_ordinal in
        let definition =
          Symbolic_executor_private.scheduled_definition scheduled
        in
        let source =
          Symbolic_executor_private.scheduled_is_receipt_source scheduled
        in
        let dependent =
          Symbolic_executor_private.scheduled_is_receipt_dependent scheduled
        in
        let invariant_prerequisites =
          Symbolic_executor_private
          .scheduled_invariant_receipt_prerequisites scheduled
        in
        let finite_prerequisites =
          Symbolic_executor_private.scheduled_finite_result_prerequisites
            scheduled
        in
        let frozen_prerequisites =
          Symbolic_executor_private.scheduled_frozen_formal_prerequisites
            scheduled
        in
        let invariant_blocked =
          List.exists
            (fun prerequisite ->
              List.mem prerequisite blocked_invariant_callables)
            invariant_prerequisites
        in
        let finite_blocked =
          List.exists
            (fun prerequisite ->
              List.mem prerequisite blocked_finite_callables)
            finite_prerequisites
        in
        let frozen_formal_blocked =
          List.exists
            (fun prerequisite ->
              List.mem prerequisite blocked_frozen_callables)
            frozen_prerequisites
        in
        let invariant_cell_blocked =
          Symbolic_executor_private
          .scheduled_has_unestablished_invariant_cell_transition prepared
            scheduled
        in
        let frozen_blocked =
          frozen_constructor_failed
          && Symbolic_executor_private.scheduled_requires_frozen_constructor
               prepared scheduled
        in
        if
          invariant_blocked || finite_blocked || invariant_cell_blocked
          || frozen_formal_blocked || frozen_blocked
        then (
          Verification_session.trace session
            (Verification_session.Blocked
               {
                 reason =
                   (if frozen_blocked then
                      Verification_session.Blocked_frozen_constructor
                    else if frozen_formal_blocked then
                      Verification_session.Blocked_frozen_formal
                    else if invariant_cell_blocked then
                      Verification_session.Blocked_invariant_cell
                    else Verification_session.Blocked_dependent);
                 function_id = definition.function_id;
               });
          loop executions functions obligations saw_counterexample
            saw_inconclusive saw_incomplete
            (if invariant_blocked then
               definition.function_id.function_index
               :: blocked_invariant_callables
             else blocked_invariant_callables)
            (if finite_blocked then
               definition.function_id.function_index :: blocked_finite_callables
             else blocked_finite_callables)
            (if frozen_formal_blocked || frozen_blocked then
               definition.function_id.function_index
               :: blocked_frozen_callables
             else blocked_frozen_callables)
            frozen_constructor_failed
            rest)
        else if !injected_materialization_error = Some source_ordinal then
          Error (injected_invariant_breach ())
        else (
          match
            Symbolic_executor_private.transfer_transition_predecessors prepared
              scheduled
          with
          | Error error -> Error (Engine_error error)
          | Ok () ->
          if dependent then Verification_session.note_dependent_lowering session;
          Verification_session.trace session
            (Verification_session.Lowering
               { function_id = definition.function_id; source; dependent });
          match
            Symbolic_executor_private.lower_scheduled prepared scheduled
          with
          | Error error -> Error (Engine_error error)
          | Ok lowered -> (
              let execution =
                Symbolic_executor_private.lowered_execution lowered
              in
              let proof_activation_batch =
                Symbolic_executor_private.lowered_proof_activation_batch
                  lowered
              in
              match
                Symbolic_executor_private.authorize_receipt_obligations prepared
                  scheduled execution
              with
              | Error error -> Error (Engine_error error)
              | Ok receipt_manifest -> (
                  let proof_activation_routes =
                    match (execution.Vir.mode, proof_activation_batch) with
                    | Sst.Proof, None ->
                        Error
                          "Proof execution has no reached activation manifest batch"
                    | (Sst.Proof | Sst.Exec | Sst.Spec), Some batch ->
                        Verification_session.consume_proof_activation_batch
                          session batch execution
                    | (Sst.Exec | Sst.Spec), None -> Ok []
                  in
                  match proof_activation_routes with
                  | Error message -> Error (Solve_error message)
                  | Ok proof_activation_routes ->
                      if dependent then
                        Verification_session.note_dependent_backend_context
                          session;
                      let request =
                        {
                          definition;
                          receipt_source = source;
                          receipt_dependent = dependent;
                          proof_activation_routes;
                          execution;
                        }
                      in
                      note_materialized source_ordinal definition;
                      match observe_serial_function solve request with
                      | Error message -> Error (Solve_error message)
                      | Ok results -> (
                      List.iter
                        (fun result ->
                          match result.Solver_backend.obligation.Vir.kind with
                          | Vir.Invariant_validity
                              {
                                boundary =
                                  Vir.Transition_preservation
                                    { transition_kind; _ };
                                _;
                              } ->
                              Verification_session.note_transition_preservation
                                session transition_kind
                          | Vir.Arithmetic_safety _ | Vir.Assertion _
                          | Vir.Local_assertion _ | Vir.Postcondition _
                          | Vir.Call_precondition _ | Vir.Callback_precondition _
                          | Vir.Invariant_validity _
                          | Vir.Entry_measure_nonnegative _
                          | Vir.Recursive_call_measure_nonnegative _
                          | Vir.Recursive_call_strict_descent _ ->
                              ())
                        results;
                      if source then
                        List.iter
                          (fun result ->
                            Verification_session.note_callee_solver_attempt
                              session;
                            Verification_session.note_callee_result session
                              result.Solver_backend.outcome)
                          results;
                      if dependent then
                        List.iter
                          (fun _ ->
                            Verification_session.note_dependent_solver_attempt
                              session)
                          results;
                      List.iter on_result results;
                      let complete =
                        List.length results
                        = List.length execution.Vir.obligations
                        && List.for_all
                             (fun result ->
                               result.Solver_backend.outcome
                               = Solver_backend.Verified)
                             results
                      in
                      let counterexample, inconclusive =
                        outcome_flags results
                      in
                      let invariant_cell_finalized =
                        Symbolic_executor_private.finalize_invariant_cell_close
                          prepared scheduled lowered results
                      in
                      match invariant_cell_finalized with
                      | Error error -> Error (Engine_error error)
                      | Ok _ ->
                      let frozen_finalized =
                        if complete then
                          match
                            Verification_session.finalize_frozen_constructor
                              session definition execution
                              receipt_manifest.frozen_constructor_manifest results
                          with
                          | Error message -> Error (Solve_error message)
                          | Ok () -> (
                              match
                                Verification_session
                                .finalize_frozen_formal_scopes session
                                  definition
                              with
                              | Ok () -> Ok ()
                              | Error message -> Error (Solve_error message))
                        else Ok ()
                      in
                      (match frozen_finalized with
                      | Error _ as error -> error
                      | Ok () -> (
                      match
                        Symbolic_executor_private.complete_owned_contents
                          prepared scheduled receipt_manifest execution results
                      with
                      | Error error -> Error (Engine_error error)
                      | Ok _ ->
                      let executions = execution :: executions in
                      let functions = functions + 1 in
                      let obligations =
                        obligations + List.length execution.Vir.obligations
                      in
                      let frozen_constructor_failed =
                        frozen_constructor_failed
                        ||
                        ((not complete)
                        && Symbolic_executor_private
                           .scheduled_is_frozen_constructor prepared scheduled)
                      in
                      let blocked_frozen_callables =
                        if
                          (not complete)
                          &&
                          (Symbolic_executor_private
                           .scheduled_is_frozen_constructor prepared scheduled
                          || Symbolic_executor_private
                             .scheduled_requires_frozen_constructor prepared
                               scheduled)
                        then
                          definition.function_id.function_index
                          :: blocked_frozen_callables
                        else blocked_frozen_callables
                      in
                      if source && not complete then
                        loop executions functions obligations
                          (saw_counterexample || counterexample)
                          (saw_inconclusive || inconclusive)
                          (saw_incomplete
                          || ((not counterexample) && not inconclusive))
                          (if
                             Symbolic_executor_private
                             .scheduled_is_invariant_receipt_source scheduled
                           then
                             definition.function_id.function_index
                             :: blocked_invariant_callables
                           else blocked_invariant_callables)
                          (if
                             Symbolic_executor_private
                             .scheduled_is_finite_result_source scheduled
                           then
                             definition.function_id.function_index
                             :: blocked_finite_callables
                           else blocked_finite_callables)
                          blocked_frozen_callables
                          frozen_constructor_failed
                          rest
                      else
                        let issued =
                          if source then
                            Symbolic_executor_private.issue_receipt prepared
                              scheduled receipt_manifest results
                          else Ok (true, true)
                        in
                        match issued with
                        | Error error -> Error (Engine_error error)
                        | Ok (invariant_issued, finite_issued) ->
                            if source then
                              Verification_session.trace session
                                Verification_session.Issued;
                            loop executions functions obligations
                              (saw_counterexample || counterexample)
                              (saw_inconclusive || inconclusive)
                              (saw_incomplete || not invariant_issued
                             || not finite_issued)
                              (if invariant_issued then
                                 blocked_invariant_callables
                               else
                                 definition.function_id.function_index
                                 :: blocked_invariant_callables)
                              (if finite_issued then blocked_finite_callables
                               else
                                 definition.function_id.function_index
                                 :: blocked_finite_callables)
                              blocked_frozen_callables
                              frozen_constructor_failed
                              rest))))))
  in
  loop [] 0 initial_obligations false false false [] [] [] false
    (Symbolic_executor_private.scheduled_functions prepared)
[@@delator.instrument] [@@delator.level debug]

type threaded_state = {
  executions : Vir.function_execution list;
  functions : int;
  obligations : int;
  saw_counterexample : bool;
  saw_inconclusive : bool;
  saw_incomplete : bool;
  blocked_invariant_callables : int list;
  blocked_finite_callables : int list;
  blocked_frozen_callables : int list;
  frozen_constructor_failed : bool;
  completed_ordinals : int list;
}

type ('scheduled, 'lowered, 'prepared) envelope = {
  source_ordinal : int;
  scheduled : 'scheduled;
  definition : Sst.function_definition;
  source : bool;
  dependent : bool;
  lowered : 'lowered;
  receipt_manifest : Symbolic_executor_private.receipt_manifests;
  execution : Vir.function_execution;
  prepared_function : 'prepared;
}

let initial_threaded_state initial_obligations =
  {
    executions = [];
    functions = 0;
    obligations = initial_obligations;
    saw_counterexample = false;
    saw_inconclusive = false;
    saw_incomplete = false;
    blocked_invariant_callables = [];
    blocked_finite_callables = [];
    blocked_frozen_callables = [];
    frozen_constructor_failed = false;
    completed_ordinals = [];
  }

let completion_of_threaded_state prepared state =
  let status =
    if state.saw_counterexample then Counterexample
    else if state.saw_inconclusive then Inconclusive
    else if state.saw_incomplete then Incomplete_source
    else Verified
  in
  {
    status;
    vir =
      Symbolic_executor_private.staged_program prepared
        (List.rev state.executions);
    functions = state.functions;
    obligations = state.obligations;
  }

let scheduled_is_blocked prepared state scheduled =
  let any_in blocked prerequisites =
    List.exists (fun prerequisite -> List.mem prerequisite blocked)
      prerequisites
  in
  any_in state.blocked_invariant_callables
    (Symbolic_executor_private
     .scheduled_invariant_receipt_prerequisites scheduled)
  || any_in state.blocked_finite_callables
       (Symbolic_executor_private
        .scheduled_finite_result_prerequisites scheduled)
  || any_in state.blocked_frozen_callables
       (Symbolic_executor_private
        .scheduled_frozen_formal_prerequisites scheduled)
  ||
  (state.frozen_constructor_failed
  && Symbolic_executor_private.scheduled_requires_frozen_constructor prepared
       scheduled)

let trace_blocked session prepared state scheduled =
  let definition =
    Symbolic_executor_private.scheduled_definition scheduled
  in
  note_frontier_event
    (Blocked
       ( definition.function_id.function_index,
         definition.function_id.function_name ));
  let any_in blocked prerequisites =
    List.exists (fun prerequisite -> List.mem prerequisite blocked)
      prerequisites
  in
  let invariant_blocked =
    any_in state.blocked_invariant_callables
      (Symbolic_executor_private
       .scheduled_invariant_receipt_prerequisites scheduled)
  in
  let finite_blocked =
    any_in state.blocked_finite_callables
      (Symbolic_executor_private
       .scheduled_finite_result_prerequisites scheduled)
  in
  let frozen_formal_blocked =
    any_in state.blocked_frozen_callables
      (Symbolic_executor_private
       .scheduled_frozen_formal_prerequisites scheduled)
  in
  let frozen_blocked =
    state.frozen_constructor_failed
    && Symbolic_executor_private.scheduled_requires_frozen_constructor prepared
         scheduled
  in
  let invariant_cell_blocked =
    Symbolic_executor_private
    .scheduled_has_unestablished_invariant_cell_transition prepared scheduled
  in
  Verification_session.trace session
    (Verification_session.Blocked
       {
         reason =
           (if frozen_blocked then
              Verification_session.Blocked_frozen_constructor
            else if frozen_formal_blocked then
              Verification_session.Blocked_frozen_formal
            else if invariant_cell_blocked then
              Verification_session.Blocked_invariant_cell
            else Verification_session.Blocked_dependent);
         function_id = definition.function_id;
       });
  {
    state with
    blocked_invariant_callables =
      (if invariant_blocked then
         definition.function_id.function_index
         :: state.blocked_invariant_callables
       else state.blocked_invariant_callables);
    blocked_finite_callables =
      (if finite_blocked then
         definition.function_id.function_index
         :: state.blocked_finite_callables
       else state.blocked_finite_callables);
    blocked_frozen_callables =
      (if frozen_formal_blocked || frozen_blocked then
         definition.function_id.function_index
         :: state.blocked_frozen_callables
       else state.blocked_frozen_callables);
  }

let materialize_function session prepared prepare source_ordinal scheduled =
  let definition =
    Symbolic_executor_private.scheduled_definition scheduled
  in
  if !injected_materialization_error = Some source_ordinal then
    Error (injected_invariant_breach ())
  else
  let source =
    Symbolic_executor_private.scheduled_is_receipt_source scheduled
  in
  let dependent =
    Symbolic_executor_private.scheduled_is_receipt_dependent scheduled
  in
  [%log.trace "materialize verification function"
    ~source_ordinal:(Delator.Field.int source_ordinal)
    ~function_name:(Delator.Field.string definition.function_id.function_name)
    ~function_index:(Delator.Field.int definition.function_id.function_index)
    ~source:(Delator.Field.bool source)
    ~dependent:(Delator.Field.bool dependent)];
  match
    Symbolic_executor_private.transfer_transition_predecessors prepared
      scheduled
  with
  | Error error -> Error (Engine_error error)
  | Ok () ->
      if dependent then Verification_session.note_dependent_lowering session;
      Verification_session.trace session
        (Verification_session.Lowering
           { function_id = definition.function_id; source; dependent });
      (match
         Symbolic_executor_private.lower_scheduled prepared scheduled
       with
      | Error error -> Error (Engine_error error)
      | Ok lowered ->
          let execution =
            Symbolic_executor_private.lowered_execution lowered
          in
          let proof_activation_batch =
            Symbolic_executor_private.lowered_proof_activation_batch lowered
          in
          (match
             Symbolic_executor_private.authorize_receipt_obligations prepared
               scheduled execution
           with
          | Error error -> Error (Engine_error error)
          | Ok receipt_manifest ->
              let proof_activation_routes =
                match (execution.Vir.mode, proof_activation_batch) with
                | Sst.Proof, None ->
                    Error
                      "Proof execution has no reached activation manifest batch"
                | (Sst.Proof | Sst.Exec | Sst.Spec), Some batch ->
                    Verification_session.consume_proof_activation_batch
                      session batch execution
                | (Sst.Exec | Sst.Spec), None -> Ok []
              in
              (match proof_activation_routes with
              | Error message -> Error (Solve_error message)
              | Ok proof_activation_routes ->
                  if dependent then
                    Verification_session.note_dependent_backend_context
                      session;
                  let request =
                    {
                      definition;
                      receipt_source = source;
                      receipt_dependent = dependent;
                      proof_activation_routes;
                      execution;
                    }
                  in
                  note_materialized source_ordinal definition;
                  (match prepare ~source_ordinal request with
                  | Error message -> Error (Solve_error message)
                  | Ok prepared_function ->
                      Ok
                        {
                          source_ordinal;
                          scheduled;
                          definition;
                          source;
                          dependent;
                          lowered;
                          receipt_manifest;
                          execution;
                          prepared_function;
                        }))))

let note_result_authority session envelope results ~on_result =
  List.iter
    (fun result ->
      match result.Solver_backend.obligation.Vir.kind with
      | Vir.Invariant_validity
          {
            boundary =
              Vir.Transition_preservation { transition_kind; _ };
            _;
          } ->
          Verification_session.note_transition_preservation session
            transition_kind
      | Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Local_assertion _
      | Vir.Postcondition _ | Vir.Call_precondition _
      | Vir.Callback_precondition _ | Vir.Invariant_validity _ | Vir.Entry_measure_nonnegative _
      | Vir.Recursive_call_measure_nonnegative _
      | Vir.Recursive_call_strict_descent _ ->
          ())
    results;
  if envelope.source then
    List.iter
      (fun (result : Solver_backend.obligation_result) ->
        Verification_session.note_callee_solver_attempt session;
        Verification_session.note_callee_result session
          result.Solver_backend.outcome)
      results;
  if envelope.dependent then
    List.iter
      (fun _ ->
        Verification_session.note_dependent_solver_attempt session)
      results;
  List.iter on_result results

let update_after_results prepared state envelope results =
  let complete =
    List.length results = List.length envelope.execution.Vir.obligations
    && List.for_all
         (fun result -> result.Solver_backend.outcome = Solver_backend.Verified)
         results
  in
  let counterexample, inconclusive = outcome_flags results in
  let index = envelope.definition.function_id.function_index in
  let frozen_failed =
    (not complete)
    && Symbolic_executor_private.scheduled_is_frozen_constructor prepared
         envelope.scheduled
  in
  let blocks_frozen =
    (not complete)
    &&
    (Symbolic_executor_private.scheduled_is_frozen_constructor prepared
       envelope.scheduled
    || Symbolic_executor_private.scheduled_requires_frozen_constructor prepared
         envelope.scheduled)
  in
  ( complete,
    {
      state with
      executions = envelope.execution :: state.executions;
      functions = state.functions + 1;
      obligations =
        state.obligations + List.length envelope.execution.Vir.obligations;
      saw_counterexample = state.saw_counterexample || counterexample;
      saw_inconclusive = state.saw_inconclusive || inconclusive;
      saw_incomplete =
        state.saw_incomplete
        || ((not complete) && not counterexample && not inconclusive);
      frozen_constructor_failed =
        state.frozen_constructor_failed || frozen_failed;
      blocked_frozen_callables =
        (if blocks_frozen then index :: state.blocked_frozen_callables
         else state.blocked_frozen_callables);
    } )

let commit_envelope session prepared commit ~on_result state envelope worker =
  [%log.trace "commit verification function"
    ~source_ordinal:(Delator.Field.int envelope.source_ordinal)
    ~function_name:
      (Delator.Field.string envelope.definition.function_id.function_name)
    ~function_index:
      (Delator.Field.int envelope.definition.function_id.function_index)
    ~obligations:
      (Delator.Field.int (List.length envelope.execution.Vir.obligations))];
  match commit envelope.prepared_function worker with
  | Error message -> Error (Solve_error message)
  | Ok results ->
      note_result_authority session envelope results ~on_result;
      let complete, state =
        update_after_results prepared state envelope results
      in
      (match
         Symbolic_executor_private.finalize_invariant_cell_close prepared
           envelope.scheduled envelope.lowered results
       with
      | Error error -> Error (Engine_error error)
      | Ok _ ->
          let frozen_finalized =
            if complete then
              match
                Verification_session.finalize_frozen_constructor session
                  envelope.definition envelope.execution
                  envelope.receipt_manifest.frozen_constructor_manifest
                  results
              with
              | Error message -> Error (Solve_error message)
              | Ok () -> (
                  match
                    Verification_session.finalize_frozen_formal_scopes session
                      envelope.definition
                  with
                  | Ok () -> Ok ()
                  | Error message -> Error (Solve_error message))
            else Ok ()
          in
          (match frozen_finalized with
          | Error _ as error -> error
          | Ok () -> (
              match
                Symbolic_executor_private.complete_owned_contents prepared
                  envelope.scheduled envelope.receipt_manifest
                  envelope.execution results
              with
              | Error error -> Error (Engine_error error)
              | Ok _ ->
                  let index =
                    envelope.definition.function_id.function_index
                  in
                  if envelope.source && not complete then
                    Ok
                      {
                        state with
                        blocked_invariant_callables =
                          (if
                             Symbolic_executor_private
                             .scheduled_is_invariant_receipt_source
                               envelope.scheduled
                           then index :: state.blocked_invariant_callables
                           else state.blocked_invariant_callables);
                        blocked_finite_callables =
                          (if
                             Symbolic_executor_private
                             .scheduled_is_finite_result_source
                               envelope.scheduled
                           then index :: state.blocked_finite_callables
                           else state.blocked_finite_callables);
                      }
                  else
                    let issued =
                      if envelope.source then
                        Symbolic_executor_private.issue_receipt prepared
                          envelope.scheduled envelope.receipt_manifest results
                      else Ok (true, true)
                    in
                    (match issued with
                    | Error error -> Error (Engine_error error)
                    | Ok (invariant_issued, finite_issued) ->
                        if envelope.source then
                          Verification_session.trace session
                            Verification_session.Issued;
                        Ok
                          {
                            state with
                            saw_incomplete =
                              state.saw_incomplete || not invariant_issued
                              || not finite_issued;
                            blocked_invariant_callables =
                              (if invariant_issued then
                                 state.blocked_invariant_callables
                               else
                                 index :: state.blocked_invariant_callables);
                            blocked_finite_callables =
                              (if finite_issued then
                                 state.blocked_finite_callables
                               else index :: state.blocked_finite_callables);
                            completed_ordinals =
                              (if invariant_issued && finite_issued then
                                 envelope.source_ordinal
                                 :: state.completed_ordinals
                               else state.completed_ordinals);
                          }))))

let dispatch_frontier scheduler
    (requests_list : Function_vc_worker_private.request list) =
  let frontier_length = List.length requests_list in
  let slots =
    Portable.Atomic_array.create ~len:frontier_length
      (Function_vc_worker_private.Pending
         { source_ordinal = 0; vcs = [] })
  in
  List.iteri
    (fun index (request : Function_vc_worker_private.request) ->
      Portable.Atomic_array.set slots index
        (Function_vc_worker_private.Pending request))
    requests_list;
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
      Parallel_kernel.for_ parallel ~start:0 ~stop:frontier_length
        ~f:(fun _ index ->
          let result =
            match Portable.Atomic_array.get slots index with
            | Function_vc_worker_private.Pending request ->
                observe_threaded_function request
            | Complete _ ->
                failwith "parallel function request ran more than once"
          in
          Portable.Atomic_array.set slots index
            (Function_vc_worker_private.Complete result)));
  List.init frontier_length (fun index ->
      match Portable.Atomic_array.get slots index with
      | Function_vc_worker_private.Complete result -> result
      | Pending _ ->
          failwith
            (Printf.sprintf "parallel function result %d was not joined" index))

let run_threaded session prepared ~initial_obligations scheduler
    (Threaded_solve { prepare; worker_request; commit }) ~on_result =
  let scheduled_functions =
    Symbolic_executor_private.scheduled_functions prepared
  in
  let ordinal_by_function =
    List.mapi
      (fun ordinal scheduled ->
        let definition =
          Symbolic_executor_private.scheduled_definition scheduled
        in
        (definition.function_id.function_index, ordinal))
      scheduled_functions
  in
  let dependency_ordinal function_index =
    match List.assoc_opt function_index ordinal_by_function with
    | Some ordinal -> ordinal
    | None -> function_index
  in
  let dependencies scheduled =
    List.concat
      [
        Symbolic_executor_private.scheduled_receipt_prerequisites scheduled;
        Symbolic_executor_private
        .scheduled_invariant_receipt_prerequisites scheduled;
        Symbolic_executor_private
        .scheduled_finite_result_prerequisites scheduled;
        Symbolic_executor_private
        .scheduled_frozen_formal_prerequisites scheduled;
      ]
    |> List.sort_uniq Int.compare
    |> List.map dependency_ordinal
  in
  let candidates =
    scheduled_functions
    |> List.mapi (fun source_ordinal scheduled ->
           Function_frontier_private.candidate
             ~source_ordinal
             ~dependency_ordinals:(dependencies scheduled)
             scheduled)
  in
  let frontier_prefix ready =
    let scalar = function
      | Sst.Unit | Bool | Int | Mathematical_int | Parameter _ -> true
      | Tuple _ | Aggregate _ | Application _ -> false
    in
    match ready with
    | [] -> []
    | first :: _ ->
        let requires_serial candidate =
          let scheduled = Function_frontier_private.value candidate in
          let definition =
            Symbolic_executor_private.scheduled_definition scheduled
          in
          definition.mode <> Sst.Exec
          || definition.recursive
          || not (scalar definition.result_type)
          || List.exists
               (function
                 | Sst.Callback_parameter _ -> true
                 | Sst.Value_parameter parameter ->
                     not (scalar parameter.Sst.pattern.typ))
               definition.parameters
          || Symbolic_executor_private.scheduled_is_receipt_source scheduled
          || Symbolic_executor_private.scheduled_is_receipt_dependent scheduled
          || Symbolic_executor_private.scheduled_is_frozen_constructor prepared
               scheduled
          || Symbolic_executor_private.scheduled_requires_frozen_constructor
               prepared scheduled
          ||
          Symbolic_executor_private
          .scheduled_invariant_receipt_prerequisites scheduled
          <> []
          ||
          Symbolic_executor_private
          .scheduled_finite_result_prerequisites scheduled
          <> []
          ||
          Symbolic_executor_private
          .scheduled_frozen_formal_prerequisites scheduled
          <> []
        in
        if requires_serial first then [ first ]
        else
          let rec ordinary_prefix selected = function
            | candidate :: rest when not (requires_serial candidate) ->
                ordinary_prefix (candidate :: selected) rest
            | _ -> List.rev selected
          in
          ordinary_prefix [] ready
  in
  let rec loop state candidates =
    match candidates with
    | [] -> Ok (completion_of_threaded_state prepared state)
    | _ ->
        let selection =
          Function_frontier_private.select
            ~completed_ordinals:state.completed_ordinals
            ~is_blocked:(scheduled_is_blocked prepared state)
            ~is_waiting:(fun scheduled ->
              Symbolic_executor_private
              .scheduled_has_unestablished_invariant_cell_transition prepared
                scheduled)
            candidates
        in
        (match selection with
        | Error message -> Error (Solve_error message)
        | Ok selection ->
            let ready = frontier_prefix selection.ready in
            let blocked =
              if ready = [] then
                selection.blocked @ selection.waiting
              else
                let first_ordinal =
                  Function_frontier_private.source_ordinal (List.hd ready)
                in
                List.filter
                  (fun candidate ->
                    Function_frontier_private.source_ordinal candidate
                    < first_ordinal)
                  selection.blocked
            in
            let state =
              List.fold_left
                (fun state candidate ->
                  trace_blocked session prepared state
                    (Function_frontier_private.value candidate))
                state blocked
            in
            let removed = blocked @ ready in
            let retained candidate =
              not (List.memq candidate removed)
            in
            let remaining = List.filter retained candidates in
            let rec materialize envelopes = function
              | [] -> (List.rev envelopes, None)
              | candidate :: rest ->
                  let source_ordinal =
                    Function_frontier_private.source_ordinal candidate
                  in
                  let scheduled =
                    Function_frontier_private.value candidate
                  in
                  (match
                     materialize_function session prepared prepare
                       source_ordinal scheduled
                   with
                  | Ok envelope -> materialize (envelope :: envelopes) rest
                  | Error error -> (List.rev envelopes, Some error))
            in
            let envelopes, retained_error =
              materialize [] ready
            in
            let requests =
              List.map
                (fun envelope ->
                  worker_request envelope.prepared_function)
                envelopes
            in
            let workers =
              dispatch_frontier scheduler requests
            in
            let rec commit_all state envelopes workers =
              match (envelopes, workers) with
              | [], [] -> Ok state
              | envelope :: envelope_rest, worker :: worker_rest -> (
                  match
                    commit_envelope session prepared commit ~on_result state
                      envelope worker
                  with
                  | Error _ as error -> error
                  | Ok state ->
                      note_frontier_event
                        (Committed
                           ( envelope.source_ordinal,
                             envelope.definition.function_id.function_name ));
                      commit_all state envelope_rest worker_rest)
              | [], _ :: _ | _ :: _, [] ->
                  Error (Solve_error "parallel function result count mismatch")
            in
            (match commit_all state envelopes workers with
            | Error _ as error -> error
            | Ok state -> (
                match retained_error with
                | Some error -> Error error
                | None -> loop state remaining)))
  in
  loop (initial_threaded_state initial_obligations) candidates
[@@delator.instrument] [@@delator.level debug]

let run_validated ~imports ~implementation ~program ~validated ~invariants
    ~preflight ~proof_entry_activations ~configure_solver ~on_result =
  incr validated_pipeline_entries;
  match
    Verification_session.create ~imports ~implementation ~validated ~invariants
  with
  | Error _ as error -> error
  | Ok session ->
      let outcome =
        Fun.protect
          ~finally:(fun () ->
            Verification_session.destroy session;
            Verification_session.trace session Verification_session.Destroy)
          (fun () ->
            match preflight () with
            | Error error -> Error (Setup_error error)
            | Ok initial_obligations -> (
                (* Recursive-definition totality is the antecedent of a
                   retained-source route.  The attack observation window starts
                   after that preflight so its counters cover only work
                   attributable to the attacked source traversal. *)
                if
                  Symbolic_executor_private.For_testing
                  .source_route_attack_observation_active ()
                then (
                  Solver_backend.For_testing.reset_solver_creation_count ();
                  Z3_bridge.reset_counters ();
                  Recursive_spec_encoding.For_testing
                  .reset_proof_query_construction_count ();
                  Recursive_spec_encoding.For_testing
                  .reset_recursive_lowering_count ();
                  Symbolic_executor_private.For_testing
                  .reset_authority_observation ());
                match
                  Verification_session.install_recursive_spec_preservation
                    session
                    (Recursive_spec_preservation.take_pending program)
                with
                | Error message -> Error (Solve_error message)
                | Ok () ->
                match
                  Symbolic_executor_private.prepare_program ~imports ~session
                    ~validated ~invariants
                    ~proof_entry_activations:(proof_entry_activations ())
                    program
                with
                | Error error -> Error (Engine_error error)
                | Ok prepared -> (
                    match configure_solver () with
                    | Error error -> Error (Setup_error error)
                    | Ok solve ->
                        run session prepared ~initial_obligations ~solve
                          ~on_result)))
      in
      Ok
        {
          outcome;
          counters = Verification_session.counters session;
          session_destroyed = not (Verification_session.is_active session);
        }
[@@delator.instrument] [@@delator.level debug]

let scheduler_creation_count = ref 0
let scheduler_stop_count = ref 0

let run_validated_with_threads ~threads ~imports ~implementation ~program
    ~validated ~invariants ~preflight ~proof_entry_activations
    ~configure_solver ~on_result =
  incr validated_pipeline_entries;
  match
    Verification_session.create ~imports ~implementation ~validated ~invariants
  with
  | Error _ as error -> error
  | Ok session ->
      let outcome =
        Fun.protect
          ~finally:(fun () ->
            Verification_session.destroy session;
            Verification_session.trace session Verification_session.Destroy)
          (fun () ->
            match preflight () with
            | Error error -> Error (Setup_error error)
            | Ok initial_obligations ->
                if
                  Symbolic_executor_private.For_testing
                  .source_route_attack_observation_active ()
                then (
                  Solver_backend.For_testing.reset_solver_creation_count ();
                  Z3_bridge.reset_counters ();
                  Recursive_spec_encoding.For_testing
                  .reset_proof_query_construction_count ();
                  Recursive_spec_encoding.For_testing
                  .reset_recursive_lowering_count ();
                  Symbolic_executor_private.For_testing
                  .reset_authority_observation ());
                (match
                   Verification_session.install_recursive_spec_preservation
                     session
                     (Recursive_spec_preservation.take_pending program)
                 with
                | Error message -> Error (Solve_error message)
                | Ok () -> (
                    match
                      Symbolic_executor_private.prepare_program ~imports
                        ~session ~validated ~invariants
                        ~proof_entry_activations:(proof_entry_activations ())
                        program
                    with
                    | Error error -> Error (Engine_error error)
                    | Ok prepared -> (
                        match configure_solver () with
                        | Error error -> Error (Setup_error error)
                        | Ok solve ->
                            let scheduler =
                              incr scheduler_creation_count;
                              Parallel_scheduler.create ~max_domains:threads ()
                            in
                            Fun.protect
                              ~finally:(fun () ->
                                incr scheduler_stop_count;
                                Parallel_scheduler.stop scheduler)
                              (fun () ->
                                run_threaded session prepared
                                  ~initial_obligations scheduler solve
                                  ~on_result)))))
      in
      Ok
        {
          outcome;
          counters = Verification_session.counters session;
          session_destroyed = not (Verification_session.is_active session);
        }
[@@delator.instrument] [@@delator.level debug]

module For_testing = struct
  let reset_validated_pipeline_entries () = validated_pipeline_entries := 0
  let validated_pipeline_entries () = !validated_pipeline_entries

  type nonrec frontier_event = frontier_event =
    | Materialized of int * string
    | Committed of int * string
    | Blocked of int * string

  let reset_scheduler_counts () =
    scheduler_creation_count := 0;
    scheduler_stop_count := 0

  let scheduler_creations () = !scheduler_creation_count
  let scheduler_stops () = !scheduler_stop_count

  let reset_function_observation () =
    Portable.Atomic.set observed_active_functions 0;
    Portable.Atomic.set observed_peak_functions 0;
    Portable.Atomic.set observed_spin_iterations 0;
    Portable.Atomic.set observed_functions_enabled true

  let set_function_spin_iterations iterations =
    Portable.Atomic.set observed_spin_iterations (max 0 iterations)

  let peak_active_functions () =
    Portable.Atomic.get observed_peak_functions

  let inject_materialization_error ordinal =
    injected_materialization_error := ordinal

  let reset_frontier_events () =
    frontier_events_reversed := [];
    capture_frontier_events := true

  let frontier_events () =
    List.rev !frontier_events_reversed
end
