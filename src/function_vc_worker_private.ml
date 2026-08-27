type recursive_retry_control : value mod contended portable =
  | Retry_real
  | Retry_unknown
  | Retry_counterexample

type ground_plan : value mod contended portable = {
  ground_complete_violation : bool;
  ground_attempts : int;
  ground_complete_violations : int;
  ground_abstentions : int;
  ground_antecedents_checked : int;
}

type recursive_route : value mod contended portable = {
  initial_query : Z3_bridge.detached_query;
  retry_query : Z3_bridge.detached_query option;
  retry_eligible : bool;
  ground_plan : ground_plan option;
  force_initial_inconclusive : bool;
  retry_control : recursive_retry_control;
  retry_rlimit : int;
}

type route : value mod contended portable =
  | Ordinary of Z3_bridge.detached_query
  | Structural_rank of Z3_bridge.detached_query
  | Logical_aggregate of Z3_bridge.detached_query
  | Recursive of recursive_route

type vc_request : value mod contended portable = {
  canonical_index : int;
  timeout_ms : int;
  rlimit : int;
  route : route;
}

type request : value mod contended portable = {
  source_ordinal : int;
  vcs : vc_request list;
}

type retry_observation : value mod contended portable = {
  retry_attempts : int;
  retry_queries : int;
  retry_facts : int;
  retry_verified : int;
  retry_counterexamples : int;
  retry_inconclusives : int;
  retry_abstentions : int;
  retry_used : bool;
}

type ground_observation : value mod contended portable = {
  observed_ground_attempts : int;
  observed_ground_complete_violations : int;
  observed_ground_abstentions : int;
  observed_ground_antecedents_checked : int;
}

type vc_result : value mod contended portable = {
  result_index : int;
  result_outcome : (Z3_bridge.detached_outcome, string) result;
  attempt_telemetry : Z3_bridge.counters list;
  ordinary_contribution : bool;
  proof_queries : int;
  retry_observation : retry_observation;
  ground_observation : ground_observation;
}

type result : value mod contended portable = {
  result_source_ordinal : int;
  result_domain : int;
  vc_results : vc_result list;
  worker_exception : (int * string) option;
}

type slot : value mod contended portable =
  | Pending of request
  | Complete of result

let zero_retry =
  {
    retry_attempts = 0;
    retry_queries = 0;
    retry_facts = 0;
    retry_verified = 0;
    retry_counterexamples = 0;
    retry_inconclusives = 0;
    retry_abstentions = 0;
    retry_used = false;
  }

let zero_ground =
  {
    observed_ground_attempts = 0;
    observed_ground_complete_violations = 0;
    observed_ground_abstentions = 0;
    observed_ground_antecedents_checked = 0;
  }

let active_functions = Portable.Atomic.make 0
let peak_functions = Portable.Atomic.make 0
let spin_iterations = Portable.Atomic.make 0
let injected_worker_error_ordinal = Portable.Atomic.make (-1)
let cleanup_failures = Portable.Atomic.make 0

let update_peak active =
  let rec loop () =
    let peak = Portable.Atomic.get peak_functions in
    if active > peak then
      match
        Portable.Atomic.compare_and_set peak_functions
          ~if_phys_equal_to:peak ~replace_with:active
      with
      | Portable.Atomic.Compare_failed_or_set_here.Set_here -> ()
      | Compare_failed -> loop ()
  in
  loop ()

let spin () =
  let rec loop remaining value =
    if remaining <= 0 then value
    else loop (remaining - 1) ((value * 1_103_515_245 + 12_345) land max_int)
  in
  ignore
    (Sys.opaque_identity
       (loop (Portable.Atomic.get spin_iterations) 1))

let private_outcome = function
  | Z3_bridge.Detached_counterexample _ ->
      Z3_bridge.Detached_counterexample []
  | (Detached_verified | Detached_inconclusive _) as outcome -> outcome

let solve_query request ~controlled ~rlimit ~model query =
  Z3_bridge.solve_detached_query_local ~controlled
    ~timeout_ms:request.timeout_ms ~rlimit ~model query

let retry_observation outcome =
  match outcome with
  | Z3_bridge.Detached_verified ->
      {
        zero_retry with
        retry_attempts = 1;
        retry_queries = 1;
        retry_facts = 1;
        retry_verified = 1;
        retry_used = true;
      }
  | Detached_counterexample _ ->
      {
        zero_retry with
        retry_attempts = 1;
        retry_queries = 1;
        retry_facts = 1;
        retry_counterexamples = 1;
        retry_used = true;
      }
  | Detached_inconclusive _ ->
      {
        zero_retry with
        retry_attempts = 1;
        retry_queries = 1;
        retry_facts = 1;
        retry_inconclusives = 1;
        retry_used = true;
      }

let apply_ground plan original =
  match plan with
  | None -> (original, zero_ground)
  | Some plan ->
      let outcome =
        if plan.ground_complete_violation then
          Z3_bridge.Detached_counterexample []
        else original
      in
      ( outcome,
        {
          observed_ground_attempts = plan.ground_attempts;
          observed_ground_complete_violations =
            plan.ground_complete_violations;
          observed_ground_abstentions = plan.ground_abstentions;
          observed_ground_antecedents_checked =
            plan.ground_antecedents_checked;
        } )

let solve_recursive request recursive =
  let initial =
    solve_query request ~controlled:Z3_bridge.Real ~rlimit:request.rlimit
      ~model:false recursive.initial_query
  in
  match initial.detached_result with
  | Error message ->
      ( Error message,
        [ initial.detached_telemetry ],
        zero_retry,
        zero_ground,
        1 )
  | Ok initial_outcome ->
      let initial_outcome =
        if recursive.force_initial_inconclusive then
          Z3_bridge.Detached_inconclusive
            (Backend_unknown "controlled recursive local unknown")
        else private_outcome initial_outcome
      in
      (match initial_outcome with
      | Z3_bridge.Detached_inconclusive Resource_exhausted ->
          ( Ok initial_outcome,
            [ initial.detached_telemetry ],
            zero_retry,
            zero_ground,
            1 )
      | Detached_verified | Detached_counterexample _ ->
          ( Ok initial_outcome,
            [ initial.detached_telemetry ],
            zero_retry,
            zero_ground,
            1 )
      | Detached_inconclusive _ -> (
          match recursive.retry_query with
          | None ->
              let outcome, ground =
                apply_ground recursive.ground_plan initial_outcome
              in
              let retry =
                if recursive.retry_eligible then
                  {
                    zero_retry with
                    retry_attempts = 1;
                    retry_abstentions = 1;
                  }
                else zero_retry
              in
              ( Ok outcome,
                [ initial.detached_telemetry ],
                retry,
                ground,
                1 )
          | Some retry_query ->
              let retry =
                match recursive.retry_control with
                | Retry_counterexample -> None
                | Retry_unknown ->
                    Some
                      (solve_query request
                         ~controlled:Z3_bridge.Force_unknown
                         ~rlimit:recursive.retry_rlimit ~model:false
                         retry_query)
                | Retry_real ->
                    Some
                      (solve_query request ~controlled:Z3_bridge.Real
                         ~rlimit:recursive.retry_rlimit ~model:false
                         retry_query)
              in
              let retry_result, telemetry =
                match retry with
                | None ->
                    ( Ok (Z3_bridge.Detached_counterexample []),
                      [ initial.detached_telemetry ] )
                | Some attempt ->
                    ( attempt.detached_result,
                      [
                        initial.detached_telemetry;
                        attempt.detached_telemetry;
                      ] )
              in
              match retry_result with
              | Error message ->
                  ( Error message,
                    telemetry,
                    {
                      zero_retry with
                      retry_attempts = 1;
                      retry_queries = 1;
                      retry_facts = 1;
                      retry_used = true;
                    },
                    zero_ground,
                    2 )
              | Ok
                  (Z3_bridge.Detached_verified
                  | Detached_inconclusive Resource_exhausted as outcome) ->
                  ( Ok (private_outcome outcome),
                    telemetry,
                    retry_observation outcome,
                    zero_ground,
                    2 )
              | Ok
                  (Z3_bridge.Detached_counterexample _
                  | Detached_inconclusive
                      (Timed_out | Backend_unknown _) as retry_outcome) ->
                  let outcome, ground =
                    apply_ground recursive.ground_plan initial_outcome
                  in
                  ( Ok outcome,
                    telemetry,
                    retry_observation retry_outcome,
                    ground,
                    2 )))

let solve_vc request =
  match request.route with
  | Ordinary query ->
      let attempt =
        solve_query request ~controlled:Z3_bridge.Real
          ~rlimit:request.rlimit ~model:true query
      in
      {
        result_index = request.canonical_index;
        result_outcome = attempt.detached_result;
        attempt_telemetry = [ attempt.detached_telemetry ];
        ordinary_contribution = true;
        proof_queries = 0;
        retry_observation = zero_retry;
        ground_observation = zero_ground;
      }
  | Structural_rank query | Logical_aggregate query ->
      let attempt =
        solve_query request ~controlled:Z3_bridge.Real
          ~rlimit:request.rlimit ~model:true query
      in
      {
        result_index = request.canonical_index;
        result_outcome = Result.map private_outcome attempt.detached_result;
        attempt_telemetry = [ attempt.detached_telemetry ];
        ordinary_contribution = false;
        proof_queries = 0;
        retry_observation = zero_retry;
        ground_observation = zero_ground;
      }
  | Recursive recursive ->
      let outcome, telemetry, retry, ground, proof_queries =
        solve_recursive request recursive
      in
      {
        result_index = request.canonical_index;
        result_outcome = outcome;
        attempt_telemetry = telemetry;
        ordinary_contribution = false;
        proof_queries;
        retry_observation = retry;
        ground_observation = ground;
      }

let verified result =
  match result.result_outcome with
  | Ok Z3_bridge.Detached_verified -> true
  | Ok (Detached_counterexample _ | Detached_inconclusive _) | Error _ ->
      false

let note_cleanup result =
  List.iter
    (fun telemetry ->
      if
        telemetry.Z3_bridge.contexts_live <> 0
        || telemetry.contexts_created <> telemetry.contexts_cleaned
      then Portable.Atomic.incr cleanup_failures)
    result.attempt_telemetry

let run request =
  let active = Portable.Atomic.fetch_and_add active_functions 1 + 1 in
  update_peak active;
  Fun.protect
    ~finally:(fun () -> Portable.Atomic.decr active_functions)
    (fun () ->
      spin ();
      let vc_results, worker_exception =
        if
          Portable.Atomic.get injected_worker_error_ordinal
          = request.source_ordinal
        then
          let index =
            match request.vcs with
            | [] -> 0
            | vc :: _ -> vc.canonical_index
          in
          ([], Some (index, "injected worker error"))
        else
          let rec loop results = function
            | [] -> (List.rev results, None)
            | vc :: rest -> (
                match solve_vc vc with
                | result ->
                    note_cleanup result;
                    if verified result then loop (result :: results) rest
                    else (List.rev (result :: results), None)
                | exception exn ->
                    ( List.rev results,
                      Some (vc.canonical_index, Printexc.to_string exn) ))
          in
          loop [] request.vcs
      in
      {
        result_source_ordinal = request.source_ordinal;
        result_domain = Multicore.current_domain ();
        vc_results;
        worker_exception;
      })

module For_testing = struct
  let reset () =
    Portable.Atomic.set active_functions 0;
    Portable.Atomic.set peak_functions 0;
    Portable.Atomic.set spin_iterations 0;
    Portable.Atomic.set injected_worker_error_ordinal (-1);
    Portable.Atomic.set cleanup_failures 0

  let set_spin_iterations iterations =
    Portable.Atomic.set spin_iterations (max 0 iterations)

  let peak_active_functions () = Portable.Atomic.get peak_functions

  let inject_worker_error = function
    | None -> Portable.Atomic.set injected_worker_error_ordinal (-1)
    | Some ordinal ->
        Portable.Atomic.set injected_worker_error_ordinal ordinal

  let cleanup_failures () = Portable.Atomic.get cleanup_failures
end
