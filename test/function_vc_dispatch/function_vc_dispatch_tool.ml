let () = ignore Function_vc_dispatch_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let require label condition = if not condition then fail "%s" label

let span =
  Diagnostic.
    {
      file = "function_vc_dispatch.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

let detached boolean =
  let builder = Logic_ir.create () in
  match
    Logic_ir.query builder ~axioms:[]
      ~assertions:[ Logic_ir.bool ~span boolean ] ~requires:[] ~span
  with
  | Error error -> fail "%s" (Logic_ir.error_to_string error)
  | Ok query -> Z3_bridge.detach_query query

let vc index boolean =
  Function_vc_worker_private.
    {
      canonical_index = index;
      timeout_ms = 60_000;
      rlimit = 100_000;
      route = Ordinary (detached boolean);
    }

let frontier () =
  let candidate ordinal dependencies value =
    Function_frontier_private.candidate ~source_ordinal:ordinal
      ~dependency_ordinals:dependencies value
  in
  let candidates =
    [ candidate 4 [ 2 ] "later"; candidate 2 [] "first"; candidate 3 [] "blocked" ]
  in
  let selected completed =
    match
      Function_frontier_private.select ~completed_ordinals:completed
        ~is_blocked:(String.equal "blocked")
        ~is_waiting:(String.equal "waiting") candidates
    with
    | Error message -> fail "%s" message
    | Ok selection -> selection
  in
  let values candidates =
    List.map Function_frontier_private.value candidates
  in
  let first = selected [] in
  require "frontier ready ordering" (values first.ready = [ "first" ]);
  require "frontier blocked classification" (values first.blocked = [ "blocked" ]);
  require "frontier waiting classification" (values first.waiting = [ "later" ]);
  let second = selected [ 2 ] in
  require "successor frontier admission" (values second.ready = [ "first"; "later" ]);
  (match
     Function_frontier_private.select ~completed_ordinals:[]
       ~is_blocked:(fun _ -> false) ~is_waiting:(fun _ -> false)
       [ candidate 1 [] (); candidate 1 [] () ]
   with
  | Error _ -> ()
  | Ok _ -> fail "duplicate source ordinal was accepted");
  print_endline "frontier=ordered ready/waiting/blocked successor=later duplicate=rejected"

let physical () =
  let resolve ~max_domains affinity topology =
    Physical_core_count_private.For_testing.default_threads_with ~max_domains
      ~affinity ~topology
  in
  require "physical pair deduplication"
    (resolve ~max_domains:8 (fun () -> Ok [ 0; 1; 2; 3 ])
       (function
         | 0 | 1 -> Ok (0, 0)
         | 2 -> Ok (0, 1)
         | _ -> Ok (1, 0))
    = 3);
  require "runtime cap"
    (resolve ~max_domains:2 (fun () -> Ok [ 0; 1; 2 ])
       (fun cpu -> Ok (0, cpu))
    = 2);
  require "affinity fallback"
    (resolve ~max_domains:5 (fun () -> Error "denied")
       (fun _ -> assert false)
    = 5);
  require "empty fallback"
    (resolve ~max_domains:4 (fun () -> Ok []) (fun _ -> assert false) = 4);
  require "topology fallback"
    (resolve ~max_domains:6 (fun () -> Ok [ 0; 1 ])
       (fun cpu -> if cpu = 0 then Ok (0, 0) else Error "malformed")
    = 6);
  require "invalid runtime fallback"
    (resolve ~max_domains:0 (fun () -> Ok [ 0 ]) (fun _ -> Ok (0, 0)) = 1);
  require "explicit runtime excess accepted"
    (Result.is_error
       (Physical_core_count_private.validate_threads
          (Multicore.max_domains () + 1)));
  print_endline "physical=affinity/topology distinct=3 cap=2 fallbacks=affinity/empty/malformed/invalid"

let worker () =
  let request =
    Function_vc_worker_private.
      {
        source_ordinal = 7;
        vcs = [ vc 0 false; vc 1 false; vc 2 true; vc 3 false ];
      }
  in
  let result = Function_vc_worker_private.run request in
  let indices = List.map (fun result -> result.Function_vc_worker_private.result_index) result.vc_results in
  require "serial VC canonical cutoff" (indices = [ 0; 1; 2 ]);
  require "source ordinal correspondence" (result.result_source_ordinal = 7);
  require "worker unexpectedly raised" (result.worker_exception = None);
  print_endline "worker=vcs-serial indices=0,1,2 first-nonverified=cutoff-3 correspondence=authenticated"

let parallel () =
  Function_vc_worker_private.For_testing.reset ();
  Function_vc_worker_private.For_testing.set_spin_iterations 20_000_000;
  let requests =
    Portable.Atomic_array.init 4 ~f:(fun index ->
        Function_vc_worker_private.{ source_ordinal = index; vcs = [ vc 0 false ] })
  in
  let results = Portable.Atomic_array.create ~len:4 None in
  let scheduler = Parallel_scheduler.create ~max_domains:2 () in
  let started = Unix.gettimeofday () in
  Fun.protect
    ~finally:(fun () -> Parallel_scheduler.stop scheduler)
    (fun () ->
      Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
          Parallel_kernel.for_ parallel ~start:0 ~stop:4
            ~f:(fun _ index ->
              Portable.Atomic_array.set results index
                (Some
                   (Function_vc_worker_private.run
                      (Portable.Atomic_array.get requests index))))));
  for index = 0 to 3 do
    match Portable.Atomic_array.get results index with
    | Some result -> require "parallel correspondence" (result.result_source_ordinal = index)
    | None -> fail "parallel result %d did not join" index
  done;
  let peak = Function_vc_worker_private.For_testing.peak_active_functions () in
  require "wall observation invalid" (Unix.gettimeofday () >= started);
  require "independent functions did not overlap" (peak >= 2);
  require "parallel bound exceeded" (peak <= 2);
  Printf.printf
    "parallel=overlap peak=%d bound=2 functions=4 slots=4 join=complete \
     wall=recorded oversubscription=none\n"
    peak

let status = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message

let run_report implementation threads =
  match
    Verification_driver_private.run_with_threads ~threads ~rlimit:100_000
      ~timeout_ms:60_000 ~allow_imported_opens:false implementation
  with
  | Ok report -> report
  | Error _ -> fail "production driver failed for threads=%d" threads

let same_report left right =
  Verification_driver_private.status left
  = Verification_driver_private.status right
  && Verification_driver_private.semantic_sst left
     = Verification_driver_private.semantic_sst right
  && Vir.to_string (Verification_driver_private.vir left)
     = Vir.to_string (Verification_driver_private.vir right)
  && Verification_driver_private.functions left
     = Verification_driver_private.functions right
  && Verification_driver_private.obligations left
     = Verification_driver_private.obligations right
  && Verification_driver_private.results left
     = Verification_driver_private.results right
  && Verification_driver_private.counters left
     = Verification_driver_private.counters right

let semantic_sst_is_forced report =
  Verification_driver_private.semantic_sst_lazy report |> Lazy.is_val

let reset_injections () =
  Verification_pipeline.For_testing.inject_materialization_error None;
  Verification_solver_private.For_testing.inject_preparation_error None;
  Function_vc_worker_private.For_testing.inject_worker_error None

let require_cleanup label =
  let counters = Z3_bridge.counters () in
  require (label ^ ": live Z3 context") (counters.contexts_live = 0);
  require (label ^ ": Z3 cleanup mismatch")
    (counters.contexts_created = counters.contexts_cleaned);
  require (label ^ ": worker-local cleanup mismatch")
    (Function_vc_worker_private.For_testing.cleanup_failures () = 0)

let default_threads () = Physical_core_count_private.default_threads ()

let production filename =
  let implementation = load filename in
  reset_injections ();
  Verification_pipeline.For_testing.reset_scheduler_counts ();
  Function_vc_worker_private.For_testing.reset ();
  Z3_bridge.reset_counters ();
  Verification_pipeline.For_testing.reset_function_observation ();
  Verification_pipeline.For_testing.set_function_spin_iterations 20_000_000;
  let serial = run_report implementation 1 in
  let serial_peak =
    Verification_pipeline.For_testing.peak_active_functions ()
  in
  require "serial production peak was not one" (serial_peak = 1);
  require "threads=1 created a scheduler"
    (Verification_pipeline.For_testing.scheduler_creations () = 0
    && Verification_pipeline.For_testing.scheduler_stops () = 0);
  Verification_pipeline.For_testing.reset_function_observation ();
  Verification_pipeline.For_testing.set_function_spin_iterations 20_000_000;
  let threaded = run_report implementation 2 in
  let threaded_peak =
    Verification_pipeline.For_testing.peak_active_functions ()
  in
  require "production frontier did not overlap" (threaded_peak >= 2);
  require "production frontier exceeded bound" (threaded_peak <= 2);
  require "threaded scheduler lifecycle"
    (Verification_pipeline.For_testing.scheduler_creations () = 1
    && Verification_pipeline.For_testing.scheduler_stops () = 1);
  Verification_pipeline.For_testing.set_function_spin_iterations 0;
  require "runtime has no valid higher thread count"
    (Multicore.max_domains () >= 3);
  let higher = run_report implementation 3 in
  let resolved_default = default_threads () in
  let default = run_report implementation resolved_default in
  List.iter
    (fun (label, report) ->
      require (label ^ ": semantic SST rendered without a request")
        (not (semantic_sst_is_forced report)))
    [
      ("serial", serial);
      ("threaded", threaded);
      ("higher", higher);
      ("default", default);
    ];
  let expected_schedulers = 2 + if resolved_default = 1 then 0 else 1 in
  require "scheduler lifecycle across production modes"
    (Verification_pipeline.For_testing.scheduler_creations ()
       = expected_schedulers
    && Verification_pipeline.For_testing.scheduler_stops ()
       = expected_schedulers);
  require "threaded production report parity" (same_report serial threaded);
  require "higher production report parity" (same_report serial higher);
  require "default production report parity" (same_report serial default);
  List.iter
    (fun (label, report) ->
      require (label ^ ": explicit semantic SST request was not memoized")
        (semantic_sst_is_forced report))
    [
      ("serial", serial);
      ("threaded", threaded);
      ("higher", higher);
      ("default", default);
    ];
  require_cleanup "verified production";
  Printf.printf
    "production=parity status=%s transcript=byte-equivalent semantic-sst=lazy/explicit-memoized frontier-peak=1@1,%d@2 scheduler=0@1,1/1@2,1/1@higher default=parity cleanup=zero\n"
    (status (Verification_driver_private.status threaded)) threaded_peak

let parity label filename =
  let implementation = load filename in
  reset_injections ();
  Verification_pipeline.For_testing.reset_scheduler_counts ();
  Verification_pipeline.For_testing.reset_function_observation ();
  Function_vc_worker_private.For_testing.reset ();
  Z3_bridge.reset_counters ();
  let serial = run_report implementation 1 in
  let threaded = run_report implementation 2 in
  require "runtime has no valid higher thread count"
    (Multicore.max_domains () >= 3);
  let higher = run_report implementation 3 in
  let resolved_default = default_threads () in
  let default = run_report implementation resolved_default in
  require (label ^ ": serial/2 mismatch") (same_report serial threaded);
  require (label ^ ": serial/higher mismatch") (same_report serial higher);
  require (label ^ ": serial/default mismatch") (same_report serial default);
  let expected_schedulers = 2 + if resolved_default = 1 then 0 else 1 in
  require (label ^ ": scheduler lifecycle")
    (Verification_pipeline.For_testing.scheduler_creations ()
       = expected_schedulers
    && Verification_pipeline.For_testing.scheduler_stops ()
       = expected_schedulers);
  require_cleanup label;
  Printf.printf
    "parity=%s status=%s serial/default/2/higher=identical rlimit=100000 scheduler/context=clean\n"
    label (status (Verification_driver_private.status serial))

let event_index predicate events =
  let rec loop index = function
    | [] -> None
    | event :: rest ->
        if predicate event then Some index else loop (index + 1) rest
  in
  loop 0 events

let receipt_frontiers success_filename failure_filename =
  let open Verification_pipeline.For_testing in
  let run filename =
    reset_injections ();
    Verification_pipeline.For_testing.reset_scheduler_counts ();
    Verification_pipeline.For_testing.reset_frontier_events ();
    Function_vc_worker_private.For_testing.reset ();
    Z3_bridge.reset_counters ();
    let report = run_report (load filename) 2 in
    let events = Verification_pipeline.For_testing.frontier_events () in
    require "receipt scheduler lifecycle"
      (Verification_pipeline.For_testing.scheduler_creations () = 1
      && Verification_pipeline.For_testing.scheduler_stops () = 1);
    require_cleanup "receipt frontier";
    (report, events)
  in
  let success, success_events = run success_filename in
  require "receipt-success status"
    (Verification_driver_private.status success
    = Verification_pipeline.Verified);
  let producer_commit =
    event_index
      (function
        | Verification_pipeline.For_testing.Committed (_, "Box.make") -> true
        | Materialized _ | Committed _ | Blocked _ -> false)
      success_events
  in
  let dependent_materialization =
    event_index
      (function
        | Verification_pipeline.For_testing.Materialized (_, "run") -> true
        | Materialized _ | Committed _ | Blocked _ -> false)
      success_events
  in
  require "dependent admitted before producer canonical commit"
    (match (producer_commit, dependent_materialization) with
    | Some commit, Some materialized -> commit < materialized
    | None, _ | _, None -> false);
  let failed, failed_events = run failure_filename in
  require "receipt-failure status"
    (Verification_driver_private.status failed
    = Verification_pipeline.Counterexample);
  require "failed producer did not block dependent"
    (List.exists
       (function
         | Verification_pipeline.For_testing.Blocked (_, "run") -> true
         | Materialized _ | Committed _ | Blocked _ -> false)
       failed_events);
  require "failed dependent was materialized"
    (not
       (List.exists
          (function
            | Verification_pipeline.For_testing.Materialized (_, "run") ->
                true
            | Materialized _ | Committed _ | Blocked _ -> false)
          failed_events));
  print_endline
    "receipts=producer-commit-before-dependent failed-producer=blocked scheduler/context=clean"

type injected_error = Materialization | Preparation | Worker

let inject_error kind ordinal =
  reset_injections ();
  match kind with
  | Materialization ->
      Verification_pipeline.For_testing.inject_materialization_error
        (Some ordinal)
  | Preparation ->
      Verification_solver_private.For_testing.inject_preparation_error
        (Some ordinal)
  | Worker ->
      Function_vc_worker_private.For_testing.inject_worker_error (Some ordinal)

let committed_ordinals events =
  let open Verification_pipeline.For_testing in
  List.filter_map
    (function
      | Verification_pipeline.For_testing.Committed (ordinal, _) ->
          Some ordinal
      | Materialized _ | Blocked _ -> None)
    events

let error_precedence filename =
  let implementation = load filename in
  let run kind ordinal expected_commits =
    Verification_pipeline.For_testing.reset_scheduler_counts ();
    Verification_pipeline.For_testing.reset_frontier_events ();
    Function_vc_worker_private.For_testing.reset ();
    Z3_bridge.reset_counters ();
    inject_error kind ordinal;
    (match
       Verification_driver_private.run_with_threads ~threads:2
         ~rlimit:100_000 ~timeout_ms:60_000 ~allow_imported_opens:false
         implementation
     with
    | Error _ -> ()
    | Ok _ -> fail "injected error ordinal=%d was not surfaced" ordinal);
    require "injected error committed a later function"
      (committed_ordinals
         (Verification_pipeline.For_testing.frontier_events ())
      = expected_commits);
    require "injected error scheduler lifecycle"
      (Verification_pipeline.For_testing.scheduler_creations () = 1
      && Verification_pipeline.For_testing.scheduler_stops () = 1);
    require_cleanup "injected error";
    reset_injections ()
  in
  List.iter
    (fun kind ->
      run kind 0 [];
      run kind 2 [ 0; 1 ])
    [ Materialization; Preparation; Worker ];
  print_endline
    "errors=materialization/preparation/worker lower=first higher=prior-canonical-commits no-later-effects scheduler/context=clean"

let () =
  match Array.to_list Sys.argv with
  | [ _; "frontier" ] -> frontier ()
  | [ _; "physical" ] -> physical ()
  | [ _; "worker" ] -> worker ()
  | [ _; "parallel" ] -> parallel ()
  | [ _; "production"; filename ] -> production filename
  | [ _; "parity"; label; filename ] -> parity label filename
  | [ _; "receipts"; success; failure ] ->
      receipt_frontiers success failure
  | [ _; "errors"; filename ] -> error_precedence filename
  | _ -> fail "usage: function_vc_dispatch_tool MODE [ARGS]"
