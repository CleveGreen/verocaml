let fail format = Printf.ksprintf failwith format

let load cmt cmi = Cmt_input.load_with_interface ~cmt ~cmi ()

let require_load cmt cmi =
  match load cmt cmi with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "load failed [%s]: %s" diagnostic.Diagnostic.code diagnostic.message

let configuration () =
  match
    Verifier_service.configuration ~threads:1 ~timeout_ms:60_000 ~rlimit:None
  with
  | Ok configuration -> configuration
  | Error error ->
      fail "configuration failed: %s"
        (Verifier_service.configuration_error_message error)

type counters = {
  retained : int;
  typed_lowering : int;
  semantic_validation : int;
  vc_pipeline : int;
  private_driver : int;
  provider_reverification : int;
  backend_solver : int;
  z3 : Z3_bridge.counters;
}

let reset_counters () =
  Interface_specification_candidate_private.For_testing
  .reset_strict_candidate_entries ();
  Typedtree_lowering_private.For_testing.reset_lowering_entries ();
  Verification_pipeline.For_testing.reset_validated_pipeline_entries ();
  Verification_driver_private.For_testing.reset_driver_entries ();
  Interface_specification_loaded_private.For_testing
  .reset_provider_verification_entries ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

let counters ~semantic_validation =
  {
    retained =
      Interface_specification_candidate_private.For_testing
      .strict_candidate_entries ();
    typed_lowering =
      Typedtree_lowering_private.For_testing.lowering_entries ();
    semantic_validation;
    vc_pipeline =
      Verification_pipeline.For_testing.validated_pipeline_entries ();
    private_driver =
      Verification_driver_private.For_testing.driver_entries ();
    provider_reverification =
      Interface_specification_loaded_private.For_testing
      .provider_verification_entries ();
    (* Solver creation is the existing actual backend/solver boundary.  The
       report names both requirements from this one observation. *)
    backend_solver = Solver_backend.For_testing.solver_creation_count ();
    z3 = Z3_bridge.counters ();
  }

(* This existing private observer runs inside Sst_validation's owned validation
   boundary; identity mutation leaves product semantics unchanged. *)
let observe_semantic_validation callback =
  let entries = ref 0 in
  let result =
    Sst_validation_private.Public.For_testing
    .with_program_mutation_at_validation_boundary ~mutate:Fun.id
      ~observe:(fun () -> incr entries) callback
  in
  (result, !entries)

let z3_is_zero counters =
  counters.Z3_bridge.capability_resolutions = 0
  && counters.translations = 0
  && counters.contexts_created = 0
  && counters.solvers_created = 0
  && counters.solver_resets = 0
  && counters.contexts_cleaned = 0

let all_zero counters =
  counters.retained = 0 && counters.typed_lowering = 0
  && counters.semantic_validation = 0 && counters.vc_pipeline = 0
  && counters.private_driver = 0 && counters.provider_reverification = 0
  && counters.backend_solver = 0 && z3_is_zero counters.z3

let print_zero label counters =
  if not (all_zero counters) then fail "%s advanced an actual owner counter" label;
  Printf.printf
    "%s retained=%d typed-lowering=%d semantic=%d vc=%d backend=%d private-driver=%d provider-reverification=%d solver=%d z3=%d/%d/%d/%d/%d/%d\n"
    label counters.retained counters.typed_lowering
    counters.semantic_validation counters.vc_pipeline counters.backend_solver
    counters.private_driver counters.provider_reverification
    counters.backend_solver counters.z3.capability_resolutions
    counters.z3.translations counters.z3.contexts_created
    counters.z3.solvers_created counters.z3.solver_resets
    counters.z3.contexts_cleaned

let positive counter = if counter > 0 then "advanced" else "zero"

let print_positive counters =
  let z3_advanced =
    counters.z3.translations > 0 && counters.z3.contexts_created > 0
    && counters.z3.solvers_created > 0 && counters.z3.contexts_cleaned > 0
  in
  if
    counters.retained = 0 || counters.typed_lowering = 0
    || counters.semantic_validation = 0 || counters.vc_pipeline = 0
    || counters.private_driver = 0 || counters.provider_reverification = 0
    || counters.backend_solver = 0 || not z3_advanced
  then fail "selected verification did not advance every actual owner counter";
  Printf.printf
    "selected-counters retained=%s typed-lowering=%s semantic=%s vc=%s backend=%s private-driver=%s provider-reverification=%s solver=%s z3=%s\n"
    (positive counters.retained) (positive counters.typed_lowering)
    (positive counters.semantic_validation) (positive counters.vc_pipeline)
    (positive counters.backend_solver) (positive counters.private_driver)
    (positive counters.provider_reverification) (positive counters.backend_solver)
    (if z3_advanced then "advanced" else "zero")

let scoped_request inventory =
  Verifier_service.scoped_request ~configuration:(configuration ()) ~inventory

let skip cmt cmi =
  let implementation = require_load cmt cmi in
  reset_counters ();
  let request =
    match
      scoped_request [ (Verifier_service.Scope_root, cmt, cmi, implementation) ]
    with
    | Ok request -> request
    | Error error ->
        fail "scope failed: %s"
          (Verifier_service.scoped_plan_error_message error)
  in
  let result, semantic_validation =
    observe_semantic_validation (fun () -> Verifier_service.verify_scope request)
  in
  let rows = Verifier_service.scoped_rows result in
  (match rows with
  | [ row ] -> (
      match
        ( Verifier_service.scoped_row_classification row,
          Verifier_service.scoped_row_outcome row )
      with
      | Scoped_skipped, Scoped_skip -> ()
      | _ -> fail "ordinary artifact did not produce one skipped row")
  | _ -> fail "ordinary artifact did not produce one skipped row");
  print_zero "skip-counters" (counters ~semantic_validation)

let selected root_cmt root_cmi dependency_cmt dependency_cmi =
  let root = require_load root_cmt root_cmi
  and dependency = require_load dependency_cmt dependency_cmi in
  reset_counters ();
  let request =
    match
      scoped_request
        [
          (Verifier_service.Scope_root, root_cmt, root_cmi, root);
          ( Scope_dependency,
            dependency_cmt,
            dependency_cmi,
            dependency );
        ]
    with
    | Ok request -> request
    | Error error ->
        fail "scope failed: %s"
          (Verifier_service.scoped_plan_error_message error)
  in
  let _, semantic_validation =
    observe_semantic_validation (fun () -> Verifier_service.verify_scope request)
  in
  print_positive (counters ~semantic_validation)

let rejected cmt cmi =
  reset_counters ();
  let preflight, semantic_validation =
    observe_semantic_validation (fun () ->
        match load cmt cmi with
        | Error diagnostic ->
            Printf.sprintf "load-rejected code=%s message=%s" diagnostic.code diagnostic.message
        | Ok implementation -> (
            match
              scoped_request
                [ (Verifier_service.Scope_root, cmt, cmi, implementation) ]
            with
            | Error error ->
                Printf.sprintf "scope-rejected message=%s"
                  (Verifier_service.scoped_plan_error_message error)
            | Ok _ -> fail "invalid explicit pair unexpectedly selected"))
  in
  Printf.printf "preflight=%s\n" preflight;
  print_zero "preflight-counters" (counters ~semantic_validation)

let () =
  match Array.to_list Sys.argv with
  | [ _; "skip"; cmt; cmi ] -> skip cmt cmi
  | [ _; "selected"; root_cmt; root_cmi; dependency_cmt; dependency_cmi ] ->
      selected root_cmt root_cmi dependency_cmt dependency_cmi
  | [ _; "rejected"; cmt; cmi ] -> rejected cmt cmi
  | _ ->
      fail
        "usage: %s skip CMT CMI | selected ROOT_CMT ROOT_CMI DEP_CMT DEP_CMI | rejected CMT CMI"
        Sys.argv.(0)
