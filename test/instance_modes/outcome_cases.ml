open Outcome_test_support

let suite_path = "test/instance_modes/outcome_cases.ml"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture_source name =
  let local = Filename.concat "fixtures" name in
  let repository = Filename.concat "test/instance_modes/fixtures" name in
  if Sys.file_exists local then read_file local
  else if Sys.file_exists repository then read_file repository
  else failwith ("missing fixture source: " ^ repository)

type source_module = {
  unit_name : string;
  implementation : string;
  interface : string option;
}

let source_module unit_name implementation =
  { unit_name; implementation; interface = None }

let interface_module unit_name ~implementation ~interface =
  { unit_name; implementation; interface = Some interface }

let project ~name modules =
  let module_names = List.map (fun source -> source.unit_name) modules in
  let source_files =
    modules
    |> List.concat_map (fun source ->
           let basename = String.uncapitalize_ascii source.unit_name in
           let implementation =
             {
               Fixture.path = basename ^ ".ml";
               contents = fixture_source source.implementation;
             }
           in
           match source.interface with
           | None -> [ implementation ]
           | Some interface ->
               [
                 {
                   Fixture.path = basename ^ ".mli";
                   contents = fixture_source interface;
                 };
                 implementation;
               ])
  in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name;
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
                 (libraries verocaml.ghost)\n (flags (:standard -ppx \
                 \"verocaml-ppx --keep-ghost\")))\n"
                name (String.concat " " module_names);
          };
        ]
        @ source_files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = module_names;
    }

let run input ~environment ~workspace = Fixture.run ~environment ~workspace input

let mismatch message = Error (Failure.make Failure.Expectation_mismatch message)

let expected_frontend_rejection ~unit_name input ~environment ~workspace =
  match Fixture.run ~environment ~workspace input with
  | Ok outcome when Outcome.status outcome = Outcome.Frontend_rejected -> Ok outcome
  | Ok outcome ->
      mismatch
        (Printf.sprintf "expected frontend rejection for %s, observed %s" unit_name
           (Outcome.status_name (Outcome.status outcome)))
  | Error failure -> (
      match Failure.category failure with
      | Failure.Dune_build | Failure.Verifier_outcome ->
          Ok
            (Outcome.observation ~status:Outcome.Frontend_rejected
               ~units:[ (unit_name, Outcome.Unit_frontend_rejected) ] ()
            |> Outcome.project)
      | _ -> Error failure)

let expectation ~units ~functions =
  let expectation =
    List.fold_left
      (fun expectation unit_name ->
        Expectation.require_unit unit_name Outcome.Unit_verified expectation)
      (Expectation.empty |> Expectation.status Outcome.Verified)
      units
  in
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_named_fact ("function:" ^ function_name)
        (Outcome.Function_exists function_name) expectation)
    expectation functions

let mode_defaults_case =
  let input =
    project ~name:"instance_mode_defaults_fixture"
      [
        source_module "Modes" "modes.ml";
        source_module "Forgetting" "forgetting.ml";
      ]
  in
  Suite.case ~name:"mode-defaults-and-forgetting"
    ~expectation:
      (expectation ~units:[ "Modes"; "Forgetting" ]
         ~functions:
           [
             "proof_observe";
             "tracked_id";
             "exec_erased";
             "exec_tracked_result";
             "run";
           ])
    (run input)

let standalone_proof_case =
  let input =
    project ~name:"standalone_proof_tracked_fixture"
      [
        source_module "Tracked_call_bare" "tracked_call_bare.ml";
        source_module "Tracked_return_bare" "tracked_return_bare.ml";
        source_module "Standalone_proof_bare_tracked"
          "standalone_proof_bare_tracked.ml";
      ]
  in
  Suite.case ~name:"standalone-proof-tracked-boundaries"
    ~expectation:
      (expectation
         ~units:
           [
             "Tracked_call_bare";
             "Tracked_return_bare";
             "Standalone_proof_bare_tracked";
           ]
         ~functions:[ "consume"; "caller"; "identity"; "observe" ])
    (run input)

let default_ghost_forgetting_expectation =
  expectation ~units:[ "Positive_forgetting" ]
    ~functions:[ "proof_observe"; "recursive_observe"; "tracked_successor"; "run" ]
  |> Expectation.require_named_fact "obligation-kind:proof_observe"
       (Outcome.Obligation_kind_exists
          { function_name = "proof_observe"; kind = Outcome.Assertion })
  |> Expectation.require_named_fact "obligation-kind:proof_observe"
       (Outcome.Obligation_kind_exists
          { function_name = "proof_observe"; kind = Outcome.Postcondition })
  |> Expectation.require_named_fact "obligation-kind:recursive_observe"
       (Outcome.Obligation_kind_exists
          {
            function_name = "recursive_observe";
            kind = Outcome.Entry_measure_nonnegative;
          })
  |> Expectation.require_named_fact "obligation-kind:recursive_observe"
       (Outcome.Obligation_kind_exists
          {
            function_name = "recursive_observe";
            kind =
              Outcome.Recursive_call_measure_nonnegative
                { callee = "recursive_observe" };
          })
  |> Expectation.require_named_fact "obligation-kind:recursive_observe"
       (Outcome.Obligation_kind_exists
          {
            function_name = "recursive_observe";
            kind =
              Outcome.Recursive_call_strict_descent
                { callee = "recursive_observe" };
          })

let default_ghost_forgetting_case =
  let input =
    project ~name:"default_ghost_forgetting_fixture"
      [ source_module "Positive_forgetting" "positive_forgetting.ml" ]
  in
  Suite.case ~name:"default-ghost-forgetting"
    ~expectation:default_ghost_forgetting_expectation (run input)

let aggregates_and_local_invariants_case =
  let input =
    project ~name:"aggregate_local_invariant_fixture"
      [
        source_module "Aggregate" "aggregate.ml";
        source_module "Local_established_invariant"
          "local_established_invariant.ml";
      ]
  in
  Suite.case ~name:"aggregates-and-local-invariants"
    ~expectation:
      (expectation ~units:[ "Aggregate"; "Local_established_invariant" ]
         ~functions:[ "make"; "read"; "make_c"; "read_c" ])
    (run input)

let rejection_case ~name source =
  let input =
    project ~name:(String.map (function '-' -> '_' | character -> character) name)
      [ source ]
  in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_unit source.unit_name
           Outcome.Unit_frontend_rejected)
    (expected_frontend_rejection ~unit_name:source.unit_name input)

let rejection_cases =
  [
    rejection_case ~name:"instance-mode-rejects-ordinary-absence"
      (source_module "Ordinary_absence" "ordinary_absence.ml");
    rejection_case ~name:"signature-mode-mismatch"
      (interface_module "Signature_mode_mismatch"
         ~implementation:"signature_mode_mismatch.ml"
         ~interface:"signature_mode_mismatch.mli");
    rejection_case ~name:"type-mode-mismatch"
      (interface_module "Type_mode_mismatch"
         ~implementation:"type_mode_mismatch.ml"
         ~interface:"type_mode_mismatch.mli");
    rejection_case ~name:"missing-actual-mode"
      (source_module "Missing_actual" "missing_actual.ml");
    rejection_case ~name:"cross-mode-escalation"
      (source_module "Cross_mode" "cross_mode.ml");
    rejection_case ~name:"tracked-formal-bare"
      (source_module "Tracked_formal_bare" "tracked_formal_bare.ml");
    rejection_case ~name:"open-aggregate-pattern"
      (source_module "Open_pattern" "open_pattern.ml");
    rejection_case ~name:"erased-effect"
      (source_module "Erased_effect" "erased_effect.ml");
    rejection_case ~name:"ghost-invariant-authority"
      (source_module "Ghost_authority" "ghost_authority.ml");
    rejection_case ~name:"mode-bearing-external-specification"
      (source_module "External_mode" "external_mode.ml");
    rejection_case ~name:"mode-bearing-trusted-body"
      (source_module "Trusted_mode" "trusted_mode.ml");
    rejection_case ~name:"exec-call-tracked-bare"
      (source_module "Exec_call_tracked_bare" "exec_call_tracked_bare.ml");
    rejection_case ~name:"forgetting-effect"
      (source_module "Forgetting_effect" "forgetting_effect.ml");
    rejection_case ~name:"explicit-ghost-spec-bare"
      (source_module "Explicit_ghost_spec_bare" "explicit_ghost_spec_bare.ml");
    rejection_case ~name:"explicit-ghost-proof-bare"
      (source_module "Explicit_ghost_proof_bare" "explicit_ghost_proof_bare.ml");
    rejection_case ~name:"recursive-tracked-formal"
      (source_module "Recursive_tracked" "recursive_tracked.ml");
    rejection_case ~name:"inherited-mode-update"
      (source_module "Inherited_update" "inherited_update.ml");
  ]

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    ([
       mode_defaults_case;
       standalone_proof_case;
       default_ghost_forgetting_case;
       aggregates_and_local_invariants_case;
     ]
    @ rejection_cases)
