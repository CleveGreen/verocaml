open Outcome_test_support

let suite_path = "test/verification_scope/outcome_cases.ml"
let ( let* ) = Result.bind

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture name =
  let relative = Filename.concat "fixtures" name in
  let path =
    if Sys.file_exists relative then relative
    else Filename.concat "test/verification_scope" relative
  in
  read_file path

let project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents =
              "(lang dune 3.17)\n(name verification_scope_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              {|
(library
 (name scope_bootstrap)
 (wrapped false)
 (modules Pass_root)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name scope_provider)
 (wrapped false)
 (modules Provider)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name scope_roots)
 (wrapped false)
 (modules Client Second_root Fail_root)
 (libraries verocaml.ghost scope_provider)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name scope_ordinary)
 (wrapped false)
 (modules Legacy Unmarked_provider)
 (flags (:standard -ppx "verocaml-ppx")))

(library
 (name scope_bypass)
 (wrapped false)
 (modules Unmarked_client)
 (libraries verocaml.ghost scope_ordinary)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          };
          { path = "pass_root.ml"; contents = fixture "pass_root.ml" };
          { path = "provider.mli"; contents = fixture "provider.mli" };
          { path = "provider.ml"; contents = fixture "provider.ml" };
          { path = "client.ml"; contents = fixture "client.ml" };
          { path = "second_root.ml"; contents = fixture "second_root.ml" };
          { path = "fail_root.ml"; contents = fixture "fail_root.ml" };
          { path = "legacy.ml"; contents = fixture "legacy.ml" };
          {
            path = "unmarked_provider.ml";
            contents = fixture "unmarked_provider.ml";
          };
          {
            path = "unmarked_client.ml";
            contents = fixture "unmarked_client.ml";
          };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Pass_root" ];
    }

let bootstrap_expectation =
  Expectation.empty |> Expectation.status Outcome.Verified
  |> Expectation.require_unit "Pass_root" Outcome.Unit_verified

let prepare_project ~environment ~workspace =
  let compiled = Filename.concat workspace "compiled" in
  let* outcome = Fixture.run ~environment ~workspace:compiled project in
  let* () =
    match Expectation.check bootstrap_expectation outcome with
    | Ok () -> Ok ()
    | Error message -> mismatch "%s" message
  in
  Ok (Filename.concat compiled "project")

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

type artifact = {
  unit_name : string;
  cmt : string;
  cmi : string;
  implementation : Cmt_input.implementation;
}

let load_artifact ~unit_name ~cmt ~cmi =
  match Cmt_input.load_with_interface ~cmt ~cmi () with
  | Ok implementation -> Ok { unit_name; cmt; cmi; implementation }
  | Error diagnostic ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (Printf.sprintf "%s [%s]: %s" unit_name diagnostic.Diagnostic.code
              diagnostic.message))

let discover_artifact project_root unit_name =
  let basename = String.uncapitalize_ascii unit_name ^ ".cmt" in
  match
    files_below (Filename.concat project_root "_build")
    |> List.filter (fun path -> String.equal (Filename.basename path) basename)
  with
  | [ cmt ] ->
      let cmi = Filename.remove_extension cmt ^ ".cmi" in
      if Sys.file_exists cmi then load_artifact ~unit_name ~cmt ~cmi
      else
        Error
          (Failure.make Failure.Selected_cmt_discovery
             ("selected CMI is absent for unit " ^ unit_name))
  | [] ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("no selected CMT for unit " ^ unit_name))
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("ambiguous selected CMT for unit " ^ unit_name))

let load_artifacts project_root unit_names =
  unit_names
  |> List.fold_left
       (fun result unit_name ->
         let* artifacts = result in
         let* artifact = discover_artifact project_root unit_name in
         Ok ((unit_name, artifact) :: artifacts))
       (Ok [])

let artifact name artifacts =
  match List.assoc_opt name artifacts with
  | Some artifact -> Ok artifact
  | None -> mismatch "prepared project omitted artifact %s" name

let configuration threads =
  match
    Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None
  with
  | Ok configuration -> Ok configuration
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Verifier_service.configuration_error_message error))

let inventory_entry role artifact =
  (role, artifact.cmt, artifact.cmi, artifact.implementation)

let run_scope ~threads inventory =
  let* configuration = configuration threads in
  match Verifier_service.scoped_request ~configuration ~inventory with
  | Error error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.scoped_plan_error_message error))
  | Ok request -> Ok (Verifier_service.verify_scope request)

let find_row unit_name scoped =
  match
    Verifier_service.scoped_rows scoped
    |> List.filter (fun row ->
           String.equal (Verifier_service.scoped_row_unit_name row) unit_name)
  with
  | [ row ] -> Ok row
  | [] -> mismatch "scope omitted keyed unit %s" unit_name
  | _ -> mismatch "scope repeated keyed unit %s" unit_name

let disposition projection =
  match Outcome.status projection with
  | Outcome.Verified -> Outcome.Unit_verified
  | Counterexample -> Unit_counterexample
  | Inconclusive -> Unit_inconclusive
  | Incomplete_source -> Unit_incomplete_source
  | Frontend_rejected -> Unit_frontend_rejected

let rejected_projection unit_name error =
  match Verifier_service.error_diagnostic error with
  | Some diagnostic ->
      Ok
        (Outcome.frontend_rejection ~code:diagnostic.Diagnostic.code
        |> Outcome.with_unit unit_name Outcome.Unit_frontend_rejected)
  | None ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))

let row_projection unit_name scoped =
  let* row = find_row unit_name scoped in
  match
    ( Verifier_service.scoped_row_classification row,
      Verifier_service.scoped_row_outcome row )
  with
  | Scoped_verified, Scoped_verification result ->
      let projection = Outcome.of_verifier_result result in
      Ok (Outcome.with_unit unit_name (disposition projection) projection)
  | (Scoped_verified | Scoped_verified_dependency), Scoped_rejection error ->
      rejected_projection unit_name error
  | Scoped_verified_dependency, Scoped_dependency_success ->
      Ok
        (Outcome.observation ~status:Outcome.Verified ()
        |> Outcome.project
        |> Outcome.with_unit unit_name Outcome.Unit_dependency_success)
  | Scoped_skipped, Scoped_skip ->
      Ok
        (Outcome.observation ~status:Outcome.Verified ()
        |> Outcome.project
        |> Outcome.with_unit unit_name Outcome.Unit_skipped)
  | ( Scoped_verified,
      (Scoped_dependency_success | Scoped_skip) )
  | ( Scoped_verified_dependency,
      (Scoped_verification _ | Scoped_skip) )
  | ( Scoped_skipped,
      (Scoped_verification _ | Scoped_dependency_success | Scoped_rejection _) ) ->
      mismatch "scope returned an inconsistent classification for %s" unit_name

let scope_projection unit_names scoped =
  unit_names
  |> List.fold_left
       (fun result unit_name ->
         let* projections = result in
         let* projection = row_projection unit_name scoped in
         Ok (projection :: projections))
       (Ok [])
  |> Result.map Outcome.merge

let mode key value projection =
  Outcome.merge
    [
      projection;
      Outcome.observation ~status:Outcome.Verified
        ~named_facts:[ (key, Outcome.Function_exists value) ] ()
      |> Outcome.project;
    ]

let require_parity ~except left right =
  match Outcome.semantic_parity ~except left right with
  | Ok () -> Ok ()
  | Error message -> mismatch "%s" message

let ordinary_root_is_skipped =
  Suite.case ~name:"ordinary-root-is-skipped"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Legacy" Outcome.Unit_skipped)
    (fun ~environment ~workspace ->
      let* project_root = prepare_project ~environment ~workspace in
      let* artifacts = load_artifacts project_root [ "Legacy" ] in
      let* legacy = artifact "Legacy" artifacts in
      let* scoped =
        run_scope ~threads:1
          [ inventory_entry Verifier_service.Scope_root legacy ]
      in
      scope_projection [ "Legacy" ] scoped)

let marked_legacy_is_rejected =
  let input =
    Fixture.single_source ~module_name:"Marked_legacy"
      ~source:(fixture "marked_legacy.ml") ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"marked-legacy-is-frontend-rejected"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_STRUCTURE_ITEM"
      |> Expectation.require_unit "Marked_legacy"
           Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace -> Fixture.run ~environment ~workspace input)

let retained_marker_parity =
  let marked_source = fixture "pass_root.ml" in
  let unmarked_source =
    match String.split_on_char '\n' marked_source with
    | _marker :: _blank :: body -> String.concat "\n" body
    | _ -> marked_source
  in
  let input source =
    Fixture.single_source ~module_name:"Pass_root" ~source
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"retained-marker-semantic-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Pass_root" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:identity"
           (Outcome.Function_exists "identity")
      |> Expectation.require_named_fact "source-marking"
           (Outcome.Function_exists "marked"))
    (fun ~environment ~workspace ->
      let* marked =
        Fixture.run ~environment ~workspace:(Filename.concat workspace "marked")
          (input marked_source)
      in
      let* unmarked =
        Fixture.run ~environment
          ~workspace:(Filename.concat workspace "unmarked")
          (input unmarked_source)
      in
      let marked = mode "source-marking" "marked" marked in
      let unmarked = mode "source-marking" "unmarked" unmarked in
      let* () = require_parity ~except:[ "source-marking" ] marked unmarked in
      Ok marked)

let scope_inventory artifacts order =
  order
  |> List.fold_left
       (fun result (role, unit_name) ->
         let* entries = result in
         let* artifact = artifact unit_name artifacts in
         Ok (inventory_entry role artifact :: entries))
       (Ok [])
  |> Result.map List.rev

let selected_scope_parity =
  let names = [ "Client"; "Second_root"; "Provider" ] in
  let serial_order =
    [
      (Verifier_service.Scope_dependency, "Provider");
      (Scope_root, "Second_root");
      (Scope_root, "Client");
    ]
  in
  let threaded_order =
    [
      (Verifier_service.Scope_root, "Client");
      (Scope_root, "Second_root");
      (Scope_dependency, "Provider");
    ]
  in
  Suite.case ~name:"selected-scope-order-thread-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Client" Outcome.Unit_verified
      |> Expectation.require_unit "Second_root" Outcome.Unit_verified
      |> Expectation.require_unit "Provider" Outcome.Unit_dependency_success
      |> Expectation.require_named_fact "function:call"
           (Outcome.Function_exists "call")
      |> Expectation.require_named_fact "function:preserve"
           (Outcome.Function_exists "preserve")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-1")
      |> Expectation.require_named_fact "inventory-order"
           (Outcome.Function_exists "provider-first"))
    (fun ~environment ~workspace ->
      let* project_root = prepare_project ~environment ~workspace in
      let* artifacts = load_artifacts project_root names in
      let* serial_inventory = scope_inventory artifacts serial_order in
      let* threaded_inventory = scope_inventory artifacts threaded_order in
      let* serial = run_scope ~threads:1 serial_inventory in
      let* threaded = run_scope ~threads:2 threaded_inventory in
      let* serial = scope_projection names serial in
      let* threaded = scope_projection names threaded in
      let serial =
        serial |> mode "execution-mode" "threads-1"
        |> mode "inventory-order" "provider-first"
      in
      let threaded =
        threaded |> mode "execution-mode" "threads-2"
        |> mode "inventory-order" "roots-first"
      in
      let* () =
        require_parity ~except:[ "execution-mode"; "inventory-order" ] serial
          threaded
      in
      Ok serial)

let copy_file source destination =
  let input_channel = open_in_bin source in
  let output_channel = open_out_bin destination in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr input_channel;
      close_out_noerr output_channel)
    (fun () ->
      let buffer = Bytes.create 8192 in
      let rec copy () =
        match input input_channel buffer 0 (Bytes.length buffer) with
        | 0 -> ()
        | length ->
            output output_channel buffer 0 length;
            copy ()
      in
      copy ())

let copy_artifact directory artifact =
  let cmt = Filename.concat directory (Filename.basename artifact.cmt) in
  let cmi = Filename.concat directory (Filename.basename artifact.cmi) in
  copy_file artifact.cmt cmt;
  copy_file artifact.cmi cmi;
  load_artifact ~unit_name:artifact.unit_name ~cmt ~cmi

let copied_artifact_parity =
  let names = [ "Client"; "Second_root"; "Provider" ] in
  let order =
    [
      (Verifier_service.Scope_root, "Client");
      (Scope_root, "Second_root");
      (Scope_dependency, "Provider");
    ]
  in
  Suite.case ~name:"copied-artifact-semantic-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Client" Outcome.Unit_verified
      |> Expectation.require_unit "Second_root" Outcome.Unit_verified
      |> Expectation.require_unit "Provider" Outcome.Unit_dependency_success
      |> Expectation.require_named_fact "artifact-location"
           (Outcome.Function_exists "original"))
    (fun ~environment ~workspace ->
      let* project_root = prepare_project ~environment ~workspace in
      let* original = load_artifacts project_root names in
      let copied_directory = Filename.concat workspace "copied" in
      Unix.mkdir copied_directory 0o755;
      let* copied =
        original
        |> List.fold_left
             (fun result (unit_name, artifact) ->
               let* artifacts = result in
               let* copied = copy_artifact copied_directory artifact in
               Ok ((unit_name, copied) :: artifacts))
             (Ok [])
      in
      let* original_inventory = scope_inventory original order in
      let* copied_inventory = scope_inventory copied order in
      let* original = run_scope ~threads:1 original_inventory in
      let* copied = run_scope ~threads:1 copied_inventory in
      let* original = scope_projection names original in
      let* copied = scope_projection names copied in
      let original = mode "artifact-location" "original" original in
      let copied = mode "artifact-location" "copied" copied in
      let* () =
        require_parity ~except:[ "artifact-location" ] original copied
      in
      Ok original)

let partial_failure_partition =
  let names = [ "Fail_root"; "Client"; "Provider"; "Legacy" ] in
  let order =
    [
      (Verifier_service.Scope_root, "Fail_root");
      (Scope_root, "Client");
      (Scope_dependency, "Provider");
      (Scope_root, "Legacy");
    ]
  in
  Suite.case ~name:"partial-failure-preserves-unit-partition"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Fail_root" Outcome.Unit_counterexample
      |> Expectation.require_unit "Client" Outcome.Unit_verified
      |> Expectation.require_unit "Provider" Outcome.Unit_dependency_success
      |> Expectation.require_unit "Legacy" Outcome.Unit_skipped
      |> Expectation.require_semantic ~function_name:"reject" Outcome.Assertion)
    (fun ~environment ~workspace ->
      let* project_root = prepare_project ~environment ~workspace in
      let* artifacts = load_artifacts project_root names in
      let* inventory = scope_inventory artifacts order in
      let* scoped = run_scope ~threads:1 inventory in
      scope_projection names scoped)

let unmarked_provider_rejection =
  let names = [ "Unmarked_client"; "Unmarked_provider" ] in
  let order =
    [
      (Verifier_service.Scope_root, "Unmarked_client");
      (Scope_dependency, "Unmarked_provider");
    ]
  in
  Suite.case ~name:"unmarked-provider-call-is-rejected"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EXTERNAL_CALL"
      |> Expectation.require_unit "Unmarked_client"
           Outcome.Unit_frontend_rejected
      |> Expectation.require_unit "Unmarked_provider" Outcome.Unit_skipped)
    (fun ~environment ~workspace ->
      let* project_root = prepare_project ~environment ~workspace in
      let* artifacts = load_artifacts project_root names in
      let* inventory = scope_inventory artifacts order in
      let* scoped = run_scope ~threads:1 inventory in
      scope_projection names scoped)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      ordinary_root_is_skipped;
      marked_legacy_is_rejected;
      retained_marker_parity;
      selected_scope_parity;
      copied_artifact_parity;
      partial_failure_partition;
      unmarked_provider_rejection;
    ]
