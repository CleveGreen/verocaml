open Outcome_test_support

let suite_path = "test/aggregate_recursive_specifications/outcome_cases.ml"

let ( let* ) = Result.bind

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let fixture_path fixture =
  let local = "fixtures/" ^ fixture ^ ".ml" in
  if Sys.file_exists local then local
  else "test/aggregate_recursive_specifications/" ^ local

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755)

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let materialize_project ~workspace ~project_name sources =
  let modules =
    List.map (fun (fixture, _) -> module_name fixture) sources
  in
  let root = Filename.concat workspace "project" in
  write_file (Filename.concat root "dune-project")
    (Printf.sprintf "(lang dune 3.17)\n(name %s)\n" project_name);
  write_file (Filename.concat root "dune")
    (Printf.sprintf
       "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
        (libraries verocaml.vstd verocaml.ghost)\n (flags (:standard -ppx \"verocaml-ppx \
        --keep-ghost\")))\n"
       project_name (String.concat " " modules));
  List.iter
    (fun (fixture, source) ->
      write_file (Filename.concat root (fixture ^ ".ml")) source)
    sources;
  (root, modules)

let dune_environment environment root =
  [
    ("PATH", Project_environment.tool_path environment);
    ("OCAMLPATH", Project_environment.ocaml_path environment);
    ("VEROCAML_DUNE", Project_environment.dune_path environment);
    ("DUNE_CACHE", "disabled");
    ("DUNE_CONFIG__DISPLAY", "quiet");
    ("HOME", root);
    ("TMPDIR", root);
    ("OCAML_COLOR", "never");
  ]

let build_project ~environment root =
  let result_path = Filename.concat root ".build-and-describe" in
  let process_result =
    Process_adapter.run ~cwd:(Sys.getcwd ())
      {
        program =
          (try Unix.realpath Sys.executable_name
           with Unix.Unix_error _ -> Sys.executable_name);
        arguments = [ "--build-and-describe"; root; result_path ];
        forwarded = dune_environment environment root;
        cleanup_paths = [];
        adjacency = [];
      }
  in
  match process_result with
  | Error failure -> Error failure
  | Ok _ ->
      Fun.protect
        ~finally:(fun () ->
          if Sys.file_exists result_path then Sys.remove result_path)
        (fun () ->
          if not (Sys.file_exists result_path) then
            Error
              (Failure.make Failure.Dune_build
                 "Dune build child produced no result")
          else
            let channel = open_in_bin result_path in
            Fun.protect
              ~finally:(fun () -> close_in_noerr channel)
              (fun () ->
                (Marshal.from_channel channel
                  : (Verocaml_bin_dune_private.project,
                     Verocaml_bin_dune_private.error)
                     result)
                |> Result.map_error (function
                     | Verocaml_bin_dune_private.Cli_error message ->
                         Failure.make Failure.Dune_build message
                     | Verocaml_bin_dune_private.Dependency_error
                         { provider; reason_class } ->
                         Failure.make Failure.Selected_cmt_load
                           (Printf.sprintf
                              "Dune dependency inventory failed provider=%s reason=%s"
                              (Option.value ~default:"unknown" provider)
                              reason_class))))

type loaded_artifact = {
  artifact : Verocaml_bin_dune_private.artifact;
  implementation : Cmt_input.implementation;
}

let load_artifacts artifacts =
  let artifact_directories =
    artifacts
    |> List.concat_map (fun (artifact : Verocaml_bin_dune_private.artifact) ->
           Filename.dirname artifact.cmt
           :: Option.to_list (Option.map Filename.dirname artifact.cmi)
           @ Option.to_list (Option.map Filename.dirname artifact.cmti)
           @ Option.to_list (Option.map Filename.dirname artifact.vri))
    |> List.sort_uniq String.compare
  in
  artifacts
  |> List.fold_left
       (fun result (artifact : Verocaml_bin_dune_private.artifact) ->
         let* loaded = result in
         match artifact.cmi with
         | None ->
             if artifact.requested then
               Error
                 (Failure.make Failure.Selected_cmt_discovery
                    ("selected CMI is absent for " ^ artifact.source))
             else Ok loaded
         | Some cmi -> (
             match
               Cmt_input.load_with_interface ~cmt:artifact.cmt ~cmi
                 ?cmti:artifact.cmti ?vri:artifact.vri ~artifact_directories
                 ~implicit_authority_discovery:false ()
             with
             | Ok implementation ->
                 Ok ({ artifact; implementation } :: loaded)
             | Error diagnostic ->
                 Error
                   (Failure.make Failure.Selected_cmt_load
                      (Printf.sprintf "%s [%s]: %s" artifact.source
                         diagnostic.Diagnostic.code diagnostic.message))))
       (Ok [])
  |> Result.map List.rev

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

let dependency_closure loaded root =
  let implementations = List.map (fun artifact -> artifact.implementation) loaded in
  let select_dependency (owner : Cmt_input.implementation)
      (imported : Cmt_input.import) =
    let candidates =
      List.filter
        (fun implementation ->
          Cmt_input.exact_import ~owner ~dependency:implementation imported)
        implementations
    in
    let retained =
      List.filter
        (fun implementation -> Option.is_some implementation.Cmt_input.retained_authority)
        candidates
    in
    match (retained, candidates) with
    | [ dependency ], _ | [], [ dependency ] -> Ok (Some dependency)
    | [], [] -> Ok None
    | _ ->
        let preferred = match retained with [] -> candidates | _ -> retained in
        let digests =
          preferred
          |> List.map (fun implementation ->
                 implementation.Cmt_input.raw_artifact_digest)
          |> List.sort_uniq String.compare
        in
        if List.length digests = 1 then
          Ok
            (List.find_opt
               (fun implementation ->
                 String.equal implementation.Cmt_input.raw_artifact_digest
                   (List.hd digests))
               preferred)
        else
          Error
            (Failure.make Failure.Verifier_outcome
               (Printf.sprintf
                  "Dune dependency %s has multiple exact compiler artifacts"
                  imported.Cmt_input.unit_name))
  in
  let rec visit visiting visited (implementation : Cmt_input.implementation) =
    if List.mem implementation.Cmt_input.unit_name visiting then
      Error
        (Failure.make Failure.Verifier_outcome
           "Dune verification dependency cycle")
    else if
      List.exists
        (fun candidate ->
          String.equal candidate.Cmt_input.raw_artifact_digest
            implementation.Cmt_input.raw_artifact_digest)
        visited
    then Ok visited
    else
      let* visited =
        Array.fold_left
          (fun result (imported : Cmt_input.import) ->
            let* visited = result in
            if
              String.equal imported.Cmt_input.unit_name
                implementation.Cmt_input.unit_name
            then Ok visited
            else
              let* dependency = select_dependency implementation imported in
              match dependency with
              | None -> Ok visited
              | Some dependency ->
                  visit
                    (implementation.Cmt_input.unit_name :: visiting)
                    visited dependency)
          (Ok visited) implementation.Cmt_input.imports
      in
      Ok (implementation :: visited)
  in
  let* closure = visit [] [] root.implementation in
  closure
  |> List.filter (fun implementation ->
         not
           (String.equal implementation.Cmt_input.raw_artifact_digest
              root.implementation.Cmt_input.raw_artifact_digest))
  |> List.sort (fun left right ->
         String.compare left.Cmt_input.unit_name right.Cmt_input.unit_name)
  |> Result.ok

let requested_artifact unit_name loaded =
  match
    List.filter
      (fun artifact ->
        artifact.artifact.requested
        && String.equal artifact.implementation.Cmt_input.unit_name unit_name)
      loaded
  with
  | [ artifact ] -> Ok artifact
  | [] ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("Dune artifact graph omitted " ^ unit_name))
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("Dune artifact graph repeated " ^ unit_name))

let require_authenticated_vstd unit_name dependencies =
  match
    List.filter
      (fun implementation ->
        String.equal implementation.Cmt_input.unit_name "Vstd__Int")
      dependencies
  with
  | [ implementation ]
    when Option.is_some implementation.Cmt_input.retained_authority
         && Cmt_input.retained_authority_identity_is_exact implementation ->
      Ok ()
  | [] ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (unit_name ^ " lacks its authenticated verocaml.vstd dependency"))
  | [ _ ] ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (unit_name ^ " has an unauthenticated verocaml.vstd dependency"))
  | _ :: _ :: _ ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (unit_name ^ " has an ambiguous verocaml.vstd dependency"))

let verify_project modules loaded =
  let* configuration =
    match
      Verifier_service.configuration ~threads:1 ~timeout_ms:60_000 ~rlimit:None
    with
    | Ok configuration -> Ok configuration
    | Error error ->
        Error
          (Failure.make Failure.Runner_internal
             (Verifier_service.configuration_error_message error))
  in
  modules
  |> List.fold_left
       (fun result unit_name ->
         let* projections = result in
         let* root = requested_artifact unit_name loaded in
         let* dependencies = dependency_closure loaded root in
         let* () = require_authenticated_vstd unit_name dependencies in
         let request =
           Verifier_service.request ~configuration ~consumer:root.implementation
             ~dependencies
         in
         let* projection =
           match Verifier_service.verify request with
           | Ok result ->
               let projection = Outcome.of_verifier_result result in
               Ok
                 (Outcome.with_unit unit_name (disposition projection)
                    projection)
           | Error error -> rejected_projection unit_name error
         in
         Ok (projection :: projections))
       (Ok [])
  |> Result.map Outcome.merge

let run_sources ~project_name sources ~environment ~workspace =
  let root, modules =
    materialize_project ~workspace ~project_name sources
  in
  let* project = build_project ~environment root in
  let* loaded = load_artifacts project.artifacts in
  verify_project modules loaded

let run_fixtures ~project_name fixtures =
  let sources =
    List.map (fun fixture -> (fixture, read_file (fixture_path fixture))) fixtures
  in
  run_sources ~project_name sources

let require_units disposition fixtures expectation =
  List.fold_left
    (fun expectation fixture ->
      Expectation.require_unit (module_name fixture) disposition expectation)
    expectation fixtures

let require_functions names expectation =
  List.fold_left
    (fun expectation name ->
      Expectation.require_named_fact ("function:" ^ name)
        (Outcome.Function_exists name) expectation)
    expectation names

let positive_fixtures =
  [
    "exact_helper";
    "exact_inline";
    "human_pfc";
    "variant_result";
    "pass_through_result";
    "rank_free_integer_result";
    "rank_free_structural_result";
    "index_minimal";
  ]

let positive_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
    |> require_functions
         [
           "consume_recursive_result";
           "consume_inline";
           "checked_push_n_consumer";
           "consume_variant_result";
           "consume_pass_through";
           "observe_recursive_index";
           "observe_recursive_index_alt";
           "index";
         ]
    |> Expectation.require_named_fact "obligation-kind:index"
         (Outcome.Obligation_kind_exists
            { function_name = "index"; kind = Outcome.Postcondition })
  in
  Suite.case ~name:"positive-recursive-result-matrix" ~expectation
    (run_fixtures ~project_name:"aggregate_recursive_positive"
       positive_fixtures)

let tuple_source = read_file (fixture_path "recursive_tuple_match")

let tuple_source_project_parity ~environment ~workspace =
  let* source =
    run_sources ~project_name:"aggregate_recursive_tuple_source"
      [ ("recursive_tuple_match", tuple_source) ] ~environment
      ~workspace:(Filename.concat workspace "single-source")
  in
  let* project =
    run_fixtures ~project_name:"aggregate_recursive_tuple"
      [ "recursive_tuple_match" ] ~environment
      ~workspace:(Filename.concat workspace "explicit-project")
  in
  match Outcome.semantic_parity ~except:[] source project with
  | Ok () -> Ok source
  | Error message -> Error (Failure.make Failure.Expectation_mismatch message)

let tuple_parity =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Recursive_tuple_match" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:check_empty_pair"
         (Outcome.Function_exists "check_empty_pair")
    |> Expectation.require_named_fact "obligation-kind:check_empty_pair"
         (Outcome.Obligation_kind_exists
            {
              function_name = "check_empty_pair";
              kind = Outcome.Local_assertion;
            })
  in
  Suite.case ~name:"tuple-source-project-parity" ~expectation
    tuple_source_project_parity

let () =
  match Array.to_list Sys.argv with
  | _ :: "--build-and-describe" :: root :: result_path :: [] ->
      let result = Verocaml_bin_dune_private.build_and_describe root in
      let channel = open_out_bin result_path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () ->
          Marshal.to_channel channel result [];
          flush channel);
      exit (match result with Ok _ -> 0 | Error _ -> 1)
  | _ ->
      Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
        ~expected_environment:Integration_environment.expected
        [ positive_matrix; tuple_parity ]
