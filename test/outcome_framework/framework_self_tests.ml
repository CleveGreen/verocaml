open Outcome_test_support

let fail format = Printf.ksprintf failwith format
let require condition format = Printf.ksprintf (fun message -> if not condition then failwith message) format

let require_ok label = function
  | Ok value -> value
  | Error _ -> fail "%s failed" label

let require_failure label = function
  | Ok () -> fail "%s unexpectedly passed" label
  | Error _ -> ()

let projection_self_test () =
  let selected = { Outcome.function_name = "reject"; kind = Outcome.Assertion } in
  let baseline =
    Outcome.observation ~status:Outcome.Counterexample
      ~frontend_codes:[ "VERO_SELECTED" ] ~semantic_facts:[ selected ]
      ~units:[ ("Unit", Outcome.Unit_counterexample) ]
      ~named_facts:[ ("function:reject", Outcome.Function_exists "reject") ] ()
    |> Outcome.project
  in
  let unstable_variant =
    Outcome.observation ~status:Outcome.Counterexample
      ~frontend_codes:[ "VERO_SELECTED" ] ~semantic_facts:[ selected ]
      ~units:[ ("Unit", Outcome.Unit_counterexample) ]
      ~named_facts:[ ("function:reject", Outcome.Function_exists "reject") ] ()
    |> Outcome.project
  in
  ignore
    (require_ok "ordinal/index/span/render/order projection stability"
       (Outcome.semantic_parity ~except:[] baseline unstable_variant));
  let with_extra =
    Outcome.observation ~status:Outcome.Counterexample
      ~frontend_codes:[ "VERO_EXTRA"; "VERO_SELECTED" ]
      ~semantic_facts:
        [ selected; { Outcome.function_name = "other"; kind = Outcome.Postcondition } ]
      ~units:[ ("Unit", Outcome.Unit_counterexample) ]
      ~named_facts:
        [
          ("extra", Outcome.Function_exists "other");
          ("function:reject", Outcome.Function_exists "reject");
        ]
      ()
    |> Outcome.project
  in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_frontend_code "VERO_SELECTED"
    |> Expectation.require_semantic ~function_name:"reject" Outcome.Assertion
    |> Expectation.require_unit "Unit" Outcome.Unit_counterexample
    |> Expectation.require_named_fact "function:reject"
         (Outcome.Function_exists "reject")
  in
  ignore (require_ok "extra unselected diagnostic" (Expectation.check expectation with_extra));
  [
    ( "status",
      expectation,
      Outcome.observation ~status:Outcome.Verified () |> Outcome.project );
    ( "code",
      Expectation.empty |> Expectation.require_frontend_code "VERO_MISSING",
      baseline );
    ( "kind",
      Expectation.empty
      |> Expectation.require_semantic ~function_name:"reject" Outcome.Postcondition,
      baseline );
    ( "function",
      Expectation.empty
      |> Expectation.require_semantic ~function_name:"different" Outcome.Assertion,
      baseline );
    ( "unit",
      Expectation.empty
      |> Expectation.require_unit "Other" Outcome.Unit_counterexample,
      baseline );
    ( "named",
      Expectation.empty
      |> Expectation.require_named_fact "missing" (Outcome.Function_exists "missing"),
      baseline );
  ]
  |> List.iter (fun (label, expectation, projection) ->
         require_failure ("selected " ^ label) (Expectation.check expectation projection))

let bounds_self_test environment =
  let workspace = Filename.temp_file "resource-bounds" "" in
  Sys.remove workspace;
  Unix.mkdir workspace 0o755;
  let projection =
    Fixture.run ~environment ~workspace
      (Fixture.single_source ~module_name:"Resource_bounds"
         ~source:
           "[@@@verocaml.verify]\nlet measured (value : int) =\n  \
            [%verocaml.assert false];\n  value\n"
         ~libraries:[ "verocaml.ghost" ])
    |> require_ok "resource-bound fixture"
  in
  let make helper maximum =
    helper ~maximum ~baseline:1 ~rationale:"measured fixture resource ceiling"
      Expectation.empty
  in
  [ ("functions", Expectation.functions_at_most); ("obligations", Expectation.obligations_at_most) ]
  |> List.iter (fun (name, helper) ->
         require_failure (name ^ " negative maximum")
           (match make helper (-1) with Ok _ -> Ok () | Error error -> Error error);
         let below = require_ok name (make helper 2) in
         let equal = require_ok name (make helper 1) in
         let above = require_ok name (make helper 0) in
         ignore (require_ok (name ^ " below") (Expectation.check below projection));
         ignore (require_ok (name ^ " equal") (Expectation.check equal projection));
         require_failure (name ^ " above")
           (Expectation.check above projection))

let parity_self_test () =
  let projection facts =
    Outcome.observation ~status:Outcome.Verified ~named_facts:facts ()
    |> Outcome.project
  in
  let common = ("function:id", Outcome.Function_exists "id") in
  ignore
    (require_ok "equal parity"
       (Outcome.semantic_parity ~except:[] (projection [ common ]) (projection [ common ])));
  ignore
    (require_ok "mode-specific parity"
       (Outcome.semantic_parity ~except:[ "mode" ]
          (projection [ common; ("mode", Outcome.Function_exists "serial") ])
          (projection [ common; ("mode", Outcome.Function_exists "parallel") ])));
  require_failure "selected parity"
    (Outcome.semantic_parity ~except:[] (projection [ common ])
       (projection [ ("function:other", Outcome.Function_exists "other") ]))

let runner_self_test () =
  let run ~environment:_ ~workspace:_ =
    Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
  in
  let case name = Suite.case ~name ~expectation:Expectation.empty run in
  let suite = "test/outcome_framework/probe.ml" in
  (match Suite.validate_cases ~suite_path:suite [ case "same"; case "same" ] with
  | Error failure ->
      require
        (Failure.category failure = Failure.Runner_duplicate_identity)
        "duplicate identity category differs"
  | Ok () -> fail "duplicate identity passed");
  (match Suite.validate_cases ~suite_path:suite [ case "../bad" ] with
  | Error failure ->
      require
        (Failure.category failure = Failure.Runner_malformed_identity)
        "malformed identity category differs"
  | Ok () -> fail "malformed identity passed");
  [
    "";
    ".";
    "..";
    "./test/outcome_framework/probe.ml";
    "test/./outcome_framework/probe.ml";
    "test/outcome_framework/../probe.ml";
    "test//outcome_framework/probe.ml";
    "test/outcome_framework/probe.ml/";
    "test\\outcome_framework\\probe.ml";
  ]
  |> List.iter (fun host_path ->
         match Suite.validate_cases ~suite_path:host_path [ case "known" ] with
         | Error failure ->
             require
               (Failure.category failure = Failure.Runner_malformed_identity)
               "host-path negative category differs: %S" host_path
         | Ok () -> fail "host-path alias passed: %S" host_path);
  (match Suite.select ~suite_path:suite [ case "known" ] (suite ^ "::unknown") with
  | Error failure ->
      require
        (Failure.category failure = Failure.Runner_unknown_identity)
        "unknown identity category differs"
  | Ok _ -> fail "unknown identity selected");
  (match Suite.select ~suite_path:suite [ case "known" ] "../probe::known" with
  | Error failure ->
      require
        (Failure.category failure = Failure.Runner_malformed_identity)
        "traversal identity category differs"
  | Ok _ -> fail "traversal identity selected")

let environment_self_test () =
  let expected = Integration_environment.expected in
  let missing = Filename.temp_file "missing-environment" ".manifest" in
  Sys.remove missing;
  (match Project_environment.validate ~expected missing with
  | Error failure ->
      require
        (Failure.category failure = Failure.Project_environment_missing)
        "missing environment category differs"
  | Ok _ -> fail "missing environment passed");
  let lines =
    let input = open_in_bin Integration_environment.manifest in
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () ->
        let rec read accumulated =
          match input_line input with
          | line -> read (line :: accumulated)
          | exception End_of_file -> List.rev accumulated
        in
        read [])
  in
  let mismatch label rewrite =
    let path = Filename.temp_file (label ^ "-environment") ".manifest" in
    let output = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr output)
      (fun () ->
        lines |> List.concat_map rewrite
        |> List.iter (fun line -> Printf.fprintf output "%s\n" line));
    Fun.protect
      ~finally:(fun () -> Sys.remove path)
      (fun () ->
        match Project_environment.validate ~expected path with
        | Error failure ->
            require
              (Failure.category failure
              = Failure.Project_environment_provenance_mismatch)
              "%s environment category differs" label
        | Ok _ -> fail "%s environment passed" label)
  in
  let substitute key value line =
    if String.starts_with ~prefix:(key ^ "=") line then [ key ^ "=" ^ value ]
    else [ line ]
  in
  mismatch "stale-source" (substitute "source_commit" "stale");
  mismatch "substituted-package-root" (substitute "package_root" "/tmp");
  mismatch "substituted-binary-root" (substitute "binary_root" "/tmp");
  mismatch "substituted-dune-path" (substitute "dune_path" "/bin/sh");
  mismatch "substituted-tool-path" (substitute "tool_path" "/bin");
  mismatch "noncanonical-package-root"
    (substitute "package_root" (expected.package_root ^ "/."));
  mismatch "duplicate-field" (fun line ->
      if String.starts_with ~prefix:"source_commit=" line then [ line; line ] else [ line ])

let process_self_test () =
  let workspace = Filename.temp_file "process-facts" "" in
  Sys.remove workspace;
  Unix.mkdir workspace 0o755;
  let run noise =
    Process_adapter.run ~cwd:workspace
      {
        program = "/bin/sh";
        arguments =
          [
            "-c";
            Printf.sprintf
              "printf 'verocaml: error[VERO_PROCESS_OK]\\nbefore %%s after %s\\n' \"$FORWARDED_TOKEN\""
              noise;
          ];
        forwarded = [ ("FORWARDED_TOKEN", "registered") ];
        cleanup_paths = [ "cleanup" ];
        adjacency = [ ("before-after", "before", "after") ];
      }
  in
  let first = require_ok "process facts first" (run "noise-one") in
  let second = require_ok "process facts second" (run "different-noise") in
  ignore
    (require_ok "raw output projection stability"
       (Outcome.semantic_parity ~except:[] first second));
  [
    ("exit", Outcome.Exit_class (Outcome.Exited 0));
    ("code", Outcome.Stable_code "VERO_PROCESS_OK");
    ("forwarding", Outcome.Forwarded "FORWARDED_TOKEN");
    ("cleanup", Outcome.Cleaned "cleanup");
    ("adjacency", Outcome.Adjacent "before-after");
  ]
  |> List.iter (fun (label, fact) ->
         let expectation = Expectation.require_process_fact fact Expectation.empty in
         match Expectation.check expectation first with
         | Ok () -> ()
         | Error message ->
             let observed =
               Outcome.process_facts first
               |> List.map (function
                    | Outcome.Exit_class (Outcome.Exited code) ->
                        "exit:" ^ string_of_int code
                    | Exit_class Signaled -> "signaled"
                    | Exit_class Stopped -> "stopped"
                    | Stable_code code -> "code:" ^ code
                    | Forwarded name -> "forwarded:" ^ name
                    | Cleaned path -> "cleaned:" ^ path
                    | Adjacent name -> "adjacent:" ^ name)
               |> String.concat ","
             in
             fail "process %s expectation: %s observed=%s" label message observed);
  let trace_spoof =
    Process_adapter.run ~cwd:workspace
      {
        program = "/bin/sh";
        arguments =
          [
            "-c";
            "printf 'INFO Diagnostic event diagnostic created code=VERO_TRACE_ONLY\\nTRACE Probe event content=\\\"[VERO_TRACE_BRACKET]\\\"\\n'";
          ];
        forwarded = [];
        cleanup_paths = [];
        adjacency = [];
      }
    |> require_ok "trace code spoof"
  in
  [ "VERO_TRACE_ONLY"; "VERO_TRACE_BRACKET" ]
  |> List.iter (fun code ->
         require
           (not (List.mem (Outcome.Stable_code code) (Outcome.process_facts trace_spoof)))
           "Delator trace code became a stable process fact: %s" code);
  let missing_process =
    Expectation.empty
    |> Expectation.require_process_fact (Outcome.Stable_code "VERO_MISSING_ONE")
    |> Expectation.require_process_fact (Outcome.Cleaned "missing-two")
  in
  (match Expectation.check missing_process trace_spoof with
  | Error message ->
      require
        (String.equal message
           "missing selected process facts: cleaned(missing-two), stable-code(VERO_MISSING_ONE)")
        "process mismatch did not enumerate missing facts: %s" message
  | Ok () -> fail "missing process facts passed");
  let tool_directory = Filename.concat workspace "forwarded-path" in
  Unix.mkdir tool_directory 0o755;
  let tool = Filename.concat tool_directory "path-probe" in
  let channel = open_out_bin tool in
  output_string channel "#!/bin/sh\nprintf 'verocaml: error[VERO_PATH_OK]\\n'\n";
  close_out channel;
  Unix.chmod tool 0o755;
  let path_projection =
    Process_adapter.run ~cwd:workspace
      {
        program = "path-probe";
        arguments = [];
        forwarded = [ ("PATH", tool_directory) ];
        cleanup_paths = [];
        adjacency = [];
      }
    |> require_ok "forwarded PATH process"
  in
  [
    Outcome.Exit_class (Outcome.Exited 0);
    Outcome.Stable_code "VERO_PATH_OK";
    Outcome.Forwarded "PATH";
  ]
  |> List.iter (fun fact ->
         match
           Expectation.check
             (Expectation.require_process_fact fact Expectation.empty)
             path_projection
         with
         | Ok () -> ()
         | Error message -> fail "forwarded PATH process: %s" message);
  Sys.remove tool;
  Unix.rmdir tool_directory;
  let unclean = Filename.concat workspace "unclean" in
  let channel = open_out_bin unclean in
  close_out channel;
  (match
     Process_adapter.run ~cwd:workspace
       {
         program = "/bin/sh";
         arguments = [ "-c"; ":" ];
         forwarded = [];
         cleanup_paths = [ "unclean" ];
         adjacency = [];
       }
   with
  | Error failure ->
      require (Failure.category failure = Failure.Process_protocol)
        "process protocol category differs"
  | Ok _ -> fail "unclean process protocol passed");
  Sys.remove unclean;
  Unix.rmdir workspace

let fixture_negatives environment =
  let workspace label =
    let path = Filename.temp_file ("outcome-" ^ label) "" in
    Sys.remove path;
    Unix.mkdir path 0o755;
    path
  in
  let invalid =
    Fixture.dune_project
      {
        files =
          [
            { Fixture.path = "../escape"; contents = "" };
            { path = "dune-project"; contents = "(lang dune 3.17)\n" };
            { path = "dune"; contents = "" };
          ];
        libraries = [];
        targets = [ "@all" ];
        selected_units = [ "Bad" ];
      }
  in
  (match Fixture.run ~environment ~workspace:(workspace "materialize") invalid with
  | Error failure ->
      require
        (Failure.category failure = Failure.Project_materialization)
        "materialization category differs"
  | Ok _ -> fail "invalid materialization passed");
  let bad_build =
    Fixture.single_source ~module_name:"Bad_build"
      ~source:"[@@@verocaml.verify]\nlet id x = x\n"
      ~libraries:[ "library.does.not.exist" ]
  in
  (match Fixture.run ~environment ~workspace:(workspace "build") bad_build with
  | Error failure ->
      require (Failure.category failure = Failure.Dune_build)
        "Dune build category differs"
  | Ok _ -> fail "bad Dune build passed");
  let missing_cmt =
    Fixture.dune_project
      {
        files =
          [
            { Fixture.path = "dune-project"; contents = "(lang dune 3.17)\n" };
            { path = "dune"; contents = "(library (name empty) (modules Empty))\n" };
            { path = "empty.ml"; contents = "let value = 1\n" };
          ];
        libraries = [];
        targets = [ "@all" ];
        selected_units = [ "Missing" ];
      }
  in
  (match Fixture.run ~environment ~workspace:(workspace "discovery") missing_cmt with
  | Error failure ->
      require
        (Failure.category failure = Failure.Selected_cmt_discovery)
        "CMT discovery category differs"
  | Ok _ -> fail "missing CMT passed");
  let bad_target =
    Fixture.dune_project
      {
        files =
          [
            { Fixture.path = "dune-project"; contents = "(lang dune 3.17)\n" };
            { path = "dune"; contents = "(library (name target) (modules Target))\n" };
            { path = "target.ml"; contents = "let value = 1\n" };
          ];
        libraries = [];
        targets = [ "target-that-does-not-exist" ];
        selected_units = [ "Target" ];
      }
  in
  (match Fixture.run ~environment ~workspace:(workspace "target") bad_target with
  | Error failure ->
      require (Failure.category failure = Failure.Dune_build)
        "target perturbation category differs"
  | Ok _ -> fail "bad target passed");
  let bad_interface =
    Fixture.dune_project
      {
        files =
          [
            { Fixture.path = "dune-project"; contents = "(lang dune 3.17)\n" };
            { path = "dune"; contents = "(library (name iface) (modules Iface))\n" };
            { path = "iface.mli"; contents = "val value : string\n" };
            { path = "iface.ml"; contents = "let value = 1\n" };
          ];
        libraries = [];
        targets = [ "@all" ];
        selected_units = [ "Iface" ];
      }
  in
  (match Fixture.run ~environment ~workspace:(workspace "interface") bad_interface with
  | Error failure ->
      require (Failure.category failure = Failure.Dune_build)
        "interface perturbation category differs"
  | Ok _ -> fail "bad interface passed");
  let fake_cmt = Filename.concat (workspace "load") "fake.cmt" in
  let channel = open_out_bin fake_cmt in
  output_string channel "not a CMT";
  close_out channel;
  let fake_input =
    require_ok "fake prepared declaration"
      (Fixture.prepared_cmt ~declared_dependencies:[ fake_cmt ] fake_cmt)
  in
  (match Fixture.run ~environment ~workspace:(workspace "load-run") fake_input with
  | Error failure ->
      require (Failure.category failure = Failure.Selected_cmt_load)
        "CMT load category differs"
  | Ok _ -> fail "invalid CMT passed")

let single_project_parity environment =
  let source = "[@@@verocaml.verify]\nlet identity (value : int) = value\n" in
  let project =
    Fixture.lower_single_source ~module_name:"Parity" ~source
      ~libraries:[ "verocaml.ghost" ]
  in
  let workspace label =
    let path = Filename.temp_file ("parity-" ^ label) "" in
    Sys.remove path;
    Unix.mkdir path 0o755;
    path
  in
  let single =
    Fixture.run ~environment ~workspace:(workspace "single")
      (Fixture.single_source ~module_name:"Parity" ~source
         ~libraries:[ "verocaml.ghost" ])
    |> require_ok "single-source parity"
  in
  let explicit =
    Fixture.run ~environment ~workspace:(workspace "project")
      (Fixture.dune_project project)
    |> require_ok "Dune-project parity"
  in
  ignore
    (require_ok "single-source/project semantic parity"
       (Outcome.semantic_parity ~except:[] single explicit))

let prepared_self_test environment artifact =
  let input =
    require_ok "declared prepared CMT"
      (Fixture.prepared_cmt ~declared_dependencies:[ artifact ] artifact)
  in
  let workspace = Filename.temp_file "prepared-cmt" "" in
  Sys.remove workspace;
  Unix.mkdir workspace 0o755;
  ignore
    (match Fixture.run ~environment ~workspace input with
    | Ok projection -> projection
    | Error failure ->
        if Failure.category failure = Failure.Selected_cmt_load then
          fail "declared prepared CMT did not load: %s" (Failure.to_string failure)
        else Outcome.observation ~status:Outcome.Frontend_rejected () |> Outcome.project)

let category_self_test () =
  [
    Failure.Project_materialization;
    Project_environment_missing;
    Project_environment_provenance_mismatch;
    Dune_build;
    Selected_cmt_discovery;
    Selected_cmt_load;
    Verifier_outcome;
    Expectation_mismatch;
    Process_protocol;
    Runner_internal;
  ]
  |> List.iter (fun category ->
         require (Failure.category_name category <> "") "empty stable category")

let () =
  match Array.to_list Sys.argv with
  | [ _; "--prepared-cmt"; prepared ] ->
      projection_self_test ();
      parity_self_test ();
      runner_self_test ();
      environment_self_test ();
      process_self_test ();
      category_self_test ();
      let environment =
        match
          Project_environment.validate ~expected:Integration_environment.expected
            Integration_environment.manifest
        with
        | Ok environment -> environment
        | Error failure -> fail "%s" (Failure.to_string failure)
      in
      bounds_self_test environment;
      fixture_negatives environment;
      single_project_parity environment;
      prepared_self_test environment prepared;
      print_endline "framework-self-tests: pass"
  | _ -> fail "usage: framework_self_tests --prepared-cmt PATH"
