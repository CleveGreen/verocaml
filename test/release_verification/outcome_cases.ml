open Outcome_test_support

let suite_path = "test/release_verification/outcome_cases.ml"
let ( let* ) = Result.bind

let executable_directory () =
  let executable =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  Filename.dirname executable

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture name =
  let relative = Filename.concat "fixtures" name in
  let adjacent = Filename.concat (executable_directory ()) relative in
  let path =
    if Sys.file_exists adjacent then adjacent
    else Filename.concat "test/release_verification" relative
  in
  read_file path

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

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let source_input module_name filename =
  Fixture.single_source ~module_name ~source:(fixture filename)
    ~libraries:[ "verocaml.ghost" ]

let semantic_case ~name ~module_name ~filename expectation =
  Suite.case ~name ~expectation
    (run_fixture (source_input module_name filename))

let stack_expectation =
  let base =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Stack_demo" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:push"
         (Outcome.Function_exists "push")
    |> Expectation.require_named_fact "function:drain"
         (Outcome.Function_exists "drain")
    |> Expectation.require_named_fact "function:drain_and_read"
         (Outcome.Function_exists "drain_and_read")
    |> Expectation.require_named_fact "obligation-kind:drain"
         (Outcome.Obligation_kind_exists
            { function_name = "drain"; kind = Outcome.Entry_measure_nonnegative })
    |> Expectation.require_named_fact "obligation-kind:drain"
         (Outcome.Obligation_kind_exists
            {
              function_name = "drain";
              kind =
                Outcome.Recursive_call_measure_nonnegative { callee = "drain" };
            })
    |> Expectation.require_named_fact "obligation-kind:drain"
         (Outcome.Obligation_kind_exists
            {
              function_name = "drain";
              kind = Outcome.Recursive_call_strict_descent { callee = "drain" };
            })
    |> Expectation.require_named_fact "obligation-kind:drain_and_read"
         (Outcome.Obligation_kind_exists
            {
              function_name = "drain_and_read";
              kind = Outcome.Call_precondition { callee = "drain" };
            })
  in
  base

let stack_case =
  semantic_case ~name:"recursive-stack-verifies" ~module_name:"Stack_demo"
    ~filename:"stack_demo.ml" stack_expectation

let erased_stack_case =
  let input =
    Fixture.dune_project
      {
        files =
          [
            {
              Fixture.path = "dune-project";
              contents = "(lang dune 3.17)\n(name erased_stack)\n";
            };
            {
              path = "dune";
              contents =
                "(library\n (name erased_stack)\n (wrapped false)\n (modules \
                 Stack_demo)\n (libraries verocaml.ghost)\n (flags (:standard \
                 -ppx \"verocaml-ppx\")))\n";
            };
            { path = "stack_demo.ml"; contents = fixture "stack_demo.ml" };
          ];
        libraries = [ "verocaml.ghost" ];
        targets = [ "@all" ];
        selected_units = [ "Stack_demo" ];
      }
  in
  Suite.case ~name:"erased-sidecars-remain-buildable"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_TYPE"
      |> Expectation.require_unit "Stack_demo" Outcome.Unit_frontend_rejected)
    (run_fixture input)

let pure_case =
  semantic_case ~name:"pure-recursion-verifies" ~module_name:"Pure_verified"
    ~filename:"pure_verified.ml"
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Pure_verified" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:safe_increment"
         (Outcome.Function_exists "safe_increment")
    |> Expectation.require_named_fact "function:countdown"
         (Outcome.Function_exists "countdown")
    |> Expectation.require_named_fact "obligation-kind:countdown"
         (Outcome.Obligation_kind_exists
            {
              function_name = "countdown";
              kind = Outcome.Recursive_call_strict_descent { callee = "countdown" };
            }))

let false_postcondition_case =
  semantic_case ~name:"false-postcondition" ~module_name:"False_postcondition"
    ~filename:"false_postcondition.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "False_postcondition" Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"false_postcondition"
         Outcome.Postcondition)

let wrong_mutation_case =
  semantic_case ~name:"wrong-mutation-postcondition"
    ~module_name:"Wrong_mutation_postcondition"
    ~filename:"wrong_mutation_postcondition.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Wrong_mutation_postcondition"
         Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"wrong_mutation"
         Outcome.Postcondition)

let arithmetic_upper_case =
  semantic_case ~name:"push-requires-upper-bound"
    ~module_name:"Push_without_upper_bound"
    ~filename:"push_without_upper_bound.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Push_without_upper_bound"
         Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"push_without_upper_bound"
         (Outcome.Arithmetic_safety (Outcome.Add, Outcome.Upper)))

let call_precondition_case =
  semantic_case ~name:"drop-requires-nonempty" ~module_name:"Drop_without_nonemptiness"
    ~filename:"drop_without_nonemptiness.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Drop_without_nonemptiness"
         Outcome.Unit_counterexample
    |> Expectation.require_semantic
         ~function_name:"call_drop_without_nonemptiness"
         (Outcome.Call_precondition { callee = "drop" }))

let recursive_descent_case =
  semantic_case ~name:"drain-must-decrease" ~module_name:"Drain_without_decrement"
    ~filename:"drain_without_decrement.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Drain_without_decrement"
         Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"drain_without_decrement"
         (Outcome.Recursive_call_strict_descent
            { callee = "drain_without_decrement" }))

let unsupported_mutation_case =
  semantic_case ~name:"aliased-write-rejected" ~module_name:"Aliased_write"
    ~filename:"aliased_write.ml"
    (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_MUTATION"
    |> Expectation.require_unit "Aliased_write" Outcome.Unit_frontend_rejected)

let installed_binary environment =
  Filename.concat (Project_environment.binary_root environment) "verocaml"

let installed_ghost_directory environment =
  Filename.concat (Project_environment.package_root environment) "verocaml/ghost"

let process_expectation ~exit_code ~code ~cleanup =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_process_fact
         (Outcome.Exit_class (Outcome.Exited exit_code))
  in
  let expectation =
    match code with
    | None -> expectation
    | Some code ->
        Expectation.require_process_fact (Outcome.Stable_code code) expectation
  in
  List.fold_left
    (fun expectation path ->
      Expectation.require_process_fact (Outcome.Cleaned path) expectation)
    expectation cleanup

let process_case ?(source = "let identity (value : int) = value\n")
    ?(input_name = "input.ml") ?(write_input = true) ?(cleanup = []) ?code
    ~name ~arguments ~exit_code () =
  let run ~environment ~workspace =
    if write_input then write_file (Filename.concat workspace input_name) source;
    Process_adapter.run ~cwd:workspace
      {
        program = installed_binary environment;
        arguments =
          List.map (fun arg -> if arg = "$SOURCE" then input_name else arg) arguments;
        forwarded =
          [
            ("PATH", Project_environment.tool_path environment);
            ( "VEROCAML_PPX",
              Filename.concat (Project_environment.binary_root environment)
                "verocaml-ppx" );
            ("VEROCAML_GHOST_DIR", installed_ghost_directory environment);
            ("OCAML_COLOR", "never");
          ];
        cleanup_paths = cleanup;
        adjacency = [];
      }
  in
  Suite.case ~name ~expectation:(process_expectation ~exit_code ~code ~cleanup)
    run

let low_budget_case =
  process_case ~name:"explicit-low-budget-is-inconclusive"
    ~source:(fixture "pure_verified.ml")
    ~arguments:
      [ "verify"; "$SOURCE"; "--timeout-ms"; "60000"; "--rlimit"; "1" ]
    ~exit_code:3 ()

let scoped_dune_directory_case =
  let run ~environment ~workspace =
    let root = Filename.concat workspace "scoped-dune-directory" in
    [
      ( "dune-project",
        "(lang dune 3.17)\n(name scoped_dune_directory)\n" );
      ( "good/dune",
        {|(library
 (name good)
 (wrapped false)
 (modules Good)
 (libraries provider type_provider verocaml.ghost)
 (preprocess (pps verocaml.ppx)))

(library
 (name explicit_ordinary)
 (wrapped false)
 (modules Explicit_ordinary)
 (libraries verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-ordinary)))
|} );
      ( "good/good.ml",
        {|
[@@@verocaml.verify]

let identity (value : int) = Provider.identity value

let option_is_some (value : 'a option) =
  match value with None -> false | Some _ -> true
[@@verocaml.spec]
|} );
      ( "good/explicit_ordinary.ml",
        {|
[@@@verocaml.verify]

let identity (value : int) = value
|} );
      ( "provider/dune",
        {|(library
 (name provider)
 (wrapped false)
 (modules Provider)
 (libraries verocaml.ghost)
 (preprocess (pps verocaml.ppx)))

(rule
 (targets provider.vri provider.verocaml-retained-interface)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .provider.objs/byte/provider.cmt)
  (:cmi .provider.objs/byte/provider.cmi)
  (:cmti .provider.objs/byte/provider.cmti))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} provider.vri
    --artifact-directory .provider.objs/byte)
   (run %{emitter} manifest provider.verocaml-retained-interface
    %{cmt} %{cmi} %{cmti} provider.vri))))

(alias
 (name all)
 (deps provider.vri provider.verocaml-retained-interface))
|} );
      ( "provider/provider.mli",
        {|val identity : int -> int
[%%verocaml.symbolic val probe : int -> bool]
val identity_refl : int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]
|} );
      ( "provider/provider.ml",
        {|
[%%verocaml.symbolic val probe : int -> bool]

let identity (value : int) =
  [%verocaml.ensures fun result -> result = value];
  value

let identity_refl (value : int) =
  [%verocaml.ensures fun _ ->
    (not ((probe value) [@trigger])) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]
|} );
      ( "type-provider/dune",
        {|(library
 (name type_provider)
 (wrapped false)
 (modules Type_provider)
 (libraries verocaml.ghost)
 (preprocess (pps verocaml.ppx)))
|} );
      ( "type-provider/type_provider.mli",
        {|type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

val identity : 'a -> 'a
[@@verocaml.spec]
|} );
      ( "type-provider/type_provider.ml",
        {|type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let identity value = value
[@@verocaml.spec]
|} );
      ( "broken/dune",
        {|(library
 (name broken)
 (wrapped false)
 (modules Broken))
|} );
      ("broken/broken.ml", "let impossible : int = \"broken sibling\"\n");
      ( "unrelated-generated/dune",
        {|(rule
 (target generated.ml)
 (action (run false)))

(library
 (name unrelated_generated)
 (wrapped false)
 (modules Generated Use))
|} );
      ("unrelated-generated/use.ml", "let value = Generated.value\n");
    ]
    |> List.iter (fun (path, contents) ->
           write_file (Filename.concat root path) contents);
    let root = Unix.realpath root in
    let* ordinary_build =
      Process_adapter.run ~cwd:root
        {
          program = Project_environment.dune_path environment;
          arguments =
            [ "build"; "--root"; root; "--profile"; "release"; "@good/all" ];
          forwarded =
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("DUNE_CACHE", "disabled");
              ("HOME", root);
              ("TMPDIR", root);
              ("OCAML_COLOR", "never");
            ];
          cleanup_paths = [];
          adjacency = [];
        }
    in
    let ordinary_succeeded =
      Outcome.process_facts ordinary_build
      |> List.exists (function
           | Outcome.Exit_class (Outcome.Exited 0) -> true
           | Exit_class (Exited _) | Exit_class Signaled | Exit_class Stopped
           | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false)
    in
    let* () =
      if ordinary_succeeded then Ok ()
      else
        Error
          (Failure.make Failure.Process_protocol
             "ordinary Dune build of requested subtree failed")
    in
    Process_adapter.run ~cwd:root
      {
        program = installed_binary environment;
        arguments =
          [
            "verify";
            "good";
            "--threads";
            "1";
            "--timeout-ms";
            "60000";
          ];
        forwarded =
          [
            ("PATH", Project_environment.tool_path environment);
            ("OCAMLPATH", Project_environment.ocaml_path environment);
            ("VEROCAML_DUNE", Project_environment.dune_path environment);
            ("DUNE_CACHE", "disabled");
            ("HOME", root);
            ("TMPDIR", root);
            ("OCAML_COLOR", "never");
          ];
        cleanup_paths = [];
        adjacency = [];
      }
  in
  Suite.case ~name:"directory-verification-scopes-dune-and-retains-ghosts"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 0)))
    run

let cli_cases =
  [
    process_case ~name:"unsupported-solver"
      ~arguments:
        [ "verify"; "$SOURCE"; "--timeout-ms"; "60000"; "--solver"; "cvc5" ]
      ~exit_code:2 ~code:"VERO_CLI" ();
    process_case ~name:"nonpositive-timeout"
      ~arguments:[ "verify"; "$SOURCE"; "--timeout-ms"; "0" ] ~exit_code:2
      ~code:"VERO_CLI" ();
    process_case ~name:"duplicate-rlimit"
      ~arguments:
        [
          "verify";
          "$SOURCE";
          "--timeout-ms";
          "60000";
          "--rlimit";
          "2";
          "--rlimit";
          "3";
        ]
      ~exit_code:2 ~code:"VERO_CLI" ();
    process_case ~name:"missing-rlimit-value"
      ~arguments:[ "verify"; "$SOURCE"; "--timeout-ms"; "60000"; "--rlimit" ]
      ~exit_code:2 ~code:"VERO_CLI" ();
    process_case ~name:"unknown-option"
      ~arguments:[ "verify"; "$SOURCE"; "--timeout-ms"; "60000"; "--unknown" ]
      ~exit_code:2 ~code:"VERO_CLI" ();
    process_case ~name:"malformed-cmt"
      ~source:"not a cmt\n" ~input_name:"malformed.cmt"
      ~arguments:[ "verify"; "$SOURCE"; "--timeout-ms"; "60000" ]
      ~exit_code:2 ~code:"VERO_MALFORMED_INPUT" ();
    process_case ~name:"missing-input"
      ~input_name:"does-not-exist.cmt" ~write_input:false
      ~arguments:[ "verify"; "$SOURCE"; "--timeout-ms"; "0x10" ]
      ~exit_code:2 ~code:"VERO_INPUT_IO" ();
    process_case ~name:"missing-dump-sst-path"
      ~arguments:
        [
          "verify";
          "$SOURCE";
          "--timeout-ms";
          "60000";
          "--dump-sst";
          "--dump-vir";
        ]
      ~exit_code:2 ~code:"VERO_CLI" ();
    process_case ~name:"missing-dump-vir-path"
      ~arguments:
        [
          "verify";
          "$SOURCE";
          "--timeout-ms";
          "60000";
          "--dump-vir";
          "--dump-sst";
        ]
      ~exit_code:2 ~code:"VERO_CLI" ();
    process_case ~name:"duplicate-dump-sst"
      ~cleanup:[ "one.sst"; "two.sst" ]
      ~arguments:
        [
          "verify";
          "$SOURCE";
          "--timeout-ms";
          "60000";
          "--dump-sst";
          "one.sst";
          "--dump-sst";
          "two.sst";
        ]
      ~exit_code:2 ~code:"VERO_CLI" ();
    process_case ~name:"duplicate-dump-vir"
      ~cleanup:[ "one.vir"; "two.vir" ]
      ~arguments:
        [
          "verify";
          "$SOURCE";
          "--timeout-ms";
          "60000";
          "--dump-vir";
          "one.vir";
          "--dump-vir";
          "two.vir";
        ]
      ~exit_code:2 ~code:"VERO_CLI" ();
    process_case ~name:"usage-without-command" ~arguments:[] ~exit_code:2
      ~code:"VERO_CLI" ();
  ]

let invalid_rlimit_cases =
  [ "0x10"; "0b10"; "0o10"; "+1"; "-1"; "1_000"; ""; "nope"; "0";
    "999999999999999999999999999999999999" ]
  |> List.mapi (fun index value ->
         process_case
           ~name:(Printf.sprintf "invalid-rlimit-%02d" (index + 1))
           ~arguments:
             [
               "verify";
               "$SOURCE";
               "--timeout-ms";
               "60000";
               "--rlimit";
               value;
             ]
           ~exit_code:2 ~code:"VERO_CLI" ())

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    ([ stack_case; erased_stack_case; pure_case; false_postcondition_case;
       wrong_mutation_case;
       arithmetic_upper_case; call_precondition_case; recursive_descent_case;
       unsupported_mutation_case; low_budget_case; scoped_dune_directory_case ]
    @ cli_cases @ invalid_rlimit_cases)
