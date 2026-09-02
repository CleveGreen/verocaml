open Outcome_test_support

let suite_path = "test/source_input/outcome_process_cases.ml"

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let write_source workspace name contents =
  let directory = Filename.concat workspace "fixtures" in
  if not (Sys.file_exists directory) then Unix.mkdir directory 0o755;
  let relative = Filename.concat "fixtures" name in
  write_file (Filename.concat workspace relative) contents;
  relative

let executable_directory () =
  let executable =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  Filename.dirname executable

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let installed_ghost_directory environment =
  Filename.concat (Project_environment.package_root environment) "verocaml/ghost"

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let expectation facts =
  List.fold_left
    (fun expectation fact ->
      Expectation.require_process_fact fact expectation)
    Expectation.empty facts

let source_compile_type_error ~environment ~workspace =
  let source =
    write_source workspace "compile_error.ml"
      "let not_an_integer : int = false\n"
  in
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments = [ "verify"; source ];
      forwarded = [ ("OCAML_COLOR", "never") ];
      cleanup_paths = [];
      adjacency = [];
    }

let cleanup_script =
  {|"$@"
status=$?
if [ -d "$TMPDIR" ] && [ -z "$(find "$TMPDIR" -mindepth 1 -print -quit)" ]; then
  rm -f source-temp-dirty
fi
exit "$status"
|}

let controlled_compiler ~outcome ~environment ~workspace =
  let source =
    write_source workspace "verified.ml"
      "let identity (x : int) : int = x\n"
  in
  let temporary_directory =
    Filename.concat (absolute workspace) "source-temp"
  in
  Unix.mkdir temporary_directory 0o755;
  write_file (Filename.concat workspace "source-temp-dirty") "";
  let helper =
    Filename.concat (executable_directory ())
      "source_input_compiler_helper.exe"
  in
  Process_adapter.run ~cwd:workspace
    {
      program = "/bin/sh";
      arguments =
        [
          "-c";
          cleanup_script;
          "source-input-cleanup";
          installed_binary environment "verocaml";
          "verify";
          source;
        ];
      forwarded =
        [
          ("PATH", Project_environment.tool_path environment);
          ("TMPDIR", temporary_directory);
          ("VEROCAML_OCAMLC", helper);
          ("VEROCAML_TEST_COMPILER_OUTCOME", outcome);
          ("VEROCAML_PPX", installed_binary environment "verocaml-ppx");
          ("VEROCAML_GHOST_DIR", installed_ghost_directory environment);
        ];
      cleanup_paths = [ "source-temp-dirty" ];
      adjacency = [];
    }

let controlled_compiler_exit = controlled_compiler ~outcome:"exit"

let controlled_compiler_signal = controlled_compiler ~outcome:"signal"

let compiler_invocation_failure ~environment ~workspace =
  let source =
    write_source workspace "verified.ml"
      "let identity (x : int) : int = x\n"
  in
  let compiler = Filename.concat (absolute workspace) "not-executable" in
  write_file compiler "";
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments = [ "verify"; source ];
      forwarded =
        [
          ("VEROCAML_OCAMLC", compiler);
          ("VEROCAML_PPX", installed_binary environment "verocaml-ppx");
          ("VEROCAML_GHOST_DIR", installed_ghost_directory environment);
        ];
      cleanup_paths = [];
      adjacency = [];
    }

let private_storage_setup_failure ~environment ~workspace =
  let source =
    write_source workspace "verified.ml"
      "let identity (x : int) : int = x\n"
  in
  let temporary_directory =
    Filename.concat (absolute workspace) "not-a-directory"
  in
  write_file temporary_directory "";
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments = [ "verify"; source ];
      forwarded = [ ("TMPDIR", temporary_directory) ];
      cleanup_paths = [];
      adjacency = [];
    }

let source_compile_type_error_case =
  Suite.case ~name:"source-compile-type-error"
    ~expectation:
      (expectation
         [
           Outcome.Exit_class (Outcome.Exited 2);
           Outcome.Forwarded "OCAML_COLOR";
           Outcome.Stable_code "VERO_SOURCE_COMPILE";
         ])
    source_compile_type_error

let controlled_compiler_exit_case =
  Suite.case ~name:"controlled-compiler-exit"
    ~expectation:
      (expectation
         [
           Outcome.Exit_class (Outcome.Exited 2);
           Outcome.Forwarded "TMPDIR";
           Outcome.Forwarded "VEROCAML_OCAMLC";
           Outcome.Forwarded "VEROCAML_TEST_COMPILER_OUTCOME";
           Outcome.Forwarded "VEROCAML_PPX";
           Outcome.Forwarded "VEROCAML_GHOST_DIR";
           Outcome.Stable_code "VERO_SOURCE_COMPILE";
           Outcome.Cleaned "source-temp-dirty";
         ])
    controlled_compiler_exit

let controlled_compiler_signal_case =
  Suite.case ~name:"controlled-compiler-signal"
    ~expectation:
      (expectation
         [
           Outcome.Exit_class (Outcome.Exited 2);
           Outcome.Forwarded "TMPDIR";
           Outcome.Forwarded "VEROCAML_OCAMLC";
           Outcome.Forwarded "VEROCAML_TEST_COMPILER_OUTCOME";
           Outcome.Forwarded "VEROCAML_PPX";
           Outcome.Forwarded "VEROCAML_GHOST_DIR";
           Outcome.Stable_code "VERO_SOURCE_COMPILE";
           Outcome.Cleaned "source-temp-dirty";
         ])
    controlled_compiler_signal

let compiler_invocation_failure_case =
  Suite.case ~name:"compiler-invocation-failure"
    ~expectation:
      (expectation
         [
           Outcome.Exit_class (Outcome.Exited 3);
           Outcome.Forwarded "VEROCAML_OCAMLC";
           Outcome.Forwarded "VEROCAML_PPX";
           Outcome.Forwarded "VEROCAML_GHOST_DIR";
           Outcome.Stable_code "VERO_INTERNAL";
         ])
    compiler_invocation_failure

let private_storage_setup_failure_case =
  Suite.case ~name:"private-storage-setup-failure"
    ~expectation:
      (expectation
         [
           Outcome.Exit_class (Outcome.Exited 3);
           Outcome.Forwarded "TMPDIR";
           Outcome.Stable_code "VERO_INTERNAL";
         ])
    private_storage_setup_failure

let with_named_fact name value outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      ((name, Outcome.Function_exists value) :: Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let direct_source_success ~environment ~workspace =
  let source =
    write_source workspace "verified.ml"
      "let identity (value : int) =\n  [%verocaml.ensures fun result -> result = value];\n  value\n"
  in
  let temporary_directory = Filename.concat (absolute workspace) "source-temp" in
  Unix.mkdir temporary_directory 0o755;
  Result.bind
    (Process_adapter.run ~cwd:workspace
       {
         program = installed_binary environment "verocaml";
         arguments = [ "verify"; source ];
         forwarded =
           [
             ("PATH", Project_environment.tool_path environment);
             ("TMPDIR", temporary_directory);
             ("OCAML_COLOR", "never");
             ("VEROCAML_PPX", installed_binary environment "verocaml-ppx");
             ("VEROCAML_GHOST_DIR", installed_ghost_directory environment);
           ];
         cleanup_paths =
           [ "fixtures/verified.cmi"; "fixtures/verified.cmo"; "fixtures/verified.cmt" ];
         adjacency = [];
       })
    (fun outcome ->
      if Array.length (Sys.readdir temporary_directory) <> 0 then
        Error
          (Failure.make Failure.Expectation_mismatch
             "private source-compilation storage was not cleaned")
      else Ok (with_named_fact "source-route" "retained-private-cmt" outcome))

let direct_source_success_case =
  Suite.case ~name:"direct-source-verifies-with-private-cleanup"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 0))
      |> Expectation.require_process_fact (Outcome.Forwarded "TMPDIR")
      |> Expectation.require_process_fact (Outcome.Cleaned "fixtures/verified.cmi")
      |> Expectation.require_process_fact (Outcome.Cleaned "fixtures/verified.cmo")
      |> Expectation.require_process_fact (Outcome.Cleaned "fixtures/verified.cmt")
      |> Expectation.require_named_fact "source-route"
           (Outcome.Function_exists "retained-private-cmt"))
    direct_source_success

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      direct_source_success_case;
      source_compile_type_error_case;
      controlled_compiler_exit_case;
      controlled_compiler_signal_case;
      compiler_invocation_failure_case;
      private_storage_setup_failure_case;
    ]
