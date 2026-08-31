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
      adjacency =
        [
          ( "source-compile-exit-2",
            "VERO_SOURCE_COMPILE",
            "compiler=exit 2" );
        ];
    }

let cleanup_script =
  {|"$@"
status=$?
if [ -d "$TMPDIR" ] && [ -z "$(find "$TMPDIR" -mindepth 1 -print -quit)" ]; then
  rm -f source-temp-dirty
fi
exit "$status"
|}

let controlled_compiler ~outcome ~adjacency ~environment ~workspace =
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
      adjacency;
    }

let controlled_compiler_exit =
  controlled_compiler ~outcome:"exit"
    ~adjacency:
      [
        ( "source-compile-exit-7",
          "VERO_SOURCE_COMPILE",
          "compiler=exit 7" );
        ( "compiler-stdout-before-stderr",
          "controlled compiler stdout",
          "controlled compiler stderr" );
        ( "private-compiler-protocol",
          "temp-mode=700",
          "output=source.cmo input=fixtures/verified.ml color=always" );
      ]

let controlled_compiler_signal =
  controlled_compiler ~outcome:"signal"
    ~adjacency:
      [
        ( "source-compile-signal",
          "compiler=signal ",
          "controlled compiler signal" );
      ]

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
           Outcome.Adjacent "source-compile-exit-2";
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
           Outcome.Adjacent "source-compile-exit-7";
           Outcome.Adjacent "compiler-stdout-before-stderr";
           Outcome.Adjacent "private-compiler-protocol";
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
           Outcome.Adjacent "source-compile-signal";
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

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      source_compile_type_error_case;
      controlled_compiler_exit_case;
      controlled_compiler_signal_case;
      compiler_invocation_failure_case;
      private_storage_setup_failure_case;
    ]
