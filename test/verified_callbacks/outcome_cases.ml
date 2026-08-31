open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/verified_callbacks/outcome_cases.ml"

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let executable_directory () = absolute Sys.executable_name |> Filename.dirname

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture_source name =
  let candidates =
    [
      Filename.concat (executable_directory ()) (Filename.concat "fixtures" name);
      Filename.concat (Sys.getcwd ()) (Filename.concat "fixtures" name);
      Filename.concat (Sys.getcwd ())
        (Filename.concat "test/verified_callbacks/fixtures" name);
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> read_file path
  | None -> failwith ("fixture source is unavailable: " ^ name)

let input module_name fixture =
  Fixture.single_source ~module_name ~source:(fixture_source fixture)
    ~libraries:[ "verocaml.ghost" ]

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let discover_cmt project_root unit_name =
  let expected = String.uncapitalize_ascii unit_name ^ ".cmt" in
  match
    files_below (Filename.concat project_root "_build")
    |> List.filter (fun path -> String.equal (Filename.basename path) expected)
  with
  | [ path ] -> Ok path
  | [] -> mismatch "no prepared CMT for unit %s" unit_name
  | _ -> mismatch "ambiguous prepared CMT for unit %s" unit_name

let disposition outcome =
  match Outcome.status outcome with
  | Outcome.Verified -> Outcome.Unit_verified
  | Counterexample -> Unit_counterexample
  | Inconclusive -> Unit_inconclusive
  | Incomplete_source -> Unit_incomplete_source
  | Frontend_rejected -> Unit_frontend_rejected

let with_modes modes outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      (List.map (fun (key, value) -> (key, Outcome.Function_exists value)) modes
      @ Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let load_implementation cmt =
  let cmi = Filename.remove_extension cmt ^ ".cmi" in
  let loaded =
    if Sys.file_exists cmi then Cmt_input.load_with_interface ~cmt ~cmi ()
    else Cmt_input.load cmt
  in
  match loaded with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (Printf.sprintf "%s: %s" diagnostic.Diagnostic.code diagnostic.message))

let verify_prepared ~threads ~unit_name cmt =
  let* () =
    match Fixture.prepared_cmt ~declared_dependencies:[ cmt ] cmt with
    | Ok _ -> Ok ()
    | Error message -> mismatch "prepared CMT declaration: %s" message
  in
  let* implementation = load_implementation cmt in
  let* configuration =
    match
      Verifier_service.configuration ~threads ~timeout_ms:10_000 ~rlimit:None
    with
    | Ok configuration -> Ok configuration
    | Error error ->
        Error
          (Failure.make Failure.Runner_internal
             (Verifier_service.configuration_error_message error))
  in
  let request =
    Verifier_service.request ~configuration ~consumer:implementation
      ~dependencies:[]
  in
  match Verifier_service.verify request with
  | Ok result ->
      let outcome = Outcome.of_verifier_result result in
      Ok (Outcome.with_unit unit_name (disposition outcome) outcome)
  | Error error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))

let require_parity ~except left right =
  match Outcome.semantic_parity ~except left right with
  | Ok () -> Ok ()
  | Error message -> mismatch "%s" message

let parity_runner module_name fixture ~environment ~workspace =
  let source_workspace = Filename.concat workspace "dune-source" in
  let* source =
    Fixture.run ~environment ~workspace:source_workspace
      (input module_name fixture)
  in
  let* cmt =
    discover_cmt (Filename.concat source_workspace "project") module_name
  in
  let* prepared = verify_prepared ~threads:2 ~unit_name:module_name cmt in
  let source =
    with_modes
      [ ("input-mode", "dune-project"); ("execution-mode", "threads-1") ]
      source
  in
  let prepared =
    with_modes
      [ ("input-mode", "prepared-cmt"); ("execution-mode", "threads-2") ]
      prepared
  in
  let* () =
    require_parity ~except:[ "input-mode"; "execution-mode" ] source prepared
  in
  Ok (Outcome.merge [ source; prepared ])

let verified_expectation module_name functions =
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_named_fact ("function:" ^ function_name)
        (Outcome.Function_exists function_name) expectation)
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit module_name Outcome.Unit_verified)
    functions

let verified_case ~name ~module_name ~fixture functions =
  Suite.case ~name ~expectation:(verified_expectation module_name functions)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let parity_case ~name ~module_name ~fixture functions =
  let expectation =
    verified_expectation module_name functions
    |> Expectation.require_named_fact "input-mode"
         (Outcome.Function_exists "dune-project")
    |> Expectation.require_named_fact "input-mode"
         (Outcome.Function_exists "prepared-cmt")
    |> Expectation.require_named_fact "execution-mode"
         (Outcome.Function_exists "threads-1")
    |> Expectation.require_named_fact "execution-mode"
         (Outcome.Function_exists "threads-2")
  in
  Suite.case ~name ~expectation (parity_runner module_name fixture)

let counterexample_case ~name ~module_name ~fixture ~function_name ~kind =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name kind
      |> Expectation.require_unit module_name Outcome.Unit_counterexample)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let source_input module_name source =
  Fixture.single_source ~module_name ~source ~libraries:[ "verocaml.ghost" ]

let rejection_case ~name ~module_name ~fixture ~code =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let source_rejection_case ~name ~module_name ~source ~code =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (source_input module_name source))

let semantic_case ~name ~module_name ~fixture ~function_name ~kind =
  counterexample_case ~name ~module_name ~fixture ~function_name ~kind

let mode_matrix_source =
  {|
let apply_portable (f : (int -> int) @ portable) x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let apply_local (f : (int -> int) @ local) x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let apply_once (f : (int -> int) @ once) x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let apply_unique (f : (int -> int) @ unique) x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let id x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  x

let local_client x = apply_local id x
let once_client x = apply_once id x
let unique_client x = apply_unique id x
|}

let mode_matrix_case =
  Suite.case ~name:"callback-mode-matrix-verifies"
    ~expectation:
      (verified_expectation "Mode_matrix"
         [ "local_client"; "once_client"; "unique_client" ])
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (source_input "Mode_matrix" mode_matrix_source))

let capture_prelude =
  {|
let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x
|}

let capture_mutable_record_source =
  {|
type box = { mutable value : int }
|}
  ^ capture_prelude
  ^ {|
let rejected x =
  let captured = { value = x } in
  let callback y =
    [%verocaml.requires true];
    [%verocaml.ensures fun _ -> true];
    y + captured.value
  in
  apply callback x
|}

let capture_mutable_adt_source =
  {|
type cell = Cell of int ref
|}
  ^ capture_prelude
  ^ {|
let rejected x =
  let captured = Cell (ref x) in
  let callback y =
    [%verocaml.requires true];
    [%verocaml.ensures fun _ -> true];
    match captured with Cell reference -> y + !reference
  in
  apply callback x
|}

let capture_transitive_source =
  {|
type inner = { cell : int ref }
type outer = { nested : inner }
|}
  ^ capture_prelude
  ^ {|
let rejected x =
  let captured = { nested = { cell = ref x } } in
  let callback y =
    [%verocaml.requires true];
    [%verocaml.ensures fun _ -> true];
    y + !(captured.nested.cell)
  in
  apply callback x
|}

let capture_function_source =
  capture_prelude
  ^ {|
let rejected x =
  let helper (value : int) = value in
  let callback y =
    [%verocaml.requires true];
    [%verocaml.ensures fun _ -> true];
    helper y
  in
  apply callback x
|}

let capture_nested_source =
  capture_prelude
  ^ {|
let rejected x =
  let outer (value : int) =
    [%verocaml.requires true];
    [%verocaml.ensures fun result -> result = value];
    value
  in
  let callback y =
    [%verocaml.requires true];
    [%verocaml.ensures fun _ -> true];
    outer y
  in
  apply callback x
|}

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      parity_case ~name:"top-level-source-cmt-thread-parity"
        ~module_name:"Top_level_callbacks" ~fixture:"top_level_callbacks.ml"
        [
          "identity";
          "select";
          "zero_or_self";
          "apply";
          "apply_labelled";
          "use_identity";
          "use_zero_or_self";
          "use_labelled";
        ];
      verified_case ~name:"local-callbacks-verify"
        ~module_name:"Local_callbacks" ~fixture:"local_callbacks.ml"
        [ "apply"; "immediate"; "captured"; "generic_capture"; "shadowed_capture"; "identical_calls" ];
      verified_case ~name:"polymorphic-callbacks-verify"
        ~module_name:"Polymorphic_apply" ~fixture:"polymorphic_apply.ml"
        [ "apply"; "relay"; "identity"; "negate"; "use_int"; "use_bool" ];
      verified_case ~name:"relational-result-verifies"
        ~module_name:"Relational_result" ~fixture:"relational_result.ml"
        [ "choose"; "apply"; "client" ];
      mode_matrix_case;
      rejection_case ~name:"reject-missing-callback-contract"
        ~module_name:"Negative_missing_contract"
        ~fixture:"negative_missing_contract.ml" ~code:"VERO_CALLBACK_CONTRACT";
      rejection_case ~name:"reject-builtin-callback-grammar"
        ~module_name:"Negative_builtin_grammar"
        ~fixture:"negative_builtin_grammar.ml"
        ~code:"VERO_INVALID_CALLBACK";
      rejection_case ~name:"reject-partial-callback-application"
        ~module_name:"Negative_partial" ~fixture:"negative_partial.ml"
        ~code:"VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
      rejection_case ~name:"reject-callback-escape"
        ~module_name:"Negative_escape" ~fixture:"negative_escape.ml"
        ~code:"VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
      rejection_case ~name:"reject-mutable-callback-capture"
        ~module_name:"Negative_capture" ~fixture:"negative_capture.ml"
        ~code:"VERO_UNSUPPORTED_MUTATION";
      rejection_case ~name:"reject-recursive-callback"
        ~module_name:"Negative_recursive" ~fixture:"negative_recursive.ml"
        ~code:"VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
      rejection_case ~name:"reject-callback-equality"
        ~module_name:"Negative_equality" ~fixture:"negative_equality.ml"
        ~code:"VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
      source_rejection_case ~name:"reject-mutable-record-capture"
        ~module_name:"Capture_mutable_record"
        ~source:capture_mutable_record_source ~code:"VERO_CALLBACK_POLICY";
      source_rejection_case ~name:"reject-mutable-adt-capture"
        ~module_name:"Capture_mutable_adt" ~source:capture_mutable_adt_source
        ~code:"VERO_UNSUPPORTED_TYPE";
      source_rejection_case ~name:"reject-transitively-mutable-capture"
        ~module_name:"Capture_transitive" ~source:capture_transitive_source
        ~code:"VERO_UNSUPPORTED_TYPE";
      source_rejection_case ~name:"reject-function-capture"
        ~module_name:"Capture_function" ~source:capture_function_source
        ~code:"VERO_CALLBACK_CONTRACT";
      source_rejection_case ~name:"reject-nested-callback-capture"
        ~module_name:"Capture_nested" ~source:capture_nested_source
        ~code:"VERO_CALLBACK_POLICY";
      semantic_case ~name:"callback-precondition-counterexample"
        ~module_name:"Semantic_precondition" ~fixture:"semantic_precondition.ml"
        ~function_name:"unchecked"
        ~kind:(Outcome.Callback_precondition { callback_name = "f" });
      semantic_case ~name:"callback-closure-postcondition-counterexample"
        ~module_name:"Semantic_closure" ~fixture:"semantic_closure.ml"
        ~function_name:"bad" ~kind:Outcome.Postcondition;
      semantic_case ~name:"stronger-client-postcondition-counterexample"
        ~module_name:"Semantic_stronger" ~fixture:"semantic_stronger.ml"
        ~function_name:"client" ~kind:Outcome.Postcondition;
    ]
