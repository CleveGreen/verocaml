open Outcome_test_support

let suite_path = "test/invariant_contract_formula/outcome_cases.ml"

let ( let* ) = Result.bind

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/invariant_contract_formula" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let project sources =
  let modules = List.map fst sources in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name invariant_formula_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name invariant_formula_outcomes)\n (wrapped false)\n \
                 (modules %s)\n (libraries verocaml.ghost)\n (flags (:standard \
                 -ppx \"verocaml-ppx --keep-ghost\")))\n"
                (String.concat " " modules);
          };
        ]
        @ List.map
            (fun (name, contents) ->
              { Fixture.path = String.uncapitalize_ascii name ^ ".ml"; contents })
            sources;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = modules;
    }

let rec files_below root =
  Sys.readdir root |> Array.to_list
  |> List.concat_map (fun name ->
         let path = Filename.concat root name in
         if Sys.is_directory path then files_below path else [ path ])

let path_product_source = read_file "fixtures/path_product.ml"

let path_product_parity ~environment ~workspace =
  let input = project [ ("Path_product", path_product_source) ] in
  let* source = Fixture.run ~environment ~workspace input in
  let workspace =
    if Filename.is_relative workspace then Filename.concat (Sys.getcwd ()) workspace
    else workspace
  in
  let matches =
    files_below (Filename.concat workspace "project/_build")
    |> List.filter (fun path -> Filename.basename path = "path_product.cmt")
  in
  match matches with
  | [ artifact ] -> (
      match Fixture.prepared_cmt ~declared_dependencies:[ artifact ] artifact with
      | Error message -> Error (Failure.make Failure.Selected_cmt_load message)
      | Ok input ->
          let* prepared = Fixture.run ~environment ~workspace input in
          (match Outcome.semantic_parity ~except:[] source prepared with
          | Ok () -> Ok source
          | Error message ->
              Error (Failure.make Failure.Expectation_mismatch message)))
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           "prepared CMT discovery failed for path_product")

let path_product =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Path_product" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:Stack.singleton"
         (Outcome.Function_exists "Stack.singleton")
    |> Expectation.require_named_fact "function:Stack.zero_head"
         (Outcome.Function_exists "Stack.zero_head")
    |> Expectation.require_named_fact "function:singleton_then_zero"
         (Outcome.Function_exists "singleton_then_zero")
    |> Expectation.require_named_fact "obligation-kind:singleton_then_zero"
         (Outcome.Obligation_kind_exists
            {
              function_name = "singleton_then_zero";
              kind = Outcome.Call_precondition { callee = "Stack.zero_head" };
            })
  in
  Suite.case ~name:"path-product-project-prepared-cmt-parity" ~expectation
    path_product_parity

let replace_once ~needle ~replacement source =
  let needle_length = String.length needle in
  let rec find index =
    if index + needle_length > String.length source then None
    else if String.sub source index needle_length = needle then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> invalid_arg ("missing fixture marker: " ^ needle)
  | Some index ->
      String.sub source 0 index ^ replacement
      ^ String.sub source (index + needle_length)
          (String.length source - index - needle_length)

let false_source = read_file "fixtures/false_claims.ml"

let false_claim_sources =
  [
    ( "Wrong_singleton",
      replace_once ~needle:"view.head = value (* WRONG_SINGLETON_HEAD *)"
        ~replacement:"view.head = 0 (* WRONG_SINGLETON_HEAD *)" false_source );
    ( "Omitted_update",
      replace_once ~needle:"record.value <- 0; (* OMIT_UPDATE *)"
        ~replacement:"record.value <- 1; (* OMIT_UPDATE *)" false_source );
    ( "Weakened_precondition",
      replace_once ~needle:"view.length = 1 (* WEAK_PRECONDITION *)"
        ~replacement:"view.length >= 0 (* WEAK_PRECONDITION *)" false_source );
    ( "Skipped_transition",
      replace_once ~needle:"Stack.zero_head stack (* SKIP_TRANSITION *)"
        ~replacement:
          "let other = Stack.singleton value in let _ = Stack.zero_head other in \
           stack (* SKIP_TRANSITION *)"
        false_source );
    ( "Node_false",
      replace_once ~needle:"view.head = 0 (* NODE_REQUIRED *)"
        ~replacement:"view.head = 1 (* NODE_REQUIRED *)" false_source );
    ("Node_true", false_source);
  ]

let false_claims =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Wrong_singleton" Outcome.Unit_counterexample
    |> Expectation.require_unit "Omitted_update" Outcome.Unit_counterexample
    |> Expectation.require_unit "Weakened_precondition" Outcome.Unit_counterexample
    |> Expectation.require_unit "Skipped_transition" Outcome.Unit_counterexample
    |> Expectation.require_unit "Node_false" Outcome.Unit_counterexample
    |> Expectation.require_unit "Node_true" Outcome.Unit_verified
  in
  Suite.case ~name:"deliberate-false-claims-are-counterexamples" ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (project false_claim_sources))

let exec_proof_paths =
  let input =
    Fixture.single_source ~module_name:"Exec_proof_paths"
      ~source:(read_file "fixtures/exec_proof_paths.ml")
      ~libraries:[ "verocaml.ghost" ]
  in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Exec_proof_paths" Outcome.Unit_verified
    |> Expectation.require_named_fact "obligation-kind:exec_if"
         (Outcome.Obligation_kind_exists
            { function_name = "exec_if"; kind = Outcome.Postcondition })
    |> Expectation.require_named_fact "obligation-kind:proof_if"
         (Outcome.Obligation_kind_exists
            { function_name = "proof_if"; kind = Outcome.Local_assertion })
  in
  Suite.case ~name:"exec-and-proof-branches-verify" ~expectation
    (fun ~environment ~workspace -> Fixture.run ~environment ~workspace input)

let partial_match_fixtures =
  [
    ("Abstention_non_total_match", read_file "fixtures/abstention_non_total_match.ml");
    ("Abstention_malformed_form", read_file "fixtures/abstention_malformed_form.ml");
  ]

let partial_matches =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_TYPE"
    |> Expectation.require_unit "Abstention_non_total_match"
         Outcome.Unit_frontend_rejected
    |> Expectation.require_unit "Abstention_malformed_form"
         Outcome.Unit_frontend_rejected
  in
  Suite.case ~name:"partial-match-forms-are-rejected" ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (project partial_match_fixtures))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ path_product; false_claims; exec_proof_paths; partial_matches ]
