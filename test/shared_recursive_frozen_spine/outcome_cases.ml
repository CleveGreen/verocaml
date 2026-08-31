open Outcome_test_support

let suite_path = "test/shared_recursive_frozen_spine/outcome_cases.ml"

let ( let* ) = Result.bind

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/shared_recursive_frozen_spine" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let exact_source = read_file "fixtures/exact_pfc.ml"

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

let project sources =
  let modules = List.map fst sources in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name frozen_spine_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name frozen_spine_outcomes)\n (wrapped false)\n \
                 (modules %s)\n (libraries verocaml.ghost)\n (flags (:standard \
                 -ppx \"verocaml-ppx --keep-ghost\")))\n"
                (String.concat " " modules);
          };
        ]
        @ List.map
            (fun (module_name, source) ->
              { Fixture.path = String.uncapitalize_ascii module_name ^ ".ml"; contents = source })
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

let exact_parity ~environment ~workspace =
  let input = project [ ("Exact_pfc", exact_source) ] in
  let* source = Fixture.run ~environment ~workspace input in
  let workspace =
    if Filename.is_relative workspace then Filename.concat (Sys.getcwd ()) workspace
    else workspace
  in
  let matches =
    files_below (Filename.concat workspace "project/_build")
    |> List.filter (fun path -> Filename.basename path = "exact_pfc.cmt")
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
           "prepared CMT discovery failed for exact_pfc")

let exact_case =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Exact_pfc" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:List.make_two"
         (Outcome.Function_exists "List.make_two")
    |> Expectation.require_named_fact "function:List.set_head"
         (Outcome.Function_exists "List.set_head")
    |> Expectation.require_named_fact "function:replace_head_and_read"
         (Outcome.Function_exists "replace_head_and_read")
  in
  Suite.case ~name:"exact-frozen-spine-project-prepared-cmt-parity"
    ~expectation exact_parity

let wrong_write =
  replace_once ~needle:"node.value <- value"
    ~replacement:"node.value <- value + 1" exact_source

let wrong_tail =
  replace_once ~needle:"contents node = More (value, tail ([%verocaml.old (contents node)]))"
    ~replacement:"contents node = More (value, End)" exact_source

let invalid_alias =
  exact_source
  ^ {|

let invalid_independent_alias
    (x : (List.t [@finite]) @ aliased)
    (y : (List.t [@finite]) @ aliased) value : unit =
  [%verocaml.requires value <> List.head x];
  [%verocaml.ensures fun _ ->
    List.contents y = [%verocaml.old (List.contents y)]];
  List.set_head x value

let invoke_same_actual value : unit =
  [%verocaml.requires value <> 1];
  let xs = List.make_two 1 2 in
  invalid_independent_alias xs xs value
|}

let invalid_programs ~environment ~workspace =
  Fixture.run ~environment ~workspace
    (project
       [
         ("Wrong_write", wrong_write);
         ("Wrong_tail", wrong_tail);
         ("Invalid_alias", invalid_alias);
       ])

let counterexamples =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Wrong_write" Outcome.Unit_counterexample
    |> Expectation.require_unit "Wrong_tail" Outcome.Unit_counterexample
    |> Expectation.require_unit "Invalid_alias" Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"List.set_head"
         (Outcome.Arithmetic_safety (Outcome.Add, Outcome.Upper))
    |> Expectation.require_semantic ~function_name:"List.set_head"
         Outcome.Postcondition
    |> Expectation.require_semantic ~function_name:"invalid_independent_alias"
         Outcome.Postcondition
  in
  Suite.case ~name:"invalid-frozen-spine-claims-are-counterexamples"
    ~expectation invalid_programs

let two_calls_source =
  exact_source
  ^ {|

let replace_twice (xs : (List.t [@finite]) @ aliased) first second : int =
  [%verocaml.requires first <> List.head xs && second <> first];
  [%verocaml.ensures fun result ->
    result = second
    && List.contents xs = More (second, tail ([%verocaml.old (List.contents xs)]))];
  let peer = xs in
  List.set_head peer first;
  List.set_head xs second;
  List.head peer
|}

let two_calls =
  let input =
    Fixture.single_source ~module_name:"Two_calls" ~source:two_calls_source
      ~libraries:[ "verocaml.ghost" ]
  in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Two_calls" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:replace_twice"
         (Outcome.Function_exists "replace_twice")
  in
  Suite.case ~name:"sequential-frozen-spine-updates-verify" ~expectation
    (fun ~environment ~workspace -> Fixture.run ~environment ~workspace input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ exact_case; counterexamples; two_calls ]
