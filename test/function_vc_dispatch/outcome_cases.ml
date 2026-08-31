open Outcome_test_support

let ( let* ) = Result.bind
let base module_name status disposition =
  Expectation.empty |> Expectation.status status
  |> Expectation.require_unit module_name disposition

let require_functions names expectation =
  List.fold_left
    (fun expectation name ->
      Expectation.require_named_fact ("function:" ^ name)
        (Outcome.Function_exists name) expectation)
    expectation names

let require_kinds function_name kinds expectation =
  List.fold_left
    (fun expectation kind ->
      Expectation.require_named_fact ("obligation-kind:" ^ function_name)
        (Outcome.Obligation_kind_exists { function_name; kind }) expectation)
    expectation kinds

let independent_source =
  {vero|let independent_alpha (value : int) : int =
  assert (value = value);
  assert (value <= value);
  value

let independent_beta (value : int) : int =
  assert (value >= value);
  assert (value = value);
  value

let independent_gamma (choose : bool) (value : int) : int =
  if choose then assert (value = value) else assert (value <= value);
  value
|vero}

let rec files_below root =
  if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let discover_cmt workspace =
  let matches =
    files_below (Filename.concat workspace "project/_build")
    |> List.filter (fun path ->
         String.equal (Filename.basename path) "independent_dispatch.cmt")
  in
  match matches with
  | [ path ] -> Ok path
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           "expected one Independent_dispatch CMT")

let tagged mode outcome =
  Outcome.merge
    [ outcome;
      Outcome.observation ~status:Outcome.Verified
        ~named_facts:[ ("execution-mode", Outcome.Function_exists mode) ] ()
      |> Outcome.project ]

let threaded_projection cmt =
  match Cmt_input.load cmt with
  | Error diagnostic ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message))
  | Ok consumer -> (
      match
        Verifier_service.configuration ~threads:2 ~timeout_ms:60_000 ~rlimit:None
      with
      | Error error ->
          Error
            (Failure.make Failure.Runner_internal
               (Verifier_service.configuration_error_message error))
      | Ok configuration ->
          let request = Verifier_service.request ~configuration ~consumer ~dependencies:[] in
          match Verifier_service.verify request with
          | Error error ->
              Error
                (Failure.make Failure.Verifier_outcome
                   (Verifier_service.error_message error))
          | Ok result ->
              Ok
                (Outcome.of_verifier_result result
                |> Outcome.with_unit "Independent_dispatch"
                     (match Verifier_service.status result with
                     | Verifier_service.Verified -> Outcome.Unit_verified
                     | Counterexample -> Outcome.Unit_counterexample
                     | Inconclusive -> Outcome.Unit_inconclusive
                     | Incomplete_source -> Outcome.Unit_incomplete_source)))

let thread_parity ~environment ~workspace =
  let input =
    Fixture.single_source ~module_name:"Independent_dispatch"
      ~source:independent_source ~libraries:[ "verocaml.ghost" ]
  in
  let serial_workspace = Filename.concat workspace "serial" in
  let* serial = Fixture.run ~environment ~workspace:serial_workspace input in
  let* cmt = discover_cmt serial_workspace in
  let* threaded = threaded_projection cmt in
  let serial = tagged "threads-1" serial in
  let threaded = tagged "threads-2" threaded in
  let* () =
    match Outcome.semantic_parity ~except:[ "execution-mode" ] serial threaded with
    | Ok () -> Ok ()
    | Error message -> Error (Failure.make Failure.Expectation_mismatch message)
  in
  Ok (Outcome.merge [ serial; threaded ])

let thread_parity_case =
  let expectation =
    base "Independent_dispatch" Outcome.Verified Outcome.Unit_verified
    |> require_functions
         [ "independent_alpha"; "independent_beta"; "independent_gamma" ]
    |> require_kinds "independent_alpha" [ Outcome.Local_assertion ]
    |> require_kinds "independent_beta" [ Outcome.Local_assertion ]
    |> require_kinds "independent_gamma" [ Outcome.Local_assertion ]
    |> Expectation.require_named_fact "execution-mode"
         (Outcome.Function_exists "threads-1")
    |> Expectation.require_named_fact "execution-mode"
         (Outcome.Function_exists "threads-2")
  in
  Suite.case ~name:"serial-threaded-semantic-parity" ~expectation thread_parity

let () =
  Suite.run_cli ~suite_path:"test/function_vc_dispatch/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected [ thread_parity_case ]
