open Outcome_test_support

let ( let* ) = Result.bind
let suite_path = "test/logical_constant_transport_routes/outcome_cases.ml"

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let require condition format = if condition then Ok () else mismatch "%s" format

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let contains text fragment =
  let text_length = String.length text
  and fragment_length = String.length fragment in
  let rec search index =
    index + fragment_length <= text_length
    &&
    (String.sub text index fragment_length = fragment || search (index + 1))
  in
  fragment_length = 0 || search 0

let artifact root unit_name extension =
  let expected = String.uncapitalize_ascii unit_name ^ extension in
  match
    files_below (Filename.concat root "_build")
    |> List.filter (fun path ->
           String.equal (Filename.basename path) expected
           && not (contains path "/install/"))
  with
  | [ path ] -> Ok path
  | [] -> mismatch "missing artifact %s%s" unit_name extension
  | _ -> mismatch "ambiguous artifact %s%s" unit_name extension

let optional_artifact root unit_name extension =
  match artifact root unit_name extension with
  | Ok path -> Some path
  | Error _ -> None

let artifact_directories root =
  files_below (Filename.concat root "_build")
  |> List.filter (fun path ->
         List.mem (Filename.extension path) [ ".cmi"; ".cmti"; ".vri" ])
  |> List.map Filename.dirname
  |> List.sort_uniq String.compare

let load root unit_name =
  let* cmt = artifact root unit_name ".cmt" in
  let* cmi = artifact root unit_name ".cmi" in
  match
    Cmt_input.load_with_interface ~cmt ~cmi
      ?cmti:(optional_artifact root unit_name ".cmti")
      ?vri:(optional_artifact root unit_name ".vri")
      ~artifact_directories:(artifact_directories root) ()
  with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      mismatch "failed to load %s: %s" unit_name diagnostic.Diagnostic.code

type route = Standalone | Ppxlib

let route_name = function Standalone -> "standalone-v1" | Ppxlib -> "ppxlib-v1"

let preprocessing = function
  | Standalone -> "(flags (:standard -ppx \"verocaml-ppx --keep-ghost\"))"
  | Ppxlib -> "(preprocess (pps verocaml.ppx -- --verocaml-retained))"

let retained_authority_rules =
  {|(rule
 (targets provider.vri provider.verocaml-retained-interface provider.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .transport_provider.objs/byte/provider.cmt)
  (:cmi .transport_provider.objs/byte/provider.cmi)
  (:cmti .transport_provider.objs/byte/provider.cmti))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} provider.vri
    --artifact-directory .transport_provider.objs/byte)
   (run %{emitter} manifest provider.verocaml-retained-interface .transport_provider.objs/byte/provider.cmt .transport_provider.objs/byte/provider.cmi .transport_provider.objs/byte/provider.cmti provider.vri)
   (run %{emitter} manifest provider.verocaml-retained-interface.install provider.cmt provider.cmi provider.cmti provider.vri))))

(alias
 (name all)
 (deps provider.vri provider.verocaml-retained-interface))

(install
 (package logical_constant_transport_routes)
 (section lib)
 (files
  (provider.vri as ./provider.vri)
  (provider.verocaml-retained-interface.install as ./provider.verocaml-retained-interface)))
|}

let project route =
  let ppx = preprocessing route in
  Fixture.dune_project
    {
      files =
        [
          { Fixture.path = "dune-project";
            contents =
              "(lang dune 3.17)\n(name logical_constant_transport_routes)\n(package (name logical_constant_transport_routes))\n" };
          { path = "dune";
            contents =
              Printf.sprintf
                {|(library
 (name transport_provider)
 (wrapped false)
 (modules Provider)
 (libraries verocaml.vstd verocaml.ghost)
 %s)

%s

(library
 (name transport_consumer)
 (wrapped false)
 (modules Consumer)
 (libraries transport_provider verocaml.vstd verocaml.ghost)
 %s)
|}
                ppx retained_authority_rules ppx };
          { path = "provider.mli";
            contents =
              {|[%%verocaml.symbolic val symbolic : bool]
val revealed : bool [@@verocaml.spec] [@@verocaml.revealed]
|} };
          { path = "provider.ml";
            contents =
              {|[@@@verocaml.verify]

[%%verocaml.symbolic val symbolic : bool]
let revealed : bool = true [@@verocaml.spec]
|} };
          { path = "consumer.ml";
            contents =
              {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ -> Provider.symbolic = Provider.symbolic];
  [%verocaml.ensures fun _ -> Provider.revealed = Provider.revealed];
  ()
[@@verocaml.proof]
|} };
        ];
      libraries = [ "verocaml.vstd"; "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider"; "Consumer" ];
    }

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let verify ~threads ~unit_name ~dependencies implementation =
  let* configuration =
    match
      Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None
    with
    | Ok configuration -> Ok configuration
    | Error error ->
        mismatch "%s" (Verifier_service.configuration_error_message error)
  in
  match
    Verifier_service.verify
      (Verifier_service.request ~configuration ~consumer:implementation
         ~dependencies)
  with
  | Ok result -> Ok result
  | Error error ->
      mismatch "%s: %s" unit_name (Verifier_service.error_message error)

let imported_authority ~consumer ~provider =
  let* policy =
    match Solver_policy_private.create_default ~timeout_ms:60_000 with
    | Ok policy -> Ok policy
    | Error error -> mismatch "%s" (Solver_policy_private.error_to_string error)
  in
  let* environment, _ =
    match
      Interface_specification_loaded_private.authenticate ~solver_policy:policy
        ~external_targets:[] ~dependencies:[ provider ] ~consumer
    with
    | Ok authenticated -> Ok authenticated
    | Error error ->
        mismatch "%s"
          (Interface_specification_loaded_private.error_message error)
  in
  match
    Interface_specification_environment_private.imported_environment_authenticated
      environment
  with
  | Ok imported -> Ok imported
  | Error error ->
      mismatch "%s"
        (Interface_specification_environment_private.error_to_string error)

let descriptor_name = function
  | Retained_interface_authority_private.Symbolic_value -> "symbolic"
  | Defined_value -> "defined"

let visibility_name = function
  | Retained_interface_authority_private.Symbolic_opaque -> "symbolic-opaque"
  | Defined_opaque -> "defined-opaque"
  | Defined_revealed -> "defined-revealed"

let descriptor_shape implementation =
  implementation.Cmt_input.interface_logical_values
  |> List.map (fun descriptor ->
         ( descriptor.Retained_interface_authority_private.logical_value_path,
           descriptor_name descriptor.logical_value_class,
           visibility_name descriptor.logical_value_visibility ))
  |> List.sort compare

let descriptor_for path implementation =
  implementation.Cmt_input.interface_logical_values
  |> List.find_opt (fun descriptor ->
         String.equal
           descriptor.Retained_interface_authority_private.logical_value_path
           path)

let route_matches descriptor (route : Imported_callable.logical_constant_route) =
  String.equal descriptor.Retained_interface_authority_private.logical_value_path
    route.logical_constant_path
  && String.equal descriptor.logical_value_uid route.logical_constant_uid

let imported_constant path imported =
  Imported_callable.logical_constants imported
  |> List.find_opt (function
       | Imported_callable.Imported_symbolic_logical_value { routes; _ }
       | Imported_callable.Imported_defined_logical_value { routes; _ } ->
           List.exists
             (fun (route : Imported_callable.logical_constant_route) ->
               String.equal route.logical_constant_path path)
             routes)

let verify_descriptor_receipt descriptor =
  let expected =
    Retained_interface_authority_private.logical_value_receipt
      ~path:descriptor.Retained_interface_authority_private.logical_value_path
      ~uid:descriptor.logical_value_uid ~marker:descriptor.logical_value_marker
      ~typed_abi:descriptor.logical_value_typed_abi descriptor.logical_value_class
      descriptor.logical_value_visibility
  in
  String.equal expected descriptor.logical_value_descriptor_receipt

let instance_identity instance =
  let constant_id = Logical_constant_instance_private.constant_id instance in
  ( constant_id.Sst.constant_origin.origin_digest,
    constant_id.Sst.constant_name,
    Logical_constant_instance_private.identity_digest instance )

let vir_instances result =
  Verifier_service.vir result
  |> fun program ->
  program.Vir.functions
  |> List.concat_map (fun execution ->
         execution.Vir.obligations
         |> List.concat_map (fun obligation ->
                List.map instance_identity obligation.Vir.logical_constant_instances))
  |> List.sort_uniq compare

let vir_instance_shapes result =
  Verifier_service.vir result
  |> fun program ->
  program.Vir.functions
  |> List.concat_map (fun execution ->
         execution.Vir.obligations
         |> List.concat_map (fun obligation ->
                List.map
                  (fun instance ->
                    let id = Logical_constant_instance_private.constant_id instance in
                    ( id.Sst.constant_name,
                      List.length
                        (Logical_constant_instance_private.type_arguments instance) ))
                  obligation.Vir.logical_constant_instances))
  |> List.sort_uniq compare

let vir_symbolic_identities result =
  let rec boolean = function
    | Vir.Boolean_symbolic_application application ->
        [ Symbolic_application_private.identity_digest application ]
    | Vir.Boolean_not value -> boolean value
    | Vir.Boolean_and (left, right)
    | Vir.Boolean_or (left, right)
    | Vir.Boolean_equal (left, right)
    | Vir.Boolean_not_equal (left, right) ->
        boolean left @ boolean right
    | Vir.Forall_term quantifier | Vir.Exists_term quantifier ->
        boolean quantifier.boolean_quantifier_body
    | Vir.Boolean_constant _ | Vir.Logical_adt_schema _ | Vir.Boolean_symbol _
    | Vir.Integer_compare _ | Vir.Bv_equal _ | Vir.Bv_not_equal _
    | Vir.Bv_compare _ | Vir.Boolean_selector _
    | Vir.Aggregate_equal _ | Vir.Parametric_equal _
    | Vir.Boolean_invariant_application _
    | Vir.Boolean_recursive_spec_application _
    | Vir.Boolean_specification_application _
    | Vir.Callback_requires _ | Vir.Callback_ensures _ ->
        []
  in
  Verifier_service.vir result
  |> fun program ->
  program.Vir.functions
  |> List.concat_map (fun execution ->
         execution.Vir.obligations
         |> List.concat_map (fun (obligation : Vir.obligation) ->
                List.concat_map boolean
                  (obligation.Vir.assumptions
                  @ obligation.Vir.required_preceding_safety
                  @ obligation.Vir.path_condition
                  @ [ obligation.Vir.goal ])))
  |> List.sort_uniq String.compare

let vir_equation_instances result =
  Verifier_service.vir result
  |> fun program ->
  program.Vir.functions
  |> List.concat_map (fun execution ->
         execution.Vir.obligations
         |> List.concat_map (fun obligation ->
                List.map
                  (fun equation -> instance_identity equation.Vir.logical_constant_instance)
                  obligation.Vir.logical_constant_equations))
  |> List.sort_uniq compare

type run = {
  fixture_outcome : Outcome.t;
  provider : Cmt_input.implementation;
  consumer : Cmt_input.implementation;
  imported : Imported_callable.environment;
  provider_results : (int * Verifier_service.result) list;
  consumer_results : (int * Verifier_service.result) list;
}

let logical_value_shapes (run : run) =
  Imported_callable.logical_constants run.imported
  |> List.filter_map (function
       | Imported_callable.Imported_symbolic_logical_value
           { symbolic; routes; _ } ->
           Some
             (List.map
                (fun (route : Imported_callable.logical_constant_route) ->
                  ( route.logical_constant_path,
                    "symbolic",
                    "symbolic-opaque",
                    Parametric_type.to_string symbolic.definition.result_type,
                    List.length symbolic.definition.parameters ))
                routes)
       | Imported_callable.Imported_defined_logical_value
           { routes; semantic_authority; _ } ->
           Some
             (List.map
                (fun (route : Imported_callable.logical_constant_route) ->
                  ( route.logical_constant_path,
                    "defined",
                    "defined-revealed",
                    Parametric_type.to_string
                      semantic_authority.constant_declared_type,
                    List.length semantic_authority.constant_type_binders ))
                routes))
  |> List.concat
  |> List.sort compare

let route_run route ~environment ~workspace =
  let input = project route in
  let* fixture_outcome = run_fixture input ~environment ~workspace in
  let root = Filename.concat workspace "project" in
  let* provider = load root "Provider" in
  let* consumer = load root "Consumer" in
  let* imported = imported_authority ~consumer ~provider in
  let* provider_one =
    verify ~threads:1 ~unit_name:"Provider" ~dependencies:[] provider
  in
  let* provider_two =
    verify ~threads:2 ~unit_name:"Provider" ~dependencies:[] provider
  in
  let* consumer_one =
    verify ~threads:1 ~unit_name:"Consumer" ~dependencies:[ provider ] consumer
  in
  let* consumer_two =
    verify ~threads:2 ~unit_name:"Consumer" ~dependencies:[ provider ] consumer
  in
  Ok
    { fixture_outcome;
      provider;
      consumer;
      imported;
      provider_results = [ (1, provider_one); (2, provider_two) ];
      consumer_results = [ (1, consumer_one); (2, consumer_two) ] }

let route_receipts route run =
  let expected_route = route_name route in
  let implementation_receipt ~authority implementation =
    implementation.Cmt_input.implementation_family_issuers = [ expected_route ]
    && implementation.implementation_family_markers = [ "retained-v1" ]
    && implementation.ppxlib_context = (route = Ppxlib)
    && (not authority || Option.is_some implementation.retained_authority)
    && (not authority
       || Option.equal
            (fun left right ->
              List.sort compare left = List.sort compare right)
            (Some implementation.interface_logical_values)
            (Option.map
               Retained_interface_authority_private.logical_values
               implementation.retained_authority))
    && Cmt_input.retained_ppx_artifact implementation
  in
  let* () =
    require (implementation_receipt ~authority:true run.provider)
      "provider route receipt is not exact"
  in
  let* () =
    require (implementation_receipt ~authority:false run.consumer)
      "consumer route receipt is not exact"
  in
    require
    (run.provider.interface_family_issuers = [ expected_route ]
    && run.provider.interface_family_markers = [ "retained-v1" ]
    && run.provider.interface_logical_values <> []
    && run.consumer.interface_logical_values = [])
    "retained route receipt did not distinguish provider values from consumer values"

let exact_descriptors run =
  let* symbolic =
    match descriptor_for "Provider.symbolic" run.provider with
    | Some descriptor -> Ok descriptor
    | None -> mismatch "symbolic logical-value descriptor is absent"
  in
  let* revealed =
    match descriptor_for "Provider.revealed" run.provider with
    | Some descriptor -> Ok descriptor
    | None -> mismatch "revealed logical-value descriptor is absent"
  in
  let* () =
    require
      (symbolic.logical_value_class = Retained_interface_authority_private.Symbolic_value
      && symbolic.logical_value_visibility = Symbolic_opaque
      && revealed.logical_value_class = Defined_value
      && revealed.logical_value_visibility = Defined_revealed)
      "logical-value descriptor class or visibility changed"
  in
  let* () =
    require
      (verify_descriptor_receipt symbolic && verify_descriptor_receipt revealed
      && List.for_all
           (fun descriptor ->
             let path =
               descriptor.Retained_interface_authority_private.logical_value_path
             in
             String.equal path "Provider.symbolic"
             || String.equal path "Provider.revealed")
           run.provider.interface_logical_values
      && List.for_all verify_descriptor_receipt run.provider.interface_logical_values
      && List.for_all
           (fun descriptor ->
             String.starts_with ~prefix:"Provider."
               descriptor.Retained_interface_authority_private.logical_value_path)
           run.provider.interface_logical_values)
      "logical-value descriptor receipt is not self-authenticating"
  in
  Ok (symbolic, revealed)

let imported_partition run =
  let constants = Imported_callable.logical_constants run.imported in
  let callables = Imported_callable.callables run.imported in
  let* symbolic_routes, symbolic_definition =
    match imported_constant "Provider.symbolic" run.imported with
    | Some
        (Imported_callable.Imported_symbolic_logical_value
          { routes; symbolic; _ }) ->
        Ok (routes, symbolic.definition)
    | Some (Imported_callable.Imported_defined_logical_value _) ->
        mismatch "symbolic value was imported as a defined value"
    | None -> mismatch "symbolic value was not imported"
  in
  let* revealed_routes, revealed_definition, revealed_authority =
    match imported_constant "Provider.revealed" run.imported with
    | Some
        (Imported_callable.Imported_defined_logical_value
          { routes; definition; semantic_authority; _ }) ->
        Ok (routes, definition, semantic_authority)
    | Some (Imported_callable.Imported_symbolic_logical_value _) ->
        mismatch "revealed value was imported as a symbolic value"
    | None -> mismatch "revealed value was not imported"
  in
  let* symbolic_descriptor, revealed_descriptor = exact_descriptors run in
  let* () =
    require
      (List.exists (route_matches symbolic_descriptor) symbolic_routes
      && List.exists (route_matches revealed_descriptor) revealed_routes
      && List.for_all
           (fun (route : Imported_callable.logical_constant_route) ->
             String.equal route.logical_constant_path "Provider.symbolic")
           symbolic_routes
      && List.for_all
           (fun (route : Imported_callable.logical_constant_route) ->
             String.equal route.logical_constant_path "Provider.revealed")
           revealed_routes)
      "logical-value route receipts do not identify their provider values"
  in
  let callable_paths =
    List.map
      (fun (callable : Imported_callable.callable_snapshot) -> callable.path)
      callables
  in
  let* () =
    require
      (not (List.mem "Provider.symbolic" callable_paths)
      && not (List.mem "Provider.revealed" callable_paths)
      && List.exists
           (function
             | Imported_callable.Imported_symbolic_logical_value _ -> true
             | Imported_callable.Imported_defined_logical_value _ -> false)
           constants
      && List.exists
           (function
             | Imported_callable.Imported_symbolic_logical_value _ -> false
             | Imported_callable.Imported_defined_logical_value _ -> true)
           constants)
      "logical values were conflated with callable imports"
  in
  let* () =
    require
      (revealed_definition.Sst.constant_provenance = Sst.Opaque_defined_identity
      && Option.is_none revealed_definition.constant_equation
      && revealed_authority.constant_provenance
         = Sst.Verified_definitional_equation
      && Option.is_some revealed_authority.constant_equation)
      "imported defined value did not keep inactive and semantic equations distinct"
  in
  let* () =
    require
      (symbolic_definition.parameters = []
      && symbolic_definition.result_type = Sst.Bool
      && revealed_authority.constant_type_binders = []
      && revealed_authority.constant_declared_type = Sst.Bool)
      "transported logical values are not actual nullary Boolean values"
  in
  Ok ()

let semantic_parity left right =
  match Outcome.semantic_parity ~except:[] left right with
  | Ok () -> Ok ()
  | Error message -> mismatch "semantic outcome parity failed: %s" message

let thread_observations run =
  let provider_outcomes =
    List.map (fun (_, result) -> Outcome.of_verifier_result result) run.provider_results
  and consumer_outcomes =
    List.map (fun (_, result) -> Outcome.of_verifier_result result) run.consumer_results
  in
  let* () =
    require
      (List.for_all (fun outcome -> Outcome.status outcome = Outcome.Verified)
         (provider_outcomes @ consumer_outcomes))
      "a retained route did not verify under every requested thread setting"
  in
  let* () =
    match consumer_outcomes with
    | [ one; two ] -> semantic_parity one two
    | _ -> mismatch "consumer thread observations are incomplete"
  in
  let* () =
    match provider_outcomes with
    | [ one; two ] -> semantic_parity one two
    | _ -> mismatch "provider thread observations are incomplete"
  in
  let provider_instances =
    List.map (fun (_, result) -> vir_instances result) run.provider_results
  and consumer_instances =
    List.map (fun (_, result) -> vir_instances result) run.consumer_results
  and consumer_symbolic_identities =
    List.map
      (fun (_, result) -> vir_symbolic_identities result)
      run.consumer_results
  in
  let* () =
    require
      (match consumer_instances with
      | first :: rest -> List.for_all (fun instances -> instances = first) rest
      | [] -> false)
      "consumer logical-constant instance identities changed with threads"
  in
  let* () =
    require
      (match provider_instances with
      | first :: rest -> List.for_all (fun instances -> instances = first) rest
      | [] -> false)
      "provider logical-constant instance identities changed with threads"
  in
  let* () =
    require
      (match consumer_symbolic_identities with
      | first :: rest ->
          first <> [] && List.for_all (fun identities -> identities = first) rest
      | [] -> false)
      "consumer symbolic logical-value identity changed with threads"
  in
  let* () =
    require
      (List.for_all
         (fun (_, result) ->
           not
             (List.exists
                (fun (_, name, _) -> String.equal name "Provider.revealed")
                (vir_equation_instances result)))
         run.consumer_results)
      "inactive imported equations entered consumer VIR authority"
  in
  match run.consumer_results with
  | [ (_, one); (_, two) ] ->
      Ok
        ( Outcome.of_verifier_result one,
          vir_instances one,
          vir_instance_shapes two )
  | _ -> mismatch "consumer thread observations are incomplete"

let logical_instance_presence instances =
  List.exists
    (fun (_, name, _) -> String.equal name "Provider.revealed")
    instances

let case =
  Suite.case ~name:"retained-nullary-logical-value-route-thread-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:stable"
           (Outcome.Function_exists "stable"))
    (fun ~environment ~workspace ->
      let* standalone =
        route_run Standalone ~environment
          ~workspace:(Filename.concat workspace "standalone")
      in
      let* ppxlib =
        route_run Ppxlib ~environment
          ~workspace:(Filename.concat workspace "ppxlib")
      in
      let* () = route_receipts Standalone standalone in
      let* () = route_receipts Ppxlib ppxlib in
      let* _ = imported_partition standalone in
      let* _ = imported_partition ppxlib in
      let* standalone_consumer, standalone_instances, standalone_shapes =
        thread_observations standalone
      in
      let* ppxlib_consumer, ppxlib_instances, ppxlib_shapes =
        thread_observations ppxlib
      in
      let* () = semantic_parity standalone_consumer ppxlib_consumer in
      let* () =
        require
          (standalone_shapes = ppxlib_shapes
          && List.map (fun (_, name, _) -> name) standalone_instances
             = List.map (fun (_, name, _) -> name) ppxlib_instances
          && logical_instance_presence standalone_instances)
          "logical-constant instance identity changed between PPX routes"
      in
      let* () =
        require
          (descriptor_shape standalone.provider = descriptor_shape ppxlib.provider
          && logical_value_shapes standalone = logical_value_shapes ppxlib)
          "standalone and Ppxlib logical-value descriptors diverged"
      in
      Ok (Outcome.merge [ standalone.fixture_outcome; ppxlib.fixture_outcome ]))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected [ case ]
