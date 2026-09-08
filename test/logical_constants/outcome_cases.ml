open Outcome_test_support

let ( let* ) = Result.bind
let suite_path = "test/logical_constants/outcome_cases.ml"

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let require condition message = if condition then Ok () else mismatch "%s" message

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

let signature_values cmi =
  let information = Cmi_format.read_cmi_lazy cmi in
  Subst.Lazy.force_signature information.Cmi_format.cmi_sign
  |> List.filter_map (function
       | Types.Sig_value (ident, _, Types.Exported) -> Some (Ident.name ident)
       | _ -> None)

let implementation_values cmt =
  match Cmt_format.read cmt with
  | _, Some { Cmt_format.cmt_annots = Implementation structure; _ } ->
      let values = ref [] in
      let default = Tast_iterator.default_iterator in
      let iterator =
        {
          default with
          value_binding =
            (fun self binding ->
              (match binding.Typedtree.vb_pat.pat_desc with
              | Typedtree.Tpat_var (_, name, _, _, _) ->
                  values := (name.txt, binding.vb_attributes) :: !values
              | _ -> ());
              default.value_binding self binding);
        }
      in
      iterator.structure iterator structure;
      Ok (List.rev !values)
  | _, Some
      {
        Cmt_format.cmt_annots =
          ( Interface _ | Partial_implementation _ | Partial_interface _
          | Packed _ );
        _;
      }
  | _, None ->
      mismatch "ordinary CMT does not contain a complete implementation"

let load_implementation root unit_name =
  let* cmt = artifact root unit_name ".cmt" in
  let* cmi = artifact root unit_name ".cmi" in
  let artifact_directories =
    files_below (Filename.concat root "_build/default")
    |> List.filter (fun filename ->
           List.mem (Filename.extension filename) [ ".cmi"; ".cmti"; ".vri" ])
    |> List.map Filename.dirname |> List.sort_uniq String.compare
  in
  match
    Cmt_input.load_with_interface ~cmt ~cmi ~artifact_directories ()
  with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      mismatch "load %s failed: %s: %s" unit_name diagnostic.code
        diagnostic.message

let lower implementation =
  match Typedtree_lowering.lower implementation with
  | Ok program -> Ok program
  | Error diagnostic ->
      mismatch "lower failed: %s: %s" diagnostic.code diagnostic.message

let verify implementation =
  let* configuration =
    match
      Verifier_service.configuration ~threads:1 ~timeout_ms:60_000 ~rlimit:None
    with
    | Ok configuration -> Ok configuration
    | Error error ->
        mismatch "%s" (Verifier_service.configuration_error_message error)
  in
  match
    Verifier_service.verify
      (Verifier_service.request ~configuration ~consumer:implementation
         ~dependencies:[])
  with
  | Ok result -> Ok result
  | Error error -> mismatch "%s" (Verifier_service.error_message error)

let imported_authority ?(external_targets = []) ~consumer ~dependencies () =
  let* policy =
    match Solver_policy_private.create_default ~timeout_ms:60_000 with
    | Ok policy -> Ok policy
    | Error error ->
        mismatch "%s" (Solver_policy_private.error_to_string error)
  in
  let* environment, _ =
    match
      Interface_specification_loaded_private.authenticate ~solver_policy:policy
        ~external_targets ~dependencies ~consumer
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

let expression_children expression =
  match expression.Sst.expression_desc with
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      quantifier.quantifier_body :: Option.to_list quantifier.quantifier_trigger
  | _ -> Sst_callback_private.expression_children expression

let logical_references expression =
  let rec collect found expression =
    let found =
      match expression.Sst.expression_desc with
      | Sst.Logical_constant_reference { constant; type_arguments } ->
          (constant, type_arguments) :: found
      | _ -> found
    in
    List.fold_left collect found (expression_children expression)
  in
  List.rev (collect [] expression)

let contract_expressions (contracts : Sst.contracts) =
  List.map
    (fun (clause : Sst.predicate_clause) -> clause.Sst.predicate.expression)
    contracts.requires
  @ List.map
      (fun (clause : Sst.ensures_clause) -> clause.Sst.predicate.expression)
      contracts.ensures
  @ List.map
      (fun (clause : Sst.predicate_clause) -> clause.Sst.predicate.expression)
      contracts.decreases
  @ List.map
      (fun (clause : Sst.predicate_clause) -> clause.Sst.predicate.expression)
      contracts.assertions

let file path contents = { Fixture.path; contents }

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let retained_input ~module_name ~source =
  Fixture.single_source ~module_name ~source
    ~libraries:[ "verocaml.vstd"; "verocaml.ghost" ]

let verified_case ~name ~module_name ~source =
  let input = retained_input ~module_name ~source in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit module_name Outcome.Unit_verified)
    (run_fixture input)

let rejected_case ?code ~name ~module_name ~source () =
  let input = retained_input ~module_name ~source in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected
  in
  let expectation =
    Option.fold ~none:expectation
      ~some:(fun code -> Expectation.require_frontend_code code expectation)
      code
  in
  Suite.case ~name ~expectation (run_fixture input)

let counterexample_case ~name ~module_name ~source =
  let input = retained_input ~module_name ~source in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit module_name Outcome.Unit_counterexample)
    (run_fixture input)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let installed_ghost_directory environment =
  Filename.concat (Project_environment.package_root environment)
    "verocaml/ghost"

let direct_source_route ~environment ~workspace =
  let workspace = Unix.realpath workspace in
  let source_name = "logical_constant_source.ml" in
  let source = Filename.concat workspace source_name in
  let temporary_directory = Filename.concat workspace "source-temp" in
  Unix.mkdir temporary_directory 0o755;
  write_file source
    {|let truth : bool = true [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> truth];
  ()
[@@verocaml.proof]
|};
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments = [ "verify"; source_name ];
      forwarded =
        [
          ("PATH", Project_environment.tool_path environment);
          ("TMPDIR", temporary_directory);
          ("OCAML_COLOR", "never");
          ("VEROCAML_PPX", installed_binary environment "verocaml-ppx");
          ("VEROCAML_GHOST_DIR", installed_ghost_directory environment);
        ];
      cleanup_paths =
        [
          "logical_constant_source.cmi";
          "logical_constant_source.cmo";
          "logical_constant_source.cmt";
        ];
      adjacency = [];
    }

let direct_source_route_case =
  Suite.case ~name:"direct-source-logical-constant"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 0))
      |> Expectation.require_process_fact
           (Outcome.Cleaned "logical_constant_source.cmi")
      |> Expectation.require_process_fact
           (Outcome.Cleaned "logical_constant_source.cmo")
      |> Expectation.require_process_fact
           (Outcome.Cleaned "logical_constant_source.cmt"))
    direct_source_route

let source_compile_rejection_case ~name ~source ~message =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 2))
      |> Expectation.require_process_fact
           (Outcome.Stable_code "VERO_SOURCE_COMPILE")
      |> Expectation.require_process_fact
           (Outcome.Adjacent "specific-source-diagnostic"))
    (fun ~environment ~workspace ->
      let workspace = Unix.realpath workspace in
      let source_name = "rejected_logical_constant.ml" in
      let temporary_directory = Filename.concat workspace "source-temp" in
      Unix.mkdir temporary_directory 0o755;
      write_file (Filename.concat workspace source_name) source;
      Process_adapter.run ~cwd:workspace
        {
          program = installed_binary environment "verocaml";
          arguments = [ "verify"; source_name ];
          forwarded =
            [
              ("PATH", Project_environment.tool_path environment);
              ("TMPDIR", temporary_directory);
              ("OCAML_COLOR", "never");
              ("VEROCAML_PPX", installed_binary environment "verocaml-ppx");
              ("VEROCAML_GHOST_DIR", installed_ghost_directory environment);
            ];
          cleanup_paths = [];
          adjacency =
            [
              ( "specific-source-diagnostic",
                "VERO_SOURCE_COMPILE",
                message );
            ];
        })

let source_grammar_rejection_cases =
  [
    source_compile_rejection_case ~name:"recursive-logical-constant-is-rejected"
      ~source:"let rec truth : bool = true [@@verocaml.spec]\n"
      ~message:"logical constants cannot be recursive";
    source_compile_rejection_case ~name:"local-logical-constant-is-rejected"
      ~source:
        {|let outer () =
  let local : bool = true [@@verocaml.spec] in
  local
|}
      ~message:"local declarations are not supported";
    source_compile_rejection_case
      ~name:"patterned-logical-constant-is-rejected"
      ~source:
        "let (left, right) : bool * bool = (true, false) [@@verocaml.spec]\n"
      ~message:"requires a simple variable binding";
    source_compile_rejection_case
      ~name:"multi-binding-logical-constant-group-is-rejected"
      ~source:
        {|let first : bool = true
and second : bool = false [@@verocaml.spec]
|}
      ~message:"requires a single top-level binding";
    source_compile_rejection_case
      ~name:"logical-constant-requires-explicit-type"
      ~source:"let truth = true [@@verocaml.spec]\n"
      ~message:"a logical constant must use";
    source_compile_rejection_case
      ~name:"logical-constant-forward-reference-remains-compiler-error"
      ~source:
        {|let before : bool = after [@@verocaml.spec]
let after : bool = true [@@verocaml.spec]
|}
      ~message:"Unbound value";
  ]

let artifact_source =
  {|type signal = On
type 'a box = Empty

[%%verocaml.symbolic val unknown : bool]

let thunk () = true [@@verocaml.spec]
let truth : bool = true [@@verocaml.spec]
let absent : 'a box = Empty [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ ->
    truth
    && thunk ()
    && (unknown || not unknown)
    && (absent : bool box) = (Empty : bool box)
    && (absent : bool box) = (Empty : bool box)
    && (absent : signal box) = (Empty : signal box)];
  ()
[@@verocaml.proof]
|}

let artifact_project =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name logical_constant_artifacts)\n";
          file "dune"
            {|(library
 (name retained_artifact)
 (wrapped false)
 (modules Retained_artifact)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name ordinary_artifact)
 (wrapped false)
 (modules Ordinary_artifact)
 (flags (:standard -ppx "verocaml-ppx")))

(executable
 (name runtime_probe)
 (modules Runtime_probe)
 (libraries ordinary_artifact))
|};
          file "retained_artifact.ml" artifact_source;
          file "ordinary_artifact.ml"
            {|let erased : bool = true [@@verocaml.spec]
let runtime () = 7
|};
          file "runtime_probe.ml"
            "let () = ignore (Ordinary_artifact.runtime ())\n";
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Retained_artifact" ];
    }

let direct_calls expression =
  let rec collect found expression =
    let found =
      match expression.Sst.expression_desc with
      | Sst.Direct_call { callee; _ } -> callee :: found
      | _ -> found
    in
    List.fold_left collect found (expression_children expression)
  in
  List.rev (collect [] expression)

let semantic_artifact_case =
  Suite.case ~name:"structured-category-instance-and-erasure-evidence"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Retained_artifact" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      let* outcome = Fixture.run ~environment ~workspace artifact_project in
      let root = Filename.concat workspace "project" in
      let* implementation = load_implementation root "Retained_artifact" in
      let* program = lower implementation in
      let graph_matrix =
        Sst_validation_private.Public.For_testing
        .logical_definition_graph_matrix ()
      in
      let* () =
        require
          (graph_matrix.mixed_cycle_rejected
          && graph_matrix.constant_cycle_rejected
          && graph_matrix.constant_self_cycle_rejected
          && graph_matrix.unresolved_edge_rejected
          && graph_matrix.recursive_self_edge_omission_accepted)
          "shared logical definition graph did not enforce cycle and closure boundaries"
      in
      let constant_names =
        program.Sst.logical_constants
        |> List.map (fun definition -> definition.Sst.constant_id.constant_name)
        |> List.sort String.compare
      in
      let* () =
        require
          (constant_names = [ "absent"; "truth" ])
          "retained SST did not contain exactly the defined logical constants"
      in
      let* truth_definition =
        match
          List.find_opt
            (fun definition ->
              String.equal definition.Sst.constant_id.constant_name "truth")
            program.logical_constants
        with
        | Some definition -> Ok definition
        | None -> mismatch "retained SST lacks truth definition"
      in
      let truth_origin = truth_definition.constant_id.constant_origin in
      let truth_id = truth_definition.constant_id in
      let* truth_equation =
        match truth_definition.constant_equation with
        | Some equation -> Ok equation
        | None -> mismatch "local truth constant unexpectedly lacks an equation"
      in
      let truth_expression = truth_equation.constant_body.expression in
      let forged_definitions =
        [
          {
            truth_definition with
            Sst.constant_declared_type = Sst.Int;
          };
          {
            truth_definition with
            Sst.constant_equation =
              Some
                {
                  truth_equation with
                  constant_body =
                    {
                      truth_equation.constant_body with
                      expression =
                        {
                          truth_expression with
                          Sst.expression_desc = Sst.Bool_constant false;
                        };
                    };
                };
          };
          {
            truth_definition with
            Sst.constant_equation =
              Some
                {
                  truth_equation with
                  constant_trust_dependencies = [ "forged-trust-authority" ];
                };
          };
          {
            truth_definition with
            Sst.constant_id =
              {
                truth_id with
                constant_origin =
                  { truth_origin with value_uid = truth_origin.value_uid ^ ".stale" };
              };
          };
          {
            truth_definition with
            Sst.constant_id =
              {
                truth_id with
                constant_origin =
                  {
                    truth_origin with
                    provider_interface =
                      truth_origin.provider_interface ^ ".stale";
                  };
              };
          };
          {
            truth_definition with
            Sst.constant_equation =
              Some
                {
                  truth_equation with
                  constant_source_body_digest =
                    truth_equation.constant_source_body_digest ^ ".stale";
                };
          };
          {
            truth_definition with
            Sst.constant_equation =
              Some
                {
                  truth_equation with
                  constant_dependency_receipt =
                    truth_equation.constant_dependency_receipt ^ ".stale";
                };
          };
        ]
      in
      let* () =
        let forged_unit_instance =
          Logical_constant_instance_private.create
            ~definition:
              { truth_definition with Sst.constant_declared_type = Sst.Unit }
            ~type_arguments:[] ~result_type:Sst.Unit
            ~span:truth_definition.constant_span
        in
        require
          (not
             (Logical_constant_private.For_testing.definition_rejected
                ~program truth_definition)
          && List.for_all
               (Logical_constant_private.For_testing.definition_rejected
                  ~program)
               forged_definitions
          && Result.is_error forged_unit_instance)
          "type, body, source receipt, UID, compilation identity, dependency receipt, or unsupported instance mutation authenticated"
      in
      Solver_backend.For_testing.reset_solver_creation_count ();
      let copied_program =
        {
          program with
          Sst.logical_constants =
            List.hd forged_definitions
            :: List.filter
                 (fun definition -> definition != truth_definition)
                 program.logical_constants;
        }
      in
      let production_rejected =
        Result.is_error (Sst_validation.validate copied_program)
      in
      let* () =
        require
          (production_rejected
          && Solver_backend.For_testing.solver_creation_count () = 0)
          "copied logical authority crossed the production validation boundary"
      in
      let function_named name =
        List.find_opt
          (fun definition ->
            String.equal definition.Sst.function_id.function_name name)
          program.functions
      in
      let* () =
        require
          (Option.is_none (function_named "truth")
          && Option.is_none (function_named "absent"))
          "logical constant was also lowered as a callable"
      in
      let* () =
        require
          (match function_named "thunk" with
          | Some { Sst.body = Sst.Spec_definition _; _ } -> true
          | _ -> false)
          "zero-argument specification function lost its callable category"
      in
      let* () =
        require
          (match function_named "unknown" with
          | Some { Sst.body = Sst.Symbolic_declaration _; _ } -> true
          | _ -> false)
          "bodyless symbolic value lost its equationless declaration category"
      in
      let* lemma =
        match function_named "lemma" with
        | Some definition -> Ok definition
        | None -> mismatch "retained SST lacks lemma"
      in
      let contract_expressions = contract_expressions lemma.contracts in
      let references = List.concat_map logical_references contract_expressions in
      let reference_names =
        List.map (fun (constant, _) -> constant.Sst.constant_name) references
      in
      let calls = List.concat_map direct_calls contract_expressions in
      let* () =
        require
          (List.mem "truth" reference_names
          && List.length
               (List.filter (String.equal "absent") reference_names)
             = 3
          && not
               (List.exists
                  (fun callee -> List.mem callee.Sst.function_name [ "truth"; "absent" ])
                  calls))
          "retained uses did not remain logical references distinct from calls"
      in
      let* result = verify implementation in
      let prepared =
        Outcome.of_verifier_result result
        |> Outcome.with_unit "Retained_artifact" Outcome.Unit_verified
      in
      let* () =
        match Outcome.semantic_parity ~except:[] outcome prepared with
        | Ok () -> Ok ()
        | Error message -> mismatch "source/retained-CMT parity: %s" message
      in
      let obligations =
        (Verifier_service.vir result).Vir.functions
        |> List.filter (fun execution ->
               String.equal execution.Vir.function_ref.function_name "lemma")
        |> List.concat_map (fun execution -> execution.Vir.obligations)
        |> List.filter (fun obligation ->
               match obligation.Vir.kind with
               | Vir.Postcondition _ -> true
               | Vir.Arithmetic_safety _ | Vir.Assertion _
               | Vir.Local_assertion _ | Vir.Call_precondition _
               | Vir.Callback_precondition _ | Vir.Invariant_validity _
               | Vir.Entry_measure_nonnegative _
               | Vir.Recursive_call_measure_nonnegative _
               | Vir.Recursive_call_strict_descent _ ->
                   false)
      in
      let raw_equations =
        obligations
        |> List.concat_map (fun obligation -> obligation.Vir.logical_constant_equations)
      in
      let unique_equations equations =
        List.sort_uniq
          (fun left right ->
            Logical_constant_instance_private.compare
              left.Vir.logical_constant_instance
              right.Vir.logical_constant_instance)
          equations
      in
      let equations =
        unique_equations raw_equations
      in
      let named_equations name =
        List.filter
          (fun equation ->
            String.equal
              (Logical_constant_instance_private.constant_id
                 equation.Vir.logical_constant_instance)
                .Sst.constant_name
              name)
          equations
      in
      let absent_equations = named_equations "absent" in
      let* () =
        require
          (List.for_all
             (fun obligation ->
               let raw = obligation.Vir.logical_constant_equations in
               List.length raw = List.length (unique_equations raw))
             obligations
          && List.exists
               (fun obligation ->
                 List.length obligation.Vir.logical_constant_equations = 3)
               obligations
          && List.length (named_equations "truth") = 1
          && List.length absent_equations = 2
          && List.length equations = 3)
          "VIR did not contain exactly one equation per used logical constant instance"
      in
      let* () =
        require
          (match absent_equations with
          | [ left; right ] ->
              not
                (Logical_constant_instance_private.equal
                   left.logical_constant_instance right.logical_constant_instance)
              && not
                   (String.equal
                      (Logical_constant_instance_private.identity_digest
                         left.logical_constant_instance)
                      (Logical_constant_instance_private.identity_digest
                         right.logical_constant_instance))
              && not
                   (Parametric_type.equal
                      (Logical_constant_instance_private.result_type
                         left.logical_constant_instance)
                      (Logical_constant_instance_private.result_type
                         right.logical_constant_instance))
          | _ -> false)
          "two concrete polymorphic instances were not sort-distinct"
      in
      let* ordinary_cmi = artifact root "Ordinary_artifact" ".cmi" in
      let* ordinary_cmt = artifact root "Ordinary_artifact" ".cmt" in
      let exported = signature_values ordinary_cmi in
      let* typed_values = implementation_values ordinary_cmt in
      let marker = "verocaml.internal.logical_constant.definition.v1" in
      let* () =
        require
          (List.mem "runtime" exported && not (List.mem "erased" exported))
          "ordinary CMI retained the logical constant or erased runtime code"
      in
      let* () =
        require
          (List.mem_assoc "runtime" typed_values
          && not (List.mem_assoc "erased" typed_values)
          && not
               (List.exists
                  (fun (_, attributes) ->
                    List.exists
                      (fun attribute ->
                        String.equal attribute.Parsetree.attr_name.txt marker)
                      attributes)
                  typed_values))
          "ordinary typed artifact retained logical constant authority or initializer"
      in
      let stale_source =
        String.concat ""
          [
            "type signal = On\ntype 'a box = Empty\n\n";
            "[%%verocaml.symbolic val unknown : bool]\n\n";
            "let thunk () = true [@@verocaml.spec]\n";
            "let truth : bool = false [@@verocaml.spec]\n";
            "let absent : 'a box = Empty [@@verocaml.spec]\n";
          ]
      in
      let source_candidates =
        [
          Filename.concat root "retained_artifact.ml";
          Filename.concat implementation.Cmt_input.build_directory
            implementation.source_file;
          Filename.concat
            (Filename.dirname implementation.filename)
            (Filename.basename implementation.source_file);
        ]
        |> List.sort_uniq String.compare
      in
      let project_source = Filename.concat root "retained_artifact.ml" in
      write_file project_source stale_source;
      List.iter
        (fun path ->
          if
            not (String.equal path project_source) && Sys.file_exists path
          then Sys.remove path)
        source_candidates;
      let require_source_independent_lowering label =
        let lower_result = Typedtree_lowering.lower implementation in
        match lower_result with
        | Error diagnostic ->
            mismatch "%s source replacement rejected authenticated CMT: %s"
              label diagnostic.Diagnostic.code
        | Ok lowered_program ->
            let definitions = lowered_program.Sst.logical_constants in
            let* () =
              require
                (definitions = program.Sst.logical_constants
                && List.for_all
                     (fun definition ->
                       not
                         (Logical_constant_private.For_testing.definition_rejected
                            ~program:lowered_program definition))
                     definitions)
                (Printf.sprintf
                   "%s source replacement changed authenticated logical definitions"
                   label)
            in
            require
              (Solver_backend.For_testing.solver_creation_count () = 0)
              (Printf.sprintf "%s source replacement reached solver creation" label)
      in
      Solver_backend.For_testing.reset_solver_creation_count ();
      let* () =
        require_source_independent_lowering "stale-adjacent-source"
      in
      if Sys.file_exists project_source then Sys.remove project_source;
      Solver_backend.For_testing.reset_solver_creation_count ();
      let* () =
        require_source_independent_lowering "removed-adjacent-source"
      in
      Ok outcome)

let explicit_interface_rejection_case ~name ~interface ~implementation =
  let input =
    Fixture.dune_project
      {
        files =
          [
            file "dune-project"
              "(lang dune 3.17)\n(name logical_constant_interface)\n";
            file "dune"
              {|(library
 (name logical_constant_interface)
 (wrapped false)
 (modules Logical_constant_interface)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
            file "logical_constant_interface.mli" interface;
            file "logical_constant_interface.ml" implementation;
          ];
        libraries = [ "verocaml.ghost" ];
        targets = [ "@all" ];
        selected_units = [ "Logical_constant_interface" ];
      }
  in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code
           "VERO_LOGICAL_CONSTANT_DECLARATION"
      |> Expectation.require_unit "Logical_constant_interface"
           Outcome.Unit_frontend_rejected)
    (run_fixture input)

let direct_interface_rejection_case =
  explicit_interface_rejection_case
    ~name:"root-interface-transport-is-not-forged"
    ~interface:"val truth : bool\n"
    ~implementation:"let truth : bool = true [@@verocaml.spec]\n"

let aliased_interface_rejection_case =
  explicit_interface_rejection_case
    ~name:"aliased-root-interface-transport-is-not-forged"
    ~interface:"module Alias : sig val truth : bool end\n"
    ~implementation:
      {|module Constants = struct
  let truth : bool = true [@@verocaml.spec]
end

module Alias = Constants
|}

let retained_logical_value_rules =
  {|(rule
 (targets provider.vri provider.verocaml-retained-interface provider.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .logical_constant_transport.objs/byte/provider.cmt)
  (:cmi .logical_constant_transport.objs/byte/provider.cmi)
  (:cmti .logical_constant_transport.objs/byte/provider.cmti))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} provider.vri
    --artifact-directory .logical_constant_transport.objs/byte)
   (run %{emitter} manifest provider.verocaml-retained-interface .logical_constant_transport.objs/byte/provider.cmt .logical_constant_transport.objs/byte/provider.cmi .logical_constant_transport.objs/byte/provider.cmti provider.vri)
   (run %{emitter} manifest provider.verocaml-retained-interface.install provider.cmt provider.cmi provider.cmti provider.vri))))

(alias
 (name all)
 (deps provider.vri provider.verocaml-retained-interface))

(install
 (package logical_constant_transport)
 (section lib)
 (files
  (provider.vri as ./provider.vri)
  (provider.verocaml-retained-interface.install as ./provider.verocaml-retained-interface)))
|}

let retained_transport_input ~provider_interface ~provider_implementation
    ~consumer_implementation =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name logical_constant_transport)\n(package (name logical_constant_transport))\n";
          file "dune"
            ("(library\n (name logical_constant_transport)\n (wrapped false)\n (modules Provider)\n (libraries verocaml.vstd verocaml.ghost)\n (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n\n"
            ^ retained_logical_value_rules
            ^ "\n(library\n (name logical_constant_consumer)\n (wrapped false)\n (modules Consumer)\n (libraries verocaml.vstd verocaml.ghost logical_constant_transport)\n (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n");
          file "provider.mli" provider_interface;
          file "provider.ml" provider_implementation;
          file "consumer.ml" consumer_implementation;
        ];
      libraries = [ "verocaml.vstd"; "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider"; "Consumer" ];
    }

let imported_defined_constant ~route imported =
  Imported_callable.logical_constants imported
  |> List.filter_map (function
       | Imported_callable.Imported_defined_logical_value constant
         when List.exists
                (fun (candidate : Imported_callable.logical_constant_route) ->
                  String.equal candidate.logical_constant_path route)
                constant.routes ->
           Some
             ( constant.semantic_authority,
               constant.dependency_closure )
       | Imported_callable.Imported_defined_logical_value _
       | Imported_callable.Imported_symbolic_logical_value _ ->
           None)
  |> function
  | [ constant ] -> Ok constant
  | [] -> mismatch "missing imported defined logical constant %s" route
  | _ -> mismatch "ambiguous imported defined logical constant %s" route

let first_class_specification_input ~helper_body =
  retained_transport_input
    ~provider_interface:
      {|val helper : bool -> bool [@@verocaml.spec]
val selected : bool [@@verocaml.spec] [@@verocaml.revealed]
|}
    ~provider_implementation:
      ("[@@@verocaml.verify]\n\nlet helper value = " ^ helper_body
      ^ " [@@verocaml.spec]\n\nlet selected : bool =\n  let f = helper in\n  f false\n[@@verocaml.spec]\n")
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let observed : bool = Provider.selected [@@verocaml.spec]

let stable () =
  [%verocaml.ensures fun _ -> observed = Provider.selected];
  ()
[@@verocaml.proof]
|}

let first_class_specification_shape expression =
  let rec collect reference_count application_count expression =
    let reference_count, application_count =
      match expression.Sst.expression_desc with
      | Sst.Direct_call { call_form = Sst.Specification_call; _ }
        when Spec_function_sst_private.is_reference expression ->
          reference_count + 1, application_count
      | Sst.Direct_call { call_form = Sst.Specification_call; _ }
        when Spec_function_sst_private.application expression <> None ->
          reference_count, application_count + 1
      | _ -> reference_count, application_count
    in
    List.fold_left
      (fun (reference_count, application_count) child ->
        collect reference_count application_count child)
      (reference_count, application_count) (expression_children expression)
  in
  collect 0 0 expression

let first_class_specification_constant_case =
  let identity_input =
    first_class_specification_input ~helper_body:"value || false"
  in
  let negation_input =
    first_class_specification_input ~helper_body:"not value"
  in
  Suite.case ~name:"retained-revealed-constant-first-class-helper-closure"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:stable"
           (Outcome.Function_exists "stable"))
    (fun ~environment ~workspace ->
      let identity_workspace = Filename.concat workspace "identity" in
      let negation_workspace = Filename.concat workspace "negation" in
      let* identity_outcome =
        run_fixture identity_input ~environment ~workspace:identity_workspace
      in
      let* negation_outcome =
        run_fixture negation_input ~environment ~workspace:negation_workspace
      in
      let* () =
        require
          (Outcome.status identity_outcome = Outcome.Verified
          && Outcome.status negation_outcome = Outcome.Verified)
          (Printf.sprintf "first-class helper fixtures did not verify: %s/%s [%s] [%s]"
             (Outcome.status_name (Outcome.status identity_outcome))
             (Outcome.status_name (Outcome.status negation_outcome))
             (String.concat ","
                (List.map
                   (fun (unit_name, disposition) ->
                     unit_name ^ "="
                     ^ Outcome.unit_disposition_name disposition)
                   (Outcome.units identity_outcome)))
             (String.concat ","
                (List.map
                   (fun (unit_name, disposition) ->
                     unit_name ^ "="
                     ^ Outcome.unit_disposition_name disposition)
                   (Outcome.units negation_outcome))))
      in
      let imported_selected workspace =
        let root = Filename.concat workspace "project" in
        let* provider = load_implementation root "Provider" in
        let* consumer = load_implementation root "Consumer" in
        let* provider_program = lower provider in
        let* imported =
          imported_authority ~consumer ~dependencies:[ provider ] ()
        in
        let* semantic_authority, dependency_closure =
          imported_defined_constant ~route:"Provider.selected" imported
        in
        let* provider_definition =
          match
            List.find_opt
              (fun definition ->
                String.equal definition.Sst.function_id.function_name "helper")
              provider_program.Sst.functions
          with
          | Some definition -> Ok definition
          | None -> mismatch "provider lost first-class helper specification"
        in
        let* selected_definition =
          match
            List.find_opt
              (fun definition ->
                String.equal
                  definition.Sst.constant_id.constant_name "selected")
              provider_program.Sst.logical_constants
          with
          | Some definition -> Ok definition
          | None -> mismatch "provider lost selected logical constant"
        in
        let* equation =
          match selected_definition.Sst.constant_equation with
          | Some equation -> Ok equation
          | None -> mismatch "selected logical constant lost its equation"
        in
        let reference_count, application_count =
          first_class_specification_shape equation.Sst.constant_body.expression
        in
        Ok
          ( semantic_authority.Sst.constant_id.Sst.constant_origin,
            dependency_closure,
            provider_definition,
            reference_count,
            application_count )
      in
      let* (identity_origin, identity_receipt, identity_helper, identity_references,
            identity_applications) = imported_selected identity_workspace
      in
      let* (negation_origin, negation_receipt, negation_helper, negation_references,
            negation_applications) = imported_selected negation_workspace
      in
      let* () =
        require
          (identity_references > 0 && identity_applications > 0
          && negation_references > 0 && negation_applications > 0)
          "revealed constant equation did not retain first-class helper reference/application"
      in
      let* () =
        require
          (String.equal identity_origin.Sst.semantic_class
             negation_origin.Sst.semantic_class
          && not (String.equal identity_receipt negation_receipt)
          && identity_helper.Sst.body <> negation_helper.Sst.body)
          "helper semantics did not change the authenticated constant closure receipt"
      in
      Ok identity_outcome)

let retained_logical_value_rules_for ~library_name ~module_name =
  let module_file = String.uncapitalize_ascii module_name in
  Printf.sprintf
    {|(rule
 (targets %s.vri %s.verocaml-retained-interface %s.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %%{bin:verocaml-retained-interface})
  (:cmt .%s.objs/byte/%s.cmt)
  (:cmi .%s.objs/byte/%s.cmi)
  (:cmti .%s.objs/byte/%s.cmti))
 (action
  (progn
   (run %%{emitter} emit %%{cmt} %%{cmi} %%{cmti} %s.vri
    --artifact-directory .%s.objs/byte)
   (run %%{emitter} manifest %s.verocaml-retained-interface .%s.objs/byte/%s.cmt .%s.objs/byte/%s.cmi .%s.objs/byte/%s.cmti %s.vri)
   (run %%{emitter} manifest %s.verocaml-retained-interface.install %s.cmt %s.cmi %s.cmti %s.vri))))

(alias
 (name all)
 (deps %s.vri %s.verocaml-retained-interface))

(install
 (package logical_constant_collision)
 (section lib)
 (files
  (%s.vri as ./%s.vri)
  (%s.verocaml-retained-interface.install as ./%s.verocaml-retained-interface)))
|}
    module_file module_file module_file library_name module_file library_name
    module_file library_name module_file module_file library_name module_file
    module_file module_file module_file module_file module_file module_file
    module_file module_file module_file module_file module_file module_file
    module_file module_file module_file module_file module_file module_file

let retained_collision_input =
  let left_rules =
    retained_logical_value_rules_for ~library_name:"left_transport"
      ~module_name:"Left"
  in
  let right_rules =
    retained_logical_value_rules_for ~library_name:"right_transport"
      ~module_name:"Right"
  in
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name logical_constant_collision)\n(package (name logical_constant_collision))\n";
          file "dune"
            (("(library\n (name left_transport)\n (wrapped false)\n (modules Left)\n (libraries verocaml.vstd verocaml.ghost)\n (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n\n"
              ^ left_rules
              ^ "\n(library\n (name right_transport)\n (wrapped false)\n (modules Right)\n (libraries verocaml.vstd verocaml.ghost)\n (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n\n"
              ^ right_rules)
            ^ "\n(library\n (name collision_consumer)\n (wrapped false)\n (modules Consumer)\n (libraries left_transport right_transport verocaml.vstd verocaml.ghost)\n (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n");
          file "left.mli"
            "val shared : bool [@@verocaml.spec] [@@verocaml.revealed]\n";
          file "left.ml"
            "[@@@verocaml.verify]\n\nlet shared : bool = true [@@verocaml.spec]\n";
          file "right.mli"
            "val shared : bool [@@verocaml.spec] [@@verocaml.revealed]\n";
          file "right.ml"
            "[@@@verocaml.verify]\n\nlet shared : bool = false [@@verocaml.spec]\n";
          file "consumer.ml"
            {|[@@@verocaml.verify]

let left_observed : bool = Left.shared [@@verocaml.spec]
let right_observed : bool = Right.shared [@@verocaml.spec]

let stable () =
  [%verocaml.ensures fun _ -> left_observed = Left.shared];
  [%verocaml.ensures fun _ -> right_observed = Right.shared];
  ()
[@@verocaml.proof]
|};
        ];
      libraries = [ "verocaml.vstd"; "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Left"; "Right"; "Consumer" ];
    }

let retained_same_leaf_provider_origins_case =
  Suite.case ~name:"retained-same-leaf-constants-preserve-provider-origins"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Left" Outcome.Unit_verified
      |> Expectation.require_unit "Right" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:stable"
           (Outcome.Function_exists "stable"))
    (fun ~environment ~workspace ->
      let* outcome = run_fixture retained_collision_input ~environment ~workspace in
      let root = Filename.concat workspace "project" in
      let* left = load_implementation root "Left" in
      let* right = load_implementation root "Right" in
      let* consumer = load_implementation root "Consumer" in
      let imported_constants dependencies =
        let* imported = imported_authority ~consumer ~dependencies () in
        let* left_authority, left_receipt =
          imported_defined_constant ~route:"Left.shared" imported
        in
        let* right_authority, right_receipt =
          imported_defined_constant ~route:"Right.shared" imported
        in
        Ok
          ( ( left_authority.Sst.constant_id.Sst.constant_origin.origin_digest,
              left_receipt ),
            ( right_authority.Sst.constant_id.Sst.constant_origin.origin_digest,
              right_receipt ) )
      in
      let* left_origin, right_origin = imported_constants [ left; right ] in
      let* permuted_left_origin, permuted_right_origin =
        imported_constants [ right; left ]
      in
      let* () =
        require
          (not (String.equal (fst left_origin) (fst right_origin))
          && left_origin = permuted_left_origin
          && right_origin = permuted_right_origin)
          "same-leaf logical constants collided or changed with provider order"
      in
      Ok outcome)

let ecosystem_existing_file path =
  Sys.file_exists path && not (Sys.is_directory path)

let rec ecosystem_mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else (
    ecosystem_mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755)

let ecosystem_process_environment forwarded =
  let names = List.map fst forwarded in
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun binding ->
           match String.index_opt binding '=' with
           | None -> true
           | Some index ->
               not (List.mem (String.sub binding 0 index) names))
  in
  List.map (fun (name, value) -> name ^ "=" ^ value) forwarded @ inherited
  |> Array.of_list

type ecosystem_process = { status : Unix.process_status }

let ecosystem_run_to_file ~workspace ~stdout_path ~program ~arguments ~forwarded () =
  ecosystem_mkdir_p workspace;
  ecosystem_mkdir_p (Filename.dirname stdout_path);
  let stderr_path = Filename.concat workspace "captured.stderr" in
  let stdout_channel = open_out_bin stdout_path
  and stderr_channel = open_out_bin stderr_path in
  let status =
    Fun.protect
      ~finally:(fun () ->
        close_out_noerr stdout_channel;
        close_out_noerr stderr_channel)
      (fun () ->
        let pid =
          Unix.create_process_env program
            (Array.of_list (program :: arguments))
            (ecosystem_process_environment forwarded) Unix.stdin
            (Unix.descr_of_out_channel stdout_channel)
            (Unix.descr_of_out_channel stderr_channel)
        in
        snd (Unix.waitpid [] pid))
  in
  { status }

let ecosystem_process_exited code process =
  process.status = Unix.WEXITED code

let ecosystem_provider_mli =
  "val selected : bool [@@verocaml.spec] [@@verocaml.revealed]\n"

let ecosystem_provider_ml =
  "[@@@verocaml.verify]\n\nlet selected : bool = true [@@verocaml.spec]\n"

let ecosystem_consumer_ml =
  {|[@@@verocaml.verify]

let observed : bool = Provider.selected [@@verocaml.spec]

let consume () =
  [%verocaml.ensures fun _ -> observed = Provider.selected];
  ()
[@@verocaml.proof]
|}

let ecosystem_logical_constant_case =
  Suite.case
    ~name:"retained-logical-constant-real-package-cold-consumer"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let workspace =
        if Filename.is_relative workspace then Filename.concat (Sys.getcwd ()) workspace
        else workspace
      in
      let provider_root = Filename.concat workspace "logical-provider" in
      let library_directory = Filename.concat provider_root "lib" in
      let prefix = Filename.concat workspace "logical-prefix" in
      ecosystem_mkdir_p library_directory;
      write_file (Filename.concat provider_root "dune-project")
        "(lang dune 3.17)\n(name logical_bundle)\n(package (name logical_bundle))\n";
      write_file (Filename.concat library_directory "provider.ml")
        ecosystem_provider_ml;
      write_file (Filename.concat library_directory "provider.mli")
        ecosystem_provider_mli;
      let generated_fragment =
        Filename.concat library_directory "provider.verocaml.inc"
      in
      let generator =
        ecosystem_run_to_file
          ~workspace:(Filename.concat workspace "logical-generator")
          ~stdout_path:generated_fragment
          ~program:
            (Filename.concat
               (Project_environment.binary_root environment)
               "verocaml-retained-interface")
          ~arguments:
            [ "dune-stanza"; "logical_bundle"; "api"; "logical_api"; "Provider" ]
          ~forwarded:
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("OCAML_COLOR", "never");
            ]
          ()
      in
      let* () =
        require (ecosystem_process_exited 0 generator)
          "retained-interface Dune stanza generation failed"
      in
      write_file (Filename.concat library_directory "dune")
        {|(library
 (name logical_api)
 (public_name logical_bundle.api)
 (wrapped false)
 (modules Provider)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(include provider.verocaml.inc)
|};
      let provider_environment =
        [
          ( "PATH",
            Project_environment.tool_path environment
            ^ ":" ^ Option.value ~default:"" (Sys.getenv_opt "PATH") );
          ("OCAMLPATH", Project_environment.ocaml_path environment);
          ("DUNE_CACHE", "disabled");
          ("HOME", provider_root);
          ("TMPDIR", Filename.concat provider_root "temp");
          ("OCAML_COLOR", "never");
        ]
      in
      ecosystem_mkdir_p (Filename.concat provider_root "temp");
      let provider_build =
        Process_adapter.run ~cwd:provider_root
          {
            program = Project_environment.dune_path environment;
            arguments =
              [
                "build";
                "--root";
                provider_root;
                "--build-dir";
                Filename.concat provider_root "_build";
                "--profile";
                "release";
                "@install";
              ];
            forwarded = provider_environment;
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* () =
        match provider_build with
        | Ok result ->
            require
              (List.mem (Outcome.Exit_class (Outcome.Exited 0))
                 (Outcome.process_facts result))
              "real retained logical provider build failed"
        | Error failure -> mismatch "%s" (Failure.to_string failure)
      in
      let build_library = Filename.concat provider_root "_build/default/lib" in
      let build_family =
        [
          Filename.concat build_library "provider.verocaml-retained-interface";
          Filename.concat build_library "provider.vri";
        ]
      in
      let* () =
        require (List.for_all ecosystem_existing_file build_family)
          "real Dune build omitted retained logical-constant products"
      in
      let install =
        Process_adapter.run ~cwd:provider_root
          {
            program = Project_environment.dune_path environment;
            arguments =
              [
                "install";
                "--root";
                provider_root;
                "--build-dir";
                Filename.concat provider_root "_build";
                "--prefix";
                prefix;
                "logical_bundle";
              ];
            forwarded = provider_environment;
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* () =
        match install with
        | Ok result ->
            require
              (List.mem (Outcome.Exit_class (Outcome.Exited 0))
                 (Outcome.process_facts result))
              "real retained logical provider install failed"
        | Error failure -> mismatch "%s" (Failure.to_string failure)
      in
      let installed_library = Filename.concat prefix "lib/logical_bundle/api" in
      let installed_family =
        [
          "provider.verocaml-retained-interface";
          "provider.cmt";
          "provider.cmi";
          "provider.cmti";
          "provider.vri";
        ]
        |> List.map (Filename.concat installed_library)
      in
      let installed_sources =
        [ Filename.concat installed_library "provider.ml";
          Filename.concat installed_library "provider.mli" ]
      in
      List.iter
        (fun path -> if Sys.file_exists path then Sys.remove path)
        installed_sources;
      let* () =
        require
          (List.for_all ecosystem_existing_file installed_family
          && List.for_all (fun path -> not (Sys.file_exists path)) installed_sources
          && not
               (List.exists
                  (fun path ->
                    List.mem (Filename.extension path) [ ".ml"; ".mli" ])
                  (files_below installed_library)))
          "installed package omitted retained authority or exposed provider source"
      in
      let installed_receipts = List.map Digest.file installed_family in
      let removed_provider = Filename.concat workspace "provider-unavailable" in
      Unix.rename provider_root removed_provider;
      let* () =
        require
          (not (Sys.file_exists provider_root)
          && Sys.file_exists removed_provider
          && not (Sys.file_exists (Filename.concat provider_root "_build")))
          "cold-consumer setup retained the original provider or build tree"
      in
      let consumer_root = Filename.concat workspace "logical-cold-consumer" in
      ecosystem_mkdir_p consumer_root;
      write_file (Filename.concat consumer_root "dune-project")
        "(lang dune 3.17)\n(name logical_cold_consumer)\n";
      write_file (Filename.concat consumer_root "dune")
        {|(library
 (name logical_cold_consumer)
 (wrapped false)
 (modules Consumer)
 (libraries logical_bundle.api verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
      write_file (Filename.concat consumer_root "consumer.ml") ecosystem_consumer_ml;
      let consumer_temp = Filename.concat consumer_root "temp" in
      ecosystem_mkdir_p consumer_temp;
      let* () =
        require (not (Sys.file_exists (Filename.concat consumer_root "_build")))
          "cold consumer was built before installed-provider verification"
      in
      let cold =
        Process_adapter.run ~cwd:consumer_root
          {
            program = Filename.concat (Project_environment.binary_root environment) "verocaml";
            arguments =
              [
                "verify";
                consumer_root;
                "--threads";
                "1";
                "--timeout-ms";
                "60000";
              ];
            forwarded =
              [
                ("PATH", Project_environment.tool_path environment);
                ( "OCAMLPATH",
                  Filename.concat prefix "lib"
                  ^ ":" ^ Project_environment.ocaml_path environment );
                ("VEROCAML_DUNE", Project_environment.dune_path environment);
                ("DUNE_CACHE", "disabled");
                ("HOME", consumer_root);
                ("TMPDIR", consumer_temp);
                ("OCAML_COLOR", "never");
              ];
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* cold =
        match cold with
        | Ok result -> Ok result
        | Error failure -> mismatch "%s" (Failure.to_string failure)
      in
      let* () =
        require
          (List.mem (Outcome.Exit_class (Outcome.Exited 0))
             (Outcome.process_facts cold)
          && Sys.file_exists (Filename.concat consumer_root "_build")
          && installed_receipts = List.map Digest.file installed_family
          && not
               (List.exists
                  (fun path -> Filename.basename path = "provider.vri")
                  (files_below (Filename.concat consumer_root "_build"))))
          "cold consumer could not discover the installed logical provider"
      in
      let* consumer = load_implementation consumer_root "Consumer" in
      let* provider =
        match
          Cmt_input.load_with_interface
            ~cmt:(Filename.concat installed_library "provider.cmt")
            ~cmi:(Filename.concat installed_library "provider.cmi")
            ~cmti:(Filename.concat installed_library "provider.cmti")
            ~vri:(Filename.concat installed_library "provider.vri")
            ~artifact_directories:[ installed_library ] ()
        with
        | Ok provider -> Ok provider
        | Error diagnostic ->
            mismatch "installed retained provider load failed: %s" diagnostic.code
      in
      let* imported = imported_authority ~consumer ~dependencies:[ provider ] () in
      let* semantic_authority, dependency_closure =
        imported_defined_constant ~route:"Provider.selected" imported
      in
      let* () =
        require
          (semantic_authority.Sst.constant_provenance
             = Sst.Verified_definitional_equation
          && semantic_authority.Sst.constant_equation <> None
          && not (String.equal dependency_closure ""))
          "installed cold consumer lost structured logical authority"
      in
      Ok (Outcome.observation ~status:Outcome.Verified
          ~process_facts:(Outcome.process_facts cold) ()
          |> Outcome.project))

let copied_logical_handle_tamper_case =
  let input = first_class_specification_input ~helper_body:"value || false" in
  Suite.case ~name:"copied-logical-handle-closure-tamper-rejected"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* outcome = run_fixture input ~environment ~workspace in
      let root = Filename.concat workspace "project" in
      let* provider = load_implementation root "Provider" in
      let* consumer = load_implementation root "Consumer" in
      let* policy =
        match Solver_policy_private.create_default ~timeout_ms:60_000 with
        | Ok policy -> Ok policy
        | Error error -> mismatch "%s" (Solver_policy_private.error_to_string error)
      in
      let* authenticated, _ =
        match
          Interface_specification_loaded_private.authenticate
            ~external_targets:[] ~solver_policy:policy ~dependencies:[ provider ]
            ~consumer
        with
        | Ok authenticated -> Ok authenticated
        | Error error -> mismatch "%s" (Interface_specification_loaded_private.error_message error)
      in
      let* handle =
        match
          List.find_opt
            (fun (handle : Interface_specification_environment_private.handle) ->
              String.equal handle.unit_name "Provider")
            authenticated.handles
        with
        | Some handle -> Ok handle
        | None -> mismatch "authenticated environment omitted Provider handle"
      in
      let tampered_constants =
        List.map
          (function
            | Interface_specification_environment_private.Public_defined_logical_value
                logical_constant ->
                Interface_specification_environment_private.Public_defined_logical_value
                  { logical_constant with
                    logical_constant_dependency_closure =
                      logical_constant.logical_constant_dependency_closure
                      ^ ".tampered" }
            | constant -> constant)
          handle.logical_constants
      in
      let forged_handle = { handle with logical_constants = tampered_constants } in
      let forged_environment =
        { authenticated with
          handles =
            List.map
              (fun candidate ->
                if candidate == handle then forged_handle else candidate)
              authenticated.handles }
      in
      let tampered = tampered_constants <> handle.logical_constants in
      let original_authentic =
        Interface_specification_environment_private.handle_is_authentic handle
      in
      let copied_authentic =
        Interface_specification_environment_private.handle_is_authentic forged_handle
      in
      let copied_identity = handle == forged_handle in
      let* () =
        require
          (tampered && original_authentic && not copied_authentic)
          (Printf.sprintf
             "copied logical handle remained authentic after receipt tampering (tampered=%b original=%b copied=%b same_identity=%b constants=%d)"
             tampered original_authentic copied_authentic
             copied_identity (List.length handle.logical_constants))
      in
      Solver_backend.For_testing.reset_solver_creation_count ();
      Interface_specification_loaded_private.For_testing
      .reset_provider_verification_entries ();
      let rejected =
        try
          ignore
            (Interface_specification_environment_private
             .imported_environment_authenticated forged_environment);
          false
        with Invalid_argument _ -> true
      in
      let* () =
        require
          (rejected
          && Solver_backend.For_testing.solver_creation_count () = 0
          && Interface_specification_loaded_private.For_testing
             .provider_verification_entries () = 0)
          "tampered issued logical handle reached provider verification or solver work"
      in
      Ok outcome)

let retained_transport_case ~name ~provider_interface ~provider_implementation
    ~consumer_implementation =
  let input =
    retained_transport_input ~provider_interface ~provider_implementation
      ~consumer_implementation
  in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:stable"
           (Outcome.Function_exists "stable"))
    (run_fixture input)

let retained_revealed_equation_inactive_case =
  let input =
    retained_transport_input
      ~provider_interface:
        "val revealed : bool [@@verocaml.spec] [@@verocaml.revealed]\n"
      ~provider_implementation:
        {|[@@@verocaml.verify]

let revealed : bool = true [@@verocaml.spec]
|}
      ~consumer_implementation:
        {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ -> Provider.revealed];
  ()
[@@verocaml.proof]
|}
  in
  Suite.case ~name:"retained-revealed-equation-requires-activation"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_counterexample
      |> Expectation.require_semantic ~function_name:"stable"
           Outcome.Postcondition)
    (run_fixture input)

let retained_interface_transport_case =
  retained_transport_case ~name:"retained-defined-and-symbolic-value-transport"
    ~provider_interface:
      {|val opaque : bool [@@verocaml.spec]
val revealed : bool [@@verocaml.spec] [@@verocaml.revealed]
[%%verocaml.symbolic val selector : bool]
val callable : unit -> bool [@@verocaml.spec]
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

let opaque : bool = true [@@verocaml.spec]
let revealed : bool = true [@@verocaml.spec]
[%%verocaml.symbolic val selector : bool]
let callable () = true [@@verocaml.spec]
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ -> Provider.opaque = Provider.opaque];
  [%verocaml.ensures fun _ -> Provider.revealed = Provider.revealed];
  [%verocaml.ensures fun _ -> Provider.selector = Provider.selector];
  [%verocaml.ensures fun _ -> Provider.callable ()];
  ()
[@@verocaml.proof]
|}

let retained_polymorphic_values_case =
  retained_transport_case ~name:"retained-result-polymorphic-logical-values"
    ~provider_interface:
      {|type 'a box = Empty | Box of 'a

val empty : 'a box [@@verocaml.spec] [@@verocaml.revealed]
[%%verocaml.symbolic val choice : 'a box]
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

type 'a box = Empty | Box of 'a

let empty : 'a box = Empty [@@verocaml.spec]
[%%verocaml.symbolic val choice : 'a box]
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ ->
    (Provider.empty : int Provider.box) = Provider.empty];
  [%verocaml.ensures fun _ ->
    (Provider.empty : bool Provider.box) = Provider.empty];
  [%verocaml.ensures fun _ ->
    (Provider.choice : int Provider.box) = Provider.choice];
  [%verocaml.ensures fun _ ->
    (Provider.choice : bool Provider.box) = Provider.choice];
  ()
[@@verocaml.proof]
|}

let retained_nested_generic_value_case =
  retained_transport_case ~name:"retained-nested-generic-logical-value"
    ~provider_interface:
      {|type 'a box = Empty | Box of 'a
type 'a nested = Nest of 'a box

val nested_empty : 'a nested [@@verocaml.spec] [@@verocaml.revealed]
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

type 'a box = Empty | Box of 'a
type 'a nested = Nest of 'a box

let nested_empty : 'a nested = Nest Empty [@@verocaml.spec]
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ ->
    (Provider.nested_empty : int Provider.nested) =
    Provider.nested_empty];
  [%verocaml.ensures fun _ ->
    (Provider.nested_empty : bool Provider.nested) =
    Provider.nested_empty];
  ()
[@@verocaml.proof]
|}

let retained_mathematical_int_value_case =
  retained_transport_case ~name:"retained-mathematical-int-logical-value"
    ~provider_interface:
      {|val twelve : Vstd.Int.t [@@verocaml.spec] [@@verocaml.revealed]
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

let twelve : Vstd.Int.t = 3 * 4 [@@verocaml.spec]
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ -> Provider.twelve = Provider.twelve];
  ()
[@@verocaml.proof]
|}

let retained_callable_partition_case =
  retained_transport_case ~name:"retained-callable-and-value-partition"
    ~provider_interface:
      {|type unary = int -> int

val identity : unary [@@verocaml.spec]
val unit_truth : unit -> bool [@@verocaml.spec]
[%%verocaml.symbolic val choose : 'a -> 'a]
val stable_value : bool [@@verocaml.spec] [@@verocaml.revealed]
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

type unary = int -> int

let identity (value : int) : int = value [@@verocaml.spec]
let unit_truth () = true [@@verocaml.spec]
[%%verocaml.symbolic val choose : 'a -> 'a]
let stable_value : bool = true [@@verocaml.spec]
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ -> Provider.identity 7 = 7];
  [%verocaml.ensures fun _ -> Provider.unit_truth ()];
  [%verocaml.ensures fun _ ->
    Provider.choose true = Provider.choose true];
  [%verocaml.ensures fun _ ->
    Provider.stable_value = Provider.stable_value];
  ()
[@@verocaml.proof]
|}

let retained_symbolic_dependency_case =
  retained_transport_case ~name:"retained-revealed-value-symbolic-dependency"
    ~provider_interface:
      {|[%%verocaml.symbolic val selector : bool]
val selected : bool [@@verocaml.spec] [@@verocaml.revealed]
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

[%%verocaml.symbolic val selector : bool]
let selected : bool = selector [@@verocaml.spec]
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ -> Provider.selected = Provider.selected];
  [%verocaml.ensures fun _ -> Provider.selector = Provider.selector];
  ()
[@@verocaml.proof]
|}

let retained_module_alias_case =
  retained_transport_case ~name:"retained-module-alias-logical-value-route"
    ~provider_interface:
      {|module Inner : sig
  val truth : bool [@@verocaml.spec] [@@verocaml.revealed]
end

module Alias = Inner
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

module Inner = struct
  let truth : bool = true [@@verocaml.spec]
end

module Alias = Inner
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ ->
    Provider.Inner.truth = Provider.Inner.truth];
  [%verocaml.ensures fun _ ->
    Provider.Alias.truth = Provider.Alias.truth];
  [%verocaml.ensures fun _ ->
    Provider.Inner.truth = Provider.Alias.truth];
  ()
[@@verocaml.proof]
|}

let retained_open_shadow_case =
  retained_transport_case ~name:"retained-open-shadow-logical-value-route"
    ~provider_interface:
      {|module Left : sig
  val selected : bool [@@verocaml.spec] [@@verocaml.revealed]
end

module Right : sig
  val selected : bool [@@verocaml.spec] [@@verocaml.revealed]
end
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

module Left = struct
  let selected : bool = true [@@verocaml.spec]
end

module Right = struct
  let selected : bool = false [@@verocaml.spec]
end
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

open Provider.Left
open Provider.Right

let stable () =
  [%verocaml.ensures fun _ ->
    Provider.Left.selected = Provider.Left.selected];
  [%verocaml.ensures fun _ -> selected = Provider.Right.selected];
  ()
[@@verocaml.proof]
|}

let retained_value_rebinding_case =
  retained_transport_case ~name:"retained-value-rebinding-logical-origins"
    ~provider_interface:
      {|val base : bool [@@verocaml.spec] [@@verocaml.revealed]
val forwarded : bool [@@verocaml.spec] [@@verocaml.revealed]
|}
    ~provider_implementation:
      {|[@@@verocaml.verify]

let base : bool = true [@@verocaml.spec]
let forwarded : bool = base [@@verocaml.spec]
|}
    ~consumer_implementation:
      {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ -> Provider.base = Provider.base];
  [%verocaml.ensures fun _ -> Provider.forwarded = Provider.forwarded];
  ()
[@@verocaml.proof]
|}

let retained_transitive_closure_case =
  let input =
    Fixture.dune_project
      {
        files =
          [
            file "dune-project"
              "(lang dune 3.17)\n(name logical_constant_transitive)\n(package (name logical_constant_transitive))\n";
            file "dune"
              {|(library
 (name base_transport)
 (wrapped false)
 (modules Base)
 (libraries verocaml.vstd verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))

(rule
 (targets base.vri base.verocaml-retained-interface base.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .base_transport.objs/byte/base.cmt)
  (:cmi .base_transport.objs/byte/base.cmi)
  (:cmti .base_transport.objs/byte/base.cmti))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} base.vri
    --artifact-directory .base_transport.objs/byte)
   (run %{emitter} manifest base.verocaml-retained-interface .base_transport.objs/byte/base.cmt .base_transport.objs/byte/base.cmi .base_transport.objs/byte/base.cmti base.vri)
   (run %{emitter} manifest base.verocaml-retained-interface.install base.cmt base.cmi base.cmti base.vri))))

(library
 (name forwarder_transport)
 (wrapped false)
 (modules Forwarder)
 (libraries base_transport verocaml.vstd verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))

(rule
 (targets forwarder.vri forwarder.verocaml-retained-interface forwarder.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .forwarder_transport.objs/byte/forwarder.cmt)
  (:cmi .forwarder_transport.objs/byte/forwarder.cmi)
  (:cmti .forwarder_transport.objs/byte/forwarder.cmti)
  (:dependency_cmt .base_transport.objs/byte/base.cmt)
  (:dependency_cmi .base_transport.objs/byte/base.cmi)
  (:dependency_cmti .base_transport.objs/byte/base.cmti)
  (:dependency_vri base.vri)
  (:dependency_manifest base.verocaml-retained-interface))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} forwarder.vri
    --artifact-directory .forwarder_transport.objs/byte
    --artifact-directory .base_transport.objs/byte
    --artifact-directory .)
   (run %{emitter} manifest forwarder.verocaml-retained-interface .forwarder_transport.objs/byte/forwarder.cmt .forwarder_transport.objs/byte/forwarder.cmi .forwarder_transport.objs/byte/forwarder.cmti forwarder.vri)
   (run %{emitter} manifest forwarder.verocaml-retained-interface.install forwarder.cmt forwarder.cmi forwarder.cmti forwarder.vri))))

(alias
 (name all)
 (deps base.vri base.verocaml-retained-interface
       forwarder.vri forwarder.verocaml-retained-interface))

(install
 (package logical_constant_transitive)
 (section lib)
 (files
  (base.vri as ./base.vri)
  (base.verocaml-retained-interface.install as ./base.verocaml-retained-interface)
  (forwarder.vri as ./forwarder.vri)
  (forwarder.verocaml-retained-interface.install as ./forwarder.verocaml-retained-interface)))

(library
 (name transitive_consumer)
 (wrapped false)
 (modules Consumer)
 (libraries forwarder_transport verocaml.vstd verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
|};
            file "base.mli"
              {|[%%verocaml.symbolic val selector : bool]
val always : bool -> bool [@@verocaml.spec]
|};
            file "base.ml"
              {|[@@@verocaml.verify]

[%%verocaml.symbolic val selector : bool]

let always value = value || selector
[@@verocaml.spec]
|};
            file "forwarder.mli"
              "val selected : bool [@@verocaml.spec] [@@verocaml.revealed]\n";
            file "forwarder.ml"
              {|[@@@verocaml.verify]

let selected : bool = Base.always true
[@@verocaml.spec]
|};
            file "consumer.ml"
              {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ ->
    Forwarder.selected = Forwarder.selected];
  ()
[@@verocaml.proof]
|};
          ];
        libraries = [ "verocaml.vstd"; "verocaml.ghost" ];
        targets = [ "@all" ];
        selected_units = [ "Base"; "Forwarder"; "Consumer" ];
      }
  in
  Suite.case ~name:"retained-transitive-logical-value-dependency-closure"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Base" Outcome.Unit_verified
      |> Expectation.require_unit "Forwarder" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:stable"
           (Outcome.Function_exists "stable"))
    (fun ~environment ~workspace ->
      let* outcome = run_fixture input ~environment ~workspace in
      let root = Filename.concat workspace "project" in
      let* base = load_implementation root "Base" in
      let* forwarder = load_implementation root "Forwarder" in
      let* consumer = load_implementation root "Consumer" in
      let* imported =
        imported_authority ~consumer ~dependencies:[ base; forwarder ] ()
      in
      let constants = Imported_callable.logical_constants imported in
      let selected =
        constants
        |> List.filter_map (function
             | Imported_callable.Imported_defined_logical_value constant
               when List.exists
                      (fun (route : Imported_callable.logical_constant_route) ->
                        String.equal route.logical_constant_path
                          "Forwarder.selected")
                      constant.routes ->
                 Some
                   ( constant.definition,
                     constant.semantic_authority,
                     constant.dependency_closure,
                     constant.trust_dependencies )
             | Imported_callable.Imported_defined_logical_value _
             | Imported_callable.Imported_symbolic_logical_value _ ->
                 None)
      in
      let symbolic_selector =
        constants
        |> List.exists (function
             | Imported_callable.Imported_symbolic_logical_value constant ->
                 List.exists
                   (fun (route : Imported_callable.logical_constant_route) ->
                     String.equal route.logical_constant_path "Base.selector")
                   constant.routes
             | Imported_callable.Imported_defined_logical_value _ -> false)
      in
      let* () =
        require
          (match selected with
          | [ (definition, semantic_authority, dependency_closure,
               trust_dependencies) ] ->
              definition.Sst.constant_provenance = Sst.Opaque_defined_identity
              && Option.is_none definition.constant_equation
              && semantic_authority.constant_provenance
                 = Sst.Verified_definitional_equation
              && Option.is_some semantic_authority.constant_equation
              && not (String.equal dependency_closure "")
              && trust_dependencies = []
              && symbolic_selector
          | [] | _ :: _ :: _ -> false)
          "transitive logical value authority was not closed, stable, and inactive"
      in
      Ok outcome)

let retained_trust_dependency_case =
  let input =
    Fixture.dune_project
      {
        files =
          [
            file "dune-project"
              "(lang dune 3.17)\n(name logical_constant_transport)\n(package (name logical_constant_transport))\n";
            file "dune"
              ("(library\n (name legacy_transport)\n (wrapped false)\n (modules Legacy)\n (flags (:standard -ppx \"verocaml-ppx\")))\n\n"
              ^ "(library\n (name logical_constant_transport)\n (wrapped false)\n (modules Provider)\n (libraries legacy_transport verocaml.vstd verocaml.ghost)\n (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n\n"
              ^ retained_logical_value_rules
              ^ "\n(library\n (name logical_constant_consumer)\n (wrapped false)\n (modules Consumer)\n (libraries logical_constant_transport verocaml.vstd verocaml.ghost)\n (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n\n"
              ^ "(library\n (name logical_constant_bootstrap)\n (wrapped false)\n (modules Bootstrap)\n (libraries verocaml.ghost)\n (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n");
            file "bootstrap.ml" "let ready () = true\n";
            file "legacy.mli" "val truth : bool -> bool\n";
            file "legacy.ml" "let truth value = value\n";
            file "provider.mli"
              "val selected : bool [@@verocaml.spec] [@@verocaml.revealed]\n";
            file "provider.ml"
              {|[@@@verocaml.verify]

let trusted_truth_specification value =
  [%verocaml.ensures fun result -> result = value];
  Legacy.truth value
[@@verocaml.external_specification]

let selected : bool = trusted_truth_specification true
[@@verocaml.spec]
|};
            file "consumer.ml"
              {|[@@@verocaml.verify]

let stable () =
  [%verocaml.ensures fun _ -> Provider.selected = Provider.selected];
  ()
[@@verocaml.proof]
|};
          ];
        libraries = [ "verocaml.vstd"; "verocaml.ghost" ];
        targets = [ "@all" ];
        selected_units = [ "Bootstrap" ];
      }
  in
  Suite.case ~name:"retained-logical-value-trust-dependency"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Legacy" Outcome.Unit_skipped
      |> Expectation.require_unit "Provider" Outcome.Unit_dependency_success
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:stable"
           (Outcome.Function_exists "stable"))
    (fun ~environment ~workspace ->
      let* bootstrap = run_fixture input ~environment ~workspace in
      let* () =
        require
          (Outcome.status bootstrap = Outcome.Verified)
          "trust fixture bootstrap did not compile"
      in
      let root = Filename.concat workspace "project" in
      let* legacy_cmt = artifact root "Legacy" ".cmt" in
      let* legacy_cmi = artifact root "Legacy" ".cmi" in
      let* provider_cmt = artifact root "Provider" ".cmt" in
      let* provider_cmi = artifact root "Provider" ".cmi" in
      let* consumer_cmt = artifact root "Consumer" ".cmt" in
      let* consumer_cmi = artifact root "Consumer" ".cmi" in
      let* legacy = load_implementation root "Legacy" in
      let* provider = load_implementation root "Provider" in
      let* consumer = load_implementation root "Consumer" in
      let* configuration =
        match
          Verifier_service.configuration ~threads:1 ~timeout_ms:60_000
            ~rlimit:None
        with
        | Ok configuration -> Ok configuration
        | Error error ->
            mismatch "%s"
              (Verifier_service.configuration_error_message error)
      in
      let* request =
        match
          Verifier_service.scoped_request ~configuration
            ~inventory:
              [
                ( Verifier_service.Scope_root,
                  consumer_cmt,
                  consumer_cmi,
                  consumer );
                ( Scope_dependency,
                  provider_cmt,
                  provider_cmi,
                  provider );
                (Scope_dependency, legacy_cmt, legacy_cmi, legacy);
              ]
        with
        | Ok request -> Ok request
        | Error error ->
            mismatch "%s"
              (Verifier_service.scoped_plan_error_message error)
      in
      let scoped = Verifier_service.verify_scope request in
      let row unit_name =
        Verifier_service.scoped_rows scoped
        |> List.filter (fun row ->
               String.equal
                 (Verifier_service.scoped_row_unit_name row)
                 unit_name)
        |> function
        | [ row ] -> Ok row
        | [] -> mismatch "scope omitted %s" unit_name
        | _ -> mismatch "scope repeated %s" unit_name
      in
      let* legacy_row = row "Legacy" in
      let* provider_row = row "Provider" in
      let* consumer_row = row "Consumer" in
      let* () =
        require
          (match
             ( Verifier_service.scoped_row_classification legacy_row,
               Verifier_service.scoped_row_outcome legacy_row )
           with
          | Scoped_skipped, Scoped_skip -> true
          | _ -> false)
          "ordinary external target was not skipped"
      in
      let* () =
        require
          (match
             ( Verifier_service.scoped_row_classification provider_row,
               Verifier_service.scoped_row_outcome provider_row )
           with
          | Scoped_verified_dependency, Scoped_dependency_success -> true
          | _ -> false)
          "retained logical-value provider was not verified as a dependency"
      in
      let* consumer_result =
        match
          ( Verifier_service.scoped_row_classification consumer_row,
            Verifier_service.scoped_row_outcome consumer_row )
        with
        | Scoped_verified, Scoped_verification result -> Ok result
        | Scoped_verified, Scoped_rejection error ->
            mismatch "%s" (Verifier_service.error_message error)
        | _ -> mismatch "logical-value consumer was not a verification root"
      in
      let* legacy = load_implementation root "Legacy" in
      let* provider = load_implementation root "Provider" in
      let* consumer = load_implementation root "Consumer" in
      let* imported =
        imported_authority ~external_targets:[ legacy ] ~consumer
          ~dependencies:[ provider ] ()
      in
      let selected =
        Imported_callable.logical_constants imported
        |> List.filter_map (function
             | Imported_callable.Imported_defined_logical_value constant
               when List.exists
                      (fun (route : Imported_callable.logical_constant_route) ->
                        String.equal route.logical_constant_path
                          "Provider.selected")
                      constant.routes ->
                 Some
                   ( constant.semantic_authority,
                     constant.dependency_closure,
                     constant.trust_dependencies )
             | Imported_callable.Imported_defined_logical_value _
             | Imported_callable.Imported_symbolic_logical_value _ ->
                 None)
      in
      let* () =
        require
          (match selected with
          | [ (semantic_authority, dependency_closure, trust_dependencies) ] ->
              not (String.equal dependency_closure "")
              && trust_dependencies <> []
              &&
              (match semantic_authority.Sst.constant_equation with
              | Some equation ->
                  equation.constant_trust_dependencies = trust_dependencies
              | None -> false)
          | [] | _ :: _ :: _ -> false)
          "trusted equation dependency was not authenticated and transported"
      in
      let outcome =
        Outcome.of_verifier_result consumer_result
        |> Outcome.with_unit "Legacy" Outcome.Unit_skipped
        |> Outcome.with_unit "Provider" Outcome.Unit_dependency_success
        |> Outcome.with_unit "Consumer" Outcome.Unit_verified
      in
      Ok outcome)

let retained_transport_cases =
  [
    first_class_specification_constant_case;
    retained_same_leaf_provider_origins_case;
    ecosystem_logical_constant_case;
    copied_logical_handle_tamper_case;
    retained_interface_transport_case;
    retained_polymorphic_values_case;
    retained_nested_generic_value_case;
    retained_mathematical_int_value_case;
    retained_callable_partition_case;
    retained_symbolic_dependency_case;
    retained_module_alias_case;
    retained_open_shadow_case;
    retained_value_rebinding_case;
    retained_transitive_closure_case;
    retained_trust_dependency_case;
  ]

let positive_cases =
  retained_transport_cases
  @ [
    verified_case ~name:"boolean-literal-definition"
      ~module_name:"Boolean_literal_definition"
      ~source:
        {|let answer : bool = true [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> answer];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"mathematical-integer-arithmetic-definition"
      ~module_name:"Mathematical_integer_definition"
      ~source:
        {|open Vstd

let twelve : Int.t = (3 * 4) [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> twelve = 12];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"repeated-reference"
      ~module_name:"Repeated_reference"
      ~source:
        {|let stable : bool = true [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> stable && stable && stable];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"earlier-constant-dependency"
      ~module_name:"Earlier_constant_dependency"
      ~source:
        {|let foundation : bool = true [@@verocaml.spec]
let consequence : bool = foundation && true [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> consequence];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"constant-dependency-chain"
      ~module_name:"Constant_dependency_chain"
      ~source:
        {|let first : bool = true [@@verocaml.spec]
let second : bool = first [@@verocaml.spec]
let third : bool = second && first [@@verocaml.spec]
let fourth : bool = third || false [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> fourth];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"specification-function-dependency"
      ~module_name:"Specification_function_dependency"
      ~source:
        {|let negate value = not value [@@verocaml.spec]
let negated_false : bool = negate false [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> negated_false];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"mixed-logical-definition-dependency-chain"
      ~module_name:"Mixed_logical_definition_dependency_chain"
      ~source:
        {|[%%verocaml.symbolic val unknown : bool]

let seed : bool = unknown || not unknown [@@verocaml.spec]
let via_seed value = seed && value [@@verocaml.spec]
let derived : bool = via_seed true [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> derived];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"polymorphic-specification-function-dependency"
      ~module_name:"Polymorphic_specification_dependency"
      ~source:
        {|let identity value = value [@@verocaml.spec]
let selected : bool = identity true [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> selected];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"symbolic-value-dependency"
      ~module_name:"Symbolic_value_dependency"
      ~source:
        {|[%%verocaml.symbolic val unknown : bool]

let excluded_middle : bool = unknown || not unknown [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> excluded_middle];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"symbolic-function-dependency"
      ~module_name:"Symbolic_function_dependency"
      ~source:
        {|open Vstd

[%%verocaml.symbolic val predicate : Int.t -> bool]

let excluded_middle : bool = predicate 0 || not (predicate 0)
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> excluded_middle];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"result-only-polymorphic-option-instances"
      ~module_name:"Polymorphic_option_instances"
      ~source:
        {|open Vstd
open Vstd.Pervasive

let absent : 'a option = None [@@verocaml.spec]

let option_is_empty value =
  match value with None -> true | Some _ -> false
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ ->
    option_is_empty (absent : bool option)
    && option_is_empty (absent : Int.t option)];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"nested-option-definition"
      ~module_name:"Nested_option_definition"
      ~source:
        {|open Vstd
open Vstd.Pervasive
open Vstd.Pervasive

let nested : bool option option = Some (Some true) [@@verocaml.spec]

let nested_holds value =
  match value with Some (Some inner) -> inner | _ -> false
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> nested_holds nested];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"result-definition"
      ~module_name:"Result_definition"
      ~source:
        {|open Vstd
open Vstd.Pervasive
open Vstd.Pervasive

let success : (bool, Int.t) result = Ok true [@@verocaml.spec]

let result_holds value =
  match value with Ok inner -> inner | Error _ -> false
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> result_holds success];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"user-defined-generic-variant"
      ~module_name:"User_defined_generic_variant"
      ~source:
        {|type 'a container = Empty | Present of 'a

let present_true : bool container = Present true [@@verocaml.spec]

let holds value =
  match value with Present inner -> inner | Empty -> false
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> holds present_true];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"nested-user-defined-types"
      ~module_name:"Nested_user_defined_types"
      ~source:
        {|type 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree
type 'a wrapper = Wrap of 'a

let nested : bool tree wrapper = Wrap (Branch (Leaf true, Leaf false))
[@@verocaml.spec]

let observes_true value =
  match value with
  | Wrap (Branch (Leaf left, Leaf _)) -> left
  | _ -> false
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> observes_true nested];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"record-definition-and-projection"
      ~module_name:"Record_definition"
      ~source:
        {|open Vstd

type point = { x : Int.t; y : Int.t }

let origin : point = { x = 0; y = 0 } [@@verocaml.spec]
let origin_x : Int.t = origin.x [@@verocaml.spec]
let origin_y : Int.t = origin.y [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> origin_x = 0 && origin_y = 0];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"nonparametric-variant-definition"
      ~module_name:"Nonparametric_variant_definition"
      ~source:
        {|type signal = Off | On

let enabled : signal = On [@@verocaml.spec]

let is_enabled value =
  match value with On -> true | Off -> false
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> is_enabled enabled];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"match-expression-definition"
      ~module_name:"Match_expression_definition"
      ~source:
        {|open Vstd
open Vstd.Pervasive

let selected : Int.t =
  match Some 7 with Some value -> value | None -> 0
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> selected = 7];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"boolean-conditional-definition"
      ~module_name:"Boolean_conditional_definition"
      ~source:
        {|[%%verocaml.symbolic val condition : bool]

let tautology : bool =
  if condition then condition else not condition
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> tautology];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"integer-conditional-definition"
      ~module_name:"Integer_conditional_definition"
      ~source:
        {|open Vstd

[%%verocaml.symbolic val integer : Int.t]

let magnitude : Int.t =
  if integer >= 0 then integer else -integer
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> magnitude >= 0];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"constant-in-requires-clause"
      ~module_name:"Constant_in_requires"
      ~source:
        {|let premise : bool = true [@@verocaml.spec]

let lemma () =
  [%verocaml.requires premise];
  [%verocaml.ensures fun _ -> premise];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"constant-in-executable-ensures-clause"
      ~module_name:"Constant_in_ensures"
      ~source:
        {|let invariant : bool = true [@@verocaml.spec]

let preserve value =
  [%verocaml.ensures fun result -> invariant && result = value];
  value
|};
    verified_case ~name:"constant-in-local-proof-assertion"
      ~module_name:"Constant_in_assertion"
      ~source:
        {|let assertion : bool = true [@@verocaml.spec]

let lemma () =
  [%verocaml.assert assertion];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"constant-returned-from-specification"
      ~module_name:"Constant_returned_from_specification"
      ~source:
        {|let invariant : bool = true [@@verocaml.spec]
let property value = invariant && value = value [@@verocaml.spec]

let lemma value =
  [%verocaml.ensures fun _ -> property value];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"forall-definition"
      ~module_name:"Forall_definition"
      ~source:
        {|open Vstd

[%%verocaml.symbolic val observed : Int.t -> bool]

let universal : bool =
  forall (fun (value : Int.t) ->
    ((observed value) [@trigger]) || value = value)
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> universal];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"exists-definition"
      ~module_name:"Exists_definition"
      ~source:
        {|open Vstd

let has_zero : bool =
  exists (fun (value : Int.t) -> value = 0)
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> has_zero];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"vstd-specification-dependency"
      ~module_name:"Vstd_specification_dependency"
      ~source:
        {|open Vstd.Pervasive

let selected : bool = identity true [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> selected];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"same-instance-across-proof-functions"
      ~module_name:"Same_instance_across_proofs"
      ~source:
        {|let shared : bool = true [@@verocaml.spec]

let first () =
  [%verocaml.ensures fun _ -> shared];
  ()
[@@verocaml.proof]

let second () =
  [%verocaml.ensures fun _ -> shared && shared];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"nested-option-result-definition"
      ~module_name:"Nested_option_result_definition"
      ~source:
        {|open Vstd.Pervasive

let nested : ((bool, bool) result option, bool) result =
  Ok (Some (Ok true))
[@@verocaml.spec]

let holds value =
  match value with Ok (Some (Ok inner)) -> inner | _ -> false
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> holds nested];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"named-module-definition"
      ~module_name:"Named_module_definition"
      ~source:
        {|module Constants = struct
  let truth : bool = true [@@verocaml.spec]
end

let lemma () =
  [%verocaml.ensures fun _ -> Constants.truth];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"aggressive-arithmetic-rewrite-definition"
      ~module_name:"Arithmetic_rewrite_definition"
      ~source:
        {|open Vstd

let reduced : Int.t =
  ((2 + 3) * 4) - ((8 - 3) * 2)
[@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ -> reduced = 10];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"constant-in-recursive-specification"
      ~module_name:"Constant_in_recursive_specification"
      ~source:
        {|open Vstd

let zero : Int.t = 0 [@@verocaml.spec]

let rec countdown (value : Int.t) : Int.t =
  [%verocaml.decreases value];
  if value <= 0 then 0 else countdown (value - 1)
[@@verocaml.spec]
[@@verocaml.revealed]

let lemma () =
  [%verocaml.ensures fun _ -> countdown 0 = zero];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"symbolic-constant-in-recursive-specification"
      ~module_name:"Symbolic_constant_in_recursive_specification"
      ~source:
        {|open Vstd

[%%verocaml.symbolic val unknown : bool]

let truth : bool = unknown || not unknown [@@verocaml.spec]

let rec always (value : Int.t) : bool =
  [%verocaml.decreases value];
  if value <= 0 then truth else always (value - 1)
[@@verocaml.spec]
[@@verocaml.revealed]

let lemma () =
  [%verocaml.ensures fun _ -> always 0];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"defined-constant-in-recursive-specification"
      ~module_name:"Defined_constant_in_recursive_specification"
      ~source:
        {|open Vstd

let successor value = value + 1 [@@verocaml.spec]
let one : Int.t = successor 0 [@@verocaml.spec]

let rec stop (value : Int.t) : Int.t =
  [%verocaml.decreases value];
  if value <= 0 then one else stop (value - 1)
[@@verocaml.spec]
[@@verocaml.revealed]

let lemma () =
  [%verocaml.ensures fun _ -> stop 0 = 1];
  ()
[@@verocaml.proof]
|};
    verified_case ~name:"constant-thunk-and-symbolic-value-remain-distinct"
      ~module_name:"Distinct_logical_value_classes"
      ~source:
        {|[%%verocaml.symbolic val unknown : bool]

let thunk () = true [@@verocaml.spec]
let constant : bool = true [@@verocaml.spec]

let lemma () =
  [%verocaml.ensures fun _ ->
    constant && thunk () && (unknown || not unknown)];
  ()
[@@verocaml.proof]
|};
  ]

let rejection_cases =
  [
    rejected_case ~code:"VERO_LOGICAL_CONSTANT_DECLARATION"
      ~name:"unit-valued-constant-is-outside-first-order-slice"
      ~module_name:"Unit_valued_constant"
      ~source:"let unsupported : unit = () [@@verocaml.spec]\n" ();
    rejected_case ~code:"VERO_LOGICAL_CONSTANT_DECLARATION"
      ~name:"tuple-valued-constant-is-outside-first-order-slice"
      ~module_name:"Tuple_valued_constant"
      ~source:
        "let unsupported : bool * bool = (true, true) [@@verocaml.spec]\n"
      ();
    rejected_case ~code:"VERO_LOGICAL_CONSTANT_DECLARATION"
      ~name:"function-valued-constant-is-outside-first-order-slice"
      ~module_name:"Function_valued_constant"
      ~source:
        {|let identity value = value [@@verocaml.spec]

let unsupported : bool -> bool =
  if true then identity else identity
[@@verocaml.spec]
|}
      ();
    rejected_case ~code:"VERO_UNSUPPORTED_TYPE"
      ~name:"runtime-integer-constant-needs-mathematical-type"
      ~module_name:"Runtime_integer_constant"
      ~source:"let unsupported : int = 1 + 1 [@@verocaml.spec]\n" ();
    rejected_case ~code:"VERO_LOGICAL_CONSTANT_DECLARATION"
      ~name:"mutable-logical-constant-body-is-rejected"
      ~module_name:"Mutable_constant_body"
      ~source:
        {|open Vstd

let unsupported : Int.t =
  let mutable value : Int.t = 0 in
  value <- 1;
  value
[@@verocaml.spec]
|}
      ();
    rejected_case ~code:"VERO_LOGICAL_CONSTANT_USE"
      ~name:"logical-constant-is-unavailable-to-executable-code"
      ~module_name:"Executable_constant_use"
      ~source:
        {|let truth : bool = true [@@verocaml.spec]
let executable () = truth
|}
      ();
    rejected_case ~code:"VERO_LOGICAL_CONSTANT_DECLARATION"
      ~name:"public-signature-transport-is-not-forged"
      ~module_name:"Public_constant_signature"
      ~source:
        {|module type CONSTANTS = sig
  val truth : bool
end

module Constants : CONSTANTS = struct
  let truth : bool = true [@@verocaml.spec]
end
|}
      ();
    rejected_case ~code:"VERO_UNSUPPORTED_EXTERNAL_CALL"
      ~name:"io-body-acquires-no-logical-authority"
      ~module_name:"Io_constant_body"
      ~source:
        {|open Vstd

let unsupported : Int.t =
  print_endline "effect";
  0
[@@verocaml.spec]
|}
      ();
    rejected_case ~code:"VERO_UNSUPPORTED_EXTERNAL_CALL"
      ~name:"raising-body-acquires-no-logical-authority"
      ~module_name:"Raising_constant_body"
      ~source:
        {|open Vstd

let unsupported : Int.t =
  if true then raise Exit else 0
[@@verocaml.spec]
|}
      ();
    rejected_case ~code:"VERO_INVALID_LOGICAL_CONSTANT"
      ~name:"functor-body-acquires-no-logical-authority"
      ~module_name:"Functor_constant_scope"
      ~source:
        {|module Make (_ : sig end) = struct
  let hidden : bool = true [@@verocaml.spec]
end
|}
      ();
    rejected_case ~code:"VERO_INVALID_LOGICAL_CONSTANT"
      ~name:"unpacked-module-acquires-no-logical-authority"
      ~module_name:"Unpacked_constant_scope"
      ~source:
        {|module type EMPTY = sig end

let packed =
  (module struct
    let hidden : bool = true [@@verocaml.spec]
  end : EMPTY)
|}
      ();
    rejected_case ~code:"VERO_INVALID_LOGICAL_CONSTANT"
      ~name:"anonymous-include-acquires-no-logical-authority"
      ~module_name:"Anonymous_include_constant_scope"
      ~source:
        {|include struct
  let hidden : bool = true [@@verocaml.spec]
end
|}
      ();
  ]

let semantic_distinction_cases =
  [
    retained_revealed_equation_inactive_case;
    counterexample_case ~name:"bodyless-symbolic-value-remains-equationless"
      ~module_name:"Equationless_symbolic_value"
      ~source:
        {|[%%verocaml.symbolic val unknown : bool]

let cannot_prove () =
  [%verocaml.ensures fun _ -> unknown];
  ()
[@@verocaml.proof]
|};
  ]

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    (direct_source_route_case :: semantic_artifact_case :: positive_cases
   @ source_grammar_rejection_cases
   @ (direct_interface_rejection_case :: aliased_interface_rejection_case
     :: rejection_cases)
   @ semantic_distinction_cases)
