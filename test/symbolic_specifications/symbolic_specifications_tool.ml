let () = ignore Symbolic_specifications_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

type input = {
  implementation : Cmt_input.implementation;
  program : Sst.program;
  validated : Sst_validation.validated_program;
  invariants : Type_invariant.environment;
}

let input filename =
  let implementation = load filename in
  let program =
    match Typedtree_lowering.lower implementation with
    | Ok program -> program
    | Error diagnostic ->
        fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message
  in
  let validated =
    match Sst_validation.validate program with
    | Ok validated -> validated
    | Error error -> fail "%s" (Sst_validation.error_to_string error)
  in
  let invariants =
    match Type_invariant.authenticate validated with
    | Ok invariants -> invariants
    | Error error -> fail "%s" (Type_invariant.error_to_string error)
  in
  { implementation; program; validated; invariants }

let policy () =
  match Solver_policy_private.create ~timeout_ms:5_000 ~rlimit:100_000 with
  | Ok policy -> policy
  | Error error -> fail "%s" (Solver_policy_private.error_to_string error)

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let pipeline_error = function
  | Verification_pipeline.Engine_error error ->
      Symbolic_executor_private.error_to_string error
  | Solve_error message -> message
  | Setup_error (Internal_setup_error message) -> message
  | Setup_error (Solver_configuration_error error) ->
      Solver_backend.error_to_string error

type observation = {
  obligation : Vir.obligation;
  broadcast : Broadcast_vc_private.report option;
}

let inspect_request observations
    (request : Verification_pipeline.solve_request) =
  List.iter
    (fun obligation ->
      observations :=
        {
          obligation;
          broadcast = Broadcast_vc_private.report obligation;
        }
        :: !observations)
    request.execution.Vir.obligations

let run filename =
  let input = input filename in
  let solver_policy = policy () in
  let preflight = ref None in
  let observations = ref [] in
  let run_preflight () =
    match Verification_solver_private.preflight ~solver_policy input.program with
    | Error error -> Error error
    | Ok prepared ->
        preflight := Some prepared;
        Ok (Verification_solver_private.termination_obligations prepared)
  in
  let configure_solver () =
    match !preflight with
    | None ->
        Error
          (Verification_pipeline.Internal_setup_error
             "symbolic test lost solver preflight")
    | Some prepared -> (
        match Verification_solver_private.configure ~solver_policy prepared with
        | Error _ as error -> error
        | Ok solve ->
            Ok
              (fun request ->
                inspect_request observations request;
                solve request))
  in
  let proof_entry_activations () =
    match !preflight with
    | None -> Fun.const []
    | Some prepared ->
        Verification_solver_private.proof_entry_activations prepared
  in
  let report =
    match
      Verification_pipeline.run_validated ~imports:None
        ~implementation:input.implementation ~program:input.program
        ~validated:input.validated ~invariants:input.invariants
        ~preflight:run_preflight ~proof_entry_activations ~configure_solver
        ~on_result:ignore
    with
    | Ok report -> report
    | Error message -> fail "%s" message
  in
  (input, report, List.rev !observations)

let resources () =
  let counters = Z3_bridge.counters () in
  Printf.printf
    "resources backend=%d contexts=%d solvers=%d resets=%d cleaned=%d live=%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    counters.contexts_created counters.solvers_created counters.solver_resets
    counters.contexts_cleaned counters.contexts_live

let reset_resources () =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

let structural filename =
  reset_resources ();
  let input, report, observations = run filename in
  let completion =
    match report.Verification_pipeline.outcome with
    | Ok completion -> completion
    | Error error -> fail "%s" (pipeline_error error)
  in
  let declarations =
    List.filter
      (fun definition ->
        match definition.Sst.body with
        | Sst.Symbolic_declaration _ -> true
        | Sst.Checked_exec _ | Sst.Spec_definition _
        | Sst.Recursive_spec_definition _ | Sst.Proof_body _
        | Sst.External_specification _ | Sst.Trusted_external_spec_target _
        | Sst.Trusted_external_body _ ->
            false)
      input.program.functions
  in
  let active, trusted_declarations, trusted_uses, inserted =
    List.fold_left
      (fun (active, declarations, uses, inserted) observation ->
        match observation.broadcast with
        | None -> (active, declarations, uses, inserted)
        | Some report ->
            ( active + report.active_declarations,
              declarations + report.trusted_broadcast_declarations,
              uses + report.trusted_broadcast_uses,
              inserted + List.length report.inserted ))
      (0, 0, 0, 0) observations
  in
  Printf.printf
    "status=%s functions=%d obligations=%d symbolic=%d destroyed=%b\n"
    (status_name completion.status) completion.functions completion.obligations
    (List.length declarations) report.session_destroyed;
  Printf.printf
    "broadcast active=%d trusted-declarations=%d trusted-uses=%d inserted=%d\n"
    active trusted_declarations trusted_uses inserted;
  resources ()

let semantic filename =
  reset_resources ();
  let _, report, _ = run filename in
  (match report.Verification_pipeline.outcome with
  | Ok completion ->
      Printf.printf "status=%s functions=%d obligations=%d destroyed=%b\n"
        (status_name completion.status) completion.functions
        completion.obligations report.session_destroyed
  | Error error -> fail "%s" (pipeline_error error));
  resources ()

let balanced (counters : Z3_bridge.counters) =
  counters.contexts_created = 1
  && counters.solvers_created = 1
  && counters.solver_resets = 1
  && counters.contexts_cleaned = 1
  && counters.contexts_live = 0
  && counters.maximum_contexts_live = 1

let route_parity filename =
  let _, _, observations = run filename in
  let requires = [ Logic_ir.Named_sorts; Logic_ir.Algebraic_datatypes ] in
  let config : Z3_bridge.config = { timeout_ms = 5_000; model = false } in
  let check observation =
    let translation =
      match
        Vir_logic_ir_translation_private.translate ~requires
          observation.obligation
      with
      | Ok translation -> translation
      | Error message -> fail "%s" message
    in
    let query = Vir_logic_ir_translation_private.query translation in
    let detached =
      match Z3_bridge.detach_vir ~requires observation.obligation with
      | Ok (query, _) -> query
      | Error error -> fail "%s" (Z3_bridge.error_to_string error)
    in
    let direct =
      Z3_bridge.solve_query_local ~controlled:Z3_bridge.Force_unknown
        ~rlimit:100_000 config query
    in
    let detached =
      Z3_bridge.solve_detached_query_local
        ~controlled:Z3_bridge.Force_unknown ~timeout_ms:5_000 ~rlimit:100_000
        ~model:false detached
    in
    let direct_unknown =
      match direct.result with
      | Ok (Z3_bridge.Inconclusive (Backend_unknown _)) -> true
      | Ok _ | Error _ -> false
    in
    let detached_unknown =
      match detached.detached_result with
      | Ok (Z3_bridge.Detached_inconclusive (Backend_unknown _)) -> true
      | Ok _ | Error _ -> false
    in
    if
      not
        (direct_unknown && detached_unknown && balanced direct.telemetry
       && balanced detached.detached_telemetry)
    then fail "direct/detached symbolic query parity changed"
  in
  List.iter check observations;
  Printf.printf
    "routes direct=%d detached=%d parity=true timeout-ms=5000 rlimit=100000\n"
    (List.length observations) (List.length observations)

let diagnostic filename =
  reset_resources ();
  match Cmt_input.load filename with
  | Error diagnostic ->
      Printf.printf "rejected=loader code=%s sst=0 vir=0 vc=0\n"
        diagnostic.Diagnostic.code;
      resources ()
  | Ok implementation -> (
      match Typedtree_lowering.lower implementation with
      | Ok _ -> fail "negative symbolic fixture unexpectedly lowered"
      | Error diagnostic ->
          Printf.printf "rejected=adapter code=%s sst=0 vir=0 vc=0\n"
            diagnostic.Diagnostic.code;
          resources ())

let wrong_artifact target donor =
  reset_resources ();
  let target = load target and donor = load donor in
  let artifact = Typedtree_adapter_private.Public.proof_capture_artifact donor in
  let result =
    Typedtree_adapter_private.Public.lower_with_capture_artifact
      ~proof_capture_artifact:artifact
      ~compilation_identity:
        (Callback_certificate_private.cmt_compilation_identity target)
      ~source_file:target.Cmt_input.source_file ~imports:target.imports
      target.structure
  in
  (match result with
  | Ok _ -> fail "wrong symbolic artifact unexpectedly authenticated"
  | Error diagnostic ->
      Printf.printf "wrong-artifact code=%s sst=0 vir=0 vc=0\n"
        diagnostic.Diagnostic.code);
  resources ()

let wrong_use filename attack source_name =
  reset_resources ();
  let implementation = load filename in
  let artifact =
    Typedtree_adapter_private.Public.proof_capture_artifact implementation
  in
  let changed = ref false in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.Typedtree.exp_desc with
          | Typedtree.Texp_ident (path, name, description, kind, use)
            when
              (not !changed)
              && String.equal (Path.name path) source_name ->
              let path, description =
                if String.equal attack "uid" then
                  ( path,
                    {
                      description with
                      Types.val_uid = Shape.Uid.internal_not_actually_unique;
                    } )
                else if String.equal attack "path" then
                  (Path.Pident (Ident.create_local source_name), description)
                else fail "unknown use attack %s" attack
              in
              Obj.set_field (Obj.repr expression) 0
                (Obj.repr
                   (Typedtree.Texp_ident
                      (path, name, description, kind, use)));
              changed := true
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator implementation.structure;
  if not !changed then fail "no symbolic use named %s" source_name;
  let result =
    Typedtree_adapter_private.Public.lower_with_capture_artifact
      ~proof_capture_artifact:artifact
      ~compilation_identity:
        (Callback_certificate_private.cmt_compilation_identity implementation)
      ~source_file:implementation.Cmt_input.source_file
      ~imports:implementation.imports implementation.structure
  in
  (match result with
  | Ok _ -> fail "wrong symbolic %s unexpectedly authenticated" attack
  | Error diagnostic ->
      Printf.printf "wrong-%s code=%s sst=0 vir=0 vc=0\n" attack
        diagnostic.Diagnostic.code);
  resources ()

let wrong_program filename =
  reset_resources ();
  let original = (input filename).program in
  let copied = { original with Sst.functions = original.functions } in
  (match Sst_validation.validate copied with
  | Ok _ -> fail "copied symbolic program unexpectedly authenticated"
  | Error _ -> print_endline "wrong-program rejected=true sst=0 vir=0 vc=0");
  resources ()

let declaration_identities filename =
  let input = input filename in
  input.program.functions
  |> List.filter_map (fun definition ->
         match definition.Sst.body with
         | Sst.Symbolic_declaration _ -> (
             match
               Symbolic_declaration_private.authenticate ~program:input.program
                 ~definition
             with
             | Ok declaration -> Some (definition, declaration)
             | Error message -> fail "%s" message)
         | Sst.Checked_exec _ | Sst.Spec_definition _
         | Sst.Recursive_spec_definition _ | Sst.Proof_body _
         | Sst.External_specification _ | Sst.Trusted_external_spec_target _
         | Sst.Trusted_external_body _ ->
             None)
  |> List.iter (fun (definition, declaration) ->
         Printf.printf "declaration name=%s path=%s authenticated=true\n"
           definition.Sst.function_id.function_name
           (Symbolic_application_private.canonical_path declaration))

let wrong_binding filename source_name =
  reset_resources ();
  let implementation = load filename in
  let artifact =
    Typedtree_adapter_private.Public.proof_capture_artifact implementation
  in
  let changed = ref false in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      value_binding =
        (fun self binding ->
          (match binding.Typedtree.vb_pat.pat_desc with
          | Typedtree.Tpat_var (_, name, uid, sort, mode)
            when
              (not !changed)
              && String.equal name.txt source_name
              && List.exists
                   (fun attribute ->
                     String.equal attribute.Parsetree.attr_name.txt
                       "verocaml.internal.symbolic.declaration.v1")
                   binding.vb_attributes ->
              Obj.set_field (Obj.repr binding.vb_pat) 0
                (Obj.repr
                   (Typedtree.Tpat_var
                      (Ident.create_local source_name, name, uid, sort, mode)));
              changed := true
          | _ -> ());
          default.value_binding self binding);
    }
  in
  iterator.structure iterator implementation.structure;
  if not !changed then fail "no symbolic binding named %s" source_name;
  let result =
    Typedtree_adapter_private.Public.lower_with_capture_artifact
      ~proof_capture_artifact:artifact
      ~compilation_identity:
        (Callback_certificate_private.cmt_compilation_identity implementation)
      ~source_file:implementation.Cmt_input.source_file
      ~imports:implementation.imports implementation.structure
  in
  (match result with
  | Ok _ -> fail "rebound symbolic carrier unexpectedly authenticated"
  | Error diagnostic ->
      Printf.printf "wrong-binding code=%s sst=0 vir=0 vc=0\n"
        diagnostic.Diagnostic.code);
  resources ()

let descriptor = function
  | Vir.Integer_application (Integer_symbolic_application application)
  | Boolean_application (Boolean_symbolic_application application)
  | Aggregate_application
      { aggregate_desc = Aggregate_symbolic_application application; _ }
  | Parametric_application
      { parametric_desc = Parametric_symbolic_application application; _ } ->
      application
  | Integer_application _ | Boolean_application _ | Aggregate_application _
  | Parametric_application _ ->
      fail "trigger head is not descriptor-backed symbolic application"

let argument_symbol = function
  | Vir.Recursive_integer_argument (Integer_symbol symbol)
  | Recursive_boolean_argument (Boolean_symbol symbol)
  | Recursive_aggregate_argument
      { aggregate_desc = Aggregate_symbol symbol; _ }
  | Recursive_parametric_argument
      { parametric_desc = Parametric_symbol symbol; _ } ->
      symbol
  | Recursive_integer_argument _ | Recursive_boolean_argument _
  | Recursive_aggregate_argument _ | Recursive_parametric_argument _ ->
      fail "symbolic trigger argument is not the corresponding binder symbol"

let same_argument left right =
  let left = argument_symbol left and right = argument_symbol right in
  left.Vir.symbol_id = right.symbol_id
  && left.source_name = right.source_name
  && left.sort = right.sort

let rec argument_descriptors = function
  | Vir.Recursive_integer_argument term -> integer_descriptors term
  | Recursive_boolean_argument term -> boolean_descriptors term
  | Recursive_aggregate_argument term -> aggregate_descriptors term
  | Recursive_parametric_argument term -> parametric_descriptors term

and arguments_descriptors arguments =
  List.concat_map argument_descriptors arguments

and integer_descriptors = function
  | Vir.Integer_constant _ | Integer_symbol _ -> []
  | Integer_add (left, right) | Integer_subtract (left, right) ->
      integer_descriptors left @ integer_descriptors right
  | Integer_negate value | Integer_multiply_constant (_, value)
  | Integer_absolute_value value ->
      integer_descriptors value
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_descriptors condition
      @ integer_descriptors consequent
      @ integer_descriptors alternative
  | Integer_rank_project (_, value) | Aggregate_tag (_, value)
  | Integer_selector (_, value) ->
      aggregate_descriptors value
  | Integer_recursive_spec_application { arguments; _ } ->
      arguments_descriptors arguments
  | Integer_symbolic_application application ->
      application
      :: arguments_descriptors
           (Symbolic_application_private.arguments application)

and aggregate_descriptors term =
  match term.Vir.aggregate_desc with
  | Aggregate_symbol _ -> []
  | Aggregate_selector (_, source) -> aggregate_descriptors source
  | Aggregate_constructor { arguments; _ }
  | Aggregate_recursive_spec_application { arguments; _ }
  | Aggregate_imported_model_application { arguments; _ } ->
      arguments_descriptors arguments
  | Aggregate_record { fields; _ } ->
      List.concat_map (fun (_, argument) -> argument_descriptors argument) fields
  | Aggregate_conditional (condition, consequent, alternative) ->
      boolean_descriptors condition
      @ aggregate_descriptors consequent
      @ aggregate_descriptors alternative
  | Aggregate_symbolic_application application ->
      application
      :: arguments_descriptors
           (Symbolic_application_private.arguments application)

and parametric_descriptors term =
  match term.Vir.parametric_desc with
  | Parametric_symbol _ -> []
  | Parametric_selector (_, source) -> aggregate_descriptors source
  | Parametric_conditional (condition, consequent, alternative) ->
      boolean_descriptors condition
      @ parametric_descriptors consequent
      @ parametric_descriptors alternative
  | Parametric_symbolic_application application ->
      application
      :: arguments_descriptors
           (Symbolic_application_private.arguments application)

and boolean_descriptors = function
  | Vir.Logical_adt_schema _ | Boolean_constant _ | Boolean_symbol _ -> []
  | Boolean_not value -> boolean_descriptors value
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_descriptors left @ boolean_descriptors right
  | Forall_term quantifier | Exists_term quantifier ->
      boolean_descriptors quantifier.boolean_quantifier_body
      @ Option.fold ~none:[]
          ~some:(function
            | Vir.Integer_application term -> integer_descriptors term
            | Vir.Boolean_application term -> boolean_descriptors term
            | Vir.Aggregate_application term -> aggregate_descriptors term
            | Vir.Parametric_application term -> parametric_descriptors term)
          quantifier.boolean_quantifier_trigger
  | Integer_compare (_, left, right) ->
      integer_descriptors left @ integer_descriptors right
  | Boolean_selector (_, value)
  | Boolean_invariant_application { value; _ } ->
      aggregate_descriptors value
  | Aggregate_equal (left, right) ->
      aggregate_descriptors left @ aggregate_descriptors right
  | Parametric_equal (left, right) ->
      parametric_descriptors left @ parametric_descriptors right
  | Boolean_recursive_spec_application { arguments; _ }
  | Boolean_specification_application { arguments; _ } ->
      arguments_descriptors arguments
  | Boolean_symbolic_application application ->
      application
      :: arguments_descriptors
           (Symbolic_application_private.arguments application)
  | Callback_requires application ->
      arguments_descriptors application.arguments
  | Callback_ensures { application; result } ->
      arguments_descriptors application.arguments @ argument_descriptors result

let expected_vector expected = function
  | [] when String.equal expected "integer" || String.equal expected "boolean"
    ->
      true
  | [ Parametric_type.Int ] when String.equal expected "aggregate" -> true
  | [ Parameter binder ] when String.equal expected "parametric" ->
      binder.ordinal = 0
      && String.equal binder.owner.owner_name "parametric_trigger"
  | _ -> false

let trigger_structure filename =
  let authenticated = input filename in
  let _, _, observations = run filename in
  let expected =
    [
      ("integer", "integer_trigger", "integer_head");
      ("boolean", "boolean_trigger", "boolean_head");
      ("aggregate", "aggregate_trigger", "aggregate_head");
      ("parametric", "parametric_trigger", "parametric_head");
    ]
  in
  let inspect (sort, function_name, declaration_name) =
    let observation =
      match
        List.filter
          (fun observation ->
            String.equal observation.obligation.Vir.function_ref.function_name
              function_name)
          observations
      with
      | [ observation ] -> observation
      | [] | _ :: _ :: _ ->
          fail "%s does not have exactly one observed obligation" function_name
    in
    let terms =
      observation.obligation.assumptions
      @ observation.obligation.required_preceding_safety
      @ observation.obligation.path_condition
      @ [ observation.obligation.goal ]
    in
    let quantifier =
      match List.filter_map (function Vir.Forall_term q -> Some q | _ -> None) terms with
      | [ quantifier ] -> quantifier
      | [] | _ :: _ :: _ ->
          fail "%s does not carry exactly one direct universal" function_name
    in
    let trigger =
      match quantifier.boolean_quantifier_trigger with
      | Some trigger -> trigger
      | None -> fail "%s trigger is absent" function_name
    in
    let application = descriptor trigger in
    let declaration = Symbolic_application_private.declaration application in
    if Symbolic_application_private.is_nullary application then
      fail "%s symbolic trigger became nullary" function_name;
    if
      not
        (String.equal
           (Symbolic_application_private.declaration_name declaration)
           declaration_name
        && String.equal
             (Symbolic_application_private.canonical_path declaration)
             declaration_name)
    then fail "%s declaration identity changed" function_name;
    let definition =
      match
        List.find_opt
          (fun definition ->
            definition.Sst.function_id.function_index
            = Symbolic_application_private.declaration_index declaration
            && String.equal definition.function_id.function_name declaration_name)
          authenticated.program.functions
      with
      | Some definition -> definition
      | None -> fail "%s declaration is absent" declaration_name
    in
    let issued =
      match
        Symbolic_declaration_private.authenticate ~program:authenticated.program
          ~definition
      with
      | Ok declaration -> declaration
      | Error message -> fail "%s" message
    in
    if not (Symbolic_application_private.same_declaration declaration issued)
    then fail "%s trigger declaration is not the issued identity" function_name;
    let type_arguments =
      Symbolic_application_private.type_arguments application
    in
    if not (expected_vector sort type_arguments) then
      fail "%s trigger type vector changed" function_name;
    let bodies =
      boolean_descriptors quantifier.boolean_quantifier_body
      |> List.filter (fun body ->
             Symbolic_application_private.same_head application body)
    in
    if bodies = [] then
      fail "%s trigger body lost its corresponding symbolic application"
        function_name;
    List.iter
      (fun body ->
        if
          not
            (Symbolic_application_private.same_declaration declaration
               (Symbolic_application_private.declaration body)
            && List.equal Parametric_type.equal type_arguments
                 (Symbolic_application_private.type_arguments body)
            && List.equal same_argument
                 (Symbolic_application_private.arguments application)
                 (Symbolic_application_private.arguments body))
        then fail "%s trigger/body identity or argument order changed" function_name)
      bodies;
    let binders =
      List.map (fun symbol -> symbol.Vir.symbol_id)
        quantifier.boolean_quantifier_binders
      |> List.sort_uniq Int.compare
    in
    let used =
      Vir.application_term_symbol_ids trigger |> List.sort_uniq Int.compare
    in
    if binders <> used then
      fail "%s trigger binder coverage is not exact" function_name;
    Printf.printf
      "trigger sort=%s declaration=%s vector=exact arguments=ordered binders=complete authenticated=true\n"
      sort declaration_name
  in
  List.iter inspect expected;
  Symbolic_declaration_private.destroy authenticated.program

let imported_issuance () =
  let span = Diagnostic.file_span "imported_symbolic_test.ml" in
  let function_id : Sst.function_id =
    { function_index = 701; function_name = "Imported_symbol.image" }
  in
  let declaration () =
    match
      Symbolic_application_private.declare ~marker_id:"provider-marker"
        ~declaration_index:function_id.function_index
        ~declaration_name:function_id.function_name
        ~canonical_path:"Provider.image" ~value_uid:"Provider.image.uid"
        ~source_file:"provider.ml" ~compilation_identity:"provider-cmt"
        ~declaration_span:span ~type_binders:[]
        ~parameter_types:[ Sst.Int ] ~result_type:Sst.Int
    with
    | Ok declaration -> declaration
    | Error message -> fail "%s" message
  in
  let issued = declaration () in
  let definition : Sst.function_definition =
    {
      function_id;
      type_binders = [];
      mode = Sst.Spec;
      recursive = false;
      parameters =
        [
          Sst.Value_parameter
            {
              label = None;
              pattern = { pattern_desc = Sst.Wildcard; typ = Sst.Int; span };
              optional_default = None;
            };
        ];
      contracts = Sst.empty_contracts;
      body = Sst.Symbolic_declaration issued;
      policy = Sst.Default_linear_z3;
      result_type = Sst.Int;
      returns_unique_parameter = None;
      span;
    }
  in
  let program : Sst.program =
    {
      policy = Sst.Default_linear_z3;
      parametric_adts = [];
      types = [];
      functions = [];
    }
  in
  let expect_error name = function
    | Ok _ -> fail "%s unexpectedly succeeded" name
    | Error _ -> ()
  in
  (match
     Symbolic_declaration_private.authenticate_imported
       ~canonical_path:"Provider.image" ~value_uid:"Provider.image.uid"
       definition
   with
  | Ok authenticated when authenticated == issued -> ()
  | Ok _ -> fail "imported authentication copied the declaration identity"
  | Error message -> fail "%s" message);
  expect_error "wrong imported path"
    (Symbolic_declaration_private.authenticate_imported
       ~canonical_path:"Provider.other" ~value_uid:"Provider.image.uid"
       definition);
  expect_error "wrong imported UID"
    (Symbolic_declaration_private.authenticate_imported
       ~canonical_path:"Provider.image" ~value_uid:"Provider.other.uid"
       definition);
  (match
     Symbolic_declaration_private.seal ~program
       ~imported_definitions:[ definition ]
   with
  | Ok () -> ()
  | Error message -> fail "%s" message);
  let argument : Sst.expression =
    { expression_desc = Sst.Int_constant (Z.of_int 7); typ = Sst.Int; span }
  in
  let application declaration =
    match
      Symbolic_declaration_private.application ~declaration ~type_arguments:[]
        ~arguments:[ argument ] ~result_type:Sst.Int ~span
    with
    | Ok application -> application
    | Error message -> fail "%s" message
  in
  (match
     Symbolic_declaration_private.validate_application ~program ~logical:true
       ~expression_type:Sst.Int (application issued)
   with
  | Ok [ authenticated_argument ] when authenticated_argument == argument -> ()
  | Ok _ -> fail "imported symbolic application arguments changed"
  | Error message -> fail "%s" message);
  expect_error "executable imported symbolic use"
    (Symbolic_declaration_private.validate_application ~program ~logical:false
       ~expression_type:Sst.Int (application issued));
  expect_error "unissued equal imported declaration"
    (Symbolic_declaration_private.validate_application ~program ~logical:true
       ~expression_type:Sst.Int (application (declaration ())));
  let duplicate_program = { program with Sst.functions = [] } in
  expect_error "duplicate imported symbolic identity"
    (Symbolic_declaration_private.seal ~program:duplicate_program
       ~imported_definitions:[ definition; definition ]);
  Symbolic_declaration_private.destroy program;
  Symbolic_declaration_private.destroy duplicate_program;
  print_endline
    "imported-symbolic path=exact uid=exact issuance=consumer-bound ambiguity=rejected"

let () =
  match Array.to_list Sys.argv with
  | [ _; "structural"; filename ] -> structural filename
  | [ _; "semantic"; filename ] -> semantic filename
  | [ _; "route-parity"; filename ] -> route_parity filename
  | [ _; "diagnostic"; filename ] -> diagnostic filename
  | [ _; "declarations"; filename ] -> declaration_identities filename
  | [ _; "trigger-structure"; filename ] -> trigger_structure filename
  | [ _; "imported-issuance" ] -> imported_issuance ()
  | [ _; "wrong-artifact"; target; donor ] -> wrong_artifact target donor
  | [ _; "wrong-use"; filename; attack; source_name ] ->
      wrong_use filename attack source_name
  | [ _; "wrong-binding"; filename; source_name ] ->
      wrong_binding filename source_name
  | [ _; "wrong-program"; filename ] -> wrong_program filename
  | _ ->
      fail
        "usage: symbolic_specifications_tool \
         (structural|semantic|route-parity|diagnostic|declarations|trigger-structure|imported-issuance|wrong-artifact|wrong-use|wrong-binding|wrong-program) \
         CMT [DONOR]"
