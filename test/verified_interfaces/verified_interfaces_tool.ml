let () = ignore Verified_interfaces_prerequisites.ready

let fail format = Printf.ksprintf failwith format

let require condition format =
  Printf.ksprintf (fun message -> if not condition then failwith message) format

let authenticate consumer dependency =
  match
    Interface_specification.authenticate ~timeout_ms:60_000
      ~dependency_files:[ dependency ]
      ~consumer_file:consumer
  with
  | Ok environment -> environment
  | Error error -> fail "%s" (Interface_specification.error_to_string error)

let callable_name callable =
  (Interface_specification.public_callable_id callable).Sst.function_name

let require_named name values project label =
  match List.find_opt (fun value -> String.equal (project value) name) values with
  | Some value -> value
  | None -> fail "missing %s %s" label name

let invalid_handle_rejects handle =
  match Marshal.to_string handle [] with
  | _ -> fail "process-local handle was serializable"
  | exception Invalid_argument _ -> ()

let type_id_name type_id = type_id.Sst.type_name

let public_identity_checks types callables models =
  let type_names =
    List.map
      (fun descriptor ->
        Interface_specification.public_type_id descriptor |> type_id_name)
      types
  in
  let revealed_names =
    List.filter_map
      (fun descriptor ->
        Option.map
          (fun _ ->
            Interface_specification.public_type_id descriptor |> type_id_name)
          (Interface_specification.public_type_kind descriptor))
      types
  in
  let callable_names = List.map callable_name callables in
  let rec public_typ = function
    | Sst.Unit | Bool | Int -> true
    | Tuple components ->
        List.for_all (fun (_, typ) -> public_typ typ) components
    | Aggregate type_id -> List.mem (type_id_name type_id) type_names
    | Parameter _ | Application _ -> false
  in
  let public_binding (binding : Sst.binding) = public_typ binding.typ in
  let public_constructor (constructor : Sst.constructor_id) =
    List.mem
      (type_id_name constructor.constructor_type)
      revealed_names
  in
  let public_field (field : Sst.field_id) =
    match field.field_owner with
    | Sst.Record_owner owner ->
        List.mem (type_id_name owner) revealed_names
    | Constructor_owner constructor -> public_constructor constructor
  in
  let rec public_cursor (cursor : Sst.owned_tree_cursor) =
    public_binding cursor.Sst.cursor_binding
    && public_binding cursor.root
    && List.for_all
         (function
           | Sst.Owned_tree_field field -> public_field field
           | Owned_tree_constructor constructor ->
               public_constructor constructor)
         cursor.guarded_path
  and public_transition (transition : Sst.owned_tree_transition) =
    public_binding transition.Sst.root
    && Option.fold ~none:true ~some:public_cursor transition.cursor
    && public_field transition.target_field
    &&
    (match transition.rhs_provenance with
    | Sst.Ground_owned_tree_value -> true
    | Guarded_descendant_move cursor -> public_cursor cursor)
    && List.for_all
         (function
           | Sst.Reconstruct_record
               { record_type; changed_field; preserved_fields } ->
               List.mem (type_id_name record_type) revealed_names
               && public_field changed_field
               && List.for_all public_field preserved_fields
           | Reconstruct_constructor constructor ->
               public_constructor constructor)
         transition.reconstruction
  and public_pattern (pattern : Sst.pattern) =
    public_typ pattern.Sst.typ
    &&
    match pattern.pattern_desc with
    | Sst.Wildcard | Int_pattern _ | Bool_pattern _ | Unit_pattern -> true
    | Bind binding -> public_binding binding
    | Owned_tree_cursor_pattern cursor -> public_cursor cursor
    | Tuple_pattern patterns ->
        List.for_all (fun (_, pattern) -> public_pattern pattern) patterns
    | Record_pattern fields ->
        List.for_all
          (fun (field, pattern) ->
            public_field field && public_pattern pattern)
          fields
    | Constructor_pattern (constructor, patterns) ->
        public_constructor constructor
        && List.for_all public_pattern patterns
    | Or_pattern (left, right) ->
        public_pattern left && public_pattern right
  and public_expression (expression : Sst.expression) =
    public_typ expression.Sst.typ
    &&
    match expression.expression_desc with
    | Sst.Int_constant _ | Bool_constant _ | Unit_constant -> true
    | Optional_absent | Optional_present _ | Optional_forward _ -> false
    | Variable { binding; _ } | Mutable_read binding ->
        public_binding binding
    | Tuple_value values ->
        List.for_all (fun (_, value) -> public_expression value) values
    | Record_value { record_type; fields } ->
        List.mem (type_id_name record_type) revealed_names
        && List.for_all
             (fun (field, value) ->
               public_field field && public_expression value)
             fields
    | Constructor_value { constructor; arguments } ->
        public_constructor constructor
        && List.for_all public_expression arguments
    | Field_read { record; field } ->
        public_field field && public_expression record
    | Field_write { provenance; field; value; transition } ->
        public_binding provenance.root
        && public_field field
        && public_expression value
        && Option.fold ~none:true ~some:public_transition transition
    | Shared_scalar_field_write
        { provenance; field; value; transition } ->
        public_binding provenance.root
        && public_field field
        && public_expression value
        && List.for_all public_binding transition.shared_formal_roots
        && public_binding transition.shared_target
        && public_binding transition.shared_canonical_root
        && List.for_all public_binding transition.shared_alias_chain
        && List.mem
             (type_id_name transition.shared_record_type)
             revealed_names
        && public_field transition.shared_target_field
    | Owned_tree_nested_write { transition; value } ->
        public_transition transition && public_expression value
    | Owned_tree_rebase { transition } -> public_transition transition
    | Let_mutable (binding, initial, body) ->
        public_binding binding
        && public_expression initial
        && public_expression body
    | Mutable_write { provenance; value } ->
        public_binding provenance.root && public_expression value
    | Let (bindings, body) ->
        List.for_all
          (fun (pattern, value) ->
            public_pattern pattern && public_expression value)
          bindings
        && public_expression body
    | Sequence (left, right) | Boolean_binary (_, left, right)
    | Compare (_, left, right) ->
        public_expression left && public_expression right
    | If (condition, yes, no) ->
        public_expression condition
        && public_expression yes
        && Option.fold ~none:true ~some:public_expression no
    | Match (scrutinee, cases) ->
        public_expression scrutinee
        && List.for_all
             (fun case ->
               public_pattern case.Sst.case_pattern
               && Option.fold ~none:true ~some:public_expression
                    case.case_guard
               && public_expression case.case_body)
             cases
    | Checked_arithmetic (_, arguments) ->
        List.for_all public_expression arguments
    | Boolean_not operand | Proof_region operand | Old operand ->
        public_expression operand
    | Forall quantifier | Exists quantifier ->
        public_binding quantifier.quantifier_binder
        && public_expression quantifier.quantifier_body
        && Option.fold ~none:true ~some:public_expression
             quantifier.quantifier_trigger
    | Direct_call { callee; arguments; _ } ->
        List.mem callee.Sst.function_name callable_names
        && List.for_all
             (fun argument ->
               public_expression (snd (Sst.require_value_argument argument)))
             arguments
    | Symbolic_application _ ->
        false
    | Callback_call _ | Callback_requires _ | Callback_ensures _
    | Reveal _ | Reveal_with_fuel _ | Use_type_invariant _
    | Local_assert _ ->
        false
  in
  let public_field_definition field =
    public_field field.Sst.field_id && public_typ field.field_type
  in
  List.iter
    (fun descriptor ->
      match Interface_specification.public_type_kind descriptor with
      | None -> ()
      | Some (Sst.Record_definition fields) ->
          require (List.for_all public_field_definition fields)
            "public record type contains a private nested identity"
      | Some (Variant_definition constructors) ->
          require
            (List.for_all
               (fun constructor ->
                 public_constructor constructor.Sst.constructor_id
                 && List.for_all public_field_definition
                      constructor.constructor_fields)
               constructors)
            "public variant type contains a private nested identity")
    types;
  List.iter
    (fun callable ->
      require
        (List.for_all public_typ
           (Interface_specification.public_callable_parameter_types callable))
        "public callable parameter contains a private nested identity";
      require
        (public_typ
           (Interface_specification.public_callable_result_type callable))
        "public callable result contains a private nested identity";
      let contract =
        Interface_specification.public_callable_contract callable
      in
      List.iter
        (fun clause ->
          require
            (public_expression
               (Interface_specification.clause_expression clause))
            "public contract contains a private nested identity")
        (Interface_specification.contract_requires contract
        @ Interface_specification.contract_ensures contract
        @ Interface_specification.contract_decreases contract))
    callables;
  List.iter
    (fun model ->
      require
        (List.mem
           (Interface_specification.public_model_domain model |> type_id_name)
           type_names)
        "public model domain contains a private identity";
      require
        (public_typ
           (Interface_specification.public_model_result_type model))
        "public model result contains a private nested identity")
    models

let query_interface consumer dependency =
  let environment = authenticate consumer dependency in
  let handles = Interface_specification.handles environment in
  require (List.length handles = 1) "expected one verified handle";
  let handle = List.hd handles in
  require (Interface_specification.handle_is_authentic handle)
    "issued handle is not authentic";
  require
    (String.length
       (Interface_specification.handle_mode_signature_digest handle)
    > 0)
    "mode-bearing signature digest is absent";
  require
    (String.equal
       (Interface_specification.handle_unit_name handle)
       "Model_dependency")
    "wrong dependency unit";
  let types = Interface_specification.public_types handle in
  let abstract_stack =
    require_named "Stack.t" types
      (fun descriptor ->
        (Interface_specification.public_type_id descriptor).Sst.type_name)
      "type"
  in
  require
    (Interface_specification.public_type_visibility abstract_stack
    = Interface_specification.Abstract)
    "Stack.t is not opaque";
  require
    (Interface_specification.public_type_kind abstract_stack = None)
    "Stack.t exposed its hidden representation";
  require
    (not (List.mem "Stack.node"
            (List.map
               (fun descriptor ->
                 Interface_specification.public_type_id descriptor
                 |> type_id_name)
               types)))
    "private Stack.node was exported";
  let callables = Interface_specification.public_callables handle in
  let stack_model =
    require_named "Stack.model" callables callable_name "callable"
  in
  require
    (Interface_specification.public_callable_parameter_modes stack_model
    = [ Sst.Ghost_instance ]
    && Interface_specification.public_callable_result_mode stack_model
       = Sst.Ghost_instance)
    "retained specification mode signature mismatch";
  let bounded = require_named "bounded" callables callable_name "callable" in
  require
    (Interface_specification.public_callable_parameter_modes bounded
    = [ Sst.Exec_instance ]
    && Interface_specification.public_callable_result_mode bounded
       = Sst.Exec_instance)
    "retained executable mode signature mismatch";
  let contract = Interface_specification.public_callable_contract bounded in
  require
    (List.length (Interface_specification.contract_requires contract) = 1)
    "bounded requires clause missing";
  require
    (List.length (Interface_specification.contract_ensures contract) = 1)
    "bounded ensures clause missing";
  let models = Interface_specification.public_models handle in
  require (List.length models = 1) "expected exactly one public model";
  let model = List.hd models in
  require
    (String.equal
       (callable_name (Interface_specification.public_model_callable model))
       "Stack.model")
    "wrong model callable";
  require
    (Interface_specification.public_model_domain model
    = Interface_specification.public_type_id abstract_stack)
    "model domain identity mismatch";
  let invariants = Interface_specification.public_invariants handle in
  require (List.length invariants = 1)
    "expected exactly one authenticated public invariant";
  let invariant = List.hd invariants in
  require
    (Interface_specification.public_invariant_abstract_type invariant
    = Interface_specification.public_type_id abstract_stack)
    "invariant abstract type identity mismatch";
  require
    (String.equal
       (Interface_specification.public_invariant_model invariant).function_name
       "Stack.model")
    "invariant model identity mismatch";
  require
    (String.equal
       (Interface_specification.public_invariant_predicate invariant).function_name
       "Stack.invariant")
    "invariant predicate identity mismatch";
  require
    (List.for_all
       (fun (operation, _) ->
         List.exists
           (fun callable ->
             Interface_specification.public_callable_id callable = operation)
           callables)
       (Interface_specification.public_invariant_operations invariant))
    "invariant operation snapshot contains a non-public identity";
  public_identity_checks types callables models;
  invalid_handle_rejects handle;
  print_endline
    "verified interface mode queries, invariant metadata, opacity, and \
     serialization checks passed"

let make_import unit_name crc =
  let name = Compilation_unit.Name.of_string unit_name in
  let crc_with_unit =
    Option.map
      (fun crc ->
        (Compilation_unit.of_string unit_name, Digest.from_hex crc))
      crc
  in
  Import_info.create name ~crc_with_unit

let rewrite_cmt input output transform =
  match Cmt_format.read input with
  | Some cmi, Some cmt ->
      let cmi =
        {
          cmi with
          Cmi_format.cmi_crcs = transform cmi.Cmi_format.cmi_crcs;
        }
      in
      let cmt =
        {
          cmt with
          Cmt_format.cmt_imports = transform cmt.Cmt_format.cmt_imports;
        }
      in
      let channel = open_out_bin output in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () ->
          ignore (Cmi_format.output_cmi output channel cmi);
          output_string channel Config.cmt_magic_number;
          Marshal.to_channel channel cmt [])
  | None, _ | _, None -> fail "%s has no embedded CMI/CMT pair" input

let add_import input output unit_name crc =
  rewrite_cmt input output (fun imports ->
      require
        (not
           (Array.exists
              (fun import ->
                Compilation_unit.Name.to_string (Import_info.name import)
                = unit_name)
              imports))
        "import %s already exists" unit_name;
      Array.append imports [| make_import unit_name (Some crc) |])

let remove_import input output unit_name =
  rewrite_cmt input output (fun imports ->
      Array.to_list imports
      |> List.filter (fun import ->
             Compilation_unit.Name.to_string (Import_info.name import)
             <> unit_name)
      |> Array.of_list)

let set_import input output unit_name crc =
  rewrite_cmt input output (fun imports ->
      let replaced = ref 0 in
      let imports =
        Array.map
          (fun import ->
            if
              Compilation_unit.Name.to_string (Import_info.name import)
              = unit_name
            then (
              incr replaced;
              make_import unit_name crc)
            else import)
          imports
      in
      require (!replaced = 1) "expected one import slot for %s" unit_name;
      imports)

let interface_digest input =
  match Cmt_format.read input with
  | _, Some { Cmt_format.cmt_interface_digest = Some digest; _ } ->
      print_endline (Digest.to_hex digest)
  | _ -> fail "%s has no interface digest" input

let contains ~needle value =
  let needle_length = String.length needle in
  let rec loop offset =
    offset + needle_length <= String.length value
    && (String.sub value offset needle_length = needle
       || loop (offset + 1))
  in
  needle_length = 0 || loop 0

let reset_aggregate_observations () =
  Imported_callable.For_testing.reset_aggregate_lifecycle ();
  Recursive_spec_encoding.For_testing.reset_recursive_lowering_count ();
  Symbolic_executor_private.For_testing.reset_authority_observation ();
  Symbolic_executor_private.For_testing
  .reset_aggregate_recursive_route_observations ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

let private_driver_report report : Verification_driver_private.report =
  Obj.obj (Obj.field (Obj.repr report) 0)

let require_zero_authority counters =
  require
    (counters.Verification_session.callee_solver_attempts = 0
    && counters.dependent_lowerings = 0
    && counters.dependent_backend_contexts = 0
    && counters.dependent_solver_attempts = 0
    && counters.receipts_issued = 0
    && counters.receipts_consumed = 0
    && counters.finite_witness_issuances = 0
    && counters.finite_parent_issuances = 0
    && counters.finite_child_derivations = 0
    && counters.finite_result_promotions = 0
    && counters.finite_result_manifests = 0
    && counters.finite_result_completions = 0
    && counters.finite_result_consumption_attempts = 0
    && counters.finite_result_consumptions = 0
    && counters.finite_consumptions = 0
    && counters.finite_formal_assumption_issuances = 0
    && counters.finite_formal_transfer_batches = 0
    && counters.finite_formal_transfers = 0
    && counters.finite_formal_transfer_consumptions = 0
    && counters.recursive_spec_result_issuances = 0
    && counters.recursive_spec_result_consumptions = 0
    && counters.owned_root_scalar_plans_issued = 0
    && counters.transition_predecessor_transfers = 0
    && counters.invariant_cell_entry_eligibilities = 0
    && counters.frozen_constructor_template_issuances = 0)
    "retained aggregate consumer acquired finite, recursive, invariant, \
     transfer, ownership, or solver authority"

let verify_retained consumer dependency =
  match
    Interface_specification.verify_consumer ~timeout_ms:60_000
      ~dependency_files:[ dependency ] ~consumer_file:consumer
  with
  | Ok report ->
      let driver = private_driver_report report in
      require
        (Verification_driver_private.status driver
        = Verification_pipeline.Verified)
        "retained consumer did not verify";
      require
        (contains ~needle:"imported-model-application"
           (Verification_driver_private.vir driver |> Vir.to_string))
        "retained aggregate application is absent from VIR";
      let counters = Verification_driver_private.counters driver in
      require_zero_authority counters;
      let vir = Verification_driver_private.vir driver in
      require (vir.Vir.rank_domains = [])
        "retained aggregate consumer imported a rank domain";
      counters
  | Error error -> fail "%s" (Interface_specification.error_to_string error)

let explicit_policy consumer dependency =
  let rlimit = Solver_policy_private.default_rlimit in
  let environment =
    match
      Interface_specification.authenticate_with_rlimit ~timeout_ms:60_000
        ~rlimit ~dependency_files:[ dependency ] ~consumer_file:consumer
    with
    | Ok environment -> environment
    | Error error -> fail "%s" (Interface_specification.error_to_string error)
  in
  require
    (List.length (Interface_specification.handles environment) = 1)
    "explicit provider authentication did not retain one handle";
  match
    Interface_specification.verify_consumer_with_rlimit ~timeout_ms:60_000
      ~rlimit ~dependency_files:[ dependency ] ~consumer_file:consumer
  with
  | Ok report ->
      let driver = private_driver_report report in
      require
        (Verification_driver_private.status driver
        = Verification_pipeline.Verified)
        "explicit consumer verification did not verify";
      print_endline "explicit-policy provider=verified consumer=verified"
  | Error error -> fail "%s" (Interface_specification.error_to_string error)

let lifecycle consumer dependency =
  reset_aggregate_observations ();
  ignore (verify_retained consumer dependency);
  ignore (verify_retained consumer dependency);
  let lifecycle = Imported_callable.For_testing.aggregate_lifecycle () in
  require
    (lifecycle.descriptors_issued = 2
    && lifecycle.applications_admitted = 2
    && lifecycle.descriptors_invalidated = 2)
    "fresh consumer descriptor lifecycle mismatch";
  Printf.printf
    "retained-model-lifecycle consumers=2 descriptors=%d/%d/%d \
     finite-result=0/0/0 finite-transfer=0/0/0 recursive=0/0 \
     aggregate-routes=0/0 solver=0 z3=0/0\n"
    lifecycle.descriptors_issued lifecycle.applications_admitted
    lifecycle.descriptors_invalidated

let failure_teardown consumer dependency =
  reset_aggregate_observations ();
  (match
     Interface_specification.verify_consumer ~timeout_ms:60_000
       ~dependency_files:[ dependency ] ~consumer_file:consumer
  with
  | Ok report ->
      let driver = private_driver_report report in
      require
        (Verification_driver_private.status driver
        = Verification_pipeline.Counterexample)
        "retained failure control did not produce a counterexample"
  | Error error -> fail "%s" (Interface_specification.error_to_string error));
  let lifecycle = Imported_callable.For_testing.aggregate_lifecycle () in
  require
    (lifecycle.descriptors_issued = 1
    && lifecycle.applications_admitted = 1
    && lifecycle.descriptors_invalidated = 1)
    "retained failure path did not invalidate exactly one consumed descriptor";
  print_endline
    "retained-model-failure status=counterexample descriptors=1/1/1 \
     teardown=exactly-once"

let rejected_zero_work label consumer dependency =
  let observations () =
    let lifecycle = Imported_callable.For_testing.aggregate_lifecycle () in
    let authority =
      Symbolic_executor_private.For_testing.authority_observation ()
    in
    let z3 = Z3_bridge.counters () in
    ( lifecycle.descriptors_issued,
      lifecycle.applications_admitted,
      lifecycle.descriptors_invalidated,
      authority.recursive_spec_lowerings,
      authority.recursive_proof_rank_lowerings,
      Recursive_spec_encoding.For_testing.recursive_lowering_count (),
      Symbolic_executor_private.For_testing
      .aggregate_recursive_rank_route_observation_count (),
      Symbolic_executor_private.For_testing
      .aggregate_recursive_argument_route_observation_count (),
      Solver_backend.For_testing.solver_creation_count (),
      z3.contexts_created,
      z3.solvers_created )
  in
  reset_aggregate_observations ();
  (match
     Interface_specification.authenticate ~timeout_ms:60_000
       ~dependency_files:[ dependency ] ~consumer_file:consumer
   with
  | Ok _ -> ()
  | Error error -> fail "%s" (Interface_specification.error_to_string error));
  let provider_only = observations () in
  reset_aggregate_observations ();
  (match
     Interface_specification.verify_consumer ~timeout_ms:60_000
       ~dependency_files:[ dependency ] ~consumer_file:consumer
   with
  | Ok _ -> fail "%s unexpectedly verified" label
  | Error _ -> ());
  require (observations () = provider_only)
    "%s advanced a retained, recursive, backend, or solver counter beyond the \
     authenticated provider baseline"
    label;
  Printf.printf
    "retained-model-rejection=%s descriptor-delta=0/0/0 recursive-delta=0/0 \
     aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0\n"
    label

let generic_adapter_matrix () =
  let module Adapter =
    Typedtree_adapter_private.Public.Imported_specialization_for_testing
  in
  let imported_path = "Generic_model_dependency.snapshot" in
  let exact_definition = imported_path ^ "<int>" in
  let accepts ?(path = imported_path) ?(definition = exact_definition)
      arguments =
    Adapter.accepts ~imported_path:path ~definition_name:definition arguments
  in
  require (not (accepts [ Some "int" ]))
    "textual closed imported specialization authorized a call";
  require (not (accepts [ None ]))
    "open imported specialization was accepted";
  require (not (accepts [ Some "bool" ]))
    "wrong imported specialization was accepted";
  require
    (not
       (accepts
          ~definition:"Stale_generic_model_dependency.snapshot<int>"
          [ Some "int" ]))
    "stale imported type snapshot was accepted";
  require
    (not (accepts [ Some "Foreign_dependency.snapshot<int>" ]))
    "mixed-family imported specialization was accepted";
  print_endline
    "retained-generic-adapter textual-exact=rejected open=rejected wrong=rejected \
     stale=rejected mixed-family=rejected"

let aggregate_backend_matrix () =
  let span = Diagnostic.file_span "<retained-model-backend-matrix>" in
  let domain =
    Vir.{ aggregate_type_index = 630; aggregate_type_name = "Provider.Stack.t" ; aggregate_type_arguments = []}
  in
  let result_type =
    Vir.{ aggregate_type_index = 631; aggregate_type_name = "Provider.snapshot" ; aggregate_type_arguments = []}
  in
  let symbol id name =
    Vir.
      {
        symbol_id = id;
        source_name = name;
        sort = Aggregate domain;
        role = Input;
        span;
      }
  in
  let aggregate_symbol symbol =
    Vir.{ aggregate_type = domain; aggregate_desc = Aggregate_symbol symbol }
  in
  let provider_unit = "Provider" in
  let provider_interface = "interface" in
  let provider_source = "source" in
  let provider_family = "family" in
  let provider_import = "imports" in
  let callable_path = "Provider.Stack.model" in
  let callable_uid = "uid" in
  let summary_digest = "summary" in
  let closure_digest = "closure" in
  let logical_digest interface =
    Digest.to_hex
      (Digest.string
         (String.concat "\000"
            [ provider_unit; interface; provider_source; provider_family;
              provider_import; callable_path; callable_uid; summary_digest;
              closure_digest ]))
  in
  let default_callee =
    { Sst.function_index = 63; function_name = callable_path }
  in
  let application_identity ordinal registration actual =
    let arguments = [ Vir.Recursive_aggregate_argument actual ] in
    let call_snapshot = Printf.sprintf "call-%d" ordinal in
    let application_snapshot =
      Vir.imported_model_application_snapshot ~callee:default_callee ~arguments
        ~result_type ~span
    in
    Imported_callable.For_testing.test_aggregate_application_identity
      ~logical_digest:(logical_digest provider_interface)
      ~call_snapshot ~registration_snapshot:registration
      ~invocation_ordinal:ordinal ~application_snapshot
  in
  let application ?(interface = provider_interface) ?identity
      ?(callee = default_callee) ?call_snapshot
      ?(application_result_type = result_type) ?(application_span = span)
      ordinal registration actual =
    let arguments = [ Vir.Recursive_aggregate_argument actual ] in
    let call_snapshot =
      Option.value ~default:(Printf.sprintf "call-%d" ordinal) call_snapshot
    in
    let application_identity =
      Option.value
        ~default:
          (application_identity ordinal registration actual)
        identity
    in
    Vir.
      {
        aggregate_type = application_result_type;
        aggregate_desc =
          Aggregate_imported_model_application
            {
              callee;
              callable_path;
              callable_uid;
              provider_unit;
              provider_interface = interface;
              provider_source;
              provider_family;
              provider_import;
              summary_digest;
              closure_digest;
              call_snapshot;
              registration_snapshot = registration;
              invocation_ordinal = ordinal;
              application_identity;
              arguments;
              result_type = application_result_type;
              span = application_span;
            };
      }
  in
  let x = symbol 0 "x" and y = symbol 1 "y" in
  let ax = aggregate_symbol x and ay = aggregate_symbol y in
  let left = application 0 "registration-a" ax in
  let right_equal_identity = application 1 "registration-b" ay in
  let right_different = application 2 "registration-c" ay in
  let obligation ?(assumptions = []) goal =
    Vir.
      {
        obligation_index = 0;
        function_ref = { function_index = 63; function_name = "consumer" };
        kind = Local_assertion { local_assertion_ordinal = 0 };
        span;
        assumptions;
        required_preceding_safety = [];
        path_condition = [];
        goal;
        projection_symbols = [];
      }
  in
  let config =
    match Solver_backend.config ~timeout_ms:60_000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  let direct_config =
    Z3_bridge.{ timeout_ms = 60_000; model = true }
  in
  let congruence =
    obligation ~assumptions:[ Vir.Aggregate_equal (ax, ay) ]
      (Vir.Aggregate_equal (left, right_equal_identity))
  in
  (match Solver_backend.solve_obligation config congruence with
  | Ok Solver_backend.Verified -> ()
  | Ok (Counterexample _ | Inconclusive _) | Error _ ->
      fail "ordinary UF congruence did not verify");
  (match
     ( Solver_backend.For_testing.solve_after_translation Unknown config
         congruence,
       Z3_bridge.solve_vir ~controlled:Z3_bridge.Force_unknown direct_config
         congruence )
   with
  | ( Ok
        (Solver_backend.Inconclusive
          { reason = Backend_unknown "controlled unknown"; _ }),
      Ok (Z3_bridge.Inconclusive (Backend_unknown "controlled unknown")) ) ->
      ()
  | _ ->
      fail
        "authentic retained model differed between characterization and direct \
         translation");
  let different =
    obligation (Vir.Aggregate_equal (left, right_different))
  in
  (match Solver_backend.solve_obligation config different with
  | Ok (Solver_backend.Counterexample _) -> ()
  | Ok (Verified | Inconclusive _) | Error _ ->
      fail "different actuals received an invented equality");
  let copied_identity =
    let ordinal = 3 and registration = "copy" in
    let callee =
      { Sst.function_index = 63; function_name = callable_path }
    in
    let arguments = [ Vir.Recursive_aggregate_argument ax ] in
    let call_snapshot = Printf.sprintf "call-%d" ordinal in
    let application_snapshot =
      Vir.imported_model_application_snapshot ~callee ~arguments ~result_type
        ~span
    in
    let identity =
      Imported_callable.For_testing.test_aggregate_application_identity
        ~logical_digest:(logical_digest provider_interface)
        ~call_snapshot ~registration_snapshot:registration
        ~invocation_ordinal:ordinal ~application_snapshot
    in
    Marshal.from_string (Marshal.to_string identity []) 0
  in
  require
    (not
       (Imported_callable.authenticate_aggregate_application_identity
          copied_identity))
    "marshalled retained aggregate application identity remained authentic";
  let stale_identity =
    let ordinal = 3 and registration = "stale" in
    let callee =
      { Sst.function_index = 63; function_name = callable_path }
    in
    let arguments = [ Vir.Recursive_aggregate_argument ax ] in
    let call_snapshot = Printf.sprintf "call-%d" ordinal in
    let application_snapshot =
      Vir.imported_model_application_snapshot ~callee ~arguments ~result_type
        ~span
    in
    Imported_callable.For_testing.test_aggregate_application_identity
      ~logical_digest:(logical_digest provider_interface)
      ~call_snapshot ~registration_snapshot:registration
      ~invocation_ordinal:ordinal ~application_snapshot
  in
  Imported_callable.For_testing.invalidate_aggregate_application_identity
    stale_identity;
  Solver_backend.For_testing.reset_solver_creation_count ();
  let stale =
    obligation
      (Vir.Aggregate_equal
         (left, application ~identity:stale_identity 3 "stale" ax))
  in
  (match Solver_backend.solve_obligation config stale with
  | Error (Solver_backend.Malformed_vir _) -> ()
  | Ok _ | Error _ -> fail "stale retained model identity was not rejected");
  require (Solver_backend.For_testing.solver_creation_count () = 0)
    "stale retained model identity created a solver before rejection";
  Solver_backend.For_testing.reset_solver_creation_count ();
  let forged =
    obligation
      (Vir.Aggregate_equal
         (left, application ~interface:"forged-interface" 3 "forged" ax))
  in
  (match Solver_backend.solve_obligation config forged with
  | Error (Solver_backend.Malformed_vir _) -> ()
  | Ok _ | Error _ -> fail "forged retained model provenance was not rejected");
  require (Solver_backend.For_testing.solver_creation_count () = 0)
    "forged retained model application created a solver before rejection";
  Z3_bridge.reset_counters ();
  (match Z3_bridge.solve_vir direct_config forged with
  | Error (Z3_bridge.Malformed_vir _) -> ()
  | Ok _ | Error _ ->
      fail "direct forged retained model provenance was not rejected");
  let forged_direct = Z3_bridge.counters () in
  require
    (forged_direct.contexts_created = 0
    && forged_direct.solvers_created = 0)
    "direct forged retained model application reached the native solver";
  let nested_actual = application 6 "nested-inner" ax in
  let nested_application = application 7 "nested-outer" nested_actual in
  let nested =
    obligation
      (Vir.Aggregate_equal (nested_application, nested_application))
  in
  Solver_backend.For_testing.reset_solver_creation_count ();
  (match
     Solver_backend.For_testing.solve_after_translation Unknown config nested
   with
  | Error (Solver_backend.Malformed_vir _) -> ()
  | Ok _ | Error _ ->
      fail "nested retained model characterization was not rejected");
  require (Solver_backend.For_testing.solver_creation_count () = 0)
    "nested retained model characterization created a solver";
  Z3_bridge.reset_counters ();
  (match Z3_bridge.solve_vir direct_config nested with
  | Error (Z3_bridge.Malformed_vir _) -> ()
  | Ok _ | Error _ ->
      fail "direct nested retained model application was not rejected");
  let nested_direct = Z3_bridge.counters () in
  require
    (nested_direct.contexts_created = 0
    && nested_direct.solvers_created = 0)
    "direct nested retained model application reached the native solver";
  let authentic_identity =
    application_identity 4 "registration-authentic" ax
  in
  let authentic =
    application ~identity:authentic_identity 4 "registration-authentic" ax
  in
  let expect_substitution label application =
    Solver_backend.For_testing.reset_solver_creation_count ();
    let obligation =
      obligation (Vir.Aggregate_equal (authentic, application))
    in
    (match Solver_backend.solve_obligation config obligation with
    | Error (Solver_backend.Malformed_vir _) -> ()
    | Ok _ | Error _ -> fail "%s provenance substitution was not rejected" label);
    require (Solver_backend.For_testing.solver_creation_count () = 0)
      "%s provenance substitution created a solver" label
  in
  expect_substitution "callee"
    (application ~identity:authentic_identity
       ~callee:
         {
           default_callee with
           Sst.function_index = default_callee.function_index + 1;
         }
       4 "registration-authentic" ax);
  expect_substitution "call-snapshot"
    (application ~identity:authentic_identity
       ~call_snapshot:"substituted-call" 4 "registration-authentic" ax);
  expect_substitution "registration"
    (application ~identity:authentic_identity 4 "substituted-registration" ax);
  expect_substitution "ordinal"
    (application ~identity:authentic_identity ~call_snapshot:"call-4" 5
       "registration-authentic" ax);
  expect_substitution "argument"
    (application ~identity:authentic_identity 4 "registration-authentic" ay);
  let substituted_span =
    Diagnostic.{ span with start_pos = { line = 2; column = 0 } }
  in
  expect_substitution "span"
    (application ~identity:authentic_identity
       ~application_span:substituted_span 4 "registration-authentic" ax);
  let forged_result_type =
    Vir.
      {
        aggregate_type_index = result_type.aggregate_type_index + 1;
        aggregate_type_name = result_type.aggregate_type_name ^ ".forged";
      aggregate_type_arguments = [];
      }
  in
  expect_substitution "result-type"
    (application ~identity:authentic_identity
       ~application_result_type:forged_result_type 4 "registration-authentic"
       ax);
  print_endline
    "retained-model-backend congruence=verified different-actual=counterexample \
     direct-parity=verified call-identities=ignored copy=unauthentic \
     stale=zero-solver-rejected forged-provenance=zero-native-rejected \
     nested=zero-native-rejected substitutions=7x-zero-solver"

let () =
  match Array.to_list Sys.argv with
  | [ _; "interface-digest"; input ] -> interface_digest input
  | [ _; "retained-lifecycle"; consumer; dependency ] ->
      lifecycle consumer dependency
  | [ _; "explicit-policy"; consumer; dependency ] ->
      explicit_policy consumer dependency
  | [ _; "retained-failure-teardown"; consumer; dependency ] ->
      failure_teardown consumer dependency
  | [ _; "retained-reject"; label; consumer; dependency ] ->
      rejected_zero_work label consumer dependency
  | [ _; "retained-backend-matrix" ] -> aggregate_backend_matrix ()
  | [ _; "retained-generic-adapter-matrix" ] -> generic_adapter_matrix ()
  | [ _; "retained-closure-matrix" ] ->
      Imported_callable.For_testing.aggregate_closure_matrix ()
      |> List.iter print_endline
  | [ _; "retained-generic-family-sealing-matrix" ] ->
      Imported_callable.For_testing.generic_family_sealing_matrix ()
      |> List.iter print_endline
  | [ _; "add-import"; input; output; unit_name; crc ] ->
      add_import input output unit_name crc
  | [ _; "remove-import"; input; output; unit_name ] ->
      remove_import input output unit_name
  | [ _; "set-import"; input; output; unit_name; "none" ] ->
      set_import input output unit_name None
  | [ _; "set-import"; input; output; unit_name; crc ] ->
      set_import input output unit_name (Some crc)
  | [ _; consumer; dependency ] -> query_interface consumer dependency
  | _ ->
      fail
        "usage: %s CONSUMER.cmt DEPENDENCY.cmt | interface-digest CMT | \
         retained-lifecycle CONSUMER DEPENDENCY | explicit-policy CONSUMER \
         DEPENDENCY | retained-reject LABEL \
         retained-failure-teardown CONSUMER DEPENDENCY | \
         CONSUMER DEPENDENCY | retained-backend-matrix | \
         retained-generic-adapter-matrix | retained-closure-matrix | \
         retained-generic-family-sealing-matrix | \
         add-import INPUT OUTPUT UNIT CRC | remove-import INPUT OUTPUT UNIT | \
         set-import INPUT OUTPUT UNIT CRC|none"
        Sys.argv.(0)
