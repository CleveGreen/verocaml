type formal_requirement = {
  ordinal : int;
  label : string option;
  pattern_digest : string;
  binding_ids : int list;
  mode : Sst.instance_mode;
  typ : Sst.typ;
  rank_domain_id : string;
  rank_domain_version : string;
  rank_domain_digest : string;
  rank_component_snapshot : string list;
  profile_actual_snapshot : string list;
  requirement_digest : string;
  rank_component : Typedtree_adapter.rank_type_identity list;
  rank_positive_children : Typedtree_adapter.rank_positive_child list;
  rank_ground_witnesses : Typedtree_adapter.rank_ground_witness list;
}

type parameter_kind = Parametric_signature_private.parameter_kind =
  | Positional_parameter
  | Labelled_parameter
  | Optional_parameter
  | Default_parameter

type provider_model = {
  domain : Sst.type_id;
  result_type : Sst.typ;
  closure_type_ids : Sst.type_id list;
  closure_digest : string;
}

type provider_callable = {
  resolved_path : string;
  binding_uid : string;
  definition : Sst.function_definition;
  signature : Parametric_signature_private.t;
  finite_requirements : formal_requirement list;
  model : provider_model option;
}

type provider_type = {
  resolved_path : string;
  source_path : string;
  source_name : string;
  binding_uid : string;
  definition : Sst.type_definition;
  parametric_descriptor : Parametric_adt.t option;
  revealed : bool;
  logical : bool;
  field_modes : (Sst.field_id * Sst.instance_mode) list;
  rank_profile_digest : string option;
}

type provider_description = {
  unit_name : string;
  interface_digest : string;
  source_digest : string;
  family_digest : string;
  import_digest : string;
  callables : provider_callable list;
  types : provider_type list;
}

type callable_snapshot = {
  path : string;
  binding_uid : string;
  definition : Sst.function_definition;
  signature : Parametric_signature_private.t;
  finite_requirements : formal_requirement list;
  provider_unit : string;
  provider_interface : string;
  provider_source : string;
  provider_family : string;
  provider_import : string;
  summary_digest : string;
  model : provider_model option;
}

type type_snapshot = {
  path : string;
  binding_uid : string;
  source_type_id : Sst.type_id;
  definition : Sst.type_definition;
  parametric_descriptor : Parametric_adt.t option;
}

type provider = {
  provider_issuer : unit ref;
  provider_token : unit ref;
  implementation : Cmt_input.implementation;
  program : Sst.program;
  direct_dependencies : provider list;
  provider_completion : Verified_provider_completion_private.t;
  description : provider_description;
  snapshot : string;
}

type environment = {
  issuer : unit ref;
  token : unit ref;
  providers : provider list;
  callables : callable_snapshot list;
  types : type_snapshot list;
  snapshot : string;
}

type call = {
  expression : Sst.expression;
  summary : callable_snapshot;
  type_arguments : Parametric_type.t list;
  invocation_ordinal : int;
  call_snapshot : string;
  mutable aggregate_admitted_by : unit ref option;
  mutable aggregate_identity : aggregate_application_identity option;
}

and aggregate_application_identity = {
  aggregate_identity_issuer : unit ref;
  aggregate_identity_token : unit ref;
  aggregate_identity_logical_digest : string;
  aggregate_identity_call_snapshot : string;
  aggregate_identity_admission_digest : string;
  aggregate_identity_session : unit ref;
  aggregate_identity_active : bool ref;
}

type registration = {
  registration_issuer : unit ref;
  token : unit ref;
  environment : environment;
  implementation : Cmt_input.implementation;
  program : Sst.program;
  calls : call list;
  snapshot : string;
  snapshot_digest : string;
  mutable active_session : unit ref option;
  mutable invalidated : bool;
}

let issuer = ref ()
let aggregate_descriptors_issued = ref 0
let aggregate_applications_admitted = ref 0
let aggregate_descriptors_invalidated = ref 0
let digest value = Digest.string value |> Digest.to_hex

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let stable_index namespace name =
  let hex = digest (namespace ^ "\000" ^ name) in
  let prefix = String.sub hex 0 7 in
  1_000_000 + int_of_string ("0x" ^ prefix)

let simple_binding = function
  | { Sst.pattern_desc = Sst.Bind binding; _ } -> Some binding
  | _ -> None

let validate_callable (callable : provider_callable) =
  let definition = callable.definition in
  let valid_parameter parameter =
    let parameter = Sst.require_value_parameter parameter in
    match parameter.optional_default with
    | None ->
        Option.is_some (simple_binding parameter.pattern)
        || parameter.pattern.pattern_desc = Sst.Unit_pattern
    | Some default ->
        parameter.pattern.pattern_desc = Sst.Wildcard
        && Option.is_some (simple_binding default.optional_pattern)
  in
  let* rebound =
    Parametric_signature_private.rebind_definition callable.signature definition
  in
  if
    not
      (String.equal
         (Parametric_signature_private.semantic_fingerprint rebound)
         (Parametric_signature_private.semantic_fingerprint callable.signature))
  then Error "[VERO_DEPENDENCY] retained parametric signature snapshot mismatch"
  else if
    List.exists
      (fun parameter -> not (valid_parameter parameter))
      definition.parameters
  then
    Error
      "[VERO_DEPENDENCY] retained callable ABI requires simple variable formals"
  else
    let ids =
      List.concat_map
        (fun parameter ->
          let parameter = Sst.require_value_parameter parameter in
          Option.to_list
            (Option.map
               (fun binding -> binding.Sst.id)
               (match parameter.Sst.optional_default with
               | None -> simple_binding parameter.pattern
               | Some default -> simple_binding default.optional_pattern)))
        definition.parameters
    in
    if List.length ids <> List.length (List.sort_uniq Int.compare ids) then
      Error "[VERO_DEPENDENCY] retained callable ABI binding identity collision"
    else
      let invalid_result =
        List.exists
          (fun clause ->
            match clause.Sst.binder with
            | None -> false
            | Some pattern -> Option.is_none (simple_binding pattern))
          definition.contracts.ensures
      in
      if invalid_result then
        Error
          "[VERO_DEPENDENCY] retained callable ABI requires a simple result \
           binder"
      else
        match (definition.mode, definition.body) with
        | Sst.Exec, Sst.Checked_exec _
        | Sst.Proof, Sst.Proof_body _
        | Sst.Spec, Sst.Spec_definition _
        | Sst.Spec, Sst.Recursive_spec_definition _ ->
            Ok ()
        | _ ->
            Error
              "[VERO_DEPENDENCY] retained provider lacks a supported checked \
               first-order body"

type target_resolution =
  | Exact_direct_target
  | Ambiguous_alias_target
  | Ambiguous_open_target
  | Functor_generated_target

let validate_application_abi (callable : provider_callable) ~target
    ~supplied_count ~has_omitted ~partial ~argument_labels
    ~private_driver_completion =
  if not private_driver_completion then
    Error "[VERO_DEPENDENCY] retained provider lacks private-driver completion"
  else
    match target with
    | Ambiguous_alias_target | Ambiguous_open_target ->
        Error
          "[VERO_DEPENDENCY] retained callable target is not an exact direct \
           dependency"
    | Functor_generated_target ->
        Error
          "[VERO_DEPENDENCY] retained callable ABI rejects functor-generated \
           paths"
    | Exact_direct_target ->
        let* () = validate_callable callable in
        if
          has_omitted || partial
          || supplied_count <> List.length callable.definition.parameters
        then
          Error
            "[VERO_DEPENDENCY] retained callable ABI requires saturated \
             application"
        else
          let expected_labels =
            Parametric_signature_private.formals callable.signature
            |> List.map (fun (formal : Parametric_signature_private.formal) ->
                formal.label)
          in
          if argument_labels <> expected_labels then
            Error
              "[VERO_DEPENDENCY] retained callable ABI label/default vector \
               mismatch"
          else Ok ()

let opaque_contract_carrier (callable : provider_callable) definition =
  Retained_callable_summary_private.obligation_definition
    ~resolved_path:callable.resolved_path
    ~opaque_binding_id:
      (stable_index "retained-opaque-result" callable.resolved_path)
    definition

let retained_provider_identity (description : provider_description) =
  {
    Retained_exec_result_private.unit_name = description.unit_name;
    interface_digest = description.interface_digest;
    source_digest = description.source_digest;
    family_digest = description.family_digest;
    import_digest = description.import_digest;
  }

let retained_provider_type (typ : provider_type) =
  {
    Retained_exec_result_private.resolved_path = typ.resolved_path;
    source_path = typ.source_path;
    source_name = typ.source_name;
    binding_uid = typ.binding_uid;
    definition = typ.definition;
    parametric_descriptor = typ.parametric_descriptor;
    revealed = typ.revealed;
    logical = typ.logical;
    field_modes = typ.field_modes;
    rank_profile_digest = typ.rank_profile_digest;
  }

let classify_result ~parametric_adts (description : provider_description)
    (callable : provider_callable) =
  let model =
    Option.map
      (fun (model : provider_model) ->
        {
          Retained_exec_result_private.domain = model.domain;
          result_type = model.result_type;
        })
      callable.model
  in
  Retained_exec_result_private.classify
    ~provider:(retained_provider_identity description)
    ~types:(List.map retained_provider_type description.types)
    ~parametric_adts ~resolved_path:callable.resolved_path
    ~binding_uid:callable.binding_uid ~definition:callable.definition ~model

let obligation_description ?(parametric_adts = [])
    (description : provider_description) =
  let rec loop callables = function
    | [] -> Ok { description with callables = List.rev callables }
    | callable :: rest -> (
        let* () = validate_callable callable in
        let* classification =
          classify_result ~parametric_adts description callable
        in
        match classification with
        | Retained_exec_result_private.Retain_existing
        | Retained_exec_result_private.Contract_only_exec ->
            loop (callable :: callables) rest
        | Retained_exec_result_private.Opaque_model_spec closure ->
            let model =
              Option.map
                (fun model ->
                  {
                    model with
                    closure_type_ids = closure.closure_type_ids;
                    closure_digest = closure.closure_digest;
                  })
                callable.model
            in
            let callable = { callable with model } in
            loop (callable :: callables) rest
        | Retained_exec_result_private.Ineligible -> loop callables rest)
  in
  loop [] description.callables

let type_map providers =
  List.concat_map
    (fun (provider : provider_description) ->
      List.map
        (fun (typ : provider_type) ->
          let old = typ.definition.Sst.type_id in
          let fresh =
            {
              Sst.type_index = stable_index provider.unit_name typ.resolved_path;
              type_name = typ.resolved_path;
            }
          in
          (old, fresh))
        provider.types)
    providers

let imported_source_type_path (_provider : provider_description)
    (typ : provider_type) =
  typ.source_path

let function_map providers =
  List.concat_map
    (fun (provider : provider_description) ->
      List.map
        (fun (callable : provider_callable) ->
          let old = callable.definition.Sst.function_id in
          let fresh =
            {
              Sst.function_index =
                stable_index provider.unit_name callable.resolved_path;
              function_name = callable.resolved_path;
            }
          in
          (old, fresh))
        provider.callables)
    providers

let rec map_typ type_ids = function
  | Sst.Unit -> Sst.Unit
  | Sst.Bool -> Sst.Bool
  | Sst.Int -> Sst.Int
  | Sst.Tuple components ->
      Sst.Tuple
        (List.map
           (fun (label, typ) -> (label, map_typ type_ids typ))
           components)
  | Sst.Aggregate id ->
      Sst.Aggregate (Option.value ~default:id (List.assoc_opt id type_ids))
  | Sst.Parameter _ as parameter -> parameter
  | Sst.Application (constructor, arguments) ->
      Sst.Application (constructor, List.map (map_typ type_ids) arguments)

let map_constructor type_ids (id : Sst.constructor_id) =
  {
    id with
    Sst.constructor_type =
      Option.value ~default:id.constructor_type
        (List.assoc_opt id.constructor_type type_ids);
  }

let map_field_owner type_ids = function
  | Sst.Record_owner id ->
      Sst.Record_owner (Option.value ~default:id (List.assoc_opt id type_ids))
  | Sst.Constructor_owner id ->
      Sst.Constructor_owner (map_constructor type_ids id)

let map_field_id type_ids (id : Sst.field_id) =
  { id with Sst.field_owner = map_field_owner type_ids id.field_owner }

let binding_offset unit_name = 2_000_000_000 + stable_index "binding" unit_name

let map_binding type_ids offset (binding : Sst.binding) =
  {
    binding with
    Sst.id = offset + binding.id;
    typ = map_typ type_ids binding.typ;
  }

let map_cursor type_ids offset (cursor : Sst.owned_tree_cursor) =
  {
    cursor with
    Sst.cursor_binding = map_binding type_ids offset cursor.cursor_binding;
    root = map_binding type_ids offset cursor.root;
    guarded_path =
      List.map
        (function
          | Sst.Owned_tree_field field ->
              Sst.Owned_tree_field (map_field_id type_ids field)
          | Sst.Owned_tree_constructor constructor ->
              Sst.Owned_tree_constructor (map_constructor type_ids constructor))
        cursor.guarded_path;
  }

let rec map_pattern type_ids offset (pattern : Sst.pattern) =
  let pattern_desc =
    match pattern.Sst.pattern_desc with
    | Sst.Wildcard -> Sst.Wildcard
    | Sst.Bind binding -> Sst.Bind (map_binding type_ids offset binding)
    | Sst.Owned_tree_cursor_pattern cursor ->
        Sst.Owned_tree_cursor_pattern (map_cursor type_ids offset cursor)
    | Sst.Int_pattern value -> Sst.Int_pattern value
    | Sst.Bool_pattern value -> Sst.Bool_pattern value
    | Sst.Unit_pattern -> Sst.Unit_pattern
    | Sst.Tuple_pattern components ->
        Sst.Tuple_pattern
          (List.map
             (fun (label, nested) ->
               (label, map_pattern type_ids offset nested))
             components)
    | Sst.Record_pattern fields ->
        Sst.Record_pattern
          (List.map
             (fun (field, nested) ->
               (map_field_id type_ids field, map_pattern type_ids offset nested))
             fields)
    | Sst.Constructor_pattern (constructor, arguments) ->
        Sst.Constructor_pattern
          ( map_constructor type_ids constructor,
            List.map (map_pattern type_ids offset) arguments )
    | Sst.Or_pattern (left, right) ->
        Sst.Or_pattern
          (map_pattern type_ids offset left, map_pattern type_ids offset right)
  in
  { pattern with Sst.pattern_desc; typ = map_typ type_ids pattern.typ }

let map_mutation type_ids offset (provenance : Sst.mutation_provenance) =
  { provenance with Sst.root = map_binding type_ids offset provenance.root }

let map_transition type_ids offset (transition : Sst.owned_tree_transition) =
  let map_layer = function
    | Sst.Reconstruct_record { record_type; changed_field; preserved_fields } ->
        Sst.Reconstruct_record
          {
            record_type =
              Option.value ~default:record_type
                (List.assoc_opt record_type type_ids);
            changed_field = map_field_id type_ids changed_field;
            preserved_fields = List.map (map_field_id type_ids) preserved_fields;
          }
    | Sst.Reconstruct_constructor constructor ->
        Sst.Reconstruct_constructor (map_constructor type_ids constructor)
  in
  {
    transition with
    Sst.root = map_binding type_ids offset transition.root;
    cursor = Option.map (map_cursor type_ids offset) transition.cursor;
    target_field = map_field_id type_ids transition.target_field;
    rhs_provenance =
      (match transition.rhs_provenance with
      | Sst.Ground_owned_tree_value -> Sst.Ground_owned_tree_value
      | Sst.Guarded_descendant_move cursor ->
          Sst.Guarded_descendant_move (map_cursor type_ids offset cursor));
    reconstruction = List.map map_layer transition.reconstruction;
    invalidated_cursor_ids =
      List.map (fun id -> offset + id) transition.invalidated_cursor_ids;
  }

let map_shared_transition type_ids function_ids offset
    (transition : Sst.shared_scalar_heap_transition) =
  let function_id =
    {
      Sst.function_index = transition.shared_function_index;
      function_name = transition.shared_function_name;
    }
  in
  let mapped_function =
    Option.value ~default:function_id (List.assoc_opt function_id function_ids)
  in
  {
    transition with
    Sst.shared_function_index = mapped_function.function_index;
    shared_function_name = mapped_function.function_name;
    shared_record_type =
      Option.value ~default:transition.shared_record_type
        (List.assoc_opt transition.shared_record_type type_ids);
    shared_formal_roots =
      List.map (map_binding type_ids offset) transition.shared_formal_roots;
    shared_target = map_binding type_ids offset transition.shared_target;
    shared_canonical_root =
      map_binding type_ids offset transition.shared_canonical_root;
    shared_alias_chain =
      List.map (map_binding type_ids offset) transition.shared_alias_chain;
    shared_target_field = map_field_id type_ids transition.shared_target_field;
  }

let rec map_expression type_ids function_ids offset expression =
  let recurse = map_expression type_ids function_ids offset in
  let expression_desc =
    match expression.Sst.expression_desc with
    | Sst.Int_constant value -> Sst.Int_constant value
    | Sst.Bool_constant value -> Sst.Bool_constant value
    | Sst.Unit_constant -> Sst.Unit_constant
    | Sst.Variable { binding; use_uniqueness } ->
        Sst.Variable
          { binding = map_binding type_ids offset binding; use_uniqueness }
    | Sst.Tuple_value values ->
        Sst.Tuple_value
          (List.map (fun (label, value) -> (label, recurse value)) values)
    | Sst.Record_value { record_type; fields } ->
        Sst.Record_value
          {
            record_type =
              Option.value ~default:record_type
                (List.assoc_opt record_type type_ids);
            fields =
              List.map
                (fun (field, value) ->
                  (map_field_id type_ids field, recurse value))
                fields;
          }
    | Sst.Constructor_value { constructor; arguments } ->
        Sst.Constructor_value
          {
            constructor = map_constructor type_ids constructor;
            arguments = List.map recurse arguments;
          }
    | Sst.Field_read { record; field } ->
        Sst.Field_read
          { record = recurse record; field = map_field_id type_ids field }
    | Sst.Field_write { provenance; field; value; transition } ->
        Sst.Field_write
          {
            provenance = map_mutation type_ids offset provenance;
            field = map_field_id type_ids field;
            value = recurse value;
            transition = Option.map (map_transition type_ids offset) transition;
          }
    | Sst.Shared_scalar_field_write { provenance; field; value; transition } ->
        Sst.Shared_scalar_field_write
          {
            provenance = map_mutation type_ids offset provenance;
            field = map_field_id type_ids field;
            value = recurse value;
            transition =
              map_shared_transition type_ids function_ids offset transition;
          }
    | Sst.Owned_tree_nested_write { transition; value } ->
        Sst.Owned_tree_nested_write
          {
            transition = map_transition type_ids offset transition;
            value = recurse value;
          }
    | Sst.Owned_tree_rebase { transition } ->
        Sst.Owned_tree_rebase
          { transition = map_transition type_ids offset transition }
    | Sst.Let_mutable (binding, initial, body) ->
        Sst.Let_mutable
          (map_binding type_ids offset binding, recurse initial, recurse body)
    | Sst.Mutable_read binding ->
        Sst.Mutable_read (map_binding type_ids offset binding)
    | Sst.Mutable_write { provenance; value } ->
        Sst.Mutable_write
          {
            provenance = map_mutation type_ids offset provenance;
            value = recurse value;
          }
    | Sst.Let (bindings, body) ->
        Sst.Let
          ( List.map
              (fun (pattern, value) ->
                (map_pattern type_ids offset pattern, recurse value))
              bindings,
            recurse body )
    | Sst.Sequence (left, right) -> Sst.Sequence (recurse left, recurse right)
    | Sst.If (condition, yes, no) ->
        Sst.If (recurse condition, recurse yes, Option.map recurse no)
    | Sst.Match (scrutinee, cases) ->
        Sst.Match
          ( recurse scrutinee,
            List.map
              (fun (case : Sst.case) ->
                {
                  case with
                  Sst.case_pattern =
                    map_pattern type_ids offset case.case_pattern;
                  case_guard = Option.map recurse case.case_guard;
                  case_body = recurse case.case_body;
                })
              cases )
    | Sst.Checked_arithmetic (operation, operands) ->
        Sst.Checked_arithmetic (operation, List.map recurse operands)
    | Sst.Compare (operation, left, right) ->
        Sst.Compare (operation, recurse left, recurse right)
    | Sst.Boolean_not operand -> Sst.Boolean_not (recurse operand)
    | Sst.Boolean_binary (operation, left, right) ->
        Sst.Boolean_binary (operation, recurse left, recurse right)
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        let binder = map_binding type_ids offset quantifier.quantifier_binder in
        let quantifier =
          {
            Sst.quantifier_metadata =
              Logic_quantifier_private.seal_import ~offset
                ~binder_index:binder.id ~binder_type:binder.typ
                quantifier.quantifier_metadata;
            quantifier_binder = binder;
            quantifier_body = recurse quantifier.quantifier_body;
            quantifier_trigger = Option.map recurse quantifier.quantifier_trigger;
          }
        in
        (match expression.expression_desc with
        | Sst.Forall _ -> Sst.Forall quantifier
        | Sst.Exists _ -> Sst.Exists quantifier
        | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
        | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
        | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
        | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
        | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
        | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _
        | Sst.Match _ | Sst.Checked_arithmetic _ | Sst.Compare _
        | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Direct_call _
        | Sst.Callback_call _ | Sst.Callback_requires _
        | Sst.Callback_ensures _ | Sst.Optional_absent
        | Sst.Symbolic_application _
        | Sst.Optional_present _ | Sst.Optional_forward _ | Sst.Reveal _
        | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
        | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _ ->
            assert false)
    | Sst.Direct_call
        { call_form; callee; type_arguments; arguments; recursive } ->
        Sst.Direct_call
          {
            call_form;
            callee =
              Option.value ~default:callee (List.assoc_opt callee function_ids);
            type_arguments = List.map (map_typ type_ids) type_arguments;
            arguments =
              List.map
                (function
                  | Sst.Value_argument { label; value } ->
                      Sst.Value_argument { label; value = recurse value }
                  | Sst.Callback_argument _ as argument -> argument)
                arguments;
            recursive;
          }
    | Sst.Callback_call application ->
        Sst.Callback_call
          { application with
            arguments =
              List.map (fun (label, value) -> (label, recurse value))
                application.arguments }
    | Sst.Callback_requires application ->
        Sst.Callback_requires
          { application with
            arguments =
              List.map (fun (label, value) -> (label, recurse value))
                application.arguments }
    | Sst.Callback_ensures { application; result } ->
        Sst.Callback_ensures
          { application =
              { application with
                arguments =
                  List.map (fun (label, value) -> (label, recurse value))
                    application.arguments };
            result = recurse result }
    | Sst.Symbolic_application _ ->
        assert false
    | Sst.Reveal id ->
        Sst.Reveal (Option.value ~default:id (List.assoc_opt id function_ids))
    | Sst.Reveal_with_fuel { function_id; literal_depth } ->
        Sst.Reveal_with_fuel
          {
            function_id =
              Option.value ~default:function_id
                (List.assoc_opt function_id function_ids);
            literal_depth;
          }
    | Sst.Use_type_invariant { use_id; value } ->
        Sst.Use_type_invariant { use_id; value = recurse value }
    | Sst.Local_assert { assertion_ordinal; predicate } ->
        Sst.Local_assert { assertion_ordinal; predicate = recurse predicate }
    | Sst.Proof_region body -> Sst.Proof_region (recurse body)
    | Sst.Old body -> Sst.Old (recurse body)
    | Sst.Optional_absent -> Sst.Optional_absent
    | Sst.Optional_present payload -> Sst.Optional_present (recurse payload)
    | Sst.Optional_forward payload -> Sst.Optional_forward (recurse payload)
  in
  { expression with Sst.expression_desc; typ = map_typ type_ids expression.typ }

let map_staged type_ids function_ids offset (staged : Sst.staged_expression) =
  {
    staged with
    Sst.expression =
      map_expression type_ids function_ids offset staged.expression;
  }

let map_contracts type_ids function_ids offset (contracts : Sst.contracts) =
  let predicate (clause : Sst.predicate_clause) =
    {
      clause with
      Sst.predicate = map_staged type_ids function_ids offset clause.predicate;
    }
  in
  let ensures (clause : Sst.ensures_clause) =
    {
      clause with
      Sst.binder = Option.map (map_pattern type_ids offset) clause.binder;
      predicate = map_staged type_ids function_ids offset clause.predicate;
    }
  in
  {
    Sst.requires = List.map predicate contracts.requires;
    ensures = List.map ensures contracts.ensures;
    decreases = List.map predicate contracts.decreases;
    assertions = List.map predicate contracts.assertions;
  }

let map_type_definition type_ids (definition : Sst.type_definition) =
  let map_field (field : Sst.field_definition) =
    {
      field with
      Sst.field_id = map_field_id type_ids field.field_id;
      field_type = map_typ type_ids field.field_type;
    }
  in
  let type_kind =
    match definition.Sst.type_kind with
    | Sst.Record_definition fields ->
        Sst.Record_definition (List.map map_field fields)
    | Sst.Variant_definition constructors ->
        Sst.Variant_definition
          (List.map
             (fun (constructor : Sst.constructor_definition) ->
               {
                 constructor with
                 Sst.constructor_id =
                   map_constructor type_ids constructor.constructor_id;
                 constructor_fields =
                   List.map map_field constructor.constructor_fields;
               })
             constructors)
  in
  {
    definition with
    Sst.type_id =
      Option.value ~default:definition.type_id
        (List.assoc_opt definition.type_id type_ids);
    type_kind;
    representation = Sst.Revealed;
  }

let transform_callable type_ids function_ids (provider : provider_description)
    (callable : provider_callable) =
  let offset = binding_offset provider.unit_name in
  let source = callable.definition in
  let function_id =
    Option.value ~default:source.function_id
      (List.assoc_opt source.function_id function_ids)
  in
  let parameters =
    List.map
      (function
        | Sst.Callback_parameter _ as parameter -> parameter
        | Sst.Value_parameter value ->
            Sst.Value_parameter
              {
                value with
                Sst.pattern = map_pattern type_ids offset value.pattern;
                optional_default =
                  Option.map
                    (fun (default : Sst.optional_default) ->
                      {
                        Sst.optional_pattern =
                          map_pattern type_ids offset default.optional_pattern;
                        optional_expression =
                          map_expression type_ids function_ids offset
                            default.optional_expression;
                      })
                    value.optional_default;
              })
      source.parameters
  in
  let map_body = function
    | Sst.Checked_exec body ->
        Sst.Checked_exec
          { body with body = map_staged type_ids function_ids offset body.body }
    | Sst.Spec_definition body ->
        Sst.Spec_definition (map_staged type_ids function_ids offset body)
    | Sst.Proof_body body ->
        Sst.Proof_body
          { body with body = map_staged type_ids function_ids offset body.body }
    | Sst.Recursive_spec_definition body ->
        Sst.Recursive_spec_definition
          { body with body = map_staged type_ids function_ids offset body.body }
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        assert false
  in
  let definition =
    {
      source with
      Sst.function_id;
      recursive = source.recursive;
      parameters;
      contracts = map_contracts type_ids function_ids offset source.contracts;
      body = map_body source.body;
      result_type = map_typ type_ids source.result_type;
      returns_unique_parameter = None;
    }
  in
  let* definition, signature =
    Parametric_signature_private.rebase_definition callable.signature
      ~source_definition:
        (Parametric_signature_private.source_definition callable.signature)
      ~function_id definition
  in
  let* definition = opaque_contract_carrier callable definition in
  let map_requirement requirement =
    let map_type_id type_id =
      Option.value ~default:type_id (List.assoc_opt type_id type_ids)
    in
    {
      requirement with
      binding_ids = List.map (fun id -> offset + id) requirement.binding_ids;
      typ = map_typ type_ids requirement.typ;
      rank_component =
        List.map
          (fun (identity : Typedtree_adapter.rank_type_identity) ->
            { identity with rank_type_id = map_type_id identity.rank_type_id })
          requirement.rank_component;
      rank_positive_children =
        List.map
          (fun (child : Typedtree_adapter.rank_positive_child) ->
            {
              child with
              rank_constructor =
                map_constructor type_ids child.rank_constructor;
              rank_field = map_field_id type_ids child.rank_field;
              rank_child_type = map_type_id child.rank_child_type;
            })
          requirement.rank_positive_children;
      rank_ground_witnesses =
        List.map
          (fun (witness : Typedtree_adapter.rank_ground_witness) ->
            {
              witness with
              rank_ground_constructor =
                map_constructor type_ids witness.rank_ground_constructor;
            })
          requirement.rank_ground_witnesses;
    }
  in
  let summary_digest =
    digest
      (Sst.to_string
         {
           Sst.policy = definition.policy;
           parametric_adts = [];
           types = [];
           functions = [ definition ];
         }
      ^ callable.binding_uid ^ provider.interface_digest
      ^ provider.source_digest ^ provider.family_digest ^ provider.import_digest
      ^ Parametric_signature_private.semantic_fingerprint signature)
  in
  let model =
    Option.map
      (fun model ->
        {
          model with
          domain =
            Option.value ~default:model.domain
              (List.assoc_opt model.domain type_ids);
          result_type = map_typ type_ids model.result_type;
          closure_type_ids =
            List.map
              (fun type_id ->
                Option.value ~default:type_id (List.assoc_opt type_id type_ids))
              model.closure_type_ids;
        })
      callable.model
  in
  Ok
    {
      path = callable.resolved_path;
      binding_uid = callable.binding_uid;
      definition;
      signature;
      finite_requirements =
        List.map map_requirement callable.finite_requirements;
      provider_unit = provider.unit_name;
      provider_interface = provider.interface_digest;
      provider_source = provider.source_digest;
      provider_family = provider.family_digest;
      provider_import = provider.import_digest;
      summary_digest;
      model;
    }

let require_provider provider =
  if provider.provider_issuer != issuer || provider.provider_token == issuer
  then invalid_arg "unauthenticated retained provider"

let provider_snapshot implementation program description direct_dependencies =
  String.concat "|"
    [
      implementation.Cmt_input.unit_name;
      implementation.filename;
      description.unit_name;
      description.interface_digest;
      description.source_digest;
      description.family_digest;
      description.import_digest;
      Sst.to_string program;
      String.concat ","
        (List.map
           (fun dependency ->
             require_provider dependency;
             dependency.description.unit_name ^ ":"
             ^ dependency.description.interface_digest)
           direct_dependencies);
    ]

let validate_description (description : provider_description) =
  if
    description.unit_name = ""
    || description.interface_digest = ""
    || description.source_digest = ""
    || description.family_digest = ""
    || description.import_digest = ""
  then Error "[VERO_DEPENDENCY] retained provider identity is incomplete"
  else if
    List.exists
      (fun (callable : provider_callable) ->
        String.equal callable.binding_uid "")
      description.callables
    || List.exists
         (fun (typ : provider_type) -> String.equal typ.binding_uid "")
         description.types
  then Error "[VERO_DEPENDENCY] retained provider compiler UID is incomplete"
  else
    let invalid_type (typ : provider_type) =
      let type_id = typ.definition.Sst.type_id in
      String.equal typ.source_name ""
      || String.equal typ.source_path ""
      || not (String.equal type_id.type_name typ.source_name)
      || (not
            (String.equal typ.source_path
               (description.unit_name ^ "." ^ typ.source_name)))
      || (not
            (String.equal typ.resolved_path
               (description.unit_name ^ "." ^ type_id.type_name)))
      || type_id.type_index < 0
    in
    if List.exists invalid_type description.types then
      Error
        "[VERO_DEPENDENCY] retained provider type identity is noncanonical or \
         inconsistent"
    else
      let conflicting_source_family =
        List.exists
          (fun (typ : provider_type) ->
            List.exists
              (fun (other : provider_type) ->
                String.equal typ.source_path other.source_path
                && String.equal typ.source_name other.source_name
                && not (String.equal typ.binding_uid other.binding_uid))
              description.types)
          description.types
      in
      if conflicting_source_family then
        Error
          "[VERO_DEPENDENCY] retained provider generic-family members have \
           conflicting compiler identities"
      else
        let duplicate project compare =
          let values = List.map project description.types in
          List.length values <> List.length (List.sort_uniq compare values)
        in
        if
          duplicate (fun typ -> typ.definition.Sst.type_id) compare
        then
          Error
            "[VERO_DEPENDENCY] retained provider has duplicate or conflicting \
             canonical schemas"
        else if
          List.exists
            (fun (callable : provider_callable) ->
              Option.fold ~none:false
                ~some:(fun model ->
                  model.closure_type_ids <> []
                  || not (String.equal model.closure_digest ""))
                callable.model)
            description.callables
        then
          Error
            "[VERO_DEPENDENCY] retained provider supplied a conflicting \
             generic-family model closure"
        else Ok ()

let description_matches_program program (description : provider_description) =
  let callable_matches (callable : provider_callable) =
    let source =
      Parametric_signature_private.source_definition callable.signature
    in
    List.exists (fun definition -> definition = source) program.Sst.functions
    && callable.definition = source
  in
  List.for_all callable_matches description.callables

let seal_provider ~provider_completion ~implementation ~program
    ~direct_dependencies description =
  if
    not
      (Verified_provider_completion_private.authenticates provider_completion
         ~implementation ~program)
  then Error "[VERO_DEPENDENCY] retained provider lacks verified completion"
  else if not (description_matches_program program description) then
    Error "[VERO_DEPENDENCY] retained summary does not match verified provider"
  else
    match validate_description description with
    | Error _ as error -> error
    | Ok () -> (
        match
          obligation_description ~parametric_adts:program.Sst.parametric_adts
            description
        with
        | Error _ as error -> error
        | Ok description ->
            let snapshot =
              provider_snapshot implementation program description
                direct_dependencies
            in
            Ok
              {
                provider_issuer = issuer;
                provider_token = ref ();
                implementation;
                program;
                direct_dependencies;
                provider_completion;
                description;
                snapshot;
              })

let provider_matches provider ~implementation ~program =
  require_provider provider;
  provider.implementation == implementation
  && provider.program == program
  && Verified_provider_completion_private.authenticates
       provider.provider_completion ~implementation ~program
  && String.equal provider.snapshot
       (provider_snapshot implementation program provider.description
          provider.direct_dependencies)

let descriptions providers =
  List.map
    (fun provider ->
      require_provider provider;
      if
        not
          (provider_matches provider ~implementation:provider.implementation
             ~program:provider.program)
      then invalid_arg "stale retained provider";
      provider.description)
    providers

let create providers =
  let descriptions = descriptions providers in
  let type_ids = type_map descriptions in
  let function_ids = function_map descriptions in
  let importable_type (_provider : provider_description)
      (_typ : provider_type) =
    true
  in
  let rec map_types mapped = function
    | [] -> Ok (List.rev mapped)
    | (provider : provider_description) :: rest ->
        let rec one mapped = function
          | [] -> map_types mapped rest
          | (typ : provider_type) :: tail
            when not (importable_type provider typ) ->
              one mapped tail
          | typ :: tail ->
              let definition = map_type_definition type_ids typ.definition in
              let* parametric_descriptor =
                match typ.parametric_descriptor with
                | None -> Ok None
                | Some descriptor ->
                    Parametric_signature_private.remap_descriptor_type_id
                      descriptor definition.type_id
                    |> Result.map Option.some
              in
              one
                ({
                   path = imported_source_type_path provider typ;
                   binding_uid = typ.binding_uid;
                   source_type_id = typ.definition.Sst.type_id;
                   definition;
                   parametric_descriptor;
                 }
                :: mapped)
                tail
        in
        one mapped provider.types
  in
  let rec map_callables mapped = function
    | [] -> Ok (List.rev mapped)
    | (provider : provider_description) :: rest ->
        let rec one mapped = function
          | [] -> map_callables mapped rest
          | callable :: tail -> (
              match
                transform_callable type_ids function_ids provider callable
              with
              | Error _ as error -> error
              | Ok summary -> one (summary :: mapped) tail)
        in
        one mapped provider.callables
  in
  let* mapped_types = map_types [] descriptions in
  match map_callables [] descriptions with
  | Error _ as error -> error
  | Ok mapped_callables ->
      let ids =
        List.map
          (fun (item : callable_snapshot) -> item.definition.Sst.function_id)
          mapped_callables
      in
      if List.length ids <> List.length (List.sort_uniq compare ids) then
        Error "[VERO_DEPENDENCY] retained callable synthetic identity collision"
      else
        let snapshot =
          String.concat "|"
            (List.map
               (fun (callable : callable_snapshot) ->
                 callable.path ^ "#" ^ callable.binding_uid ^ "#"
                 ^ callable.summary_digest)
               mapped_callables)
        in
        Ok
          {
            issuer;
            token = ref ();
            providers;
            callables = mapped_callables;
            types = mapped_types;
            snapshot;
          }

let empty =
  {
    issuer;
    token = ref ();
    providers = [];
    callables = [];
    types = [];
    snapshot = "empty";
  }

let require (environment : environment) =
  ignore environment.snapshot;
  if environment.issuer != issuer || environment.token == issuer then
    invalid_arg "unauthenticated imported-call environment"

let callables environment =
  require environment;
  environment.callables

let types environment =
  require environment;
  environment.types

let same_function_id left right =
  left.Sst.function_index = right.Sst.function_index
  && String.equal left.function_name right.function_name

let expression_children = Sst_callback_private.expression_children

let program_expressions program =
  let from_staged staged = staged.Sst.expression in
  let roots definition =
    List.map
      (fun (clause : Sst.predicate_clause) -> from_staged clause.Sst.predicate)
      definition.Sst.contracts.requires
    @ List.map
        (fun (clause : Sst.ensures_clause) -> from_staged clause.Sst.predicate)
        definition.contracts.ensures
    @ List.map
        (fun (clause : Sst.predicate_clause) ->
          from_staged clause.Sst.predicate)
        definition.contracts.decreases
    @ List.map
        (fun (clause : Sst.predicate_clause) ->
          from_staged clause.Sst.predicate)
        definition.contracts.assertions
    @
    match definition.body with
    | Sst.Checked_exec body -> [ from_staged body.body ]
    | Sst.Proof_body body -> [ from_staged body.body ]
    | Sst.Spec_definition body -> [ from_staged body ]
    | Sst.Recursive_spec_definition body -> [ from_staged body.body ]
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        []
  in
  let rec flatten acc expression =
    List.fold_left flatten (expression :: acc) (expression_children expression)
  in
  List.concat_map roots program.Sst.functions
  |> List.fold_left flatten [] |> List.rev

let call_snapshot (expression : Sst.expression) (summary : callable_snapshot)
    ordinal =
  let span = expression.Sst.span in
  digest
    (Printf.sprintf "%s:%d:%d-%d:%d|%s|%d|%s" span.file span.start_pos.line
       span.start_pos.column span.end_pos.line span.end_pos.column
       summary.summary_digest ordinal summary.binding_uid)

let summary_is_aggregate_model (summary : callable_snapshot) =
  match summary.model with
  | Some model
    when Parametric_type.equal model.result_type summary.definition.result_type
    -> (
      match summary.definition.result_type with
      | Sst.Aggregate _ | Sst.Application _ -> true
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> false)
  | Some _ | None -> false

let seal_calls environment ~implementation ~program =
  require environment;
  let imports = Array.to_list implementation.Cmt_input.imports in
  let direct_provider provider =
    List.exists
      (fun (import : Cmt_input.import) ->
        String.equal import.Cmt_input.unit_name provider.description.unit_name
        && Option.fold ~none:false
             ~some:(String.equal provider.description.interface_digest)
             import.crc)
      imports
  in
  if not (List.for_all direct_provider environment.providers) then
    Error
      "[VERO_DEPENDENCY] retained callable target is not an exact direct \
       dependency"
  else
    let summary_for callee =
      List.find_opt
        (fun (summary : callable_snapshot) ->
          same_function_id summary.definition.Sst.function_id callee)
        environment.callables
    in
    let rec validate_expression ~opaque_position expression =
      let validate_children opaque children =
        List.fold_left
          (fun result child ->
            let* () = result in
            validate_expression ~opaque_position:opaque child)
          (Ok ()) children
      in
      match expression.Sst.expression_desc with
      | Sst.Direct_call { call_form; callee; recursive; _ } -> (
          match summary_for callee with
          | Some summary when summary_is_aggregate_model summary ->
              if
                opaque_position
                && call_form = Sst.Specification_call
                && not recursive
              then validate_children false (expression_children expression)
              else
                Error
                  "[VERO_DEPENDENCY] retained aggregate model result has a \
                   structural, recursive, authority-bearing, or non-opaque use"
          | Some _ | None ->
              validate_children false (expression_children expression))
      | Sst.Callback_call _ | Sst.Callback_requires _
      | Sst.Callback_ensures _ ->
          validate_children false (expression_children expression)
      | Sst.Symbolic_application _ ->
          validate_children false (expression_children expression)
      | Sst.Forall quantifier | Sst.Exists quantifier ->
          validate_children false
            (quantifier.quantifier_body
            :: Option.to_list quantifier.quantifier_trigger)
      | Sst.Compare ((Sst.Equal | Sst.Not_equal), left, right) ->
          let* () = validate_expression ~opaque_position:true left in
          validate_expression ~opaque_position:true right
      | Sst.Reveal function_id | Sst.Reveal_with_fuel { function_id; _ } -> (
          match summary_for function_id with
          | Some summary when summary_is_aggregate_model summary ->
              Error
                "[VERO_DEPENDENCY] retained aggregate model cannot be revealed \
                 or fueled"
          | Some _ | None -> Ok ())
      | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ | Sst.Mutable_read _ | Sst.Owned_tree_rebase _ ->
          Ok ()
      | Sst.Tuple_value _ | Sst.Record_value _ | Sst.Constructor_value _
      | Sst.Field_read _ | Sst.Field_write _ | Sst.Shared_scalar_field_write _
      | Sst.Owned_tree_nested_write _ | Sst.Let_mutable _ | Sst.Mutable_write _
      | Sst.Let _ | Sst.Sequence _ | Sst.If _ | Sst.Match _
      | Sst.Checked_arithmetic _ | Sst.Compare _ | Sst.Boolean_not _
      | Sst.Boolean_binary _ | Sst.Proof_region _ | Sst.Old _
      | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Optional_absent
      | Sst.Optional_present _ | Sst.Optional_forward _ ->
          validate_children false (expression_children expression)
    in
    let validate_definition (definition : Sst.function_definition) =
      let validate_root root =
        validate_expression ~opaque_position:(not definition.recursive) root
      in
      let roots =
        List.map
          (fun (clause : Sst.predicate_clause) ->
            clause.Sst.predicate.expression)
          definition.contracts.requires
        @ List.map
            (fun (clause : Sst.ensures_clause) ->
              clause.Sst.predicate.expression)
            definition.contracts.ensures
        @ List.map
            (fun (clause : Sst.predicate_clause) ->
              clause.Sst.predicate.expression)
            definition.contracts.decreases
        @ List.map
            (fun (clause : Sst.predicate_clause) ->
              clause.Sst.predicate.expression)
            definition.contracts.assertions
        @
        match definition.body with
        | Sst.Checked_exec { body; _ }
        | Sst.Proof_body { body; _ }
        | Sst.Spec_definition body
        | Sst.Recursive_spec_definition { body; _ } ->
            [ body.expression ]
        | Sst.External_specification _ | Sst.Trusted_external_spec_target _
        | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
            []
      in
      List.fold_left
        (fun result root ->
          let* () = result in
          validate_root root)
        (Ok ()) roots
    in
    let* () =
      List.fold_left
        (fun result definition ->
          let* () = result in
          validate_definition definition)
        (Ok ()) program.Sst.functions
    in
    let rec collect ordinal calls = function
      | [] -> Ok (List.rev calls)
      | expression :: rest -> (
          match expression.Sst.expression_desc with
          | Sst.Direct_call { callee; _ } -> (
              match
                List.find_opt
                  (fun (summary : callable_snapshot) ->
                    same_function_id summary.definition.Sst.function_id callee)
                  environment.callables
              with
              | None -> collect ordinal calls rest
              | Some summary ->
                  let type_arguments, arguments =
                    match expression.Sst.expression_desc with
                    | Sst.Direct_call { type_arguments; arguments; _ } ->
                        (type_arguments, arguments)
                    | _ -> assert false
                  in
                  let arguments =
                    List.map Sst.require_value_argument arguments
                  in
                  let* _ =
                    Parametric_signature_private.validate_call summary.signature
                      ~type_arguments ~actual_result:expression.typ ~arguments
                  in
                  let call =
                    {
                      expression;
                      summary;
                      type_arguments;
                      invocation_ordinal = ordinal;
                      call_snapshot = call_snapshot expression summary ordinal;
                      aggregate_admitted_by = None;
                      aggregate_identity = None;
                    }
                  in
                  collect (ordinal + 1) (call :: calls) rest)
          | _ -> collect ordinal calls rest)
    in
    match collect 0 [] (program_expressions program) with
    | Error _ as error -> error
    | Ok calls ->
        let program_snapshot = Sst.to_string program in
        aggregate_descriptors_issued :=
          !aggregate_descriptors_issued
          + List.length
              (List.filter
                 (fun call -> summary_is_aggregate_model call.summary)
                 calls);
        Ok
          {
            registration_issuer = issuer;
            token = ref ();
            environment;
            implementation;
            program;
            calls;
            snapshot = program_snapshot;
            snapshot_digest = digest program_snapshot;
            active_session = None;
            invalidated = false;
          }

let require_registration (registration : registration) =
  ignore registration.implementation;
  if registration.registration_issuer != issuer || registration.token == issuer
  then invalid_arg "unauthenticated imported-call registration"

(* The private token authenticates ordinary lookups in constant time.  The
   canonical consumer snapshot is checked once before a session can use the
   registration, rather than rendering the whole program at every call. *)
let require_registration_snapshot (registration : registration) =
  require_registration registration;
  if
    not
      (String.equal registration.snapshot (Sst.to_string registration.program))
  then invalid_arg "unauthenticated imported-call registration"

let registration_environment registration =
  require_registration registration;
  registration.environment

let find_call registration expression =
  require_registration registration;
  List.find_opt
    (fun call ->
      call.expression == expression
      && String.equal call.call_snapshot
           (call_snapshot expression call.summary call.invocation_ordinal))
    registration.calls

let call_summary call = call.summary

let ephemeral_contract_view registration expression fallback =
  Option.bind registration (fun registration ->
      Option.bind (find_call registration expression) (fun call ->
          match
            Parametric_signature_private.instantiate call.summary.signature
              call.type_arguments
          with
          | Error message ->
              invalid_arg
                ("authenticated retained schema no longer instantiates: "
               ^ message)
          | Ok instantiated ->
              Some
                { call.summary.definition with
                  Sst.type_binders = [];
                  parameters = instantiated.parameters;
                  contracts = instantiated.contracts;
                  result_type = instantiated.result_type }))
  |> Option.value ~default:fallback

let call_invocation_ordinal call = call.invocation_ordinal
let call_snapshot call = call.call_snapshot

let call_is_aggregate_model call =
  summary_is_aggregate_model call.summary

let begin_consumer_session registration ~session =
  require_registration_snapshot registration;
  if registration.invalidated then
    Error "[VERO_DEPENDENCY] retained aggregate descriptor is stale"
  else
    match registration.active_session with
    | None ->
        registration.active_session <- Some session;
        Ok ()
    | Some _ ->
        Error
          "[VERO_DEPENDENCY] retained aggregate descriptor is already bound to \
           a consumer session"

let end_consumer_session registration ~session =
  require_registration registration;
  match registration.active_session with
  | Some active when active == session ->
      registration.active_session <- None;
      if not registration.invalidated then (
        registration.invalidated <- true;
        aggregate_descriptors_invalidated :=
          !aggregate_descriptors_invalidated
          + List.length (List.filter call_is_aggregate_model registration.calls));
      List.iter
        (fun call ->
          match call.aggregate_admitted_by with
          | Some admitted when admitted == session ->
              call.aggregate_admitted_by <- Some issuer;
              Option.iter
                (fun identity -> identity.aggregate_identity_active := false)
                call.aggregate_identity
          | Some _ | None -> ())
        registration.calls
  | Some _ | None -> ()

let invalidate_registration registration =
  require_registration registration;
  if not registration.invalidated then (
    registration.active_session <- None;
    registration.invalidated <- true;
    aggregate_descriptors_invalidated :=
      !aggregate_descriptors_invalidated
      + List.length (List.filter call_is_aggregate_model registration.calls);
    List.iter
      (fun call ->
        if call_is_aggregate_model call then (
          call.aggregate_admitted_by <- Some issuer;
          Option.iter
            (fun identity -> identity.aggregate_identity_active := false)
            call.aggregate_identity))
      registration.calls)

let authenticate_aggregate_application_identity identity =
  identity.aggregate_identity_issuer == issuer
  && identity.aggregate_identity_token != issuer
  && !(identity.aggregate_identity_active)

let aggregate_application_logical_digest identity =
  if authenticate_aggregate_application_identity identity then
    identity.aggregate_identity_logical_digest
  else invalid_arg "unauthenticated retained aggregate application identity"

let aggregate_application_identity_matches identity ~logical_digest
    ~call_snapshot ~registration_snapshot ~invocation_ordinal
    ~application_snapshot =
  authenticate_aggregate_application_identity identity
  && String.equal identity.aggregate_identity_logical_digest logical_digest
  && String.equal identity.aggregate_identity_call_snapshot call_snapshot
  && String.equal identity.aggregate_identity_admission_digest
       (digest
          (String.concat "\000"
             [
               logical_digest;
               call_snapshot;
               registration_snapshot;
               string_of_int invocation_ordinal;
               application_snapshot;
             ]))

let consume_aggregate_model_call registration ~session ~expression ~actual_types
    ~application_snapshot =
  require_registration registration;
  if registration.invalidated then
    Error "[VERO_DEPENDENCY] retained aggregate descriptor is stale"
  else
    match (registration.active_session, find_call registration expression) with
    | Some active, Some call
      when active == session
           && call_is_aggregate_model call
           && Parametric_type.equal expression.Sst.typ
                call.summary.definition.Sst.result_type -> (
        let expected =
          List.map
            (fun parameter ->
              let parameter = Sst.require_value_parameter parameter in
              parameter.Sst.pattern.typ)
            call.summary.definition.parameters
        in
        if expected <> actual_types then
          Error
            "[VERO_DEPENDENCY] retained aggregate descriptor actual/formal \
             substitution mismatch"
        else
          match call.aggregate_admitted_by with
          | None ->
              call.aggregate_admitted_by <- Some session;
              aggregate_applications_admitted :=
                !aggregate_applications_admitted + 1;
              let logical_digest =
                digest
                  (String.concat "\000"
                     [
                       call.summary.provider_unit;
                       call.summary.provider_interface;
                       call.summary.provider_source;
                       call.summary.provider_family;
                       call.summary.provider_import;
                       call.summary.path;
                       call.summary.binding_uid;
                       call.summary.summary_digest;
                       Option.fold ~none:""
                         ~some:(fun model -> model.closure_digest)
                         call.summary.model;
                     ])
              in
              let identity =
                {
                  aggregate_identity_issuer = issuer;
                  aggregate_identity_token = ref ();
                  aggregate_identity_logical_digest = logical_digest;
                  aggregate_identity_call_snapshot = call.call_snapshot;
                  aggregate_identity_admission_digest =
                    digest
                      (String.concat "\000"
                         [
                           logical_digest;
                           call.call_snapshot;
                           registration.snapshot_digest;
                           string_of_int call.invocation_ordinal;
                           application_snapshot;
                         ]);
                  aggregate_identity_session = session;
                  aggregate_identity_active = ref true;
                }
              in
              call.aggregate_identity <- Some identity;
              Ok
                ( call.summary,
                  call.call_snapshot,
                  registration.snapshot_digest,
                  identity )
          | Some admitted when admitted == session -> (
              match call.aggregate_identity with
              | Some identity
                when identity.aggregate_identity_session == session
                     && String.equal identity.aggregate_identity_call_snapshot
                          call.call_snapshot
                     && authenticate_aggregate_application_identity identity ->
                  if
                    aggregate_application_identity_matches identity
                      ~logical_digest:identity.aggregate_identity_logical_digest
                      ~call_snapshot:call.call_snapshot
                      ~registration_snapshot:registration.snapshot_digest
                      ~invocation_ordinal:call.invocation_ordinal
                      ~application_snapshot
                  then
                    Ok
                      ( call.summary,
                        call.call_snapshot,
                        registration.snapshot_digest,
                        identity )
                  else
                    Error
                      "[VERO_DEPENDENCY] retained aggregate application \
                       admission snapshot changed"
              | Some _ | None ->
                  Error
                    "[VERO_DEPENDENCY] retained aggregate application identity \
                     is stale")
          | Some _ ->
              Error
                "[VERO_DEPENDENCY] retained aggregate descriptor was copied or \
                 replayed")
    | Some _, Some _ ->
        Error
          "[VERO_DEPENDENCY] retained aggregate descriptor does not authorize \
           this call"
    | Some _, None ->
        Error
          "[VERO_DEPENDENCY] retained aggregate call is not in the physical \
           consumer registration"
    | None, _ ->
        Error
          "[VERO_DEPENDENCY] retained aggregate descriptor is not bound to \
           this consumer session"

let is_imported registration id =
  require_registration registration;
  List.exists
    (fun (callable : callable_snapshot) ->
      same_function_id callable.definition.Sst.function_id id)
    registration.environment.callables

let finite_requirement registration id ordinal =
  require_registration registration;
  if
    List.exists
      (fun call ->
        same_function_id call.summary.definition.Sst.function_id id)
      registration.calls
  then
    Option.bind
      (List.find_opt
         (fun (callable : callable_snapshot) ->
           same_function_id callable.definition.Sst.function_id id)
         registration.environment.callables)
      (fun callable ->
        List.find_opt
          (fun requirement -> requirement.ordinal = ordinal)
          callable.finite_requirements)
  else None

let dump environment =
  require environment;
  environment.callables
  |> List.map (fun (callable : callable_snapshot) ->
      Printf.sprintf "%s uid=%s formals=%d digest=%s" callable.path
        callable.binding_uid
        (List.length callable.definition.parameters)
        callable.summary_digest)
  |> String.concat "\n"

module For_testing = struct
  type aggregate_lifecycle = {
    descriptors_issued : int;
    applications_admitted : int;
    descriptors_invalidated : int;
  }

  let reset_aggregate_lifecycle () =
    aggregate_descriptors_issued := 0;
    aggregate_applications_admitted := 0;
    aggregate_descriptors_invalidated := 0

  let aggregate_lifecycle () =
    {
      descriptors_issued = !aggregate_descriptors_issued;
      applications_admitted = !aggregate_applications_admitted;
      descriptors_invalidated = !aggregate_descriptors_invalidated;
    }

  let test_aggregate_application_identity ~logical_digest ~call_snapshot
      ~registration_snapshot ~invocation_ordinal ~application_snapshot =
    {
      aggregate_identity_issuer = issuer;
      aggregate_identity_token = ref ();
      aggregate_identity_logical_digest = logical_digest;
      aggregate_identity_call_snapshot = call_snapshot;
      aggregate_identity_admission_digest =
        digest
          (String.concat "\000"
             [
               logical_digest;
               call_snapshot;
               registration_snapshot;
               string_of_int invocation_ordinal;
               application_snapshot;
             ]);
      aggregate_identity_session = ref ();
      aggregate_identity_active = ref true;
    }

  let invalidate_aggregate_application_identity identity =
    identity.aggregate_identity_active := false

  type authority_attack =
    | Raw_retained_summary
    | Forged_retained_summary
    | Textual_specialization_authority

  let authority_attacks =
    [
      Raw_retained_summary;
      Forged_retained_summary;
      Textual_specialization_authority;
    ]

  let authority_attack_name = function
    | Raw_retained_summary -> "raw-retained-summary"
    | Forged_retained_summary -> "forged-retained-summary"
    | Textual_specialization_authority -> "textual-specialization-authority"

  let first_provider environment =
    require environment;
    match environment.providers with
    | provider :: _ -> provider
    | [] -> invalid_arg "authority attack requires a retained provider"

  let first_callable provider =
    match provider.description.callables with
    | callable :: _ -> callable
    | [] -> invalid_arg "authority attack requires a retained callable"

  let first_type provider =
    match provider.description.types with
    | typ :: _ -> typ
    | [] -> invalid_arg "authority attack requires a retained type"

  let run_authority_attack environment attack =
    let provider = first_provider environment in
    let description = provider.description in
    match attack with
    | Raw_retained_summary ->
        let callable = first_callable provider in
        let provider_completion =
          Verified_provider_completion_private.For_testing.forged
            ~definition:callable.definition
        in
        seal_provider ~provider_completion
          ~implementation:provider.implementation ~program:provider.program
          ~direct_dependencies:provider.direct_dependencies description
        |> Result.map (fun _ -> ())
    | Forged_retained_summary ->
        let callable = first_callable provider in
        let definition =
          {
            callable.definition with
            Sst.result_type =
              Sst.Tuple [ (None, callable.definition.result_type) ];
          }
        in
        let description =
          {
            description with
            callables =
              { callable with definition } :: List.tl description.callables;
          }
        in
        seal_provider ~provider_completion:provider.provider_completion
          ~implementation:provider.implementation ~program:provider.program
          ~direct_dependencies:provider.direct_dependencies description
        |> Result.map (fun _ -> ())
    | Textual_specialization_authority ->
        let typ = first_type provider in
        let type_id = typ.definition.Sst.type_id in
        let definition =
          {
            typ.definition with
            Sst.type_id =
              { type_id with type_name = type_id.type_name ^ "<forged>" };
          }
        in
        validate_description
          {
            description with
            types =
              {
                typ with
                definition;
                resolved_path = typ.resolved_path ^ "<forged>";
              }
              :: List.tl description.types;
          }

  let stale_recursive_evidence environment =
    let provider = first_provider environment in
    match
      List.find_opt
        (fun (callable : provider_callable) ->
          Parametric_signature_private.recursive_verified callable.signature)
        provider.description.callables
    with
    | None -> Error "retained provider has no verified recursive signature"
    | Some callable ->
        Parametric_signature_private.For_testing.stale_recursive_body
          callable.signature

  type abi_attack =
    | Wrong_label
    | Reordered_same_type
    | Binding_id_collision
    | Omitted_argument
    | Default_argument
    | Optional_argument
    | Destructured_formal
    | Wildcard_formal
    | Invalid_result_binder
    | Partial_application
    | Ambiguous_alias
    | Ambiguous_open
    | Functor_path
    | Public_executor_only_provider

  let abi_attacks =
    [
      Wrong_label;
      Reordered_same_type;
      Binding_id_collision;
      Omitted_argument;
      Default_argument;
      Optional_argument;
      Destructured_formal;
      Wildcard_formal;
      Invalid_result_binder;
      Partial_application;
      Ambiguous_alias;
      Ambiguous_open;
      Functor_path;
      Public_executor_only_provider;
    ]

  let abi_attack_name = function
    | Wrong_label -> "retained_wrong_label"
    | Reordered_same_type -> "retained_reordered_same_type"
    | Binding_id_collision -> "retained_binding_id_collision"
    | Omitted_argument -> "retained_omitted_argument"
    | Default_argument -> "retained_default_argument"
    | Optional_argument -> "retained_optional_argument"
    | Destructured_formal -> "retained_destructured_formal"
    | Wildcard_formal -> "retained_wildcard_formal"
    | Invalid_result_binder -> "retained_invalid_result_binder"
    | Partial_application -> "retained_partial_application"
    | Ambiguous_alias -> "retained_ambiguous_alias"
    | Ambiguous_open -> "retained_ambiguous_open"
    | Functor_path -> "retained_functor_path"
    | Public_executor_only_provider -> "public_executor_only_provider"

  let abi_attack_named name =
    List.find_opt
      (fun attack -> String.equal (abi_attack_name attack) name)
      abi_attacks

  let span = Diagnostic.file_span "retained_abi_attack.ml"
  let node = { Sst.type_index = 0; type_name = "node" }

  let binding id name =
    {
      Sst.id;
      name;
      typ = Sst.Aggregate node;
      uniqueness = Sst.Definitely_aliased;
      span;
    }

  let pattern id name =
    {
      Sst.pattern_desc = Sst.Bind (binding id name);
      typ = Sst.Aggregate node;
      span;
    }

  let parameter id name =
    Sst.Value_parameter
      { Sst.label = None; pattern = pattern id name; optional_default = None }

  let unit_expression =
    { Sst.expression_desc = Sst.Unit_constant; typ = Sst.Unit; span }

  let bool_expression =
    { Sst.expression_desc = Sst.Bool_constant true; typ = Sst.Bool; span }

  let base_definition =
    {
      Sst.function_id = { function_index = 0; function_name = "checked" };
      type_binders = [];
      mode = Sst.Exec;
      recursive = false;
      parameters = [ parameter 1 "left"; parameter 2 "right" ];
      contracts = Sst.empty_contracts;
      body =
        Sst.Checked_exec
          {
            body = { stage = Sst.Runtime; expression = unit_expression };
            provenance =
              Sst.Authenticated_typedtree
                { source_file = span.file; declaration_span = span };
          };
      policy = Sst.Default_linear_z3;
      result_type = Sst.Unit;
      returns_unique_parameter = None;
      span;
    }

  let signature definition parameter_kinds parameter_modes result_mode =
    match
      Parametric_signature_private.create ~definition ~parameter_kinds
        ~parameter_modes ~result_mode ~recursive_evidence:None
    with
    | Ok signature -> signature
    | Error message -> failwith message

  let base_callable =
    {
      resolved_path = "Provider.checked";
      binding_uid = "Provider.0";
      definition = base_definition;
      signature =
        signature base_definition
          [ Positional_parameter; Positional_parameter ]
          [ Sst.Exec_instance; Sst.Exec_instance ]
          Sst.Exec_instance;
      finite_requirements = [];
      model = None;
    }

  let aggregate_model_callable ~domain ~snapshot =
    let result_binding =
      {
        Sst.id = 90;
        name = "opaque";
        typ = Sst.Aggregate snapshot;
        uniqueness = Sst.Definitely_aliased;
        span;
      }
    in
    let domain_pattern =
      {
        Sst.pattern_desc =
          Sst.Bind
            {
              id = 1;
              name = "stack";
              typ = Sst.Aggregate domain;
              uniqueness = Sst.Definitely_aliased;
              span;
            };
        typ = Sst.Aggregate domain;
        span;
      }
    in
    let model_definition =
      {
        base_definition with
        Sst.function_id = { function_index = 10; function_name = "Stack.model" };
        mode = Sst.Spec;
        parameters =
          [
            Sst.Value_parameter
              {
                Sst.label = None;
                pattern = domain_pattern;
                optional_default = None;
              };
          ];
        body =
          Sst.Spec_definition
            {
              stage = Sst.Logical;
              expression =
                {
                  expression_desc =
                    Sst.Variable
                      {
                        binding = result_binding;
                        use_uniqueness = Sst.Definitely_aliased;
                      };
                  typ = Sst.Aggregate snapshot;
                  span;
                };
            };
        result_type = Sst.Aggregate snapshot;
      }
    in
    {
      base_callable with
      resolved_path = "Provider.Stack.model";
      definition = model_definition;
      signature =
        signature model_definition [ Positional_parameter ]
          [ Sst.Ghost_instance ] Sst.Ghost_instance;
      model =
        Some
          {
            domain;
            result_type = Sst.Aggregate snapshot;
            closure_type_ids = [];
            closure_digest = "";
          };
    }

  let aggregate_closure_matrix () =
    let domain = { Sst.type_index = 10; type_name = "Stack.t" } in
    let snapshot = { Sst.type_index = 11; type_name = "snapshot" } in
    let foreign = { Sst.type_index = 12; type_name = "foreign" } in
    let field_id =
      {
        Sst.field_owner = Sst.Record_owner snapshot;
        field_index = 0;
        field_name = "value";
      }
    in
    let field field_type field_mutability =
      {
        Sst.field_id;
        field_type;
        field_mutability;
        field_modalities =
          {
            Sst.uniqueness_modality = Sst.Preserve_uniqueness;
            linearity_modality = Sst.Preserve_linearity;
          };
        span;
      }
    in
    let type_definition field =
      {
        Sst.type_id = snapshot;
        type_kind = Sst.Record_definition [ field ];
        representation = Sst.Revealed;
        span;
      }
    in
    let model_callable = aggregate_model_callable ~domain ~snapshot in
    let provider_type ?(revealed = true) ?(logical = true) field =
      {
        resolved_path = "Provider.snapshot";
        source_path = "Provider.snapshot";
        source_name = "snapshot";
        binding_uid = "Provider.snapshot.uid";
        definition = type_definition field;
        parametric_descriptor = None;
        revealed;
        logical;
        field_modes = [ (field.field_id, Sst.Ghost_instance) ];
        rank_profile_digest = None;
      }
    in
    let description types callables =
      {
        unit_name = "Provider";
        interface_digest = "interface";
        source_digest = "source";
        family_digest = "family";
        import_digest = "imports";
        callables;
        types;
      }
    in
    let retained name provider =
      match obligation_description provider with
      | Ok { callables = [ _ ]; _ } -> name ^ "=retained"
      | Ok _ -> failwith (name ^ " unexpectedly excluded")
      | Error message -> failwith (name ^ " unexpectedly rejected: " ^ message)
    in
    let excluded_schema name provider =
      match obligation_description provider with
      | Ok { callables = []; _ } -> name ^ "=excluded"
      | Ok _ -> failwith (name ^ " unexpectedly retained")
      | Error message -> failwith (name ^ " unexpectedly rejected: " ^ message)
    in
    let scalar = field Sst.Int Sst.Immutable_field in
    let recursive = field (Sst.Aggregate snapshot) Sst.Immutable_field in
    let excluded_definition =
      { base_definition with result_type = Sst.Aggregate foreign }
    in
    let excluded =
      {
        base_callable with
        resolved_path = "Provider.aggregate_operation";
        definition = excluded_definition;
        signature =
          signature excluded_definition
            [ Positional_parameter; Positional_parameter ]
            [ Sst.Exec_instance; Sst.Exec_instance ]
            Sst.Exec_instance;
      }
    in
    [
      retained "schema-record"
        (description [ provider_type scalar ] [ model_callable ]);
      excluded_schema "schema-hidden"
        (description
           [ provider_type ~revealed:false ~logical:false scalar ]
           [ model_callable ]);
      excluded_schema "schema-mutable"
        (description
           [ provider_type (field Sst.Int Sst.Mutable_field) ]
           [ model_callable ]);
      retained "schema-tuple"
        (description
           [
             provider_type
               (field
                  (Sst.Tuple [ (None, Sst.Int); (None, Sst.Bool) ])
                  Sst.Immutable_field);
           ]
           [ model_callable ]);
      excluded_schema "schema-foreign"
        (description
           [ provider_type (field (Sst.Aggregate foreign) Sst.Immutable_field) ]
           [ model_callable ]);
      retained "schema-uniform-recursive"
        (description [ provider_type recursive ] [ model_callable ]);
      (match
         obligation_description
           (description [ provider_type scalar ] [ excluded; model_callable ])
       with
      | Ok { callables = [ _ ]; _ } ->
          "schema-provider-filter=accepted-model-only"
      | Ok _ | Error _ -> failwith "provider-wide filtering changed");
    ]

  let generic_family_sealing_matrix () =
    let rejected name type_name =
      let snapshot = { Sst.type_index = 640; type_name } in
      let typ =
        {
          resolved_path = "Provider." ^ type_name;
          source_path = "Provider.snapshot";
          source_name = "snapshot";
          binding_uid = "Provider.snapshot.uid";
          definition =
            {
              Sst.type_id = snapshot;
              type_kind = Sst.Record_definition [];
              representation = Sst.Revealed;
              span;
            };
          parametric_descriptor = None;
          revealed = true;
          logical = true;
          field_modes = [];
          rank_profile_digest = None;
        }
      in
      let description =
        {
          unit_name = "Provider";
          interface_digest = "interface";
          source_digest = "source";
          family_digest = "family";
          import_digest = "imports";
          callables = [];
          types = [ typ ];
        }
      in
      match validate_description description with
      | Error _ -> name ^ "=rejected"
      | Ok () -> failwith (name ^ " unexpectedly accepted")
    in
    [
      "closed-arguments-authority=removed";
      rejected "textual-int-specialization" "snapshot<int>";
      rejected "textual-bool-specialization" "snapshot<bool>";
      rejected "textual-nested-specialization" "snapshot<list<int>>";
    ]

  let with_pattern index replacement (callable : provider_callable) =
    let parameters =
      List.mapi
        (fun ordinal parameter ->
          match parameter with
          | Sst.Callback_parameter _ -> parameter
          | Sst.Value_parameter value ->
              if ordinal = index then
                Sst.Value_parameter { value with Sst.pattern = replacement }
              else parameter)
        callable.definition.parameters
    in
    { callable with definition = { callable.definition with parameters } }

  let invalid_result (callable : provider_callable) =
    let binder = { Sst.pattern_desc = Sst.Wildcard; typ = Sst.Unit; span } in
    let ensure =
      {
        Sst.clause_index = 0;
        binder = Some binder;
        predicate = { stage = Sst.Logical; expression = bool_expression };
        span;
      }
    in
    {
      callable with
      definition =
        {
          callable.definition with
          contracts = { Sst.empty_contracts with ensures = [ ensure ] };
        };
    }

  let run callable ?(target = Exact_direct_target) ?(supplied_count = 2)
      ?(has_omitted = false) ?(partial = false)
      ?(argument_labels = [ None; None ]) ?(private_driver_completion = true) ()
      =
    validate_application_abi callable ~target ~supplied_count ~has_omitted
      ~partial ~argument_labels ~private_driver_completion

  let run_abi_attack = function
    | Wrong_label ->
        let first = Sst.require_value_parameter (List.hd base_definition.parameters)
        and rest = List.tl base_definition.parameters in
        let definition =
          {
            base_definition with
            parameters = Sst.Value_parameter { first with Sst.label = Some "wrong" } :: rest;
          }
        in
        run
          { base_callable with definition }
          ~argument_labels:[ Some "wrong"; None ] ()
    | Reordered_same_type ->
        let first = Sst.require_value_parameter (List.hd base_definition.parameters)
        and rest = List.tl base_definition.parameters in
        let definition =
          {
            base_definition with
            parameters = Sst.Value_parameter { first with Sst.label = Some "reordered" } :: rest;
          }
        in
        run
          { base_callable with definition }
          ~argument_labels:[ Some "reordered"; None ] ()
    | Binding_id_collision ->
        run (with_pattern 1 (pattern 1 "right") base_callable) ()
    | Omitted_argument ->
        run base_callable ~supplied_count:1 ~has_omitted:true ()
    | Default_argument ->
        let first = Sst.require_value_parameter (List.hd base_definition.parameters)
        and rest = List.tl base_definition.parameters in
        let default =
          {
            Sst.optional_pattern = pattern 3 "default";
            optional_expression = unit_expression;
          }
        in
        let definition =
          {
            base_definition with
            parameters =
              Sst.Value_parameter
                {
                  first with
                  Sst.label = Some "?left";
                  optional_default = Some default;
                }
              :: rest;
          }
        in
        run
          { base_callable with definition }
          ~argument_labels:[ Some "?left"; None ] ()
    | Optional_argument ->
        let first = Sst.require_value_parameter (List.hd base_definition.parameters)
        and rest = List.tl base_definition.parameters in
        let definition =
          {
            base_definition with
            parameters = Sst.Value_parameter { first with Sst.label = Some "?left" } :: rest;
          }
        in
        run
          { base_callable with definition }
          ~argument_labels:[ Some "?left"; None ] ()
    | Destructured_formal ->
        let destructured =
          {
            Sst.pattern_desc =
              Sst.Tuple_pattern
                [ (None, pattern 3 "first"); (None, pattern 4 "second") ];
            typ = Sst.Aggregate node;
            span;
          }
        in
        run (with_pattern 0 destructured base_callable) ()
    | Wildcard_formal ->
        let wildcard =
          { Sst.pattern_desc = Sst.Wildcard; typ = Sst.Aggregate node; span }
        in
        run (with_pattern 0 wildcard base_callable) ()
    | Invalid_result_binder -> run (invalid_result base_callable) ()
    | Partial_application ->
        run base_callable ~supplied_count:1 ~partial:true ()
    | Ambiguous_alias -> run base_callable ~target:Ambiguous_alias_target ()
    | Ambiguous_open -> run base_callable ~target:Ambiguous_open_target ()
    | Functor_path -> run base_callable ~target:Functor_generated_target ()
    | Public_executor_only_provider ->
        run base_callable ~private_driver_completion:false ()
end
