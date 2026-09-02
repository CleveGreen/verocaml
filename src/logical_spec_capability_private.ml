type model_capability = {
  model_handle_token : unit ref;
  model_invariant_id : string;
  model_abstract_type : Sst.type_id;
  model_representation_root : Sst.type_id;
  predicate_callable : Sst.function_id;
  model_descriptor : Sst_validation.model_descriptor;
  callable_descriptor : Sst_validation.callable_descriptor;
  definition : Sst.function_definition;
  model_callable : Sst.function_id;
  model_result_type : Sst.typ;
  predicate_digest : string;
}

let model_capability ~model_handle_token ~model_invariant_id
    ~model_abstract_type ~model_representation_root ~predicate_callable
    ~model_descriptor ~callable_descriptor ~definition ~model_callable
    ~model_result_type ~predicate_digest =
  {
    model_handle_token;
    model_invariant_id;
    model_abstract_type;
    model_representation_root;
    predicate_callable;
    model_descriptor;
    callable_descriptor;
    definition;
    model_callable;
    model_result_type;
    predicate_digest;
  }

let model_definition capability = capability.definition
let model_callable capability = capability.model_callable
let model_result_type capability = capability.model_result_type
let model_representation_root capability = capability.model_representation_root

type contract_clause_kind = Requires | Ensures

type root_identity =
  | Invariant_root of {
      invariant_id : string;
      predicate_callable : Sst.function_id;
      predicate_digest : string;
    }
  | Contract_root of {
      callable : Sst.function_id;
      clause_kind : contract_clause_kind;
      ordinal : int;
    }

let invariant_root ~invariant_id ~predicate_callable ~predicate_digest =
  Invariant_root { invariant_id; predicate_callable; predicate_digest }

let contract_root ~callable ~clause_kind ~ordinal =
  Contract_root { callable; clause_kind; ordinal }

module type Model_source = sig
  type handle

  val model_callable : handle -> Sst.function_id
  val invariant_id : handle -> string
  val abstract_type : handle -> Sst.type_id
  val predicate_callable : handle -> Sst.function_id
  val model_snapshot_type : handle -> Sst.typ
  val predicate_digest : handle -> string
  val predicate_definition : handle -> Sst.function_definition
end

module Make_model_capture (Source : Model_source) = struct
  let identity handle =
    invariant_root ~invariant_id:(Source.invariant_id handle)
      ~predicate_callable:(Source.predicate_callable handle)
      ~predicate_digest:(Source.predicate_digest handle)

  let model validated handle =
    let model_callable = Source.model_callable handle in
    match
      ( Sst_validation.find_callable validated model_callable,
        Sst_validation.find_model validated model_callable )
    with
    | Some descriptor, Some model_descriptor ->
        let model_abstract_type = Source.abstract_type handle in
        let model_representation_root =
          match Sst_validation.abstraction_evidence validated model_abstract_type with
          | Some evidence -> evidence.hidden_implementation_type
          | None -> model_abstract_type
        in
        Some
          (model_capability ~model_handle_token:(ref ())
             ~model_invariant_id:(Source.invariant_id handle)
             ~model_abstract_type ~model_representation_root
             ~predicate_callable:(Source.predicate_callable handle)
             ~model_descriptor
             ~callable_descriptor:(Sst_validation.model_callable model_descriptor)
             ~definition:(Sst_validation.callable_definition descriptor)
             ~model_callable ~model_result_type:(Source.model_snapshot_type handle)
             ~predicate_digest:(Source.predicate_digest handle))
    | (Some _ | None), (Some _ | None) -> None

  let root handle reason =
    let expression =
      match (Source.predicate_definition handle).body with
      | Sst.Spec_definition body -> body.expression
      | _ -> assert false
    in
    (identity handle, expression, reason)

  let materialize validated decisions =
    ( List.filter_map
        (fun (handle, include_model, _) ->
          if include_model then model validated handle else None)
        decisions,
      List.map (fun (handle, _, reason) -> root handle reason) decisions )
end

type call_target =
  | Local_nonrecursive of Sst.function_definition
  | Captured_model of model_capability
  | Opaque_recursive
  | Unsupported

type permit = {
  validated : Sst_validation.validated_program;
  program : Sst.program;
  policy : Sst.verification_policy;
  root_identity : root_identity;
  root : Sst.expression;
  definitions : (int * Sst.function_definition) list;
  callable_descriptors : Sst_validation.callable_descriptor list;
  aggregate_descriptors : Sst.type_definition list;
  option_descriptors : Sst.typ list;
  field_descriptors : Sst.field_definition list;
  model_capabilities : (model_capability * Sst.field_id list) list;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name

let same_field_id (left : Sst.field_id) (right : Sst.field_id) =
  left.field_index = right.field_index
  && String.equal left.field_name right.field_name
  &&
  match (left.field_owner, right.field_owner) with
  | Sst.Record_owner left, Sst.Record_owner right -> same_type_id left right
  | Sst.Constructor_owner left, Sst.Constructor_owner right ->
      left.constructor_index = right.constructor_index
      && String.equal left.constructor_name right.constructor_name
      && same_type_id left.constructor_type right.constructor_type
  | Sst.Record_owner _, Sst.Constructor_owner _
  | Sst.Constructor_owner _, Sst.Record_owner _ ->
      false

let same_clause_kind left right =
  match (left, right) with
  | Requires, Requires | Ensures, Ensures -> true
  | Requires, Ensures | Ensures, Requires -> false

let same_root_identity left right =
  match (left, right) with
  | Invariant_root left, Invariant_root right ->
      String.equal left.invariant_id right.invariant_id
      && same_function_id left.predicate_callable right.predicate_callable
      && String.equal left.predicate_digest right.predicate_digest
  | Contract_root left, Contract_root right ->
      same_function_id left.callable right.callable
      && same_clause_kind left.clause_kind right.clause_kind
      && left.ordinal = right.ordinal
  | Invariant_root _, Contract_root _ | Contract_root _, Invariant_root _ -> false

let root_identity_to_string = function
  | Invariant_root { invariant_id; predicate_callable; predicate_digest } ->
      Printf.sprintf "invariant:%s:%s#%d:%s" invariant_id
        predicate_callable.function_name predicate_callable.function_index
        predicate_digest
  | Contract_root { callable; clause_kind; ordinal } ->
      Printf.sprintf "contract:%s#%d:%s:%d" callable.function_name
        callable.function_index
        (match clause_kind with Requires -> "requires" | Ensures -> "ensures")
        ordinal

let create_permit ~validated ~root_identity ~root ~definitions
    ~callable_descriptors ~aggregate_descriptors ~option_descriptors
    ~field_descriptors ~model_capabilities =
  let program = Sst_validation.program validated in
  {
    validated;
    program;
    policy = program.policy;
    root_identity;
    root;
    definitions;
    callable_descriptors;
    aggregate_descriptors;
    option_descriptors;
    field_descriptors;
    model_capabilities;
  }

let validate_program permit validated =
  if
    permit.validated == validated
    && permit.program == Sst_validation.program validated
    && permit.program.policy = permit.policy
  then Ok ()
  else Error "strict permit program or policy mismatch"

let validate_root permit ~validated ~root_identity root =
  let* () = validate_program permit validated in
  if
    same_root_identity permit.root_identity root_identity && permit.root == root
  then Ok ()
  else Error "strict permit root token mismatch"

type authorized_call = {
  definition : Sst.function_definition;
  model : model_capability option;
}

let same_model_capability left right =
  left.model_handle_token == right.model_handle_token
  && String.equal left.model_invariant_id right.model_invariant_id
  && same_type_id left.model_abstract_type right.model_abstract_type
  && same_type_id left.model_representation_root right.model_representation_root
  && same_function_id left.predicate_callable right.predicate_callable
  && left.model_descriptor == right.model_descriptor
  && left.callable_descriptor == right.callable_descriptor
  && left.definition == right.definition
  && same_function_id left.model_callable right.model_callable
  && left.model_result_type = right.model_result_type
  && String.equal left.predicate_digest right.predicate_digest

let validate_model_capture validated capability =
  if
    same_function_id capability.model_callable capability.definition.function_id
    && capability.definition
       == Sst_validation.callable_definition capability.callable_descriptor
    && capability.callable_descriptor
       == Sst_validation.model_callable capability.model_descriptor
    &&
    match Sst_validation.find_model validated capability.model_callable with
    | Some descriptor -> descriptor == capability.model_descriptor
    | None -> false
  then Ok ()
  else Error "stale-model-identity"

let find_definition permit callee =
  match List.assoc_opt callee.Sst.function_index permit.definitions with
  | Some definition when same_function_id definition.function_id callee ->
      Some definition
  | Some _ | None -> None

let captured_model permit definition =
  permit.model_capabilities
  |> List.find_map (fun ((capability : model_capability), _) ->
         if capability.definition == definition then Some capability else None)

let authorize_call permit ~validated callee ~result_type =
  let* () = validate_program permit validated in
  match find_definition permit callee with
  | None -> Error "strict permit does not cover the call target"
  | Some definition ->
      let* () =
        match
          List.find_opt
            (fun descriptor ->
              same_function_id (Sst_validation.callable_id descriptor) callee)
            permit.callable_descriptors
        with
        | Some descriptor
          when Sst_validation.callable_definition descriptor == definition ->
            Ok ()
        | Some _ | None -> Error "strict callable descriptor mismatch"
      in
      (match captured_model permit definition with
      | None ->
          if definition.result_type = result_type then
            Ok { definition; model = None }
          else Error "strict call result type mismatch"
      | Some model ->
          if
            definition == model.definition
            && same_function_id callee model.model_callable
            && model.callable_descriptor
               == Sst_validation.model_callable model.model_descriptor
            && model.definition
               == Sst_validation.callable_definition model.callable_descriptor
            &&
            (match Sst_validation.find_model validated model.model_callable with
            | Some descriptor -> descriptor == model.model_descriptor
            | None -> false)
            && result_type = model.model_result_type
          then Ok { definition; model = Some model }
          else Error "strict model call capability mismatch")

let authorize_aggregate permit ~validated type_id =
  let* () = validate_program permit validated in
  if
    List.exists
      (fun descriptor -> same_type_id descriptor.Sst.type_id type_id)
      permit.aggregate_descriptors
  then Ok ()
  else Error "strict permit does not cover the aggregate descriptor"

let vir_aggregate_type_of_sst descriptors = function
  | Sst.Aggregate type_id ->
      Some
        {
          Vir.aggregate_type_index = type_id.type_index;
          aggregate_type_name = type_id.type_name;
          aggregate_type_arguments = [];
        }
  | Sst.Application (constructor, arguments) -> (
      match Parametric_adt.find descriptors constructor with
      | None -> None
      | Some descriptor ->
          let type_id = Parametric_adt.type_id descriptor in
          Some
            {
              Vir.aggregate_type_index = type_id.type_index;
              aggregate_type_name =
                type_id.type_name ^ "<"
                ^ String.concat "," (List.map Parametric_type.to_string arguments)
                ^ ">";
              aggregate_type_arguments = arguments;
            })
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
  | Sst.Parameter _ ->
      None

let validate_aggregate_value permit ~validated ~descriptor typ actual =
  let* () = authorize_aggregate permit ~validated descriptor in
  let expected =
    vir_aggregate_type_of_sst
      (Sst_validation.program validated).parametric_adts typ
  in
  if expected = Some actual then Ok ()
  else Error "strict aggregate descriptor does not match the captured program"

let authorize_option permit ~validated typ =
  let* () = validate_program permit validated in
  if List.mem typ permit.option_descriptors then Ok ()
  else Error "strict permit does not cover the option descriptor"

let validate_option_value permit ~validated typ actual_option actual_aggregate =
  let* () = authorize_option permit ~validated typ in
  let descriptors = (Sst_validation.program validated).parametric_adts in
  let expected_option = Parametric_adt.option_instance descriptors typ in
  let expected_aggregate = vir_aggregate_type_of_sst descriptors typ in
  if
    expected_option = Some actual_option
    && expected_aggregate = Some actual_aggregate
  then Ok ()
  else Error "strict option descriptor does not match the captured program"

let authorize_field_read permit ~validated ~current_model field ~result_type =
  let* () = validate_program permit validated in
  match
    List.find_opt
      (fun descriptor -> same_field_id descriptor.Sst.field_id field)
      permit.field_descriptors
  with
  | None -> Error "strict permit does not cover the field descriptor"
  | Some descriptor when descriptor.field_type <> result_type ->
      Error "strict field result type mismatch"
  | Some { Sst.field_mutability = Sst.Immutable_field; _ } -> Ok false
  | Some { Sst.field_mutability = Sst.Mutable_field; _ } -> (
      match current_model with
      | None -> Error "mutable field read has no captured model capability"
      | Some current -> (
          match
            List.find_opt
              (fun (captured, _) -> same_model_capability captured current)
              permit.model_capabilities
          with
          | None -> Error "mutable field read used a sibling model capability"
          | Some (_, fields) ->
              if List.exists (same_field_id field) fields then Ok true
              else
                Error
                  "mutable field is outside the captured model representation"))

module For_testing = struct
  type formula_observation = {
    identity : root_identity;
    authority_before : string [@log_value.info];
    events_before : int [@log_value.info];
  }

  let formula_events = ref []
  let formula_observations = ref []
  let registry_unchanged = ref false

  let tracing () = Sys.getenv_opt "VEROCAML_TEST_FORMULA_TRACE" = Some "1"

  let event_count identity =
    List.assoc_opt (root_identity_to_string identity) !formula_events
    |> Option.value ~default:0

  let reset_formula_observations () =
    formula_events := [];
    formula_observations := [];
    registry_unchanged := false

  let before_authentication identity ~authority_snapshot:_authority_snapshot =
    formula_observations :=
      {
        identity;
        authority_before = (_authority_snapshot [@log_value.info]);
        events_before = (event_count identity [@log_value.info]);
      }
      :: !formula_observations

  let note_evaluation identity =
    let key = root_identity_to_string identity in
    let next = event_count identity + 1 in
    formula_events := (key, next) :: List.remove_assoc key !formula_events

  (* FIXME(delator): fully gated hook formals and tuple destructuring *)
  let trace_profile _identity _outcome =
    if tracing () then
      [%log.info "formula profile"
        ~root:
          (Delator.Field.string
             (root_identity_to_string _identity))
        ~outcome:(Delator.Field.string _outcome)]

  let trace_registry ~unchanged ~entries:_entries =
    registry_unchanged := unchanged;
    if tracing () then
      [%log.info "formula registry constructed"
        ~authority_unchanged:(Delator.Field.bool unchanged)
        ~entries:(Delator.Field.int _entries)]

  let trace_authority _function_name =
    if tracing () then
      [%log.info "formula authority observed"
        ~function_name:
          (Delator.Field.string _function_name)
        ~registry_unchanged:(Delator.Field.bool !registry_unchanged)]

  let trace_abstention _identity ~reason:_reason
      ~authority_snapshot:_authority_snapshot =
    if tracing () then
      let _before =
        List.find
          (fun observation ->
            root_identity_to_string observation.identity
            = root_identity_to_string _identity)
          !formula_observations
      in
      [%log.info "formula abstention"
        ~root:
          (Delator.Field.string
             (root_identity_to_string _identity))
        ~reason:(Delator.Field.string _reason)
        ~formula_events_before:
          (Delator.Field.int
             (_before.events_before [@log_value.info]))
        ~formula_events_before_fallback:
          (Delator.Field.int
             (event_count _identity))
        ~authority_before:
          (Delator.Field.string
             (_before.authority_before [@log_value.info]))
        ~authority_before_fallback:
          (Delator.Field.string _authority_snapshot)
        ~authority_equal:
          (Delator.Field.bool
             (String.equal
                (_before.authority_before [@log_value.info])
                _authority_snapshot))
        ~fallback_unchanged:(Delator.Field.bool true)]

  let trace_evaluation _identity ~function_name:_function_name
      ~authority_unchanged:_authority_unchanged =
    if tracing () then
      [%log.info "formula evaluation"
        ~root:
          (Delator.Field.string
             (root_identity_to_string _identity))
        ~function_name:
          (Delator.Field.string _function_name)
        ~formula_events:
          (Delator.Field.int (event_count _identity))
        ~authority_unchanged:
          (Delator.Field.bool _authority_unchanged)]

  let has_model_capability permit =
    List.exists (fun (_, fields) -> fields <> []) permit.model_capabilities

  let stale_model_control permit validated =
    match permit.model_capabilities with
    | (model, _) :: _ ->
        let stale =
          {
            model with
            model_callable =
              {
                model.model_callable with
                function_name = model.model_callable.function_name ^ "-stale";
              };
          }
        in
        let reason, _rejected =
          match validate_model_capture validated stale with
          | Error reason -> (reason, true)
          | Ok () -> ("accepted", false)
        in
        [%log.info "formula validator control"
          ~candidate:(Delator.Field.string "stale-model")
          ~reason:(Delator.Field.string reason)
          ~rejected:(Delator.Field.bool _rejected)];
        Some
          (Printf.sprintf "formula-validator candidate=stale-model reason=%s"
             reason)
    | [] -> None

  let rejected = function Error _ -> true | Ok _ -> false

  let row name rejected =
    [%log.info "capability negative control"
      ~control:(Delator.Field.string name)
      ~rejected:(Delator.Field.bool rejected)];
    Printf.sprintf "capability-negative %s=%s" name
      (if rejected then "rejected" else "accepted")

  let mutable_controls permit validated =
    match
      permit.model_capabilities
      |> List.find_map (fun (model, fields) ->
             match fields with field :: _ -> Some (model, field) | [] -> None)
    with
    | None -> [ false; false; false; false ]
    | Some (model, field) ->
        let descriptor =
          List.find
            (fun descriptor -> same_field_id descriptor.Sst.field_id field)
            permit.field_descriptors
        in
        let result_type = descriptor.field_type in
        let sibling =
          {
            model with
            model_handle_token = ref ();
            model_invariant_id = model.model_invariant_id ^ "-sibling";
          }
        in
        let narrowed =
          {
            permit with
            model_capabilities =
              List.map
                (fun (candidate, captured) ->
                  if same_model_capability candidate model then (candidate, [])
                  else (candidate, captured))
                permit.model_capabilities;
          }
        in
        [
          authorize_field_read permit ~validated ~current_model:None field
            ~result_type
          |> rejected;
          authorize_field_read permit ~validated ~current_model:(Some sibling)
            field ~result_type
          |> rejected;
          authorize_field_read narrowed ~validated ~current_model:(Some model)
            field ~result_type
          |> rejected;
          authorize_field_read permit ~validated ~current_model:(Some model)
            { field with Sst.field_name = field.field_name ^ "-wrong" }
            ~result_type
          |> rejected;
        ]

  let capability_negative_controls permit validated =
    let equal_root =
      validate_root permit ~validated ~root_identity:permit.root_identity
        { permit.root with Sst.expression_desc = permit.root.expression_desc }
      |> rejected
    in
    let wrong_root_identity =
      match permit.root_identity with
      | Contract_root identity ->
          let clause_kind =
            match identity.clause_kind with Requires -> Ensures | Ensures -> Requires
          in
          validate_root permit ~validated
            ~root_identity:(Contract_root { identity with clause_kind }) permit.root
          |> rejected
      | Invariant_root identity ->
          validate_root permit ~validated
            ~root_identity:
              (Invariant_root
                 {
                   identity with
                   predicate_digest = identity.predicate_digest ^ "-stale";
                 })
            permit.root
          |> rejected
    in
    let cross_program =
      match Sst_validation.validate permit.program with
      | Ok other ->
          validate_root permit ~validated:other
            ~root_identity:permit.root_identity permit.root
          |> rejected
      | Error _ -> false
    in
    let wrong_callable, wrong_result =
      match permit.definitions with
      | (_, definition) :: _ ->
          let sibling =
            {
              definition.function_id with
              function_name = definition.function_id.function_name ^ "-sibling";
            }
          in
          ( authorize_call permit ~validated sibling
              ~result_type:definition.result_type
            |> rejected,
            authorize_call permit ~validated definition.function_id
              ~result_type:
                (if definition.result_type = Sst.Bool then Sst.Unit else Sst.Bool)
            |> rejected )
      | [] -> (false, false)
    in
    let wrong_aggregate =
      authorize_aggregate permit ~validated
        { Sst.type_index = min_int; type_name = "$wrong-descriptor" }
      |> rejected
    in
    let mutable_controls = mutable_controls permit validated in
    List.map2 row
      [
        "equal-distinct-root";
        "requires-ensures-token-swap";
        "cross-program-root";
        "sibling-callable";
        "wrong-model-result";
        "wrong-aggregate-descriptor";
        "ordinary-spec-mutable-field";
        "sibling-model";
        "non-representation-mutable-field";
        "wrong-field-descriptor";
      ]
      ([
         equal_root;
         wrong_root_identity;
         cross_program;
         wrong_callable;
         wrong_result;
         wrong_aggregate;
       ]
      @ mutable_controls)
end
