type handle = {
  invariant_id : string;
  abstract_type : Sst.type_id;
  certificate_id : string;
  model_callable : Sst.function_id;
  model_snapshot_type : Sst.typ;
  predicate_callable : Sst.function_id;
  predicate_digest : string;
  public_operations :
    (Sst.function_id * Sst.abstract_operation_role) list;
  predicate_definition : Sst.function_definition;
}

type environment = { handles : handle list }

type error = {
  span : Diagnostic.span;
  message : string;
}

let ( let* ) result f = match result with Ok value -> f value | Error _ as e -> e

let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let error span message = Error { span; message }

let operation_id (operation : Sst.abstract_public_operation) =
  {
    Sst.function_index = operation.public_function_index;
    function_name = operation.public_function_name;
  }

let expression_children (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Lift_runtime_int operand -> [ operand ]
  | Sst.Tuple_value values -> List.map snd values
  | Sst.Record_value { fields; _ } -> List.map snd fields
  | Sst.Constructor_value { arguments; _ } -> arguments
  | Sst.Field_read { record; _ } -> [ record ]
  | Sst.Field_write { value; _ }
  | Sst.Shared_scalar_field_write { value; _ }
  | Sst.Mutable_write { value; _ }
  | Sst.Owned_tree_nested_write { value; _ }
  | Sst.Use_type_invariant { value; _ } ->
      [ value ]
  | Sst.Owned_tree_rebase _ -> []
  | Sst.Let_mutable (_, initial, body) -> [ initial; body ]
  | Sst.Let (bindings, body) -> List.map snd bindings @ [ body ]
  | Sst.Sequence (first, second)
  | Sst.Compare (_, first, second)
  | Sst.Boolean_binary (_, first, second) ->
      [ first; second ]
  | Sst.If (condition, consequent, alternative) ->
      condition :: consequent :: Option.to_list alternative
  | Sst.Match (scrutinee, cases) ->
      scrutinee
      :: List.concat_map
           (fun case -> Option.to_list case.Sst.case_guard @ [ case.case_body ])
           cases
  | Sst.Checked_arithmetic (_, operands) -> operands
  | Sst.Boolean_not operand | Sst.Proof_region operand | Sst.Old operand ->
      [ operand ]
  | Sst.Local_assert { predicate; _ } -> [ predicate ]
  | Sst.Direct_call { arguments; _ } ->
      List.filter_map
        (function
          | Sst.Value_argument { value; _ } -> Some value
          | Sst.Callback_argument _ -> None)
        arguments
  | Sst.Callback_call { arguments; _ } | Sst.Callback_requires { arguments; _ } ->
      List.map snd arguments
  | Sst.Callback_ensures { application = { arguments; _ }; result } ->
      List.map snd arguments @ [ result ]
  | Sst.Symbolic_application application ->
      Symbolic_application_private.arguments application
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      [ quantifier.quantifier_body ]
  | Sst.Reveal _ | Sst.Reveal_with_fuel _
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Mutable_read _ | Sst.Optional_absent ->
      []
  | Sst.Optional_present payload | Sst.Optional_forward payload -> [ payload ]

let rec count_calls target expression =
  let current =
    match expression.Sst.expression_desc with
    | Sst.Direct_call
        { call_form = Sst.Specification_call; callee; recursive = false; _ }
      when same_function_id target callee ->
        1
    | _ -> 0
  in
  current
  + List.fold_left
      (fun count child -> count + count_calls target child)
      0 (expression_children expression)

let use_type expression =
  match expression.Sst.expression_desc with
  | Sst.Use_type_invariant
      {
        value =
          {
            expression_desc =
              Sst.Variable
                {
                  binding = { typ = Sst.Aggregate type_id; _ };
                  _;
                };
            typ = Sst.Aggregate value_type;
            _;
          };
        _;
      }
    when same_type_id type_id value_type ->
      Some type_id
  | _ -> None

let rec collect_use_types expression =
  Option.to_list (use_type expression)
  @ List.concat_map collect_use_types (expression_children expression)

let rec transition_coverage expression =
  let own =
    match expression.Sst.expression_desc with
    | Sst.Field_write { transition = Some transition; _ }
    | Sst.Owned_tree_nested_write { transition; _ }
    | Sst.Owned_tree_rebase { transition } ->
        (true, [ transition ])
    | Sst.Field_write { transition = None; _ }
    | Sst.Shared_scalar_field_write _ | Sst.Mutable_write _ ->
        (false, [])
    | _ -> (true, [])
  in
  List.fold_left
    (fun (covered, transitions) child ->
      let child_covered, child_transitions = transition_coverage child in
      (covered && child_covered, transitions @ child_transitions))
    own (expression_children expression)

let callable_expressions (definition : Sst.function_definition) =
  let predicate (clause : Sst.predicate_clause) =
    clause.Sst.predicate.expression
  in
  let ensure (clause : Sst.ensures_clause) =
    clause.Sst.predicate.expression
  in
  List.map predicate definition.contracts.requires
  @ List.map ensure definition.contracts.ensures
  @ List.map predicate definition.contracts.decreases
  @ List.map predicate definition.contracts.assertions
  @
  match definition.body with
  | Sst.Checked_exec { body; _ }
  | Sst.Spec_definition body
  | Sst.Proof_body { body; _ }
  | Sst.Recursive_spec_definition { body; _ } ->
      [ body.expression ]
  | Sst.External_specification _
  | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _
  | Sst.Symbolic_declaration _ ->
      []

let authenticate_handle validated type_descriptor evidence invariant_operation =
  let type_id = Sst_validation.type_id type_descriptor in
  let predicate_id = operation_id invariant_operation in
  let* predicate =
    match Sst_validation.find_callable validated predicate_id with
    | Some predicate -> Ok predicate
    | None ->
        error invariant_operation.public_span
          "invariant predicate identity is absent from the validated snapshot"
  in
  let predicate_definition =
    Sst_validation.callable_definition predicate
  in
  let* () =
    match
      Spec_definition.authenticated_invariant_domain
        (Sst_validation.program validated).types predicate_definition
    with
    | Some domain when same_type_id domain type_id -> Ok ()
    | Some _ | None ->
        error predicate_definition.span
          "invariant predicate does not have the exact authenticated abstract \
           type and Boolean signature"
  in
  let models =
    List.filter
      (fun operation ->
        operation.Sst.public_role = Sst.Abstract_model
        || operation.public_role = Sst.Current_model)
      evidence.Sst.public_surface
  in
  let* model_operation =
    match models with
    | [ model ] -> Ok model
    | [] ->
        error predicate_definition.span
          "invariant requires exactly one authenticated immutable model"
    | _ :: duplicate :: _ ->
        error duplicate.public_span
          "invariant has multiple authenticated model identities"
  in
  let model_id = operation_id model_operation in
  let* model =
    match Sst_validation.find_model validated model_id with
    | Some model
      when
        same_type_id
          (Sst_validation.type_id (Sst_validation.model_domain model))
          type_id ->
        Ok model
    | Some _ | None ->
        error model_operation.public_span
          "invariant model identity or exact domain does not match its \
           certificate"
  in
  let model_snapshot_type =
    Sst_validation.model_result model
    |> Sst_validation.logical_source_type
  in
  let predicate_body =
    match predicate_definition.body with
    | Sst.Spec_definition body -> body.expression
    | _ -> assert false
  in
  let* () =
    if count_calls model_id predicate_body = 1 then Ok ()
    else
      error predicate_definition.span
        "invariant predicate must retain exactly one call to its authenticated \
         model"
  in
  let public_operations =
    List.map
      (fun operation -> (operation_id operation, operation.Sst.public_role))
      evidence.public_surface
  in
  let* () =
    let rec verify = function
      | [] -> Ok ()
      | (id, role) :: rest -> (
          match Sst_validation.find_callable validated id with
          | Some descriptor ->
              let definition =
                Sst_validation.callable_definition descriptor
              in
              let* () =
                match (role, definition.body) with
                | ( _,
                    ( Sst.Trusted_external_spec_target _
                    | Sst.Trusted_external_body _
                    | Sst.External_specification _ ) ) ->
                    error definition.span
                      "trusted or external code cannot issue invariant boundary \
                       authority"
                | Sst.Unique_transition, Sst.Checked_exec { body; _ } ->
                    let covered, transitions =
                      transition_coverage body.expression
                    in
                    if not covered then
                      error definition.span
                        "invariant transition contains an unauthenticated state \
                         write"
                    else if transitions = [] then
                      error definition.span
                        "invariant transition has no exact reconstructed \
                         successor"
                    else if
                      List.exists
                        (fun (transition : Sst.owned_tree_transition) ->
                          match transition.root.typ with
                          | Sst.Aggregate root_type ->
                              not (same_type_id root_type type_id)
                          | Sst.Unit | Sst.Int | Sst.Mathematical_int | Sst.Bool
                          | Sst.Tuple _
                          | Sst.Parameter _ | Sst.Application _ -> true)
                        transitions
                    then
                      error definition.span
                        "invariant transition successor belongs to the wrong \
                         exact root type"
                    else Ok ()
                | Sst.Unique_transition, _ ->
                    error definition.span
                      "invariant transition lacks a checked executable body"
                | ( Sst.Shared_invariant_transition,
                    Sst.Checked_exec { body; _ } ) ->
                    let covered, _ = transition_coverage body.expression in
                    if covered then
                      error definition.span
                        "shared invariant transition has no authenticated hidden write"
                    else Ok ()
                | Sst.Shared_invariant_transition, _ ->
                    error definition.span
                      "shared invariant transition lacks a checked executable body"
                | ( Sst.Abstract_constructor | Sst.Abstract_model
                  | Sst.Current_model | Sst.Abstract_invariant
                  | Sst.Terminal_read | Sst.Current_terminal_read
                  | Sst.Terminal_snapshot ),
                  _ ->
                    Ok ()
              in
              verify rest
          | None ->
              error evidence.constraint_span
                "invariant public-operation identity is absent from the exact \
                 semantic snapshot")
    in
    verify public_operations
  in
  let certificate_id = evidence.evidence_id in
  let invariant_id =
    Printf.sprintf "invariant:%s:%d:%s:%d" type_id.type_name
      type_id.type_index predicate_id.function_name predicate_id.function_index
  in
  let predicate_digest =
    Digest.to_hex
      (Digest.string
         (invariant_id ^ "\n"
        ^ Sst.to_string (Sst_validation.program validated)))
  in
  Ok
    {
      invariant_id;
      abstract_type = type_id;
      certificate_id;
      model_callable = model_id;
      model_snapshot_type;
      predicate_callable = predicate_id;
      predicate_digest;
      public_operations;
      predicate_definition;
    }

let authenticate validated =
  let* handles =
    let rec types handles = function
      | [] -> Ok (List.rev handles)
      | descriptor :: rest -> (
          match
            Sst_validation.visibility_representation
              (Sst_validation.type_visibility descriptor)
          with
          | Sst.Abstract_with_evidence
              (Sst.Authenticated_same_cmt_abstraction evidence) ->
              let invariants =
                List.filter
                  (fun operation ->
                    operation.Sst.public_role = Sst.Abstract_invariant)
                  evidence.public_surface
              in
              (match invariants with
              | [] -> types handles rest
              | [ invariant ] ->
                  let* handle =
                    authenticate_handle validated descriptor evidence invariant
                  in
                  types (handle :: handles) rest
              | _ :: duplicate :: _ ->
                  error duplicate.public_span
                    "an abstract type may retain only one invariant predicate")
          | Sst.Revealed
          | Sst.Abstract_with_evidence
              (Sst.Incomplete_abstraction_evidence _
              | Sst.Proposed_same_cmt_abstraction _) ->
              types handles rest)
    in
    types [] (Sst_validation.type_descriptors validated)
  in
  let find type_id =
    List.find_opt
      (fun handle -> same_type_id handle.abstract_type type_id)
      handles
  in
  let* () =
    let definitions =
      Sst_validation.callable_descriptors validated
      |> List.map Sst_validation.callable_definition
    in
    let rec functions = function
      | [] -> Ok ()
      | definition :: rest ->
          let use_types =
            callable_expressions definition
            |> List.concat_map collect_use_types
          in
          let rec uses = function
            | [] -> functions rest
            | type_id :: remaining -> (
                match find type_id with
                | Some handle
                  when
                    handle.predicate_callable.function_index
                    < definition.function_id.function_index ->
                    uses remaining
                | Some _ ->
                    error definition.span
                      "type invariant predicate is not accessible at this use"
                | None ->
                    error definition.span
                      "use_type_invariant has no authenticated exact-type \
                       invariant handle")
          in
          uses use_types
    in
    functions definitions
  in
  Ok { handles }

let error_to_string error =
  let span = error.span in
  Printf.sprintf "%s at %s:%d:%d-%d:%d" error.message span.file
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let handles environment = environment.handles

let find_for_type environment type_id =
  List.find_opt
    (fun handle -> same_type_id handle.abstract_type type_id)
    environment.handles

let find_for_constructor environment function_id =
  List.find_opt
    (fun handle ->
      List.exists
        (fun (candidate, role) ->
          role = Sst.Abstract_constructor
          && same_function_id candidate function_id)
        handle.public_operations)
    environment.handles

let find_for_operation environment function_id =
  List.find_map
    (fun handle ->
      List.find_map
        (fun (candidate, role) ->
          if same_function_id candidate function_id then Some (handle, role)
          else None)
        handle.public_operations)
    environment.handles

let invariant_id handle = handle.invariant_id
let abstract_type handle = handle.abstract_type
let certificate_id handle = handle.certificate_id
let model_callable handle = handle.model_callable
let model_snapshot_type handle = handle.model_snapshot_type
let predicate_callable handle = handle.predicate_callable
let predicate_digest handle = handle.predicate_digest

let transition_obligation_snapshot
    (definition : Sst.function_definition) handle =
  let transitions =
    match definition.body with
    | Sst.Checked_exec { body; _ } ->
        Immutable_aggregate_reconstruction_private.owned_tree_transitions
          body.expression
    | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
    | Sst.Proof_body _ | Sst.External_specification _
    | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
    | Sst.Symbolic_declaration _ ->
        []
  in
  let header =
    String.concat "\000"
      [
        definition.function_id.function_name;
        string_of_int definition.function_id.function_index;
        invariant_id handle;
        (abstract_type handle).type_name;
        string_of_int (abstract_type handle).type_index;
        (model_callable handle).function_name;
        string_of_int (model_callable handle).function_index;
        (predicate_callable handle).function_name;
        string_of_int (predicate_callable handle).function_index;
        predicate_digest handle;
      ]
  in
  header
  :: (List.mapi
        (fun ordinal transition ->
          Printf.sprintf "preservation:%d:%s" ordinal
            (Marshal.to_string transition [ Marshal.No_sharing ]
            |> Digest.string |> Digest.to_hex))
        transitions
     @ [ "completed-return:" ^ header ])
let public_operations handle = handle.public_operations
let predicate_definition handle = handle.predicate_definition
