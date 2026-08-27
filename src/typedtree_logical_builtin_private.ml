open Typedtree

type kind = Call_requires | Call_ensures | Forall | Exists

type payload =
  | Callback of {
      application : expression;
      result : expression option;
    }
  | Quantifier of expression

type t = {
  kind : kind;
  span : Location.t;
  payload : payload;
}

let kind_of_name = function
  | "call_requires" -> Some Call_requires
  | "call_ensures" -> Some Call_ensures
  | "forall" -> Some Forall
  | "exists" -> Some Exists
  | _ -> None

let parse_marker marker =
  match String.split_on_char ':' marker with
  | [ "verocaml"; "logical-builtin"; "1"; name; start_; end_ ] -> (
      match (kind_of_name name, int_of_string_opt start_, int_of_string_opt end_) with
      | Some kind, Some start_, Some end_ -> Some (kind, start_, end_)
      | _ -> None)
  | _ -> None

let marker_text ~canonical_marker = function
  | {
      exp_desc =
        Texp_apply
          ( { exp_desc = Texp_ident (path, _, _, _, _); exp_loc; _ },
            [ (Nolabel, Arg ({ exp_desc = Texp_constant (Const_string (text, _, _)); exp_loc = argument_loc; _ }, _)) ],
            _,
            _,
            _ );
      exp_loc = application_loc;
      _;
    }
    when
      canonical_marker path
      && exp_loc.loc_ghost
      && argument_loc.loc_ghost
      && application_loc.loc_ghost ->
      Some text
  | _ -> None

let helper_arguments = function
  | {
      exp_desc =
        Texp_apply
          ( {
              exp_desc =
                Texp_function
                  {
                    params;
                    body = Tfunction_body body;
                    _;
                  };
              exp_loc = function_loc;
              _;
            },
            arguments,
            _,
            _,
            _ );
      exp_loc;
      _;
    }
    when
      exp_loc.loc_ghost
      && function_loc.loc_ghost
      && body.exp_loc.loc_ghost
      && List.for_all (fun parameter -> parameter.fp_loc.loc_ghost) params ->
      let rec collect collected = function
        | [] -> Some (params, List.rev collected)
        | (Nolabel, Arg (argument, _)) :: rest ->
            collect (argument :: collected) rest
        | ( (Labelled _ | Optional _),
            (Arg _ | Omitted _) )
          :: _
        | (Position _, (Arg _ | Omitted _)) :: _
        | (Nolabel, Omitted _) :: _ ->
            None
      in
      collect [] arguments
  | _ -> None

let one_binder = function
  | {
      exp_desc =
        Texp_function
          {
            params = [ { fp_kind = Tparam_pat pattern; fp_arg_label = Nolabel; _ } ];
            body = Tfunction_body body;
            _;
          };
      _;
    } ->
      pattern.pat_loc.loc_ghost = false
      && body.exp_loc.loc_ghost = false
  | _ -> false

let application = function
  | { exp_desc = Texp_apply _; exp_loc; _ } -> not exp_loc.loc_ghost
  | _ -> false

let same_type left right = Types.get_id left.exp_type = Types.get_id right.exp_type

let validate_payload kind params arguments =
  match (kind, params, arguments) with
  | Call_requires, [ _ ], [ application_expression ]
    when application application_expression ->
      Ok (Callback { application = application_expression; result = None })
  | Call_ensures, [ _; _ ], [ application_expression; result ]
    when application application_expression
         && not result.exp_loc.loc_ghost
         && same_type application_expression result ->
      Ok
        (Callback
           {
             application = application_expression;
             result = Some result;
           })
  | (Forall | Exists), [ _ ], [ quantifier ] when one_binder quantifier ->
      Ok (Quantifier quantifier)
  | Call_requires, _, _
  | Call_ensures, _, _
  | Forall, _, _
  | Exists, _, _ ->
      Error "logical builtin carrier has the wrong typed payload shape"

let authenticate ~artifact ~source_file ~canonical_marker expression =
  match expression.exp_desc with
  | Texp_sequence (marker, _, helper) -> (
      match
        Option.bind (marker_text ~canonical_marker marker) parse_marker
      with
      | None -> Ok None
      | Some (kind, start_, end_) -> (
          match artifact with
          | None -> Error "logical builtin carrier has no authenticated CMT issuance"
          | Some artifact
            when
              not
                (Typedtree_adapter_issuance_private
                 .authenticate_logical_builtin_artifact artifact ~source_file) ->
              Error "logical builtin carrier CMT issuance mismatch"
          | Some _ ->
              if
                (not expression.exp_loc.loc_ghost)
                || expression.exp_loc.loc_start.pos_cnum <> start_
                || expression.exp_loc.loc_end.pos_cnum <> end_
              then Error "logical builtin marker/source span mismatch"
              else
                match helper_arguments helper with
                | None -> Error "logical builtin carrier helper is malformed"
                | Some (params, arguments) ->
                    Result.map
                      (fun payload ->
                        Some { kind; span = expression.exp_loc; payload })
                      (validate_payload kind params arguments)))
  | _ -> Ok None

let kind envelope = envelope.kind
let span envelope = envelope.span

let callback_application envelope =
  match envelope.payload with
  | Callback { application; _ } -> Some application
  | Quantifier _ -> None

let callback_result envelope =
  match envelope.payload with
  | Callback { result; _ } -> result
  | Quantifier _ -> None

let quantifier envelope =
  match envelope.payload with
  | Quantifier expression -> Some expression
  | Callback _ -> None

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

type 'error lower_services = {
  lower_expression :
    Typedtree.expression -> (Sst.expression, 'error) result;
  application_binding :
    Typedtree.expression ->
    ((Typedtree_callback_private.application * Sst.callback_binding), 'error)
    result;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Sst.span;
  policy_error : Location.t -> string -> 'error;
  authentication_error : Location.t -> string -> 'error;
  unsupported_quantifier : Location.t -> 'error;
}

type projection = [ `Call | `Requires | `Ensures ]

let lower_application services projection application result =
  let* flattened, callback = services.application_binding application in
  let rec lower_arguments lowered = function
    | [] -> Ok (List.rev lowered)
    | (label, argument) :: rest ->
        let* value = services.lower_expression argument in
        lower_arguments
          ((services.parameter_label label, value) :: lowered)
          rest
  in
  let* arguments = lower_arguments [] flattened.arguments in
  let* arguments =
    match
      Callback_shape_private.order_arguments callback.callback_shape arguments
    with
    | Ok arguments -> Ok arguments
    | Error message -> Error (services.policy_error application.exp_loc message)
  in
  let callback_result =
    Callback_shape_private.result callback.callback_shape
  in
  let* () =
    match
      Callback_shape_private.validate_saturated callback.callback_shape
        ~labels:(List.map fst arguments)
        ~arguments:
          (List.map
             (fun (_, (argument : Sst.expression)) -> argument.typ)
             arguments)
        ~result:callback_result
    with
    | Ok () -> Ok ()
    | Error message -> Error (services.policy_error application.exp_loc message)
  in
  let callback_application =
    {
      Sst.callback;
      arguments;
      callback_span = services.span application.exp_loc;
    }
  in
  match projection, result with
  | `Call, None ->
      Ok
        {
          Sst.expression_desc = Sst.Callback_call callback_application;
          typ = callback_result;
          span = services.span application.exp_loc;
        }
  | `Requires, None ->
      Ok
        {
          Sst.expression_desc = Sst.Callback_requires callback_application;
          typ = Sst.Bool;
          span = services.span application.exp_loc;
        }
  | `Ensures, Some result ->
      let* result = services.lower_expression result in
      if result.typ <> callback_result then
        Error
          (services.authentication_error application.exp_loc
             "call_ensures result does not have the callback result type")
      else
        Ok
          {
            Sst.expression_desc =
              Sst.Callback_ensures
                { application = callback_application; result };
            typ = Sst.Bool;
            span = services.span application.exp_loc;
          }
  | (`Call | `Requires), Some _ | `Ensures, None ->
      Error
        (services.authentication_error application.exp_loc
           "callback projection arity is inconsistent")

let lower services ~expression envelope =
  match envelope.kind with
  | Forall | Exists ->
      Error (services.unsupported_quantifier envelope.span)
  | Call_requires -> (
      match callback_application envelope with
      | Some application ->
          lower_application services `Requires application None
      | None ->
          Error
            (services.authentication_error expression.exp_loc
               "call_requires carrier has no callback application"))
  | Call_ensures -> (
      match callback_application envelope, callback_result envelope with
      | Some application, Some result ->
          lower_application services `Ensures application (Some result)
      | (None | Some _), (None | Some _) ->
          Error
            (services.authentication_error expression.exp_loc
               "call_ensures carrier lacks its exact application or result"))

type proof_capture_manifest_slot = {
  proof_capture_slot_name : string;
  proof_capture_slot_start : int;
  proof_capture_slot_end : int;
}

type proof_capture_kind =
  | Captured_proof_region
  | Captured_local_assert of {
      assertion_ordinal : int;
      predicate_start : int;
      predicate_end : int;
    }

type proof_capture_manifest = {
  proof_capture_callable_name : string;
  proof_capture_binding_start : int;
  proof_capture_binding_end : int;
  proof_capture_body_start : int;
  proof_capture_body_end : int;
  proof_capture_region_start : int;
  proof_capture_region_end : int;
  proof_capture_slots : proof_capture_manifest_slot list;
  proof_capture_kind : proof_capture_kind;
}

type local_assertion_discovery =
  | Builtin_assertion_discovery of {
      source : Typedtree.expression;
      predicate : Typedtree.expression;
      keyword_location : Location.t;
    }
  | Retained_assertion_discovery of proof_capture_manifest

type issued_proof_capture = {
  proof_capture_token : unit ref;
  proof_capture_artifact : Typedtree_adapter_issuance_private.proof_capture_artifact;
  proof_capture_function : Sst_callback_private.top_function;
  proof_capture_manifest : proof_capture_manifest;
  proof_capture_bindings : Sst.binding list;
  proof_capture_shadow_idents : Ident.t list;
  mutable proof_capture_program : Sst.program option;
  mutable proof_capture_expression : Sst.expression option;
}

type issued_local_assertion = {
  local_assertion_token : unit ref;
  local_assertion_program : Sst.program;
  local_assertion_definition : Sst.function_definition;
  local_assertion_expression : Sst.expression;
  local_assertion_parent_region : Sst.expression option;
  local_assertion_source : local_assertion_static_source;
}

and local_assertion_static_source =
  | Retained_ppx_assertion of proof_capture_manifest
  | Compiler_builtin_assertion of {
      builtin_source_expression : Typedtree.expression;
      builtin_predicate_expression : Typedtree.expression;
      builtin_keyword_location : Location.t;
      builtin_source_location : Location.t;
      builtin_predicate_location : Location.t;
      builtin_assertion_ordinal : int;
    }

type pending_builtin_assertion = {
  pending_builtin_token : unit ref;
  pending_builtin_program : Sst.program;
  pending_builtin_definition : Sst.function_definition;
  pending_builtin_expression : Sst.expression;
  pending_builtin_parent_region : Sst.expression option;
  pending_builtin_source : local_assertion_static_source;
  pending_builtin_program_snapshot : string;
}

type lowered_builtin_assertion = {
  lowered_builtin_token : unit ref;
  lowered_builtin_function : Sst_callback_private.top_function;
  lowered_builtin_source : Sst_callback_private.planned_builtin_assertion;
  lowered_builtin_expression : Sst.expression;
}

let proof_capture_issuer = ref ()
let issued_proof_captures : issued_proof_capture list ref = ref []
let proof_capture_issuance_count = ref 0
let proof_capture_remapping_count = ref 0
let proof_capture_sst_count = ref 0
let local_assertion_sst_count = ref 0

type issued_proof_region = {
  proof_region_token : unit ref;
  proof_region_program : Sst.program;
  proof_region_definition : Sst.function_definition;
  proof_region_expression : Sst.expression;
  proof_region_manifest : proof_capture_manifest;
}

let local_assertion_issuer = ref ()
let issued_local_assertions : issued_local_assertion list ref = ref []
let pending_builtin_issuer = ref ()
let pending_builtin_assertions : pending_builtin_assertion list ref = ref []
let lowered_builtin_issuer = ref ()
let lowered_builtin_assertions : lowered_builtin_assertion list ref = ref []
let proof_region_issuer = ref ()
let issued_proof_regions : issued_proof_region list ref = ref []
let local_assertion_static_issuance_count = ref 0

type assertion_authentication =
  program:Sst.program ->
  definition:Sst.function_definition ->
  expression:Sst.expression ->
  bool

(* These registries are private capabilities bound to the exact physical
   program, definition, and expression.  Builtin candidates additionally
   compare their canonical snapshot once when they become issued assertions;
   hot authentication queries must not render the whole SST. *)
let authenticate_proof_region ~program ~definition ~expression =
  List.exists
    (fun issued ->
      issued.proof_region_token == proof_region_issuer
      && issued.proof_region_program == program
      && issued.proof_region_definition == definition
      && issued.proof_region_expression == expression
      && issued.proof_region_manifest.proof_capture_kind
         = Captured_proof_region
      &&
      match expression.Sst.expression_desc with
      | Sst.Proof_region _ -> true
      | _ -> false)
    !issued_proof_regions

let authenticate_local_assertion ~program ~definition ~expression =
  List.exists
    (fun issued ->
      issued.local_assertion_token == local_assertion_issuer
      && issued.local_assertion_program == program
      && issued.local_assertion_definition == definition
      && issued.local_assertion_expression == expression
      &&
      (match
         ( issued.local_assertion_source,
           definition.Sst.mode,
           issued.local_assertion_parent_region )
       with
      | ( ( Retained_ppx_assertion _ | Compiler_builtin_assertion _ ),
        Sst.Proof,
        None ) ->
          true
      | ( ( Retained_ppx_assertion _ | Compiler_builtin_assertion _ ),
        Sst.Exec,
        Some region ) ->
          authenticate_proof_region ~program ~definition ~expression:region
      | Compiler_builtin_assertion _, Sst.Exec, None -> true
      | Retained_ppx_assertion _, Sst.Exec, None
      | ( (Retained_ppx_assertion _ | Compiler_builtin_assertion _),
        (Sst.Proof | Sst.Spec),
        Some _ )
      | ( (Retained_ppx_assertion _ | Compiler_builtin_assertion _),
        Sst.Spec,
        None ) ->
          false)
      &&
      match
        ( issued.local_assertion_source,
          expression.Sst.expression_desc )
      with
      | ( Retained_ppx_assertion
            {
              proof_capture_kind =
                Captured_local_assert { assertion_ordinal = expected; _ };
              _;
            },
          Sst.Local_assert { assertion_ordinal; _ } ) ->
          expected = assertion_ordinal
      | ( Compiler_builtin_assertion
            { builtin_assertion_ordinal = expected; _ },
          Sst.Local_assert { assertion_ordinal; _ } ) ->
          expected = assertion_ordinal
      | Retained_ppx_assertion _, _ | Compiler_builtin_assertion _, _ -> false)
    !issued_local_assertions

let pending_builtin_assertion ~program ~definition ~expression =
  List.find_opt
    (fun pending ->
      pending.pending_builtin_token == pending_builtin_issuer
      && pending.pending_builtin_program == program
      && pending.pending_builtin_definition == definition
      && pending.pending_builtin_expression == expression)
    !pending_builtin_assertions

let authenticate_local_assertion_candidate ~program ~definition ~expression =
  authenticate_local_assertion ~program ~definition ~expression
  || Option.is_some
       (pending_builtin_assertion ~program ~definition ~expression)

let is_builtin_local_assertion ~program ~definition ~expression =
  match pending_builtin_assertion ~program ~definition ~expression with
  | Some _ -> true
  | None ->
      List.exists
        (fun issued ->
          issued.local_assertion_token == local_assertion_issuer
          && issued.local_assertion_program == program
          && issued.local_assertion_definition == definition
          && issued.local_assertion_expression == expression
          &&
          match issued.local_assertion_source with
          | Compiler_builtin_assertion _ -> true
          | Retained_ppx_assertion _ -> false)
        !issued_local_assertions

let local_assertion_parent_proof_region ~program ~definition ~expression =
  match
    List.find_map
    (fun issued ->
      if
        issued.local_assertion_token == local_assertion_issuer
        && issued.local_assertion_program == program
        && issued.local_assertion_definition == definition
        && issued.local_assertion_expression == expression
      then issued.local_assertion_parent_region
      else None)
    !issued_local_assertions
  with
  | Some _ as parent -> parent
  | None ->
      Option.bind
        (pending_builtin_assertion ~program ~definition ~expression)
        (fun pending -> pending.pending_builtin_parent_region)

let is_direct_exec_builtin_local_assertion ~program ~definition ~expression =
  definition.Sst.mode = Sst.Exec
  && is_builtin_local_assertion ~program ~definition ~expression
  && Option.is_none
       (local_assertion_parent_proof_region ~program ~definition ~expression)

let issue_builtin_local_assertions ~program =
  let program_snapshot = Sst.to_string program in
  let candidates =
    List.filter
      (fun pending ->
        pending.pending_builtin_token == pending_builtin_issuer
        && pending.pending_builtin_program == program)
      !pending_builtin_assertions
  in
  let valid pending =
    String.equal pending.pending_builtin_program_snapshot
      program_snapshot
    && List.exists
         (fun definition -> definition == pending.pending_builtin_definition)
         program.Sst.functions
    &&
    match
      ( pending.pending_builtin_definition.Sst.mode,
        pending.pending_builtin_parent_region,
        pending.pending_builtin_source,
        pending.pending_builtin_expression.Sst.expression_desc )
    with
    | ( Sst.Proof,
        None,
        Compiler_builtin_assertion
          { builtin_assertion_ordinal = expected; _ },
        Sst.Local_assert { assertion_ordinal; _ } ) ->
        expected = assertion_ordinal
    | ( Sst.Exec,
        parent_region,
        Compiler_builtin_assertion
          { builtin_assertion_ordinal = expected; _ },
        Sst.Local_assert { assertion_ordinal; _ } ) ->
        expected = assertion_ordinal
        &&
        Option.fold ~none:true
          ~some:(fun region ->
            authenticate_proof_region ~program
              ~definition:pending.pending_builtin_definition
              ~expression:region)
          parent_region
    | (Sst.Proof | Sst.Spec), Some _, _, _
    | Sst.Spec, None, _, _
    | (Sst.Proof | Sst.Exec), _, Retained_ppx_assertion _, _
    | (Sst.Proof | Sst.Exec), _, Compiler_builtin_assertion _, _ ->
        false
  in
  if not (List.for_all valid candidates) then
    Error "builtin local assertion candidate authentication failed"
  else (
    List.iter
      (fun pending ->
        if
          not
            (List.exists
               (fun issued ->
                 issued.local_assertion_token == local_assertion_issuer
                 && issued.local_assertion_program == program
                 && issued.local_assertion_definition
                    == pending.pending_builtin_definition
                 && issued.local_assertion_expression
                    == pending.pending_builtin_expression)
               !issued_local_assertions)
        then (
          issued_local_assertions :=
            {
              local_assertion_token = local_assertion_issuer;
              local_assertion_program = program;
              local_assertion_definition =
                pending.pending_builtin_definition;
              local_assertion_expression =
                pending.pending_builtin_expression;
              local_assertion_parent_region =
                pending.pending_builtin_parent_region;
              local_assertion_source = pending.pending_builtin_source;
            }
            :: !issued_local_assertions;
          incr local_assertion_static_issuance_count))
      candidates;
    Ok ())
