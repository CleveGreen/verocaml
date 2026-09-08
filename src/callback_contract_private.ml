type carrier_kind = Requires | Ensures | Decreases | Assert

type carrier = {
  kind : carrier_kind;
  binder : Sst.pattern option;
  payload : Sst.expression;
  span : Sst.span;
}

type execution_clause = {
  ordinal : int;
  span : Sst.span;
  binder : Sst.pattern option;
  payload : Sst.expression;
}

type retained_ghost_clause = {
  clause_id : string;
  shadow_parameters : Typedtree.function_param list;
  contract_application : Typedtree.expression;
}

let contracts carriers =
  let requires = ref []
  and ensures = ref []
  and decreases = ref []
  and assertions = ref [] in
  List.iter
    (fun carrier ->
      match carrier.kind with
      | Requires ->
          requires :=
            {
              Sst.clause_index = List.length !requires;
              predicate = { stage = Sst.Logical; expression = carrier.payload };
              span = carrier.span;
            }
            :: !requires
      | Ensures ->
          ensures :=
            {
              Sst.clause_index = List.length !ensures;
              binder = carrier.binder;
              predicate = { stage = Sst.Logical; expression = carrier.payload };
              span = carrier.span;
            }
            :: !ensures
      | Decreases ->
          decreases :=
            {
              Sst.clause_index = List.length !decreases;
              predicate = { stage = Sst.Logical; expression = carrier.payload };
              span = carrier.span;
            }
            :: !decreases
      | Assert ->
          assertions :=
            {
              Sst.clause_index = List.length !assertions;
              predicate = { stage = Sst.Logical; expression = carrier.payload };
              span = carrier.span;
            }
            :: !assertions)
    carriers;
  {
    Sst.requires = List.rev !requires;
    ensures = List.rev !ensures;
    decreases = List.rev !decreases;
    assertions = List.rev !assertions;
  }

let extract carriers body =
  let all_carriers : carrier list = List.rev carriers in
  let same_span left right =
    left.Diagnostic.start_pos = right.Diagnostic.start_pos
    && left.end_pos = right.end_pos
    && String.equal left.file right.file
  in
  let rec consume (carriers : carrier list) expression =
    match carriers, expression.Sst.expression_desc with
    | ( carrier :: rest,
        Sst.Sequence
          ({ expression_desc = Sst.Unit_constant; span; _ }, following) )
      when same_span carrier.span span ->
        consume rest following
    | [], _ -> Ok (contracts all_carriers, expression)
    | carrier :: _, _ ->
        Error
          (Diagnostic.make
             (Diagnostic.Unsupported_construct Diagnostic.Malformed_ghost_call)
             carrier.span)
  in
  consume all_carriers body

let validate_explicit contracts =
  if contracts.Sst.requires = [] then
    Error "verified callback requires an explicit requires clause"
  else if contracts.ensures = [] then
    Error "verified callback requires an explicit ensures clause"
  else if contracts.decreases <> [] then
    Error "recursive callback contracts are unsupported"
  else Ok ()

let total_body value_binding =
  match value_binding.Typedtree.vb_expr.exp_desc with
  | Texp_function { body = Tfunction_body body; _ } -> Some body
  | Texp_function _ | Texp_ident _ | Texp_constant _ | Texp_let _
  | Texp_letmutable _ | Texp_apply _ | Texp_match _ | Texp_try _
  | Texp_tuple _ | Texp_unboxed_tuple _ | Texp_construct _ | Texp_variant _
  | Texp_record _ | Texp_record_unboxed_product _ | Texp_atomic_loc _
  | Texp_field _ | Texp_unboxed_field _ | Texp_setfield _ | Texp_array _
  | Texp_idx _ | Texp_list_comprehension _ | Texp_array_comprehension _
  | Texp_ifthenelse _ | Texp_sequence _ | Texp_while _ | Texp_for _
  | Texp_send _ | Texp_new _ | Texp_instvar _ | Texp_mutvar _
  | Texp_setinstvar _ | Texp_setmutvar _ | Texp_override _
  | Texp_letmodule _ | Texp_letexception _ | Texp_assert _ | Texp_lazy _
  | Texp_object _ | Texp_pack _ | Texp_letop _ | Texp_unreachable
  | Texp_extension_constructor _ | Texp_open _ | Texp_probe _
  | Texp_probe_is_enabled _ | Texp_exclave _ | Texp_src_pos
  | Texp_overwrite _ | Texp_hole _ | Texp_quotation _
  | Texp_antiquotation _ | Texp_eval _ ->
      None

let validate_top_level ~retained_pair ~application ~resolves value_binding =
  let classify retained =
    match (application retained).Typedtree.exp_desc with
    | Texp_apply
        ({ exp_desc = Texp_ident (path, _, _, _, _); _ }, _, _, _, _)
      when resolves path "Vero_ghost" "requires" ->
        `Requires
    | Texp_apply
        ({ exp_desc = Texp_ident (path, _, _, _, _); _ }, _, _, _, _)
      when resolves path "Vero_ghost" "ensures" ->
        `Ensures
    | _ -> `Other
  in
  let rec prefix seen_requires seen_ensures expression =
    match expression.Typedtree.exp_desc with
    | Texp_sequence
        ( marker,
          _,
          { exp_desc = Texp_sequence (sidecar, _, tail); _ } ) -> (
        match retained_pair marker sidecar with
        | Some retained -> (
            match classify retained with
            | `Requires -> prefix true seen_ensures tail
            | `Ensures -> prefix seen_requires true tail
            | `Other -> (seen_requires, seen_ensures))
        | None -> (seen_requires, seen_ensures))
    | _ -> (seen_requires, seen_ensures)
  in
  match total_body value_binding with
  | Some body -> (
      match prefix false false body with
      | true, true -> Ok ()
      | false, _ ->
          Error
            "top-level verified callback requires an explicit requires clause"
      | true, false ->
          Error
            "top-level verified callback requires an explicit ensures clause")
  | None -> Error "top-level callback has no total function body"

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

type ('candidate, 'error) top_level_services = {
  candidates : Path.t -> 'candidate list;
  eligible : 'candidate -> bool;
  candidate_identity : 'candidate -> string;
  candidate_id : 'candidate -> Sst.function_id;
  shape :
    Env.t ->
    Location.t ->
    Types.type_expr ->
    (Callback_shape_private.t, 'error) result;
  validate_contract : 'candidate -> (unit, 'error) result;
  compilation_identity :
    Location.t ->
    (Callback_certificate_private.compilation_identity, 'error) result;
  caller_identity : string;
  call_edge_identity : string;
  compiler_mode : Types.type_expr -> string;
  span : Location.t -> Sst.span;
  policy_error : Location.t -> string -> 'error;
  authentication_error : Location.t -> string -> 'error;
}

let top_level_argument services expected_shape label
    (source : Typedtree.expression) =
  match source.exp_desc with
  | Typedtree.Texp_ident (path, _, _, _, _) -> (
      match services.candidates path with
      | [ candidate ] when services.eligible candidate ->
          let* actual_shape =
            services.shape source.exp_env source.exp_loc
              (Ctype.full_expand ~may_forget_scope:false source.exp_env
                 source.exp_type)
          in
          if not (Callback_shape_private.equal expected_shape actual_shape)
          then
            Error
              (services.policy_error source.exp_loc
                 "callback actual shape differs from the formal shape")
          else
            let* () = services.validate_contract candidate in
            let callable_identity = services.candidate_identity candidate in
            let* compilation_identity =
              services.compilation_identity source.exp_loc
            in
            let* origin =
              Callback_certificate_private.issue_origin
                ~kind:Callback_certificate_private.Top_level
                ~compilation_identity ~callable_identity ~shape:actual_shape
                ~compiler_mode:(services.compiler_mode source.exp_type)
                ~contract_identity:
                  (callable_identity ^ ":explicit-contract")
                ~captures:[] ~pure:true ~total:true ~complete:true
              |> Result.map_error
                   (services.authentication_error source.exp_loc)
            in
            let callback_certificate =
              Callback_certificate_private.seal_call_edge origin
                ~caller_identity:services.caller_identity
                ~call_edge_identity:services.call_edge_identity
            in
            let id = services.candidate_id candidate in
            Ok
              (Sst.Callback_argument
                 {
                   label;
                   callback =
                     {
                       callback_id = id.function_index;
                       callback_name = id.function_name;
                       callback_shape = actual_shape;
                       callback_certificate;
                       callback_span = services.span source.exp_loc;
                     };
                 })
      | [] | [ _ ] | _ :: _ :: _ ->
          Error
            (services.authentication_error source.exp_loc
               "callback actual is not an exact nonrecursive same-unit Exec \
                function"))
  | Typedtree.Texp_constant _ | Typedtree.Texp_let _
  | Typedtree.Texp_letmutable _ | Typedtree.Texp_function _
  | Typedtree.Texp_apply _ | Typedtree.Texp_match _ | Typedtree.Texp_try _
  | Typedtree.Texp_tuple _ | Typedtree.Texp_unboxed_tuple _
  | Typedtree.Texp_construct _ | Typedtree.Texp_variant _
  | Typedtree.Texp_record _ | Typedtree.Texp_record_unboxed_product _
  | Typedtree.Texp_atomic_loc _ | Typedtree.Texp_field _
  | Typedtree.Texp_unboxed_field _ | Typedtree.Texp_setfield _
  | Typedtree.Texp_array _ | Typedtree.Texp_idx _
  | Typedtree.Texp_list_comprehension _
  | Typedtree.Texp_array_comprehension _ | Typedtree.Texp_ifthenelse _
  | Typedtree.Texp_sequence _ | Typedtree.Texp_while _
  | Typedtree.Texp_for _ | Typedtree.Texp_send _ | Typedtree.Texp_new _
  | Typedtree.Texp_instvar _ | Typedtree.Texp_mutvar _
  | Typedtree.Texp_setinstvar _ | Typedtree.Texp_setmutvar _
  | Typedtree.Texp_override _ | Typedtree.Texp_letmodule _
  | Typedtree.Texp_letexception _ | Typedtree.Texp_assert _
  | Typedtree.Texp_lazy _ | Typedtree.Texp_object _
  | Typedtree.Texp_pack _ | Typedtree.Texp_letop _
  | Typedtree.Texp_unreachable | Typedtree.Texp_extension_constructor _
  | Typedtree.Texp_open _ | Typedtree.Texp_probe _
  | Typedtree.Texp_probe_is_enabled _ | Typedtree.Texp_exclave _
  | Typedtree.Texp_src_pos | Typedtree.Texp_overwrite _
  | Typedtree.Texp_hole _ | Typedtree.Texp_quotation _
  | Typedtree.Texp_antiquotation _ | Typedtree.Texp_eval _ ->
      Error
        (services.authentication_error source.exp_loc
           "callback actual is not an exact callback identifier")

type 'error actual_services = {
  find_binding : Ident.t -> Sst.callback_binding option;
  seal : Location.t -> Sst.callback_binding -> Sst.callback_binding;
  lower_local :
    name:string ->
    Typedtree.expression ->
    (Sst.callback_binding, 'error) result;
  top_level :
    Callback_shape_private.t ->
    string option ->
    Typedtree.expression ->
    (Sst.call_argument, 'error) result;
  call_location : Location.t;
  next_local_name : unit -> string;
  policy_error : Location.t -> string -> 'error;
  authentication_error : Location.t -> string -> 'error;
}

let resolve_actual services expected_shape label
    (source : Typedtree.expression) =
  let argument callback =
    Sst.Callback_argument
      {
        label;
        callback = services.seal services.call_location callback;
      }
  in
  match source.exp_desc with
  | Typedtree.Texp_ident (Path.Pident ident, _, _, _, _) -> (
      match services.find_binding ident with
      | Some callback
        when
          Callback_shape_private.equal expected_shape
            callback.callback_shape ->
          Ok (argument callback)
      | Some _ ->
          Error
            (services.policy_error source.exp_loc
               "callback actual shape differs from the formal shape")
      | None -> services.top_level expected_shape label source)
  | Typedtree.Texp_function _ ->
      let* callback =
        services.lower_local ~name:(services.next_local_name ()) source
      in
      if
        Callback_shape_private.equal expected_shape callback.callback_shape
      then Ok (argument callback)
      else
        Error
          (services.policy_error source.exp_loc
             "immediate callback shape differs from its formal")
  | Typedtree.Texp_ident _
  | Typedtree.Texp_constant _ | Typedtree.Texp_let _
  | Typedtree.Texp_letmutable _ | Typedtree.Texp_apply _
  | Typedtree.Texp_match _ | Typedtree.Texp_try _
  | Typedtree.Texp_tuple _ | Typedtree.Texp_unboxed_tuple _
  | Typedtree.Texp_construct _ | Typedtree.Texp_variant _
  | Typedtree.Texp_record _ | Typedtree.Texp_record_unboxed_product _
  | Typedtree.Texp_atomic_loc _ | Typedtree.Texp_field _
  | Typedtree.Texp_unboxed_field _ | Typedtree.Texp_setfield _
  | Typedtree.Texp_array _ | Typedtree.Texp_idx _
  | Typedtree.Texp_list_comprehension _
  | Typedtree.Texp_array_comprehension _ | Typedtree.Texp_ifthenelse _
  | Typedtree.Texp_sequence _ | Typedtree.Texp_while _
  | Typedtree.Texp_for _ | Typedtree.Texp_send _ | Typedtree.Texp_new _
  | Typedtree.Texp_instvar _ | Typedtree.Texp_mutvar _
  | Typedtree.Texp_setinstvar _ | Typedtree.Texp_setmutvar _
  | Typedtree.Texp_override _ | Typedtree.Texp_letmodule _
  | Typedtree.Texp_letexception _ | Typedtree.Texp_assert _
  | Typedtree.Texp_lazy _ | Typedtree.Texp_object _
  | Typedtree.Texp_pack _ | Typedtree.Texp_letop _
  | Typedtree.Texp_unreachable | Typedtree.Texp_extension_constructor _
  | Typedtree.Texp_open _ | Typedtree.Texp_probe _
  | Typedtree.Texp_probe_is_enabled _ | Typedtree.Texp_exclave _
  | Typedtree.Texp_src_pos | Typedtree.Texp_overwrite _
  | Typedtree.Texp_hole _ | Typedtree.Texp_quotation _
  | Typedtree.Texp_antiquotation _ | Typedtree.Texp_eval _ ->
      Error
        (services.authentication_error source.exp_loc
           "callback actual is not an exact callback identifier")

type definition_kind =
  | Checked_exec of int option
  | Spec_definition
  | Recursive_spec_definition of [ `Opaque | `Revealed ]
  | Proof_definition

let make_definition ~source_file ~function_id ~recursive ~parameters
    ~contracts ~body ~span = function
  | Checked_exec returns_unique_parameter ->
      Sst_normalize.authenticated_checked_exec ~source_file ~function_id
        ~recursive ~parameters ~contracts ~body ~result_type:body.Sst.typ
        ~returns_unique_parameter ~span
  | Spec_definition ->
      Sst_normalize.authenticated_spec_definition ~function_id ~parameters
        ~body ~result_type:body.typ ~span
  | Recursive_spec_definition visibility ->
      {
        Sst.function_id;
        type_binders = [];
        mode = Sst.Spec;
        recursive = true;
        parameters;
        contracts;
        body =
          Sst.Recursive_spec_definition
            {
              body = { Sst.stage = Sst.Logical; expression = body };
              visibility;
              provenance =
                Sst.Authenticated_typedtree
                  { source_file; declaration_span = span };
            };
        policy = Sst.Default_linear_z3;
        result_type = body.typ;
        returns_unique_parameter = None;
        span;
      }
  | Proof_definition ->
      let definition =
        Sst_normalize.authenticated_proof_definition ~source_file
          ~function_id ~parameters ~contracts ~body ~span
      in
      let definition = { definition with Sst.result_type = body.typ } in
      if recursive then { definition with Sst.recursive = true }
      else definition

type ('bindings, 'error) parameter_services = {
  normalized_type :
    Location.t -> Types.type_expr -> (Parametric_type.t, 'error) result;
  optional_carrier :
    Location.t ->
    Parametric_type.t ->
    Parametric_type.t ->
    (Parametric_type.t, 'error) result;
  lower_expression :
    'bindings -> Typedtree.expression -> (Sst.expression, 'error) result;
  lower_pattern :
    'bindings ->
    Typedtree.pattern ->
    ((Sst.pattern * 'bindings), 'error) result;
  issue_callback :
    Typedtree.function_param ->
    Typedtree.pattern ->
    (Sst.parameter, 'error) result;
  is_callback : Types.type_expr -> bool;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Sst.span;
  partial_error : Location.t -> 'error;
  parameter_pattern_error : Location.t -> 'error;
  parameter_type_error : Location.t -> string -> 'error;
}

let rec irrefutable_parameter_pattern (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Bind _ | Sst.Wildcard | Sst.Unit_pattern -> true
  | Sst.Tuple_pattern components ->
      List.for_all (fun (_, nested) -> irrefutable_parameter_pattern nested) components
  | Sst.Record_pattern fields ->
      List.for_all (fun (_, nested) -> irrefutable_parameter_pattern nested) fields
  | Sst.Constructor_pattern _ | Sst.Int_pattern _ | Sst.Bool_pattern _
  | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
      false

let lower_parameters services ~function_type initial parameters =
  match
    Parametric_lowering_private.compiler_parameter_domains function_type
      parameters
  with
  | Error (location, message) ->
      Error (services.parameter_type_error location message)
  | Ok domains ->
      let rec lower lowered bindings parameters domains =
        match parameters, domains with
        | [], [] -> Ok (List.rev lowered, bindings)
        | (parameter : Typedtree.function_param) :: rest,
          domain :: remaining_domains ->
            if parameter.fp_partial = Typedtree.Partial then
              Error (services.partial_error parameter.fp_loc)
            else (
              match parameter.fp_kind with
              | Typedtree.Tparam_optional_default
                  (source_pattern, default_expression, _) ->
                  let* payload_type =
                    services.normalized_type source_pattern.pat_loc
                      source_pattern.pat_type
                  in
                  let* carrier_type =
                    let* carrier_type =
                      services.normalized_type source_pattern.pat_loc domain
                    in
                    services.optional_carrier source_pattern.pat_loc carrier_type
                      payload_type
                  in
                  let carrier_pattern =
                    {
                      Sst.pattern_desc = Sst.Wildcard;
                      typ = carrier_type;
                      span = services.span parameter.fp_loc;
                    }
                  in
                  let* default_expression =
                    services.lower_expression bindings default_expression
                  in
                  let* optional_pattern, bindings =
                    services.lower_pattern bindings source_pattern
                  in
                  if not (irrefutable_parameter_pattern optional_pattern) then
                    Error (services.parameter_pattern_error parameter.fp_loc)
                  else
                    lower
                      (Sst.Value_parameter
                         {
                           Sst.label =
                             services.parameter_label parameter.fp_arg_label;
                           pattern = carrier_pattern;
                           optional_default =
                             Some
                               {
                                 Sst.optional_pattern;
                                 optional_expression = default_expression;
                               };
                         }
                      :: lowered)
                      bindings rest remaining_domains
              | Typedtree.Tparam_pat pattern ->
                  if services.is_callback pattern.pat_type then
                    let* callback = services.issue_callback parameter pattern in
                    lower (callback :: lowered) bindings rest remaining_domains
                  else
                    let* pattern, bindings =
                      services.lower_pattern bindings pattern
                    in
                    if not (irrefutable_parameter_pattern pattern) then
                      Error (services.parameter_pattern_error parameter.fp_loc)
                    else
                      lower
                        (Sst.Value_parameter
                           {
                             Sst.label =
                               services.parameter_label parameter.fp_arg_label;
                             pattern;
                             optional_default = None;
                           }
                        :: lowered)
                        bindings rest remaining_domains)
        | [], _ :: _ | _ :: _, [] -> assert false
      in
      lower [] initial parameters domains

type recursive_helper_role =
  | Ordinary_direct_spec
  | Direct_type_invariant
  | Direct_model
  | Not_a_direct_spec

type recursive_helper_member = {
  helper_definition : Sst.function_definition;
  helper_source_order : int;
  helper_role : recursive_helper_role;
}

type recursive_helper_certificate = {
  (* The certificate deliberately retains physical source objects.  Names,
     indices, normalized snapshots, and imported descriptors are descriptive
     checks only and cannot replace these identities. *)
  helper_token : unit ref;
  helper_program : Sst.program;
  helper_program_snapshot : string;
  helper_root : recursive_helper_member;
  helper_members : recursive_helper_member list;
  helper_local_types : Sst.type_id list;
  mutable helper_expanded_body : Sst.expression option;
  mutable helper_adversary_edges :
    (recursive_helper_member * recursive_helper_member) list option;
  mutable helper_adversary_body :
    (recursive_helper_member * Sst.body_disposition) option;
  mutable helper_adversary_call :
    ( recursive_helper_member
    * recursive_helper_member
    * (string option * Sst.expression) list )
    option;
}

let recursive_helper_error format =
  Printf.ksprintf (fun message -> Error message) format

let recursive_helper_expression_children (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Bv_literal _ -> []
  | Sst.Bv_int_to_bv_mod { input; _ }
  | Sst.Bv_to_int_unsigned input
  | Sst.Bv_to_int_signed input
  | Sst.Bv_not input ->
      [ input ]
  | Sst.Bv_binary (_, left, right) | Sst.Bv_compare (_, left, right) ->
      [ left; right ]
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Mutable_read _ | Sst.Owned_tree_rebase _
  | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Optional_absent
  | Sst.Logical_constant_reference _ ->
      []
  | Sst.Lift_runtime_int operand -> [ operand ]
  | Sst.Tuple_value values -> List.map snd values
  | Sst.Record_value { fields; _ } -> List.map snd fields
  | Sst.Constructor_value { arguments; _ }
  | Sst.Checked_arithmetic (_, arguments) ->
      arguments
  | Sst.Field_read { record; _ } -> [ record ]
  | Sst.Field_write { value; _ }
  | Sst.Shared_scalar_field_write { value; _ }
  | Sst.Owned_tree_nested_write { value; _ }
  | Sst.Mutable_write { value; _ }
  | Sst.Use_type_invariant { value; _ } ->
      [ value ]
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
           (fun (case : Sst.case) ->
             Option.to_list case.case_guard @ [ case.case_body ])
           cases
  | Sst.Boolean_not operand | Sst.Proof_region operand | Sst.Old operand
    | Sst.Optional_present operand
    | Sst.Optional_forward operand ->
      [ operand ]
  | Sst.Local_assert { predicate; _ } -> [ predicate ]
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      [ quantifier.quantifier_body ]
  | Sst.Direct_call { arguments; _ } ->
      List.filter_map
        (function
          | Sst.Value_argument { value; _ } -> Some value
          | Sst.Callback_argument _ -> None)
        arguments
  | Sst.Callback_call application | Sst.Callback_requires application ->
      List.map snd application.arguments
  | Sst.Callback_ensures { application; result } ->
      List.map snd application.arguments @ [ result ]
  | Sst.Symbolic_application application ->
      Symbolic_application_private.arguments application

let validate_builtin_assertion_predicate predicate =
  let rec pure (expression : Sst.expression) =
    let reject detail =
      Error
        ( expression.span,
          "builtin assertion condition is outside the pure Boolean subset: "
          ^ detail )
    in
    let all expressions =
      List.fold_left
        (fun result expression ->
          let* () = result in
          pure expression)
        (Ok ()) expressions
    in
    match expression.expression_desc with
    | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
    | Sst.Bv_literal _
    | Sst.Variable _ | Sst.Logical_constant_reference _ ->
        Ok ()
    | Sst.Bv_int_to_bv_mod { input; _ }
    | Sst.Bv_to_int_unsigned input
    | Sst.Bv_to_int_signed input
    | Sst.Bv_not input ->
        pure input
    | Sst.Bv_binary (_, left, right) | Sst.Bv_compare (_, left, right) ->
        all [ left; right ]
    | Sst.Lift_runtime_int operand -> pure operand
    | Sst.Tuple_value values -> all (List.map snd values)
    | Sst.Record_value { fields; _ } -> all (List.map snd fields)
    | Sst.Constructor_value { arguments; _ }
    | Sst.Checked_arithmetic (_, arguments) ->
        all arguments
    | Sst.Field_read { record; _ } | Sst.Boolean_not record -> pure record
    | Sst.Let (bindings, body) -> all (List.map snd bindings @ [ body ])
    | Sst.Sequence (first, second)
    | Sst.Compare (_, first, second)
    | Sst.Boolean_binary (_, first, second) ->
        all [ first; second ]
    | Sst.If (condition, consequent, alternative) ->
        all (condition :: consequent :: Option.to_list alternative)
    | Sst.Match (scrutinee, cases) ->
        all
          (scrutinee
          :: List.concat_map
               (fun (case : Sst.case) ->
                 Option.to_list case.case_guard @ [ case.case_body ])
               cases)
    | Sst.Direct_call
        {
          call_form = Sst.Specification_call | Sst.Proof_call;
          arguments;
          _;
        } ->
        all
          (List.filter_map
             (function
               | Sst.Value_argument { value; _ } -> Some value
               | Sst.Callback_argument _ -> None)
             arguments)
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        all
          (quantifier.quantifier_body
          :: Option.to_list quantifier.quantifier_trigger)
    | Sst.Symbolic_application application ->
        all (Symbolic_application_private.arguments application)
    | Sst.Callback_call _ | Sst.Callback_requires _
    | Sst.Callback_ensures _ ->
        reject "callback node"
    | Sst.Direct_call _ -> reject "unsupported call"
    | Sst.Field_write _ | Sst.Shared_scalar_field_write _
    | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _
    | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _ ->
        reject "mutation or mutable state"
    | Sst.Reveal _ | Sst.Reveal_with_fuel _ -> reject "reveal"
    | Sst.Local_assert _ | Sst.Proof_region _
    | Sst.Use_type_invariant _ | Sst.Old _ | Sst.Optional_absent
    | Sst.Optional_present _ | Sst.Optional_forward _ ->
        reject "nested proof construct"
  in
  if predicate.Sst.typ <> Sst.Bool then
    Error (predicate.span, "builtin assertion condition must be Boolean")
  else pure predicate
