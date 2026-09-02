open Typedtree

type application = {
  callee : expression;
  arguments : (arg_label * expression) list;
  result_type : Types.type_expr;
  span : Location.t;
}

type 'error shape_error =
  | Lowering_error of 'error
  | Invalid_shape of string

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let shape ~env ~lower ~location source_type =
  let rec collect endpoints typ =
    match Types.get_desc typ with
    | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
        collect endpoints body
    | Types.Tarrow ((label, argument_mode, result_mode), argument, result, commutable) ->
        let () = ignore (argument_mode, result_mode, argument, commutable) in
        let* argument =
          try
            let filtered = Ctype.filter_arrow_mono env typ label in
            Ok filtered.ty_arg
          with Ctype.Filter_arrow_mono_failed ->
            Error
              (Invalid_shape
                 "callback arrow does not have a compiler-resolved \
                  monomorphic endpoint")
        in
        let* argument =
          Result.map_error (fun error -> Lowering_error error)
            (lower location argument)
        in
        let label =
          match label with
          | Types.Nolabel -> Ok Callback_shape_private.Unlabelled
          | Types.Labelled label ->
              Ok (Callback_shape_private.Labelled label)
          | Types.Optional _ ->
              Error
                (Invalid_shape
                   "optional callback endpoints are not supported")
          | Types.Position _ ->
              Error
                (Invalid_shape
                   "position-labelled callback endpoints are not supported")
        in
        let* label = label in
        collect ((label, argument) :: endpoints) result
    | _ ->
        let* result =
          Result.map_error (fun error -> Lowering_error error)
            (lower location typ)
        in
        (match
           Callback_shape_private.create ~endpoints:(List.rev endpoints)
             ~result
         with
        | Ok shape -> Ok shape
        | Error message -> Error (Invalid_shape message))
  in
  collect [] source_type

let application expression =
  match expression.exp_desc with
  | Texp_apply (callee, arguments, _, _, _) ->
      let rec collect collected = function
        | [] ->
            Ok
              {
                callee;
                arguments = List.rev collected;
                result_type = expression.exp_type;
                span = expression.exp_loc;
              }
        | (label, Arg (argument, _)) :: rest ->
            collect ((label, argument) :: collected) rest
        | (_, Omitted _) :: _ ->
            Error "callback application is partial or has an omitted endpoint"
      in
      collect [] arguments
  | _ -> Error "callback projection payload is not an application"

let add_ident ident idents =
  if List.exists (Ident.same ident) idents then idents else ident :: idents

let pattern_idents pattern =
  let idents = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      pat =
        (fun (type k) self (pattern : k general_pattern) ->
          (match pattern.pat_desc with
          | Tpat_var (ident, _, _, _, _)
          | Tpat_alias (_, ident, _, _, _, _, _) ->
              idents := add_ident ident !idents
          | Tpat_any | Tpat_constant _ | Tpat_tuple _ | Tpat_construct _
          | Tpat_variant _ | Tpat_record _ | Tpat_array _ | Tpat_or _
          | Tpat_lazy _ | Tpat_value _ | Tpat_exception _
          | Tpat_unboxed_tuple _ | Tpat_record_unboxed_product _ ->
              ());
          default.pat self pattern);
    }
  in
  iterator.pat iterator pattern;
  !idents

let free_idents expression =
  let bound = ref [] in
  let referenced = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      pat =
        (fun (type k) self (pattern : k general_pattern) ->
          bound := List.rev_append (pattern_idents pattern) !bound;
          default.pat self pattern);
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_ident (Path.Pident ident, _, _, _, _)
          | Texp_mutvar { txt = ident; _ } ->
              referenced := add_ident ident !referenced
          | Texp_ident ((Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _), _, _, _, _)
          | Texp_constant _ | Texp_let _ | Texp_letmutable _
          | Texp_function _ | Texp_apply _ | Texp_match _ | Texp_try _
          | Texp_tuple _ | Texp_unboxed_tuple _ | Texp_construct _
          | Texp_variant _ | Texp_record _ | Texp_record_unboxed_product _
          | Texp_atomic_loc _ | Texp_field _ | Texp_unboxed_field _
          | Texp_setfield _ | Texp_array _ | Texp_idx _
          | Texp_list_comprehension _ | Texp_array_comprehension _
          | Texp_ifthenelse _ | Texp_sequence _ | Texp_while _ | Texp_for _
          | Texp_send _ | Texp_new _ | Texp_instvar _ | Texp_setinstvar _
          | Texp_setmutvar _ | Texp_override _ | Texp_letmodule _
          | Texp_letexception _ | Texp_assert _ | Texp_lazy _ | Texp_object _
          | Texp_pack _ | Texp_letop _ | Texp_unreachable
          | Texp_extension_constructor _ | Texp_open _ | Texp_probe _
          | Texp_probe_is_enabled _ | Texp_exclave _ | Texp_src_pos
          | Texp_overwrite _ | Texp_hole _ | Texp_quotation _
          | Texp_antiquotation _ | Texp_eval _ ->
              ());
          default.expr self expression);
    }
  in
  iterator.expr iterator expression;
  List.rev !referenced
  |> List.filter (fun ident ->
         not (List.exists (Ident.same ident) !bound))

let compiler_identity = Ident.unique_name

let type_evidence typ =
  Format.asprintf "%a" Printtyp.type_expr typ

type lowering_state = {
  mutable callback_bindings : (Ident.t * Sst.callback_binding) list;
  mutable callback_origins :
    (int * Callback_certificate_private.origin) list;
  mutable callback_definitions : Sst.function_definition list;
  mutable next_callback_id : int;
  compilation_identity :
    (Callback_certificate_private.compilation_identity, string) result option;
}

type mixed_call_argument =
  | Lowered_call_argument of {
      argument : Sst.call_argument;
      source : Typedtree.expression;
    }
  | Pending_callback_argument of {
      shape : Callback_shape_private.t;
      label : string option;
      source : Typedtree.expression;
    }

let create_state ?(next_callback_id = 0) ?compilation_identity () =
  {
    callback_bindings = [];
    callback_origins = [];
    callback_definitions = [];
    next_callback_id;
    compilation_identity;
  }

let deeply_immutable ~parametric_adts ~definitions typ =
  let descriptors =
    List.map
      (fun item -> item.Parametric_adt_lowering_private.descriptor)
      parametric_adts
  in
  let rec aggregate visiting type_id =
    if List.mem type_id visiting then true
    else
      match
        List.find_opt
          (fun (definition : Sst.type_definition) ->
            definition.type_id = type_id)
          definitions
      with
      | None -> false
      | Some definition ->
          let fields =
            match definition.type_kind with
            | Sst.Record_definition fields -> fields
            | Sst.Variant_definition constructors ->
                List.concat_map
                  (fun constructor -> constructor.Sst.constructor_fields)
                  constructors
          in
          List.for_all
            (fun (field : Sst.field_definition) ->
              field.field_mutability = Sst.Immutable_field
              &&
              match field.field_type with
              | Sst.Aggregate nested -> aggregate (type_id :: visiting) nested
              | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int -> true
              | Sst.Tuple _ | Sst.Parameter _ | Sst.Application _ ->
                  Parametric_adt.deeply_immutable_instance descriptors
                    field.field_type)
            fields
  in
  match typ with
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int -> true
  | Sst.Aggregate type_id -> aggregate [] type_id
  | Sst.Tuple _ | Sst.Application _ | Sst.Parameter _ ->
      Parametric_adt.deeply_immutable_instance descriptors typ

type 'error local_services = {
  shape :
    Env.t ->
    Location.t ->
    Types.type_expr ->
    (Callback_shape_private.t, 'error) result;
  lower_pattern :
    (Ident.t * Sst.binding) list ->
    Typedtree.pattern ->
    (Sst.pattern * (Ident.t * Sst.binding) list, 'error) result;
  lower_body :
    (Ident.t * Sst.binding) list ->
    Typedtree.expression ->
    (Sst.contracts * Sst.expression, 'error) result;
  parameter_label : Typedtree.arg_label -> string option;
  find_value : Ident.t -> Sst.binding option;
  find_callback : Ident.t -> Sst.callback_binding option;
  source_type : Sst.binding -> Types.type_expr option;
  immutable : Sst.typ -> bool;
  fresh_id : unit -> int;
  compilation_identity :
    Location.t ->
    (Callback_certificate_private.compilation_identity, 'error) result;
  seal :
    Callback_certificate_private.origin ->
    Location.t ->
    Callback_certificate_private.t;
  source_file : string;
  owner_name : string;
  type_binders : Parametric_type.binder list;
  span : Location.t -> Sst.span;
  policy_error : Location.t -> string -> 'error;
  authentication_error : Location.t -> string -> 'error;
  contract_error : Location.t -> string -> 'error;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let callback_arrow_type typ =
  let rec arrow typ =
    match Types.get_desc typ with
    | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
        arrow body
    | Types.Tarrow _ -> true
    | _ -> false
  in
  arrow typ

let lower_local_parameters services params =
  let rec loop lowered bindings = function
    | [] -> Ok (List.rev lowered, bindings)
    | parameter :: rest ->
        if parameter.Typedtree.fp_partial = Partial then
          Error
            (services.policy_error parameter.fp_loc
               "partial callback parameters are unsupported")
        else
          match parameter.fp_kind with
          | Tparam_optional_default _ ->
              Error
                (services.policy_error parameter.fp_loc
                   "optional callback parameters are unsupported")
          | Tparam_pat pattern ->
              if callback_arrow_type pattern.pat_type then
                Error
                  (services.policy_error pattern.pat_loc
                     "nested callback parameters are unsupported")
              else
                let* pattern, bindings =
                  services.lower_pattern bindings pattern
                in
                loop
                  (Sst.Value_parameter
                     {
                       label =
                         services.parameter_label parameter.fp_arg_label;
                       pattern;
                       optional_default = None;
                     }
                  :: lowered)
                  bindings rest
  in
  loop [] [] params

let collect_captures services expression =
  let rec collect captures = function
    | [] -> Ok (List.rev captures)
    | ident :: rest -> (
        match services.find_value ident, services.find_callback ident with
        | Some binding, None ->
            if not (services.immutable binding.Sst.typ) then
              Error
                (services.policy_error expression.Typedtree.exp_loc
                   "callback capture is not deeply immutable")
            else
              let source_type =
                Option.value ~default:expression.exp_type
                  (services.source_type binding)
              in
              collect
                ({
                   Callback_certificate_private.binding_id = binding.id;
                   binding_uid = compiler_identity ident;
                   typ = binding.typ;
                   compiler_mode = type_evidence source_type;
                   instance_mode = "exec-immutable";
                   frozen_value_identity =
                     Printf.sprintf "%s:%d" services.source_file binding.id;
                 }
                :: captures)
                rest
        | None, Some _ ->
            Error
              (services.policy_error expression.exp_loc
                 "nested callback capture is unsupported")
        | Some _, Some _ ->
            Error
              (services.authentication_error expression.exp_loc
                 "capture identity is ambiguous")
        | None, None -> collect captures rest)
  in
  collect [] (free_idents expression)

let issue_local services state ?ident ~name expression shape parameters
    contracts body captures =
  let callback_id = services.fresh_id () in
  let callback_name =
    Printf.sprintf "%s.%s.%d" services.owner_name name (-callback_id)
  in
  let function_id =
    { Sst.function_index = callback_id; function_name = callback_name }
  in
  let definition =
    Sst_normalize.authenticated_checked_exec ~source_file:services.source_file
      ~function_id ~recursive:false ~parameters ~contracts ~body
      ~result_type:body.Sst.typ ~returns_unique_parameter:None
      ~span:(services.span expression.Typedtree.exp_loc)
  in
  let definition = { definition with Sst.type_binders = services.type_binders } in
  let callable_identity =
    Option.fold
      ~none:(Printf.sprintf "%s:anonymous:%d" services.source_file callback_id)
      ~some:compiler_identity ident
  in
  let* compilation_identity =
    services.compilation_identity expression.exp_loc
  in
  let* origin =
    match
      Callback_certificate_private.issue_origin
        ~kind:Callback_certificate_private.Local ~compilation_identity
        ~callable_identity ~shape ~compiler_mode:(type_evidence expression.exp_type)
        ~contract_identity:
          (Printf.sprintf "%s:%d:contract" callable_identity callback_id)
        ~captures ~pure:true ~total:true ~complete:true
    with
    | Ok origin -> Ok origin
    | Error message ->
        Error (services.authentication_error expression.exp_loc message)
  in
  let binding =
    {
      Sst.callback_id;
      callback_name;
      callback_shape = shape;
      callback_certificate = services.seal origin expression.exp_loc;
      callback_span = services.span expression.exp_loc;
    }
  in
  state.callback_origins <- (callback_id, origin) :: state.callback_origins;
  state.callback_definitions <- definition :: state.callback_definitions;
  Ok binding

let lower_local services state bindings ?ident ~name expression =
  match expression.Typedtree.exp_desc with
  | Texp_function { params; body = Tfunction_body body; _ } ->
      let source_type =
        Ctype.full_expand ~may_forget_scope:false expression.exp_env
          expression.exp_type
      in
      let* shape =
        services.shape expression.exp_env expression.exp_loc source_type
      in
      let* parameters, endpoint_bindings =
        lower_local_parameters services params
      in
      let* captures = collect_captures services expression in
      let* contracts, body =
        services.lower_body (endpoint_bindings @ bindings) body
      in
      let* () =
        match Callback_contract_private.validate_explicit contracts with
        | Ok () -> Ok ()
        | Error message ->
            Error (services.contract_error expression.exp_loc message)
      in
      issue_local services state ?ident ~name expression shape parameters
        contracts body captures
  | _ ->
      Error
        (services.policy_error expression.exp_loc
           "callback actual is not an immediate function")

let find_binding (state : lowering_state) ident =
  List.find_map
    (fun (candidate, binding) ->
      if Ident.same ident candidate then Some binding else None)
    state.callback_bindings

let caller_identity = function
  | None -> None
  | Some function_id ->
      Some
        (Callback_certificate_private.caller_identity
           ~function_index:function_id.Sst.function_index
           ~function_name:function_id.function_name)

let call_edge_identity caller location =
  Callback_certificate_private.edge_identity ~caller
    ~start_line:location.Location.loc_start.pos_lnum
    ~start_column:(location.loc_start.pos_cnum - location.loc_start.pos_bol)
    ~end_line:location.loc_end.pos_lnum
    ~end_column:(location.loc_end.pos_cnum - location.loc_end.pos_bol)

let validate_top_level_contract ~retained_pair ~application ~resolves ~error
    value_binding =
  Callback_contract_private.validate_top_level ~retained_pair ~application
    ~resolves value_binding
  |> Result.map_error (error value_binding.Typedtree.vb_loc)

let compilation_identity (state : lowering_state) =
  match state.compilation_identity with
  | Some result -> result
  | None ->
      Error
        "callback authority requires authenticated compiler compilation metadata"

let fresh_id (state : lowering_state) ~owner_index =
  let ordinal = state.next_callback_id in
  state.next_callback_id <- ordinal + 1;
  let owner = Option.value ~default:(-1) owner_index + 1 in
  let diagonal = owner + ordinal in
  -1 - (((diagonal * (diagonal + 1)) / 2) + ordinal)

let fresh_id_for_function state function_id =
  fresh_id state
    ~owner_index:
      (Option.map (fun id -> id.Sst.function_index) function_id)

let seal_binding (state : lowering_state) ~caller_identity ~span location
    binding =
  match List.assoc_opt binding.Sst.callback_id state.callback_origins with
  | None -> binding
  | Some origin ->
      {
        binding with
        callback_certificate =
          Callback_certificate_private.seal_call_edge origin ~caller_identity
            ~call_edge_identity:(call_edge_identity caller_identity location);
        callback_span = span location;
      }

let application_binding (state : lowering_state) ~caller_identity ~span
    expression =
  let* flattened = application expression in
  match flattened.callee.exp_desc with
  | Texp_ident (Path.Pident ident, _, _, _, _) -> (
      match find_binding state ident with
      | Some binding ->
          Ok
            ( flattened,
              seal_binding state ~caller_identity ~span expression.exp_loc
                binding )
      | None ->
          Error "callback application does not name the exact callback binding")
  | _ -> Error "callback application callee is not an exact callback identifier"

type 'error formal_services = {
  shape :
    Env.t ->
    Location.t ->
    Types.type_expr ->
    (Callback_shape_private.t, 'error) result;
  compilation_identity :
    Location.t ->
    (Callback_certificate_private.compilation_identity, 'error) result;
  fresh_id : unit -> int;
  owner_identity : string;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Sst.span;
  policy_error : Location.t -> string -> 'error;
}

let issue_formal services state parameter pattern =
  match pattern.Typedtree.pat_desc with
  | Tpat_var (ident, name, _, _, _) ->
      let source_type =
        Ctype.full_expand ~may_forget_scope:false pattern.pat_env
          pattern.pat_type
      in
      let* shape =
        services.shape pattern.pat_env pattern.pat_loc source_type
      in
      let callback_id = services.fresh_id () in
      let* compilation_identity =
        services.compilation_identity pattern.pat_loc
      in
      let callable_identity =
        Printf.sprintf "%s:formal:%s" services.owner_identity
          (compiler_identity ident)
      in
      let binding =
        {
          Sst.callback_id;
          callback_name = name.txt;
          callback_shape = shape;
          callback_certificate =
            Callback_certificate_private.issue_formal ~compilation_identity
              ~callable_identity ~shape;
          callback_span = services.span pattern.pat_loc;
        }
      in
      state.callback_bindings <- (ident, binding) :: state.callback_bindings;
      Ok
        (Sst.Callback_parameter
           {
             label = services.parameter_label parameter.fp_arg_label;
             binding;
           })
  | _ ->
      Error
        (services.policy_error pattern.pat_loc
           "callback formal must be one exact compiler binding")

let source_type_variables ~parameters ~result ~substitutions =
  let variables =
    List.concat_map
      Parametric_lowering_private.source_type_variable_ids
      (List.map (fun pattern -> pattern.Typedtree.pat_type) parameters
      @ List.map (fun body -> body.Typedtree.exp_type)
          (Option.to_list result))
    |> List.fold_left
         (fun ordered id ->
           if List.mem id ordered then ordered else ordered @ [ id ])
         []
  in
  List.filter
    (fun id ->
      not (List.exists (fun (candidate, _) -> candidate = id) substitutions))
    variables

let same_retained_source_type left right =
  let rec same seen left right =
    let left = Callback_shape_private.canonical_type left in
    let right = Callback_shape_private.canonical_type right in
    let pair = (Types.get_id left, Types.get_id right) in
    if fst pair = snd pair || List.mem pair seen then true
    else
      let seen = pair :: seen in
      match (Types.get_desc left, Types.get_desc right) with
      | ( (Types.Tvar _ | Types.Tunivar _),
          (Types.Tvar _ | Types.Tunivar _) ) ->
          false
      | ( Types.Tconstr (left_path, left_arguments, _),
          Types.Tconstr (right_path, right_arguments, _) ) ->
          Path.same left_path right_path
          && List.length left_arguments = List.length right_arguments
          && List.for_all2 (same seen) left_arguments right_arguments
      | Types.Ttuple left, Types.Ttuple right
      | Types.Tunboxed_tuple left, Types.Tunboxed_tuple right ->
          List.map fst left = List.map fst right
          && List.for_all2
               (fun (_, left) (_, right) -> same seen left right)
               left right
      | Types.Tpoly (left, []), _
      | Types.Tlink left, _
      | Types.Tsubst (left, _), _ ->
          same seen left right
      | _, Types.Tpoly (right, [])
      | _, Types.Tlink right
      | _, Types.Tsubst (right, _) ->
          same seen left right
      | ( Types.Tarrow (left_label, left_domain, left_range, _),
          Types.Tarrow (right_label, right_domain, right_range, _) ) ->
          left_label = right_label
          && same seen left_domain right_domain
          && same seen left_range right_range
      | ( Types.Tpoly (_, _ :: _), _ )
      | ( _, Types.Tpoly (_, _ :: _) )
      | ( Types.Tobject _, _ ) | ( _, Types.Tobject _ )
      | ( Types.Tfield _, _ ) | ( _, Types.Tfield _ )
      | ( Types.Tnil, _ ) | ( _, Types.Tnil )
      | ( Types.Tvariant _, _ ) | ( _, Types.Tvariant _ )
      | ( Types.Tpackage _, _ ) | ( _, Types.Tpackage _ )
      | ( Types.Tquote _, _ ) | ( _, Types.Tquote _ )
      | ( Types.Tsplice _, _ ) | ( _, Types.Tsplice _ )
      | ( Types.Tof_kind _, _ ) | ( _, Types.Tof_kind _ )
      | ( Types.Tconstr _,
          (Types.Tvar _ | Types.Tunivar _ | Types.Ttuple _
          | Types.Tunboxed_tuple _ | Types.Tarrow _) )
      | ( (Types.Tvar _ | Types.Tunivar _ | Types.Ttuple _
          | Types.Tunboxed_tuple _ | Types.Tarrow _),
          Types.Tconstr _ )
      | ( (Types.Tvar _ | Types.Tunivar _),
          (Types.Ttuple _ | Types.Tunboxed_tuple _ | Types.Tarrow _) )
      | ( (Types.Ttuple _ | Types.Tunboxed_tuple _ | Types.Tarrow _),
          (Types.Tvar _ | Types.Tunivar _) )
      | ( Types.Ttuple _, (Types.Tunboxed_tuple _ | Types.Tarrow _) )
      | ( (Types.Tunboxed_tuple _ | Types.Tarrow _), Types.Ttuple _ )
      | ( Types.Tunboxed_tuple _, Types.Tarrow _ )
      | ( Types.Tarrow _, Types.Tunboxed_tuple _ ) ->
          false
  in
  same [] left right

let rec retained_shadow_type_aliases aliases source_type shadow_type =
  let source_type = Callback_shape_private.canonical_type source_type in
  let shadow_type = Callback_shape_private.canonical_type shadow_type in
  match Types.get_desc shadow_type with
  | Types.Tvar _ | Types.Tunivar _ -> (
      let shadow_id = Types.get_id shadow_type in
      if shadow_id = Types.get_id source_type then Some aliases
      else
        match List.assoc_opt shadow_id aliases with
        | None -> Some ((shadow_id, source_type) :: aliases)
        | Some previous ->
            let same_source =
              same_retained_source_type previous source_type
            in
            [%log.trace "validated retained callback shadow alias reuse"
              ~stage:(Delator.Field.string "retained-shadow-alias")
              ~decision:
                (Delator.Field.string
                   (if same_source then "accepted" else "rejected"))
              ~reason_class:
                (Delator.Field.string
                   (if same_source then "structural-source-match"
                    else "structural-source-mismatch"))
              ~alias_count:(Delator.Field.int (List.length aliases))];
            if same_source then Some aliases else None)
  | Types.Tpoly (shadow_body, [])
  | Types.Tlink shadow_body
  | Types.Tsubst (shadow_body, _) ->
      retained_shadow_type_aliases aliases source_type shadow_body
  | Types.Tconstr (shadow_path, shadow_arguments, _) -> (
      match Types.get_desc source_type with
      | Types.Tpoly (source_body, [])
      | Types.Tlink source_body
      | Types.Tsubst (source_body, _) ->
          retained_shadow_type_aliases aliases source_body shadow_type
      | Types.Tconstr (source_path, source_arguments, _)
        when
          Path.same source_path shadow_path
          && List.length source_arguments = List.length shadow_arguments ->
          List.fold_left2
            (fun aliases source shadow ->
              Option.bind aliases (fun aliases ->
                  retained_shadow_type_aliases aliases source shadow))
            (Some aliases) source_arguments shadow_arguments
      | _ -> None)
  | Types.Ttuple shadow_components -> (
      match Types.get_desc source_type with
      | Types.Ttuple source_components
        when
          List.map fst source_components = List.map fst shadow_components ->
          List.fold_left2
            (fun aliases (_, source) (_, shadow) ->
              Option.bind aliases (fun aliases ->
                  retained_shadow_type_aliases aliases source shadow))
            (Some aliases) source_components shadow_components
      | _ -> None)
  | Types.Tunboxed_tuple shadow_components -> (
      match Types.get_desc source_type with
      | Types.Tunboxed_tuple source_components
        when
          List.map fst source_components = List.map fst shadow_components ->
          List.fold_left2
            (fun aliases (_, source) (_, shadow) ->
              Option.bind aliases (fun aliases ->
                  retained_shadow_type_aliases aliases source shadow))
            (Some aliases) source_components shadow_components
      | _ -> None)
  | Types.Tarrow _ | Types.Tpoly (_, _ :: _) | Types.Tobject _
  | Types.Tfield _ | Types.Tnil | Types.Tvariant _ | Types.Tpackage _
  | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _ ->
      None
