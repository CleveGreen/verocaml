let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let infer_type_arguments_impl ?(logical_integer_join = false) ~binders ~formals
    ~actuals result =
  if List.length formals <> List.length actuals then
    Error "generic call formal/actual arity mismatch"
  else
    let is_bound binder =
      List.exists
        (fun candidate -> Parametric_type.compare_binder candidate binder = 0)
        binders
    in
    let rec unify substitutions formal actual =
      match formal with
      | Parametric_type.Parameter binder when is_bound binder -> (
          match
            List.find_opt
              (fun (candidate, _) ->
                Parametric_type.compare_binder candidate binder = 0)
              substitutions
          with
          | None -> Ok ((binder, actual) :: substitutions)
          | Some (_, previous) when Parametric_type.equal previous actual ->
              Ok substitutions
          | Some (_, Parametric_type.Int)
            when logical_integer_join
                 && Parametric_type.equal actual Parametric_type.Mathematical_int ->
              Ok
                ((binder, Parametric_type.Mathematical_int)
                :: List.filter
                     (fun (candidate, _) ->
                       Parametric_type.compare_binder candidate binder <> 0)
                     substitutions)
          | Some (_, Parametric_type.Mathematical_int)
            when logical_integer_join
                 && Parametric_type.equal actual Parametric_type.Int ->
              Ok substitutions
          | Some (_, previous) ->
              Error
                (Printf.sprintf
                   "generic call infers inconsistent type argument for '%d@%s#%d: %s versus %s"
                   binder.ordinal binder.owner.owner_name binder.owner.owner_index
                   (Parametric_type.to_string previous)
                   (Parametric_type.to_string actual)))
      | Parametric_type.Tuple formals -> (
          match actual with
          | Parametric_type.Tuple actuals
            when List.length formals = List.length actuals ->
              List.fold_left2
                (fun result (formal_label, formal) (actual_label, actual) ->
                  let* substitutions = result in
                  if not (Option.equal String.equal formal_label actual_label) then
                    Error "generic tuple labels differ during type inference"
                  else unify substitutions formal actual)
                (Ok substitutions) formals actuals
          | _ -> Error "generic tuple type does not match actual type")
      | Parametric_type.Application (formal_constructor, formals) -> (
          match actual with
          | Parametric_type.Application (actual_constructor, actuals)
            when Parametric_type.compare_constructor formal_constructor
                   actual_constructor
                 = 0
                 && List.length formals = List.length actuals ->
              List.fold_left2
                (fun result formal actual ->
                  let* substitutions = result in
                  unify substitutions formal actual)
                (Ok substitutions) formals actuals
          | _ -> Error "generic type application does not match actual type")
      | Parametric_type.Mathematical_int
        when logical_integer_join
             && Parametric_type.equal actual Parametric_type.Int ->
          [%log.trace "joined logical integer types during generic inference"
            ~stage:(Delator.Field.string "generic-logical-type-inference")
            ~type_view:
              (Delator.Field.map
                 [ ("formal", Delator.Field.string "Int");
                   ("actual", Delator.Field.string "int") ])
            ~decision:(Delator.Field.string "accepted-logical-join")];
          Ok substitutions
      | closed when Parametric_type.equal closed actual -> Ok substitutions
      | _ -> Error "generic formal type does not match actual type"
    in
    let* substitutions =
      List.fold_left2
        (fun result formal actual ->
          let* substitutions = result in
          unify substitutions formal actual)
        (Ok []) formals actuals
    in
    let* substitutions =
      match result with
      | None -> Ok substitutions
      | Some (formal_result, actual_result) ->
          unify substitutions formal_result actual_result
    in
    List.fold_left
      (fun result binder ->
        let* arguments = result in
        match
          List.find_opt
            (fun (candidate, _) ->
              Parametric_type.compare_binder candidate binder = 0)
            substitutions
        with
        | Some (_, argument) -> Ok (argument :: arguments)
        | None -> Error "generic call leaves a type parameter unconstrained")
      (Ok []) (List.rev binders)

let infer_type_arguments ~binders ~formals ~actuals ~formal_result
    ~actual_result =
  infer_type_arguments_impl ~binders ~formals ~actuals
    (Some (formal_result, actual_result))

let infer_type_arguments_from_actuals ~binders ~formals ~actuals =
  infer_type_arguments_impl ~binders ~formals ~actuals None

let infer_type_arguments_from_logical_actuals ~binders ~formals ~actuals =
  infer_type_arguments_impl ~logical_integer_join:true ~binders ~formals ~actuals
    None

let infer_type_arguments_for_logical_call ~binders ~formals ~actuals
    ~formal_result ~actual_result =
  infer_type_arguments_impl ~logical_integer_join:true ~binders ~formals ~actuals
    (Some (formal_result, actual_result))

let reconcile_authenticated_result ~binders ~semantic ~compiler =
  let is_bound binder =
    List.exists
      (fun candidate -> Parametric_type.compare_binder candidate binder = 0)
      binders
  in
  let rec reconcile semantic compiler =
    match (semantic, compiler) with
    | Parametric_type.Parameter binder, compiler when is_bound binder ->
        Ok compiler
    | Parametric_type.Mathematical_int, Parametric_type.Int ->
        Ok Parametric_type.Mathematical_int
    | Parametric_type.Tuple semantics, Parametric_type.Tuple compilers
      when List.length semantics = List.length compilers ->
        let rec components reconciled semantics compilers =
          match (semantics, compilers) with
          | [], [] -> Ok (Parametric_type.Tuple (List.rev reconciled))
          | (semantic_label, semantic) :: semantics,
            (compiler_label, compiler) :: compilers
            when Option.equal String.equal semantic_label compiler_label ->
              let* reconciled_component = reconcile semantic compiler in
              components
                ((semantic_label, reconciled_component) :: reconciled)
                semantics compilers
          | ( _ :: _, _ :: _ ) ->
              Error
                "authenticated result tuple labels differ from the compiler result"
          | [], _ :: _ | _ :: _, [] -> assert false
        in
        components [] semantics compilers
    | ( Parametric_type.Application (semantic_constructor, semantics),
        Parametric_type.Application (compiler_constructor, compilers) )
      when Parametric_type.compare_constructor semantic_constructor
             compiler_constructor
           = 0
           && List.length semantics = List.length compilers ->
        let rec arguments reconciled semantics compilers =
          match (semantics, compilers) with
          | [], [] ->
              Ok
                (Parametric_type.Application
                   (semantic_constructor, List.rev reconciled))
          | semantic :: semantics, compiler :: compilers ->
              let* reconciled_argument = reconcile semantic compiler in
              arguments (reconciled_argument :: reconciled) semantics compilers
          | [], _ :: _ | _ :: _, [] -> assert false
        in
        arguments [] semantics compilers
    | semantic, compiler when Parametric_type.equal semantic compiler ->
        Ok semantic
    | Parametric_type.Mathematical_int, _
    | ( Parametric_type.Unit | Parametric_type.Bool | Parametric_type.Int
      | Parametric_type.Bit_vector _ | Parametric_type.Tuple _ | Parametric_type.Aggregate _
      | Parametric_type.Parameter _ | Parametric_type.Application _ ), _ ->
        Error
          "compiler result type cannot satisfy the authenticated semantic result shape"
  in
  let result = reconcile semantic compiler in
  (match result with
  | Ok (reconciled [@log_value.debug]) ->
      [%log.debug "reconciled authenticated callable result type"
        ~stage:(Delator.Field.string "authenticated-result-reconciliation")
        ~semantic_sort:
          (Delator.Field.string (Parametric_type.to_string semantic))
        ~compiler_sort:
          (Delator.Field.string (Parametric_type.to_string compiler))
        ~reconciled_sort:
          (Delator.Field.string
             (Parametric_type.to_string
                (reconciled [@log_value.debug])))
        ~binder_count:(Delator.Field.int (List.length binders))
        ~decision:(Delator.Field.string "accepted")]
  | Error _ ->
      [%log.debug "rejected incompatible authenticated callable result type"
        ~stage:(Delator.Field.string "authenticated-result-reconciliation")
        ~semantic_sort:
          (Delator.Field.string (Parametric_type.to_string semantic))
        ~compiler_sort:
          (Delator.Field.string (Parametric_type.to_string compiler))
        ~binder_count:(Delator.Field.int (List.length binders))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "result-shape-mismatch")]);
  result
[@@delator.instrument] [@@delator.level debug] [@@delator.no_exn_log]

let instantiate ~binders ~arguments typ =
  Parametric_type.instantiate binders arguments typ

type source_type_error =
  | Polymorphic_source_type
  | Higher_order_source_type
  | Unsupported_source_type

let rec source_type_variable_ids typ =
  match Types.get_desc typ with
  | Types.Tvar _ | Types.Tunivar _ -> [ Types.get_id typ ]
  | Types.Tarrow (_, domain, codomain, _) ->
      source_type_variable_ids domain @ source_type_variable_ids codomain
  | Types.Ttuple components | Types.Tunboxed_tuple components ->
      List.concat_map
        (fun (_, component) -> source_type_variable_ids component)
        components
  | Types.Tconstr (_, arguments, _) ->
      List.concat_map source_type_variable_ids arguments
  | Types.Tpoly (body, _) | Types.Tlink body | Types.Tsubst (body, _)
  | Types.Tobject (body, _) | Types.Tfield (_, _, body, _) | Types.Tquote body
  | Types.Tsplice body ->
      source_type_variable_ids body
  | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ | Types.Tof_kind _ -> []

let rec first_order_source_type typ =
  match Types.get_desc typ with
  | Types.Tvar _ | Types.Tunivar _ -> true
  | Types.Tconstr (_, arguments, _) ->
      List.for_all first_order_source_type arguments
  | Types.Ttuple components | Types.Tunboxed_tuple components ->
      List.for_all
        (fun (_, component) -> first_order_source_type component)
        components
  | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
      first_order_source_type body
  | Types.Tarrow _ | Types.Tpoly (_, _ :: _) | Types.Tobject _
  | Types.Tfield _ | Types.Tnil | Types.Tvariant _ | Types.Tpackage _
  | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _ ->
      false

let rec contains_source_application ~is_application typ =
  match Types.get_desc typ with
  | Types.Tconstr (path, arguments, _) ->
      is_application path
      || List.exists (contains_source_application ~is_application) arguments
  | Types.Ttuple components | Types.Tunboxed_tuple components ->
      List.exists
        (fun (_, component) ->
          contains_source_application ~is_application component)
        components
  | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
      contains_source_application ~is_application body
  | Types.Tvar _ | Types.Tunivar _ | Types.Tarrow _ | Types.Tpoly _
  | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tvariant _
  | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _ ->
      false

let rec variables_under_source_application ~is_application typ =
  match Types.get_desc typ with
  | Types.Tconstr (path, arguments, _) ->
      if is_application path then List.concat_map source_type_variable_ids arguments
      else
        List.concat_map
          (variables_under_source_application ~is_application)
          arguments
  | Types.Ttuple components | Types.Tunboxed_tuple components ->
      List.concat_map
        (fun (_, component) ->
          variables_under_source_application ~is_application component)
        components
  | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
      variables_under_source_application ~is_application body
  | Types.Tvar _ | Types.Tunivar _ | Types.Tarrow _ | Types.Tpoly _
  | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tvariant _
  | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _ ->
      []

let lower_source_type ~substitutions ~binders
    ?(arrow = fun _ _ _ -> Error Higher_order_source_type) ~application typ =
  let rec lower seen typ =
    let type_id = Types.get_id typ in
    if List.mem type_id seen then Error Polymorphic_source_type
    else
      match List.assoc_opt type_id substitutions with
      | Some replacement -> lower (type_id :: seen) replacement
      | None -> (
          match Types.get_desc typ with
          | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
              lower (type_id :: seen) body
          | Types.Tconstr (path, [], _) when Path.same path Predef.path_unit ->
              Ok Parametric_type.Unit
          | Types.Tconstr (path, [], _) when Path.same path Predef.path_bool ->
              Ok Parametric_type.Bool
          | Types.Tconstr (path, [], _) when Path.same path Predef.path_int ->
              Ok Parametric_type.Int
          | Types.Ttuple components ->
              List.fold_left
                (fun result (label, component) ->
                  let* lowered = result in
                  let* component = lower [] component in
                  Ok ((label, component) :: lowered))
                (Ok []) components
              |> Result.map (fun components ->
                     Parametric_type.Tuple (List.rev components))
          | Types.Tconstr (path, arguments, _) -> application path arguments
          | Types.Tvar _ | Types.Tunivar _ -> (
              match List.assoc_opt type_id binders with
              | Some binder -> Ok (Parametric_type.Parameter binder)
              | None -> Error Polymorphic_source_type)
          | Types.Tarrow ((label, _, _), domain, range, _) ->
              let* domain = lower [] domain in
              let* range = lower [] range in
              arrow label domain range
          | Types.Tpoly (_, _ :: _) -> Error Polymorphic_source_type
          | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _
          | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _
          | Types.Tsplice _ | Types.Tof_kind _ ->
              Error Unsupported_source_type)
  in
  lower [] typ

let formal_label (label : Typedtree.arg_label) =
  match label with
  | Nolabel -> None
  | Labelled label -> Some label
  | Optional label -> Some ("?" ^ label)
  | Position label -> Some ("@" ^ label)

let compiler_parameter_domains function_type parameters =
  let rec arrow seen typ =
    let id = Types.get_id typ in
    if List.mem id seen then None
    else
      match Types.get_desc typ with
      | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
          arrow (id :: seen) body
      | Types.Tarrow (descriptor, domain, range, commutable) ->
          Some (descriptor, domain, range, commutable)
      | Types.Tvar _ | Types.Tunivar _ | Types.Tconstr _ | Types.Ttuple _
      | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
      | Types.Tvariant _ | Types.Tpoly (_, _ :: _) | Types.Tpackage _
      | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _ ->
          None
  in
  let rec collect index typ domains = function
    | [] -> Ok (List.rev domains)
    | (parameter : Typedtree.function_param) :: rest -> (
        match arrow [] typ with
        | Some ((label, _, _), domain, range, _)
          when label = parameter.fp_arg_label ->
            collect (index + 1) range (domain :: domains) rest
        | Some _ ->
            Error
              ( parameter.fp_loc,
                Printf.sprintf
                  "typedtree parameter %d label differs from its compiler arrow"
                  index )
        | None ->
            Error
              ( parameter.fp_loc,
                Printf.sprintf
                  "typedtree parameter %d has no compiler arrow domain" index ))
  in
  collect 0 function_type [] parameters

let lower_typedtree_formals ~lower ~optional_carrier ~parameter_error
    ~function_type parameters =
  match compiler_parameter_domains function_type parameters with
  | Error (location, message) -> Error (parameter_error location message)
  | Ok domains ->
      let rec collect types labels parameters domains =
        match parameters, domains with
        | [], [] -> Ok (List.rev types, List.rev labels)
        | parameter :: rest, domain :: remaining_domains ->
            let* typ =
              match parameter.Typedtree.fp_kind with
              | Tparam_pat pattern -> lower pattern.pat_loc pattern.pat_type
              | Tparam_optional_default (pattern, _, _) ->
                  let* payload = lower pattern.pat_loc pattern.pat_type in
                  let* carrier = lower pattern.pat_loc domain in
                  optional_carrier pattern.pat_loc carrier payload
            in
            collect (typ :: types)
              (formal_label parameter.fp_arg_label :: labels)
              rest remaining_domains
        | [], _ :: _ | _ :: _, [] -> assert false
      in
      collect [] [] parameters domains

let infer_labeled_type_arguments ~binders ~formal_types ~formal_labels
    ~actual_types ~actual_labels ~formal_result ~actual_result =
  if formal_labels <> actual_labels then
    Error "generic call labels differ from typedtree formal order"
  else
    infer_type_arguments ~binders ~formals:formal_types ~actuals:actual_types
      ~formal_result ~actual_result

let infer_labeled_type_arguments_from_actuals ~binders ~formal_types
    ~formal_labels ~actual_types ~actual_labels =
  if formal_labels <> actual_labels then
    Error "generic call labels differ from typedtree formal order"
  else
    infer_type_arguments_from_actuals ~binders ~formals:formal_types
      ~actuals:actual_types

let infer_labeled_type_arguments_from_logical_actuals ~binders ~formal_types
    ~formal_labels ~actual_types ~actual_labels =
  if formal_labels <> actual_labels then
    Error "generic call labels differ from typedtree formal order"
  else
    infer_type_arguments_from_logical_actuals ~binders ~formals:formal_types
      ~actuals:actual_types

let infer_labeled_type_arguments_for_logical_call ~binders ~formal_types
    ~formal_labels ~actual_types ~actual_labels ~formal_result ~actual_result =
  if formal_labels <> actual_labels then
    Error "generic call labels differ from typedtree formal order"
  else
    infer_type_arguments_for_logical_call ~binders ~formals:formal_types
      ~actuals:actual_types ~formal_result ~actual_result

type call_argument = {
  argument_label : string option;
  argument_type : Parametric_type.t;
}

type call_error = {
  argument_index : int option;
  message : string;
}

let validate_direct_call ~logical:_ ~binders ~type_arguments
    ~formal_result ~actual_result ~formals ~actuals =
  let fail ?argument_index message = Error { argument_index; message } in
  let instantiate typ =
    match instantiate ~binders ~arguments:type_arguments typ with
    | Ok typ -> Ok typ
    | Error message -> fail message
  in
  let* result = instantiate formal_result in
  let compatible declared actual = Parametric_type.equal declared actual in
  if not (compatible result actual_result) then
    fail "call result type differs from its instantiated target"
  else if List.length formals <> List.length actuals then
    fail "call argument count differs from its target"
  else
    let rec validate index formals actuals =
      match (formals, actuals) with
      | [], [] -> Ok ()
      | formal :: formals, actual :: actuals ->
          let* expected = instantiate formal.argument_type in
          if formal.argument_label <> actual.argument_label then
            fail ~argument_index:index
              "call argument label differs from its parameter"
          else if not (compatible expected actual.argument_type) then
            fail ~argument_index:index
              "call argument type differs from its instantiated parameter"
          else validate (index + 1) formals actuals
      | [], _ :: _ | _ :: _, [] -> assert false
    in
    validate 0 formals actuals

type optional_shape =
  | Absent
  | Present of Parametric_type.t
  | Forward of Parametric_type.t

let validate_sst_optional ~descriptors (expression : Sst.expression) =
  let shape, child =
    match expression.expression_desc with
    | Sst.Optional_absent -> Absent, None
    | Sst.Optional_present payload -> Present payload.typ, Some payload
    | Sst.Optional_forward carrier -> Forward carrier.typ, Some carrier
    | _ -> invalid_arg "validate_sst_optional requires an optional expression"
  in
  match Parametric_adt.option_instance descriptors expression.typ with
  | None -> Error "optional expression lacks the pinned compiler descriptor"
  | Some instance -> (
      match (shape, child) with
      | Absent, None -> Ok None
      | Present payload, Some child
        when Parametric_type.equal payload instance.option_payload_type ->
          Ok (Some child)
      | Forward forwarded, Some child
        when Parametric_type.equal forwarded expression.typ ->
          Ok (Some child)
      | (Absent | Present _ | Forward _), (None | Some _) ->
          Error "optional descriptor payload or forwarding type mismatch")

let validate_sst_direct_call ~logical ~definition ~type_arguments
    ~actual_result ~call_span ~arguments =
  let convert label typ = { argument_label = label; argument_type = typ } in
  let formals =
    List.filter_map
      (function
        | Sst.Value_parameter parameter ->
            Some (convert parameter.label parameter.pattern.typ)
        | Sst.Callback_parameter _ -> None)
      definition.Sst.parameters
  in
  let actuals =
    List.map
      (fun (label, (argument : Sst.expression)) -> convert label argument.typ)
      arguments
  in
  match
    validate_direct_call ~logical ~binders:definition.type_binders ~type_arguments
      ~formal_result:definition.result_type ~actual_result ~formals ~actuals
  with
  | Ok () -> Ok ()
  | Error error ->
      let span =
        match error.argument_index with
        | Some index -> (
            match List.nth_opt arguments index with
            | Some (_, argument) -> argument.Sst.span
            | None -> call_span)
        | None -> call_span
      in
      Error (span, error.message)
