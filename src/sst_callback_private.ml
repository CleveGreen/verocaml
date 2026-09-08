let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let same_binding (left : Sst.callback_binding)
    (right : Sst.callback_binding) =
  left.Sst.callback_id = right.callback_id
  && String.equal left.callback_name right.callback_name
  && Callback_shape_private.same_identity left.callback_shape
       right.callback_shape
  && Callback_certificate_private.same_identity left.callback_certificate
       right.callback_certificate
  && left.callback_span = right.callback_span

let known_binding known candidate =
  List.exists (fun binding -> same_binding binding candidate) known

let validate_application ~known application ~result_type =
  let callback = application.Sst.callback in
  if not (known_binding known callback) then
    Error "callback application does not use an exact in-scope binding"
  else
    match
      Callback_shape_private.validate_saturated callback.callback_shape
        ~labels:(List.map fst application.arguments)
        ~arguments:
          (List.map
             (fun (_, (argument : Sst.expression)) -> argument.typ)
             application.arguments)
        ~result:result_type
    with
    | Error message -> Error message
    | Ok () ->
        Callback_certificate_private.authenticate_shape
          callback.callback_certificate callback.callback_shape

let split_direct_arguments ~substitutions formals arguments =
  let rec split values formals arguments =
    match formals, arguments with
    | [], [] -> Ok (List.rev values)
    | ( Sst.Value_parameter formal :: formals,
        Sst.Value_argument { label; value } :: arguments ) ->
        if formal.label = label then
          split ((formal, (label, value)) :: values) formals arguments
        else Error "value argument label differs from its compiler formal"
    | ( Sst.Callback_parameter formal :: formals,
        Sst.Callback_argument { label; callback } :: arguments ) ->
        if formal.label <> label then
          Error "callback argument label differs from its compiler formal"
        else if
          not
            (Callback_shape_private.equal
               (Callback_shape_private.instantiate substitutions
                  formal.binding.callback_shape)
               callback.callback_shape)
        then Error "callback actual shape differs from its compiler formal"
        else
          let* () =
            Callback_certificate_private.authenticate_shape
              callback.callback_certificate callback.callback_shape
          in
          split values formals arguments
    | (Sst.Value_parameter _ | Sst.Callback_parameter _) :: _,
      (Sst.Value_argument _ | Sst.Callback_argument _) :: _ ->
        Error "value/callback call argument kind differs from its formal"
    | [], (Sst.Value_argument _ | Sst.Callback_argument _) :: _
    | (Sst.Value_parameter _ | Sst.Callback_parameter _) :: _, [] ->
        Error "direct call arity differs from its compiler formals"
  in
  split [] formals arguments

let value_parameters =
  List.filter_map (function
    | Sst.Value_parameter parameter -> Some parameter
    | Sst.Callback_parameter _ -> None)

let has_callback_parameters =
  List.exists (function
    | Sst.Callback_parameter _ -> true
    | Sst.Value_parameter _ -> false)

let has_callback_arguments =
  List.exists (function
    | Sst.Callback_argument _ -> true
    | Sst.Value_argument _ -> false)

let validate_expression ~known ~stage expression =
  match expression.Sst.expression_desc with
  | Sst.Callback_call application ->
      let* () =
        if stage = Sst.Runtime then Ok ()
        else Error "callback calls are executable expressions"
      in
      let* () =
        validate_application ~known application ~result_type:expression.typ
      in
      Ok (List.map snd application.arguments)
  | Sst.Callback_requires application ->
      let* () =
        if stage = Sst.Logical && expression.typ = Sst.Bool then Ok ()
        else
          Error
            "call_requires is Boolean and available only in logical expressions"
      in
      let* () =
        validate_application ~known application
          ~result_type:
            (Callback_shape_private.result
               application.callback.callback_shape)
      in
      Ok (List.map snd application.arguments)
  | Sst.Callback_ensures { application; result } ->
      let callback_result =
        Callback_shape_private.result application.callback.callback_shape
      in
      let* () =
        if
          stage = Sst.Logical && expression.typ = Sst.Bool
          && result.typ = callback_result
        then Ok ()
        else Error "call_ensures has an invalid stage or result type"
      in
      let* () =
        validate_application ~known application ~result_type:callback_result
      in
      Ok (List.map snd application.arguments @ [ result ])
  | _ -> Error "callback validation received an ordinary expression"

let relation suffix binding =
  Printf.sprintf "vero_callback_%s_%s" suffix
    (Callback_certificate_private.relation_identity
       binding.Sst.callback_certificate)

let requires_relation = relation "requires"
let ensures_relation = relation "ensures"
let precondition_name ~span callback call_span =
  Printf.sprintf "callback-pre:%s#%d:%s" callback.Sst.callback_name
    callback.callback_id (span call_span)

let expression_children expression =
  match expression.Sst.expression_desc with
  | Sst.Tuple_value values -> List.map snd values
  | Sst.Record_value { fields; _ } -> List.map snd fields
  | Sst.Constructor_value { arguments; _ } -> arguments
  | Sst.Field_read { record; _ } -> [ record ]
  | Sst.Field_write { value; _ }
  | Sst.Shared_scalar_field_write { value; _ }
  | Sst.Mutable_write { value; _ }
  | Sst.Owned_tree_nested_write { value; _ } ->
      [ value ]
  | Sst.Let_mutable (_, initial, body) -> [ initial; body ]
  | Sst.Let (bindings, body) -> List.map snd bindings @ [ body ]
  | Sst.Sequence (left, right)
  | Sst.Compare (_, left, right)
  | Sst.Bv_binary (_, left, right)
  | Sst.Bv_compare (_, left, right)
  | Sst.Boolean_binary (_, left, right) ->
      [ left; right ]
  | Sst.If (condition, left, right) ->
      condition :: left :: Option.to_list right
  | Sst.Match (scrutinee, cases) ->
      scrutinee
      :: List.concat_map
           (fun case ->
             Option.to_list case.Sst.case_guard @ [ case.case_body ])
           cases
  | Sst.Lift_runtime_int operand | Sst.Bv_to_int_unsigned operand
  | Sst.Bv_to_int_signed operand | Sst.Bv_not operand -> [ operand ]
  | Sst.Bv_int_to_bv_mod { input; _ } -> [ input ]
  | Sst.Checked_arithmetic (_, operands) -> operands
  | Sst.Boolean_not operand | Sst.Old operand -> [ operand ]
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
  | Sst.Use_type_invariant { value; _ } -> [ value ]
  | Sst.Local_assert { predicate; _ } -> [ predicate ]
  | Sst.Proof_region body -> [ body ]
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      [ quantifier.quantifier_body ]
  | Sst.Optional_present payload | Sst.Optional_forward payload -> [ payload ]
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Bv_literal _
  | Sst.Variable _ | Sst.Mutable_read _ | Sst.Owned_tree_rebase _
  | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Optional_absent
  | Sst.Logical_constant_reference _ ->
      []

let definition_roots definition =
  let predicates =
    List.map
      (fun (clause : Sst.predicate_clause) ->
        clause.Sst.predicate.expression)
      definition.Sst.contracts.requires
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
  in
  let body =
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
  in
  predicates @ body

let callback_bindings roots =
  let rec collect callbacks expression =
    let callbacks =
      match expression.Sst.expression_desc with
      | Sst.Direct_call { arguments; _ } ->
          List.fold_left
            (fun callbacks -> function
              | Sst.Value_argument _ -> callbacks
              | Sst.Callback_argument { callback; _ } ->
                  callback :: callbacks)
            callbacks arguments
      | Sst.Callback_call application
      | Sst.Callback_requires application ->
          application.callback :: callbacks
      | Sst.Callback_ensures { application; _ } ->
          application.callback :: callbacks
      | Sst.Symbolic_application _ | Sst.Logical_constant_reference _ ->
          callbacks
      | Sst.Forall _ | Sst.Exists _
      | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Bv_literal _ | Sst.Bv_int_to_bv_mod _
      | Sst.Bv_to_int_unsigned _ | Sst.Bv_to_int_signed _ | Sst.Bv_not _
      | Sst.Bv_binary _ | Sst.Bv_compare _
      | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
      | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
      | Sst.Shared_scalar_field_write _ | Sst.Mutable_read _
      | Sst.Mutable_write _ | Sst.Owned_tree_nested_write _
      | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Let _
      | Sst.Sequence _ | Sst.Compare _ | Sst.Boolean_binary _
      | Sst.Boolean_not _ | Sst.If _ | Sst.Match _
      | Sst.Lift_runtime_int _ | Sst.Checked_arithmetic _
      | Sst.Use_type_invariant _
      | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Reveal _
      | Sst.Reveal_with_fuel _ | Sst.Old _ | Sst.Optional_absent
      | Sst.Optional_present _ | Sst.Optional_forward _ ->
          callbacks
    in
    List.fold_left collect callbacks (expression_children expression)
  in
  List.fold_left collect [] roots

let bindings_in_definition definition =
  List.filter_map
    (function
      | Sst.Callback_parameter formal -> Some formal.binding
      | Sst.Value_parameter _ -> None)
    definition.Sst.parameters
  @ callback_bindings (definition_roots definition)

let has_callbacks program =
  List.exists
    (fun definition -> callback_bindings (definition_roots definition) <> [])
    program.Sst.functions

let caller_identity definition =
  Callback_certificate_private.caller_identity
    ~function_index:definition.Sst.function_id.function_index
    ~function_name:definition.Sst.function_id.function_name

let call_edge_identity caller span =
  Callback_certificate_private.edge_identity ~caller
    ~start_line:span.Diagnostic.start_pos.line
    ~start_column:span.start_pos.column ~end_line:span.end_pos.line
    ~end_column:span.end_pos.column

let authenticate_binding ~compilation_identity ~session ~definition ~span
    callback =
  let caller_identity = caller_identity definition in
  let caller_identity, call_edge_identity =
    match Callback_certificate_private.kind callback.Sst.callback_certificate with
    | Callback_certificate_private.Formal -> (None, None)
    | Callback_certificate_private.Top_level | Callback_certificate_private.Local ->
        (Some caller_identity, Some (call_edge_identity caller_identity span))
  in
  let certificate = callback.callback_certificate in
  let* () =
    Callback_certificate_private.bind_session certificate ~compilation_identity
      ~session
  in
  Callback_certificate_private.authenticate certificate
    ~shape:callback.callback_shape ~compilation_identity ~session
    ~caller_identity ~call_edge_identity

let authenticate_program ~compilation_identity ~session program =
  let rec authenticate_expression definition expression =
    let* () =
      match expression.Sst.expression_desc with
      | Sst.Direct_call { arguments; _ } ->
          List.fold_left
            (fun result -> function
              | Sst.Value_argument _ -> result
              | Sst.Callback_argument { callback; _ } ->
                  let* () = result in
                  authenticate_binding ~compilation_identity ~session ~definition
                    ~span:expression.span callback)
            (Ok ()) arguments
      | Sst.Callback_call application | Sst.Callback_requires application ->
          authenticate_binding ~compilation_identity ~session ~definition
            ~span:expression.span application.callback
      | Sst.Callback_ensures { application; _ } ->
          authenticate_binding ~compilation_identity ~session ~definition
            ~span:expression.span application.callback
      | Sst.Symbolic_application _ | Sst.Logical_constant_reference _ ->
          Ok ()
      | Sst.Forall _ | Sst.Exists _
      | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Bv_literal _ | Sst.Bv_int_to_bv_mod _
      | Sst.Bv_to_int_unsigned _ | Sst.Bv_to_int_signed _ | Sst.Bv_not _
      | Sst.Bv_binary _ | Sst.Bv_compare _
      | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
      | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
      | Sst.Shared_scalar_field_write _ | Sst.Mutable_read _
      | Sst.Mutable_write _ | Sst.Owned_tree_nested_write _
      | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Let _
      | Sst.Sequence _ | Sst.Compare _ | Sst.Boolean_binary _
      | Sst.Boolean_not _ | Sst.If _ | Sst.Match _
      | Sst.Lift_runtime_int _ | Sst.Checked_arithmetic _
      | Sst.Use_type_invariant _
      | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Reveal _
      | Sst.Reveal_with_fuel _ | Sst.Old _ | Sst.Optional_absent
      | Sst.Optional_present _ | Sst.Optional_forward _ ->
          Ok ()
    in
    List.fold_left
      (fun result child ->
        let* () = result in
        authenticate_expression definition child)
      (Ok ()) (expression_children expression)
  in
  List.fold_left
    (fun result definition ->
      let* () = result in
      List.fold_left
        (fun result expression ->
          let* () = result in
          authenticate_expression definition expression)
        (Ok ()) (definition_roots definition))
    (Ok ()) program.Sst.functions

let authenticate_implementation ~implementation ~session program =
  if not (has_callbacks program) then Ok ()
  else
    match Callback_certificate_private.cmt_compilation_identity implementation with
    | Error message -> Error message
    | Ok compilation_identity ->
        authenticate_program ~compilation_identity ~session program

let find_binding capture roots =
  let rec find = function
    | [] -> None
    | expression :: rest -> (
        match expression.Sst.expression_desc with
        | Sst.Variable { binding; _ }
          when
            binding.id = capture.Callback_certificate_private.binding_id
            && binding.typ = capture.typ ->
            Some binding
        | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
        | Sst.Bv_literal _ | Sst.Bv_int_to_bv_mod _
        | Sst.Bv_to_int_unsigned _ | Sst.Bv_to_int_signed _ | Sst.Bv_not _
        | Sst.Bv_binary _ | Sst.Bv_compare _
        | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
        | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
        | Sst.Shared_scalar_field_write _ | Sst.Mutable_read _
        | Sst.Mutable_write _ | Sst.Owned_tree_nested_write _
        | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Let _
        | Sst.Sequence _ | Sst.Compare _ | Sst.Boolean_binary _
        | Sst.Boolean_not _ | Sst.If _ | Sst.Match _
        | Sst.Lift_runtime_int _ | Sst.Checked_arithmetic _
        | Sst.Direct_call _
        | Sst.Callback_call _ | Sst.Callback_requires _
        | Sst.Callback_ensures _ | Sst.Use_type_invariant _
        | Sst.Forall _ | Sst.Exists _ | Sst.Symbolic_application _
        | Sst.Logical_constant_reference _
        | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Reveal _
        | Sst.Reveal_with_fuel _ | Sst.Old _ | Sst.Optional_absent
        | Sst.Optional_present _ | Sst.Optional_forward _ ->
            find (expression_children expression @ rest))
  in
  find roots

let authenticated_captures program definition =
  let all_callbacks =
    List.concat_map
      (fun candidate -> callback_bindings (definition_roots candidate))
      program.Sst.functions
  in
  let matching =
    List.filter
      (fun callback ->
        callback.Sst.callback_id
        = definition.Sst.function_id.function_index
        &&
        match
          Callback_certificate_private.kind callback.callback_certificate
        with
        | Callback_certificate_private.Local -> true
        | Callback_certificate_private.Formal
        | Callback_certificate_private.Top_level ->
            false)
      all_callbacks
  in
  match matching with
  | [] -> Ok []
  | callback :: rest ->
      let certificate = callback.Sst.callback_certificate in
      let captures = Callback_certificate_private.captures certificate in
      let* () =
        Callback_certificate_private.authenticate_shape certificate
          callback.callback_shape
        |> Result.map_error (fun message -> (callback.callback_span, message))
      in
      let* () =
        List.fold_left
          (fun result candidate ->
            let* () = result in
            let candidate_certificate =
              candidate.Sst.callback_certificate
            in
            if
              Callback_certificate_private.same_origin certificate
                candidate_certificate
              && Callback_shape_private.same_identity callback.callback_shape
                   candidate.callback_shape
              && Callback_certificate_private.captures candidate_certificate
                 = captures
            then Ok ()
            else
              Error
                ( candidate.callback_span,
                  "local callback capture authority is inconsistent" ))
          (Ok ()) rest
      in
      let roots = definition_roots definition in
      List.fold_left
        (fun result capture ->
          let* resolved = result in
          match find_binding capture roots with
          | Some binding -> Ok ((capture, binding) :: resolved)
          | None ->
              Error
                ( definition.span,
                  "authenticated callback capture has no exact local binding"
                ))
        (Ok []) captures
      |> Result.map List.rev

let authenticated_local_binding program definition =
  let matching =
    List.concat_map
      (fun candidate -> callback_bindings (definition_roots candidate))
      program.Sst.functions
    |> List.filter (fun callback ->
           callback.Sst.callback_id
           = definition.Sst.function_id.function_index
           &&
           match
             Callback_certificate_private.kind
               callback.callback_certificate
           with
           | Callback_certificate_private.Local -> true
           | Callback_certificate_private.Formal
           | Callback_certificate_private.Top_level ->
               false)
  in
  match matching with
  | [] -> Ok None
  | callback :: rest ->
      let certificate = callback.Sst.callback_certificate in
      let captures = Callback_certificate_private.captures certificate in
      let rec validate = function
        | [] -> Ok (Some callback)
        | candidate :: rest ->
            let candidate_certificate =
              candidate.Sst.callback_certificate
            in
            if
              Callback_certificate_private.same_origin certificate
                candidate_certificate
              && Callback_shape_private.same_identity callback.callback_shape
                   candidate.callback_shape
              && Callback_certificate_private.captures candidate_certificate
                 = captures
            then validate rest
            else
              Error
                ( candidate.callback_span,
                  "local callback authority is inconsistent" )
      in
      (match
         Callback_certificate_private.authenticate_shape certificate
           callback.callback_shape
       with
      | Error message -> Error (callback.callback_span, message)
      | Ok () -> validate rest)

let local_scope_type_binders program definition =
  let* binding = authenticated_local_binding program definition in
  match binding with
  | None -> Ok None
  | Some binding ->
      let owners =
        List.filter
          (fun candidate ->
            candidate.Sst.function_id <> definition.Sst.function_id
            && List.exists
                 (fun callback ->
                   callback.Sst.callback_id = binding.callback_id
                   && Callback_certificate_private.same_origin
                        callback.callback_certificate
                        binding.callback_certificate)
                 (callback_bindings (definition_roots candidate)))
          program.Sst.functions
      in
      let owner_ids =
        List.map (fun owner -> owner.Sst.function_id) owners
        |> List.sort_uniq compare
      in
      (match owner_ids with
      | [ owner_id ] ->
          let owner =
            List.find
              (fun candidate -> candidate.Sst.function_id = owner_id)
              owners
          in
          Ok (Some owner.Sst.type_binders)
      | [] ->
          Error
            ( binding.callback_span,
              "local callback has no exact lexical owner" )
      | _ :: _ :: _ ->
          Error
            ( binding.callback_span,
              "local callback crosses lexical function owners" ))

type local_callable_instance = {
  definition : Sst.function_definition;
  source_paths : Path.t list;
  binding_uid : string;
  relation_identity : string;
}

let position_compare left right =
  match Int.compare left.Diagnostic.line right.Diagnostic.line with
  | 0 -> Int.compare left.column right.column
  | compared -> compared

let contains outer inner =
  String.equal outer.Diagnostic.file inner.Diagnostic.file
  && position_compare outer.start_pos inner.start_pos <= 0
  && position_compare inner.end_pos outer.end_pos <= 0

let local_callable_instances program ~owners =
  let owner_ids = List.map (fun (id, _, _) -> id) owners in
  let rec collect instances = function
    | [] -> Ok (List.rev instances)
    | definition :: rest ->
        if List.mem definition.Sst.function_id owner_ids then
          collect instances rest
        else
          match authenticated_local_binding program definition with
          | Error (_, message) -> Error message
          | Ok None -> collect instances rest
          | Ok (Some callback) -> (
              match
                List.find_opt
                  (fun (_, span, _) -> contains span callback.callback_span)
                  owners
              with
              | None | Some (_, _, []) ->
                  Error
                    "local callback callable identity has no compiler owner path"
              | Some (_, _, source_paths) ->
                  let certificate = callback.callback_certificate in
                  collect
                    ({
                       definition;
                       source_paths;
                       binding_uid =
                         Callback_certificate_private.callable_identity
                           certificate;
                       relation_identity =
                         Callback_certificate_private.relation_identity
                           certificate;
                     }
                    :: instances)
                    rest)
  in
  collect [] program.Sst.functions

type authenticated_spec_carrier = {
  definition_body : Typedtree.expression;
  witness_location : Location.t;
  recursive_visibility : [ `Opaque | `Revealed ] option;
}

type top_function_kind =
  | Top_exec
  | Top_spec of authenticated_spec_carrier
  | Top_logical_constant of Typedtree_logical_constant_private.declaration
  | Top_type_invariant of authenticated_spec_carrier
  | Top_recursive_spec of authenticated_spec_carrier
  | Top_proof of authenticated_spec_carrier
  | Top_external_specification of authenticated_spec_carrier
  | Top_external_body of Sst.verification_mode * authenticated_spec_carrier

type planned_builtin_assertion = {
  builtin_source_expression : Typedtree.expression;
  builtin_predicate_expression : Typedtree.expression;
  builtin_keyword_location : Location.t;
  builtin_source_location : Location.t;
  builtin_predicate_location : Location.t;
  builtin_assertion_ordinal : int;
}

type top_function = {
  ident : Ident.t;
  resolved_paths : Path.t list;
  function_id : Sst.function_id;
  value_binding : Typedtree.value_binding;
  rec_flag : Asttypes.rec_flag;
  function_kind : top_function_kind;
  type_substitutions : (int * Types.type_expr) list;
    parametric_type_binders : (int * Parametric_type.binder) list;
  proof_capture_hints : (Location.t * Types.type_expr) list;
  builtin_assertions : planned_builtin_assertion list;
  mutable semantic_parameters : (Sst.typ * string option) list option;
  mutable semantic_result_type : Sst.typ option;
}

let source_definition_body function_ =
  match function_.function_kind with
  | Top_spec carrier | Top_recursive_spec carrier | Top_proof carrier ->
      Some carrier.definition_body
  | Top_logical_constant declaration ->
      Some (Typedtree_logical_constant_private.body declaration)
  | Top_external_body (Sst.Proof, carrier) -> Some carrier.definition_body
  | Top_exec | Top_type_invariant _ | Top_external_specification _
  | Top_external_body (Sst.Exec, _)
  | Top_external_body (Sst.Spec, _) -> (
      match function_.value_binding.vb_expr.exp_desc with
      | Typedtree.Texp_function
          { body = Typedtree.Tfunction_body body; _ } ->
          Some body
      | _ -> None)
