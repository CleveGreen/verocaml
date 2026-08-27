let logical_context mode ~ghost_depth =
  ghost_depth > 0
  ||
  match mode with
  | Some (Sst.Spec | Sst.Proof) -> true
  | Some Sst.Exec | None -> false

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let source_arrow = Spec_function_type_private.lower_source_arrow

let captures ~bindings ~parameter body =
  let found = ref [] in
  let add ident =
    if not (Option.fold ~none:false ~some:(Ident.same ident) parameter) then
      match
        List.find_opt
          (fun (candidate, _) -> Ident.same ident candidate)
          bindings
      with
      | Some (_, binding)
        when not
               (List.exists
                  (fun candidate -> candidate.Sst.id = binding.Sst.id)
                  !found) ->
          found := binding :: !found
      | Some _ | None -> ()
  in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.Typedtree.exp_desc with
          | Texp_ident (Path.Pident ident, _, _, _, _) -> add ident
          | Texp_mutvar ident -> add ident.txt
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.expr iterator body;
  List.sort (fun left right -> Int.compare left.Sst.id right.Sst.id) !found

let captures_live_binding ?(allow = fun _ -> false) bindings expression =
  let captured = ref false in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.Typedtree.exp_desc with
          | Texp_ident (Path.Pident ident, _, _, _, _) -> (
              match
                List.find_opt
                  (fun (candidate, _) -> Ident.same ident candidate)
                  bindings
              with
              | Some (_, binding) when not (allow binding) -> captured := true
              | Some _ | None -> ())
          | Texp_mutvar ident ->
              if
                List.exists
                  (fun (candidate, _) -> Ident.same ident.txt candidate)
                  bindings
              then captured := true
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.expr iterator expression;
  !captured

let function_bindings =
  List.filter (fun (_, (binding : Sst.binding)) ->
      Parametric_type.is_spec_function binding.typ)

let lambda_site ~source_file location typ =
  let start = location.Location.loc_start.Lexing.pos_cnum
  and finish = location.Location.loc_end.Lexing.pos_cnum in
  String.concat ":"
    [
      source_file;
      string_of_int start;
      string_of_int finish;
      Parametric_type.structural_identity_digest typ;
    ]

let residual_type arrow arguments =
  let rec apply arrow = function
    | [] -> Some arrow
    | (_, Typedtree.Arg _) :: rest ->
        Option.bind (Spec_function_type_private.classify arrow) (fun view ->
            apply view.range rest)
    | (_, Typedtree.Omitted _) :: _ -> None
  in
  apply arrow arguments

let rec admissible_capture_type ~aggregate = function
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _ -> true
  | typ when Parametric_type.is_spec_function typ -> true
  | Sst.Tuple components ->
      List.for_all
        (fun (_, typ) -> admissible_capture_type ~aggregate typ)
        components
  | Sst.Application _ -> true
  | Sst.Aggregate type_id -> aggregate type_id

type 'error lambda_services = {
  lower_pattern :
    (Ident.t * Sst.binding) list ->
    Typedtree.pattern ->
    (Sst.pattern * (Ident.t * Sst.binding) list, 'error) result;
  lower_expression :
    (Ident.t * Sst.binding) list ->
    Typedtree.expression ->
    (Sst.expression, 'error) result;
  admit_capture : Sst.binding -> bool;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Diagnostic.span;
  higher_order_error : Location.t -> 'error;
}

let lower_lambda services ~source_file ~bindings ~arrow ~location ~parameters
    ~body =
  let ( let* ) result continuation =
    match result with
    | Ok value -> continuation value
    | Error _ as error -> error
  in
  let rec stages index bindings arrow = function
    | [] -> services.lower_expression bindings body
    | parameter :: rest -> (
        match
          ( parameter.Typedtree.fp_partial,
            parameter.fp_kind,
            Spec_function_type_private.classify arrow )
        with
        | Typedtree.Total, Tparam_pat pattern, Some view -> (
            let label = services.parameter_label parameter.fp_arg_label in
            if not (Option.equal String.equal label view.label) then
              Error (services.higher_order_error parameter.fp_loc)
            else
              let* lowered_pattern, nested_bindings =
                services.lower_pattern bindings pattern
              in
              let source_parameter =
                match pattern.pat_desc with
                | Tpat_var (ident, _, _, _, _) -> Some ident
                | _ -> None
              in
              match (source_parameter, lowered_pattern.Sst.pattern_desc) with
              | Some _, Sst.Bind _ | None, Sst.Wildcard ->
                  let lambda_parameter =
                    match lowered_pattern.pattern_desc with
                    | Sst.Bind binding -> binding
                    | Sst.Wildcard ->
                        {
                          Sst.id =
                            -1
                            - abs
                                (Hashtbl.hash
                                   ( source_file,
                                     location.Location.loc_start.pos_cnum,
                                     index ));
                          name = "$spec_ignored";
                          typ = view.domain;
                          uniqueness = Sst.Definitely_aliased;
                          span = services.span pattern.pat_loc;
                        }
                    | _ -> assert false
                  in
                  let* lambda_body =
                    stages (index + 1) nested_bindings view.range rest
                  in
                  let lambda_captures =
                    captures ~bindings ~parameter:source_parameter body
                  in
                  if
                    List.exists
                      (fun binding -> not (services.admit_capture binding))
                      lambda_captures
                  then Error (services.higher_order_error location)
                  else
                    Spec_function_sst_private.make_lambda
                      {
                        Spec_function_sst_private.lambda_site =
                          lambda_site ~source_file location arrow
                          ^ ":" ^ string_of_int index;
                        lambda_arrow = arrow;
                        lambda_parameter;
                        lambda_body;
                        lambda_captures;
                      }
                    |> Result.map_error (fun _ ->
                        services.higher_order_error location)
              | _ -> Error (services.higher_order_error parameter.fp_loc))
        | Typedtree.Partial, _, _
        | Typedtree.Total, Tparam_optional_default _, _
        | Typedtree.Total, Tparam_pat _, None ->
            Error (services.higher_order_error parameter.fp_loc))
  in
  stages 0 bindings arrow parameters

type 'error application_services = {
  normalize : Typedtree.expression -> (Sst.typ, 'error) result;
  lower : Typedtree.expression -> (Sst.expression, 'error) result;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Diagnostic.span;
  higher_order_error : Location.t -> 'error;
}

let lower_application services ~enabled ~(application : Typedtree.expression)
    ~result_type ~(callee : Typedtree.expression)
    ~(arguments : (Typedtree.arg_label * Typedtree.apply_arg) list) =
  if not enabled then None
  else
    match services.normalize callee with
    | Ok arrow when Parametric_type.is_spec_function arrow ->
        let result =
          let ( let* ) result continuation =
            match result with
            | Ok value -> continuation value
            | Error _ as error -> error
          in
          let* function_ = services.lower callee in
          let rec apply arrow function_ = function
            | [] ->
                if Parametric_type.equal function_.Sst.typ result_type then
                  Ok function_
                else Error (services.higher_order_error application.exp_loc)
            | ( source_label,
                Typedtree.Arg ((argument : Typedtree.expression), _) )
              :: rest -> (
                match Spec_function_type_private.classify arrow with
                | None ->
                    Error (services.higher_order_error application.exp_loc)
                | Some view ->
                    let label = services.parameter_label source_label in
                    if not (Option.equal String.equal label view.label) then
                      Error (services.higher_order_error argument.exp_loc)
                    else
                      let* argument = services.lower argument in
                      let* applied =
                        Spec_function_sst_private.make_application ~arrow
                          ~function_ ~argument ~label
                          ~span:(services.span application.exp_loc)
                        |> Result.map_error (fun _ ->
                            services.higher_order_error application.exp_loc)
                      in
                      apply view.range applied rest)
            | (_, Typedtree.Omitted _) :: _ ->
                Error (services.higher_order_error application.exp_loc)
          in
          apply arrow function_ arguments
        in
        Some result
    | Ok _ | Error _ -> None

let lower_symbolic_application ~invalid ~span ~declaration ~result_type
    ~arguments =
  let labels = List.map fst arguments in
  let actual_types =
    List.map (fun (_, (argument : Sst.expression)) -> argument.typ) arguments
  in
  let formal_types = Symbolic_application_private.parameter_types declaration in
  let* type_arguments =
    Parametric_lowering_private.infer_labeled_type_arguments
      ~binders:(Symbolic_application_private.type_binders declaration)
      ~formal_types
      ~formal_labels:(List.map (fun _ -> None) formal_types)
      ~actual_types ~actual_labels:labels
      ~formal_result:
        (Symbolic_application_private.declaration_result_type declaration)
      ~actual_result:result_type
    |> Result.map_error invalid
  in
  let* application =
    Symbolic_application_private.create declaration ~type_arguments
      ~arguments:(List.map snd arguments) ~argument_types:actual_types
      ~result_type ~span
    |> Result.map_error invalid
  in
  Ok
    {
      Sst.expression_desc = Sst.Symbolic_application application;
      typ = result_type;
      span;
    }

let symbolic_result_type declaration ~actual_types ~actual_labels =
  let binders = Symbolic_application_private.type_binders declaration in
  let* type_arguments =
    Parametric_lowering_private.infer_labeled_type_arguments_from_actuals
      ~binders
      ~formal_types:(Symbolic_application_private.parameter_types declaration)
      ~formal_labels:
        (List.map
           (fun _ -> None)
           (Symbolic_application_private.parameter_types declaration))
      ~actual_types ~actual_labels
  in
  Parametric_lowering_private.instantiate ~binders ~arguments:type_arguments
    (Symbolic_application_private.declaration_result_type declaration)

type 'error expression_type_services = {
  fallback : Typedtree.expression -> (Sst.typ, 'error) result;
  binding : Ident.t -> Sst.typ option;
  symbolic :
    Typedtree.expression ->
    (Typedtree.arg_label * Typedtree.apply_arg) list ->
    (Symbolic_application_private.declaration
    * (Typedtree.arg_label * Typedtree.apply_arg) list)
    option
    option;
  parameter_label : Typedtree.arg_label -> string option;
  polymorphic_error : Typedtree.expression -> 'error;
  higher_order_error : Typedtree.expression -> 'error;
}

let rec expression_type services expression =
  match expression.Typedtree.exp_desc with
  | Texp_apply (callee, arguments, _, _, _) -> (
      match services.symbolic callee arguments with
      | Some (Some (declaration, arguments)) ->
          let rec actuals types labels = function
            | [] ->
                symbolic_result_type declaration ~actual_types:(List.rev types)
                  ~actual_labels:(List.rev labels)
                |> Result.map_error (fun _ ->
                    services.polymorphic_error expression)
            | (label, Typedtree.Arg (argument, _)) :: rest ->
                let* typ = expression_type services argument in
                actuals (typ :: types)
                  (services.parameter_label label :: labels)
                  rest
            | (_, Typedtree.Omitted _) :: _ ->
                Error (services.higher_order_error expression)
          in
          actuals [] [] arguments
      | Some None -> services.fallback expression
      | None -> (
          match expression_type services callee with
          | Ok arrow when Parametric_type.is_spec_function arrow -> (
              match residual_type arrow arguments with
              | Some typ -> Ok typ
              | None -> services.fallback expression)
          | Ok _ | Error _ -> services.fallback expression))
  | Texp_ident (Path.Pident ident, _, _, _, _) ->
      Option.fold
        ~none:(services.fallback expression)
        ~some:(fun typ -> Ok typ)
        (services.binding ident)
  | Texp_mutvar ident ->
      Option.fold
        ~none:(services.fallback expression)
        ~some:(fun typ -> Ok typ)
        (services.binding ident.txt)
  | _ -> services.fallback expression

type call = {
  function_id : Sst.function_id;
  type_binders : Parametric_type.binder list;
  formal_types : Sst.typ list;
  formal_labels : string option list;
  formal_result : Sst.typ;
  recursive : bool;
}

let direct_call ~invalid ~span ~result_type ~actuals call =
  let actual_types =
    List.map (fun (_, (actual : Sst.expression)) -> actual.typ) actuals
  in
  let* type_arguments =
    Parametric_lowering_private.infer_labeled_type_arguments
      ~binders:call.type_binders ~formal_types:call.formal_types
      ~formal_labels:call.formal_labels ~actual_types
      ~actual_labels:(List.map fst actuals) ~formal_result:call.formal_result
      ~actual_result:result_type
    |> Result.map_error invalid
  in
  Ok
    {
      Sst.expression_desc =
        Sst.Direct_call
          {
            call_form = Sst.Specification_call;
            callee = call.function_id;
            type_arguments;
            arguments =
              List.map
                (fun (label, value) -> Sst.Value_argument { label; value })
                actuals;
            recursive = call.recursive;
          };
      typ = result_type;
      span;
    }

let reference ~invalid ~span ~result_type ~function_id ~type_binders
    ~formal_types ~formal_labels ~recursive =
  let formal_result =
    match List.rev formal_types with
    | result :: reversed_domains ->
        List.fold_left2
          (fun range domain label ->
            Spec_function_type_private.make ~label ~domain ~range)
          result reversed_domains (List.rev formal_labels)
    | [] -> assert false
  in
  direct_call ~invalid ~span ~result_type ~actuals:[]
    {
      function_id;
      type_binders;
      formal_types = [];
      formal_labels = [];
      formal_result;
      recursive;
    }

type preclassification_services = {
  logical : bool;
  normalize : Typedtree.expression -> Path.t -> Path.t;
  symbolic : Path.t -> bool;
  resolves_to : Path.t -> string -> string -> bool;
  known : Path.t -> string -> bool;
  local : Ident.t -> bool;
  callback : Ident.t -> bool;
  uid : Types.value_description -> string;
}

let rec symbolic_application_head services callee arguments =
  match callee.Typedtree.exp_desc with
  | Texp_ident (path, source_name, description, _, _) ->
      let path = services.normalize callee path in
      if services.symbolic path then
        Some (path, source_name, description, arguments)
      else None
  | Texp_apply (inner, inner_arguments, _, _, _) ->
      symbolic_application_head services inner (inner_arguments @ arguments)
  | _ -> None

let preclassify_application services callee =
  let local_function =
    match callee.Typedtree.exp_desc with
    | Texp_ident (Path.Pident ident, _, _, _, _) -> services.local ident
    | _ -> (
        match Types.get_desc callee.exp_type with
        | Types.Tarrow _ -> true
        | _ -> false)
  in
  if services.logical && local_function then None
  else
    match symbolic_application_head services callee [] with
    | Some _ -> None
    | None -> (
        match callee.exp_desc with
        | Texp_ident (path, _, description, _, _) -> (
            let path = services.normalize callee path in
            let uid = services.uid description in
            let stdlib name = services.resolves_to path "Stdlib" name in
            let ghost name = services.resolves_to path "Vero_ghost" name in
            if
              services.symbolic path
              || List.exists stdlib
                   [
                     "+";
                     "-";
                     "~-";
                     "*";
                     "succ";
                     "pred";
                     "abs";
                     "not";
                     "&&";
                     "||";
                     "=";
                     "<>";
                     "<";
                     "<=";
                     ">";
                     ">=";
                   ]
              || List.exists ghost
                   [
                     "requires";
                     "ensures";
                     "decreases";
                     "structural_decreases";
                     "assert_";
                     "old";
                     "marker";
                     "sidecar";
                     "spec_definition";
                     "recursive_spec_definition";
                     "proof_definition";
                     "proof_region";
                     "type_invariant_definition";
                     "use_type_invariant";
                     "reveal";
                     "reveal_with_fuel";
                   ]
              || services.known path uid
              ||
              match path with
              | Path.Pident ident -> services.callback ident
              | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
            then None
            else if
              List.exists stdlib
                [ "/"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr" ]
            then Some Diagnostic.Wrapping_arithmetic
            else if List.exists stdlib [ "ref"; "!"; ":=" ] then
              Some Diagnostic.Mutation
            else if services.resolves_to path "Effect" "perform" then
              Some Diagnostic.Effect
            else if
              List.exists
                (services.resolves_to path "Domain")
                [ "spawn"; "join" ]
            then Some Diagnostic.Concurrency
            else
              match path with
              | Path.Pident ident
                when services.local ident && not (services.callback ident) ->
                  Some Diagnostic.Higher_order_call
              | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _
                ->
                  Some Diagnostic.Unknown_or_external_call)
        | _ -> Some Diagnostic.Higher_order_call)

let exact_callback_callee ~find (callee : Typedtree.expression) =
  match callee.exp_desc with
  | Texp_ident (Path.Pident ident, _, _, _, _) -> Option.is_some (find ident)
  | _ -> false

let source_signature ~lower (expression : Typedtree.expression) =
  match expression.exp_desc with
  | Texp_function { params; body = Tfunction_body body; _ } ->
      Some
        (let* formal_types, formal_labels =
           Parametric_lowering_private.lower_typedtree_formals ~lower params
         in
         let* result_type = lower body.exp_loc body.exp_type in
         Ok (formal_types, formal_labels, result_type))
  | _ -> None

let first_class_surface ~signature (expression : Typedtree.expression) =
  List.exists Spec_function_type_private.contains_source_arrow signature
  ||
  match expression.exp_desc with
  | Texp_function { params; _ } ->
      List.exists
        (fun (parameter : Typedtree.function_param) ->
          match parameter.fp_arg_label with
          | Types.Labelled _ -> true
          | Types.Nolabel | Types.Optional _ | Types.Position _ -> false)
        params
  | _ -> false

let supported_recursive_hof_surface ~recursive expression =
  let tuple_match = ref false in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.Typedtree.exp_desc with
          | Texp_match ({ exp_desc = Texp_tuple _; _ }, _, _, _) ->
              tuple_match := true
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.expr iterator expression;
  (not recursive) || not !tuple_match

let supported_first_class_surface ~recursive ~signature expression =
  first_class_surface ~signature expression
  && supported_recursive_hof_surface ~recursive expression

let classify_callee ~normalize ~candidates ~kind ~id ~arity ~first_class
    ~current ~local ~argument_count ~result_type (callee : Typedtree.expression)
    =
  let same_current candidate =
    Option.fold ~none:false
      ~some:(fun current -> current = id candidate)
      current
  in
  let local_function, named_stage, direct =
    match callee.exp_desc with
    | Texp_ident (Path.Pident ident, _, _, _, _) when local ident ->
        (true, false, None)
    | Texp_ident (path, _, _, _, _) -> (
        let candidates = candidates (normalize callee path) in
        let direct =
          List.find_opt
            (fun candidate ->
              match kind candidate with
              | `Spec | `Recursive true -> true
              | `Recursive false | `Other -> false)
            candidates
        in
        let named_stage =
          Option.fold ~none:false
            ~some:(fun candidate ->
              match kind candidate with
              | `Spec ->
                  Parametric_type.is_spec_function result_type
                  || argument_count > arity candidate
              | `Recursive _ ->
                  Parametric_type.is_spec_function result_type
                  && not (same_current candidate)
              | `Other -> false)
            direct
        in
        ( false,
          named_stage,
          if named_stage then None
          else
            match direct with
            | Some candidate when first_class candidate -> direct
            | Some _ | None -> None ))
    | _ -> (true, false, None)
  in
  (local_function || named_stage, direct)
