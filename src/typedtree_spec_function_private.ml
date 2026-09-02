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
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Parameter _ ->
      true
  | typ when Parametric_type.is_spec_function typ -> true
  | Sst.Tuple components ->
      List.for_all
        (fun (_, typ) -> admissible_capture_type ~aggregate typ)
        components
  | Sst.Application _ -> true
  | Sst.Aggregate type_id -> aggregate type_id

type 'error lambda_services = {
  pattern_type : Typedtree.pattern -> (Sst.typ, 'error) result;
  lower_pattern :
    expected:Sst.typ ->
    (Ident.t * Sst.binding) list ->
    Typedtree.pattern ->
    (Sst.pattern * (Ident.t * Sst.binding) list, 'error) result;
  lower_expression :
    (Ident.t * Sst.binding) list ->
    Typedtree.expression ->
    (Sst.expression, 'error) result;
  adapt_result :
    Location.t ->
    expected:Sst.typ ->
    Sst.expression ->
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
  let rec specialize substitutions formal actual =
    match formal with
    | Parametric_type.Parameter binder -> (
        match
          List.find_opt
            (fun (candidate, _) ->
              Parametric_type.compare_binder candidate binder = 0)
            substitutions
        with
        | None -> Ok (actual, (binder, actual) :: substitutions)
        | Some (_, previous) when Parametric_type.equal previous actual ->
            Ok (previous, substitutions)
        | Some _ -> Error (services.higher_order_error location))
    | Parametric_type.Mathematical_int
      when Parametric_type.equal actual Parametric_type.Int ->
        Ok (Parametric_type.Mathematical_int, substitutions)
    | Parametric_type.Int
      when Parametric_type.equal actual Parametric_type.Mathematical_int ->
        Ok (Parametric_type.Mathematical_int, substitutions)
    | Parametric_type.Tuple formals -> (
        match actual with
        | Parametric_type.Tuple actuals
          when List.length formals = List.length actuals ->
            let rec components substitutions specialized formals actuals =
              match (formals, actuals) with
              | [], [] ->
                  Ok (Parametric_type.Tuple (List.rev specialized), substitutions)
              | (formal_label, formal) :: formals,
                (actual_label, actual) :: actuals
                when Option.equal String.equal formal_label actual_label ->
                  let* component, substitutions =
                    specialize substitutions formal actual
                  in
                  components substitutions
                    ((formal_label, component) :: specialized)
                    formals actuals
              | _ -> Error (services.higher_order_error location)
            in
            components substitutions [] formals actuals
        | _ -> Error (services.higher_order_error location))
    | Parametric_type.Application (formal_constructor, formals) -> (
        match actual with
        | Parametric_type.Application (actual_constructor, actuals)
          when Parametric_type.compare_constructor formal_constructor
                 actual_constructor
               = 0
               && List.length formals = List.length actuals ->
            let rec arguments substitutions specialized formals actuals =
              match (formals, actuals) with
              | [], [] ->
                  Ok
                    ( Parametric_type.Application
                        (formal_constructor, List.rev specialized),
                      substitutions )
              | formal :: formals, actual :: actuals ->
                  let* argument, substitutions =
                    specialize substitutions formal actual
                  in
                  arguments substitutions (argument :: specialized) formals
                    actuals
              | _ -> Error (services.higher_order_error location)
            in
            arguments substitutions [] formals actuals
        | _ -> Error (services.higher_order_error location))
    | closed when Parametric_type.equal closed actual -> Ok (closed, substitutions)
    | Parametric_type.Unit | Parametric_type.Bool | Parametric_type.Int
    | Parametric_type.Mathematical_int | Parametric_type.Aggregate _ ->
        Error (services.higher_order_error location)
  in
  let rec stages index bindings substitutions arrow = function
    | [] ->
        let* body = services.lower_expression bindings body in
        Ok (body, substitutions)
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
              let* source_domain = services.pattern_type pattern in
              let* domain, substitutions =
                specialize substitutions view.domain source_domain
              in
              let* lowered_pattern, nested_bindings =
                services.lower_pattern ~expected:domain bindings pattern
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
                  let* lambda_body, substitutions =
                    stages (index + 1) nested_bindings substitutions view.range
                      rest
                  in
                  let* range, substitutions =
                    specialize substitutions view.range lambda_body.Sst.typ
                  in
                  let* lambda_body =
                    services.adapt_result parameter.fp_loc ~expected:range
                      lambda_body
                  in
                  let lambda_arrow =
                    Spec_function_type_private.make ~label:view.label ~domain
                      ~range
                  in
                  let lambda_captures =
                    captures ~bindings ~parameter:source_parameter body
                  in
                  if
                    List.exists
                      (fun binding -> not (services.admit_capture binding))
                      lambda_captures
                  then Error (services.higher_order_error location)
                  else (
                    [%log.trace "lowered lambda against authenticated callback type"
                      ~stage:
                        (Delator.Field.string "specification-function-lowering")
                      ~parameter_index:(Delator.Field.int index)
                      ~expected_arrow:
                        (Delator.Field.string (Parametric_type.to_string arrow))
                      ~actual_arrow:
                        (Delator.Field.string
                           (Parametric_type.to_string lambda_arrow))
                      ~capture_count:
                        (Delator.Field.int (List.length lambda_captures))
                      ~decision:(Delator.Field.string "accepted")];
                    Spec_function_sst_private.make_lambda
                      {
                        Spec_function_sst_private.lambda_site =
                          lambda_site ~source_file location lambda_arrow
                          ^ ":" ^ string_of_int index;
                        lambda_arrow;
                        lambda_parameter;
                        lambda_body;
                        lambda_captures;
                      }
                    |> Result.map (fun expression -> (expression, substitutions))
                    |> Result.map_error (fun _ ->
                        services.higher_order_error location))
              | _ -> Error (services.higher_order_error parameter.fp_loc))
        | Typedtree.Partial, _, _
        | Typedtree.Total, Tparam_optional_default _, _
        | Typedtree.Total, Tparam_pat _, None ->
            Error (services.higher_order_error parameter.fp_loc))
  in
  Result.map fst (stages 0 bindings [] arrow parameters)

type 'error application_services = {
  normalize : Typedtree.expression -> (Sst.typ, 'error) result;
  lower : Typedtree.expression -> (Sst.expression, 'error) result;
  adapt_argument :
    Location.t ->
    expected:Sst.typ ->
    Sst.expression ->
    (Sst.expression, 'error) result;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Diagnostic.span;
  higher_order_error : Location.t -> 'error;
}

let apply_lowered services ~(application : Typedtree.expression) ~result_type
    ~(function_ : Sst.expression) ~arguments =
  let ( let* ) result continuation =
    match result with
    | Ok value -> continuation value
    | Error _ as error -> error
  in
  let rec apply arrow function_ = function
    | [] ->
        if
          Parametric_type.equal function_.Sst.typ result_type
          || Parametric_type.compiler_erasure_compatible
               ~compiler:result_type ~semantic:function_.typ
        then (
          if not (Parametric_type.equal function_.typ result_type) then
            [%log.trace
              "preserved logical result of specification-function application"
              ~stage:
                (Delator.Field.string "specification-function-application")
              ~compiler_sort:
                (Delator.Field.string (Parametric_type.to_string result_type))
              ~logical_sort:
                (Delator.Field.string
                   (Parametric_type.to_string function_.typ))
              ~decision:(Delator.Field.string "accepted-logical-view")];
          Ok function_
        ) else Error (services.higher_order_error application.exp_loc)
    | (source_label, Typedtree.Arg ((argument : Typedtree.expression), _))
      :: rest -> (
        match Spec_function_type_private.classify arrow with
        | None -> Error (services.higher_order_error application.exp_loc)
        | Some view ->
            let label = services.parameter_label source_label in
            if not (Option.equal String.equal label view.label) then
              Error (services.higher_order_error argument.exp_loc)
            else
              let* lowered_argument = services.lower argument in
              let* argument =
                services.adapt_argument argument.exp_loc ~expected:view.domain
                  lowered_argument
              in
              let* applied =
                Spec_function_sst_private.make_application ~arrow ~function_
                  ~argument ~label ~span:(services.span application.exp_loc)
                |> Result.map_error (fun _ ->
                       services.higher_order_error application.exp_loc)
              in
              apply view.range applied rest)
    | (_, Typedtree.Omitted _) :: rest ->
        if
          List.for_all
            (function _, Typedtree.Omitted _ -> true | _, Typedtree.Arg _ -> false)
            rest
        then apply arrow function_ []
        else Error (services.higher_order_error application.exp_loc)
  in
  apply function_.typ function_ arguments

let lower_application services ~enabled ~(application : Typedtree.expression)
    ~result_type ~(callee : Typedtree.expression)
    ~(arguments : (Typedtree.arg_label * Typedtree.apply_arg) list) =
  if not enabled then None
  else
    match services.normalize callee with
    | Ok compiler_arrow when Parametric_type.is_spec_function compiler_arrow ->
        let result =
          let ( let* ) result continuation =
            match result with
            | Ok value -> continuation value
            | Error _ as error -> error
          in
          let* function_ = services.lower callee in
          let* arrow =
            if Parametric_type.equal compiler_arrow function_.Sst.typ then
              Ok function_.typ
            else if
              Parametric_type.is_spec_function function_.typ
              && Parametric_type.compiler_erasure_compatible
                   ~compiler:compiler_arrow ~semantic:function_.typ
            then (
              [%log.trace
                "selected authenticated semantic arrow for specification-function application"
                ~stage:
                  (Delator.Field.string "specification-function-application")
                ~arrow_view:
                  (Delator.Field.map
                     [ ( "compiler",
                         Delator.Field.string
                           (Parametric_type.to_string compiler_arrow) );
                       ( "semantic",
                         Delator.Field.string
                           (Parametric_type.to_string function_.typ) ) ])
                ~decision:(Delator.Field.string "accepted-logical-view")];
              Ok function_.typ
            ) else (
              [%log.debug
                "rejected specification-function callee semantic arrow"
                ~stage:
                  (Delator.Field.string "specification-function-application")
                ~arrow_view:
                  (Delator.Field.map
                     [ ( "compiler",
                         Delator.Field.string
                           (Parametric_type.to_string compiler_arrow) );
                       ( "semantic",
                         Delator.Field.string
                           (Parametric_type.to_string function_.typ) ) ])
                ~reason:(Delator.Field.string "incompatible-semantic-arrow")
                ~decision:(Delator.Field.string "rejected")];
              Error (services.higher_order_error application.exp_loc)
            )
          in
          if Parametric_type.equal arrow function_.typ then
            apply_lowered services ~application ~result_type ~function_
              ~arguments
          else Error (services.higher_order_error application.exp_loc)
        in
        Some result
    | Ok _ | Error _ -> None

let lower_symbolic_application ~invalid ~adapt_argument ~span ~declaration
    ~result_type ~arguments =
  let formal_types = Symbolic_application_private.parameter_types declaration in
  let* arguments =
    if List.length formal_types <> List.length arguments then
      Error (invalid "symbolic application argument arity differs")
    else
      List.fold_left2
        (fun result formal_type (label, argument) ->
          let* arguments = result in
          let* argument = adapt_argument ~expected:formal_type argument in
          Ok ((label, argument) :: arguments))
        (Ok []) formal_types arguments
      |> Result.map List.rev
  in
  let labels = List.map fst arguments in
  let actual_types =
    List.map (fun (_, (argument : Sst.expression)) -> argument.typ) arguments
  in
  let binders = Symbolic_application_private.type_binders declaration in
  let formal_labels = List.map (fun _ -> None) formal_types in
  let formal_result =
    Symbolic_application_private.declaration_result_type declaration
  in
  let* type_arguments, result_type =
    match
      Parametric_lowering_private
      .infer_labeled_type_arguments_for_logical_call
        ~binders ~formal_types ~formal_labels ~actual_types
        ~actual_labels:labels ~formal_result ~actual_result:result_type
    with
    | Ok type_arguments ->
        let* semantic_result =
          Parametric_lowering_private.instantiate ~binders
            ~arguments:type_arguments formal_result
          |> Result.map_error invalid
        in
        let* reconciled_result =
          Parametric_lowering_private.reconcile_authenticated_result ~binders:[]
            ~semantic:semantic_result ~compiler:result_type
          |> Result.map_error invalid
        in
        [%log.trace "selected symbolic type inference strategy"
          ~stage:(Delator.Field.string "symbolic-application-lowering")
          ~inference_strategy:(Delator.Field.string "semantic-arguments")
          ~decision:(Delator.Field.string "selected")];
        Ok (type_arguments, reconciled_result)
    | Error _ ->
        let* type_arguments =
          Parametric_lowering_private.infer_labeled_type_arguments ~binders
            ~formal_types ~formal_labels ~actual_types ~actual_labels:labels
            ~formal_result ~actual_result:result_type
          |> Result.map_error invalid
        in
        [%log.trace "selected symbolic type inference strategy"
          ~stage:(Delator.Field.string "symbolic-application-lowering")
          ~inference_strategy:
            (Delator.Field.string "compiler-result-fallback")
          ~decision:(Delator.Field.string "selected")];
        Ok (type_arguments, result_type)
  in
  let* instantiated_formals =
    List.fold_left
      (fun result formal ->
        let* formals = result in
        let* formal =
          Parametric_lowering_private.instantiate ~binders
            ~arguments:type_arguments formal
          |> Result.map_error invalid
        in
        Ok (formal :: formals))
      (Ok []) (List.rev formal_types)
  in
  let* arguments =
    List.fold_left2
      (fun result expected (label, argument) ->
        let* arguments = result in
        let* argument = adapt_argument ~expected argument in
        Ok ((label, argument) :: arguments))
      (Ok []) instantiated_formals arguments
    |> Result.map List.rev
  in
  let adapted_actual_types =
    List.map (fun (_, (argument : Sst.expression)) -> argument.typ) arguments
  in
  let[@log_value.trace] integer_adaptation_count =
    List.fold_left2
      (fun count before after ->
        if Parametric_type.equal before after then count else count + 1)
      0 actual_types adapted_actual_types
  in
  let actual_types = adapted_actual_types in
  [%log.trace "inferred symbolic application semantic result"
    ~stage:(Delator.Field.string "symbolic-application-lowering")
    ~declaration:
      (Delator.Field.string
         (Symbolic_application_private.declaration_name declaration))
    ~argument_count:(Delator.Field.int (List.length actual_types))
    ~type_argument_count:(Delator.Field.int (List.length type_arguments))
    ~integer_adaptation_count:
      (Delator.Field.int (integer_adaptation_count [@log_value.trace]))
    ~result_type:(Delator.Field.string (Parametric_type.to_string result_type))
    ~decision:(Delator.Field.string "accepted")];
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
  let formal_types = Symbolic_application_private.parameter_types declaration in
  let* actual_types =
    if List.length formal_types <> List.length actual_types then
      Error "symbolic result inference argument arity differs"
    else
      let rec adapt formal actual =
        match (formal, actual) with
        | Parametric_type.Parameter _, actual -> actual
        | Parametric_type.Mathematical_int, Parametric_type.Int ->
            Parametric_type.Mathematical_int
        | Parametric_type.Tuple formals, Parametric_type.Tuple actuals
          when List.length formals = List.length actuals
               && List.for_all2
                    (fun (formal_label, _) (actual_label, _) ->
                      Option.equal String.equal formal_label actual_label)
                    formals actuals ->
            Parametric_type.Tuple
              (List.map2
                 (fun (label, formal) (_, actual) ->
                   (label, adapt formal actual))
                 formals actuals)
        | ( Parametric_type.Application (formal_constructor, formals),
            Parametric_type.Application (actual_constructor, actuals) )
          when Parametric_type.compare_constructor formal_constructor
                 actual_constructor
               = 0
               && List.length formals = List.length actuals ->
            Parametric_type.Application
              (formal_constructor, List.map2 adapt formals actuals)
        | _, actual -> actual
      in
      let adapted = List.map2 adapt formal_types actual_types
      in
      let[@log_value.trace] _lift_count =
        List.fold_left2
          (fun count before after ->
            if Parametric_type.equal before after then count else count + 1)
          0 actual_types adapted
      in
      [%log.trace "adapted symbolic result inference boundaries"
        ~stage:(Delator.Field.string "symbolic-result-inference")
        ~argument_count:(Delator.Field.int (List.length actual_types))
        ~runtime_to_mathematical_count:
          (Delator.Field.int (_lift_count [@log_value.trace]))
        ~decision:(Delator.Field.string "adapted")];
      Ok adapted
  in
  let* type_arguments =
    Parametric_lowering_private
    .infer_labeled_type_arguments_from_logical_actuals
      ~binders
      ~formal_types
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

type semantic_signature = {
  formal_types : Sst.typ list;
  formal_labels : string option list;
  formal_result : Sst.typ;
}

let direct_call ~invalid ~adapt_argument ~span ~result_type ~actuals
    (call : call) =
  let* actuals =
    if List.length call.formal_types <> List.length actuals then
      Error (invalid "direct call argument arity differs")
    else
      List.fold_left2
        (fun result formal_type (label, actual) ->
          let* actuals = result in
          let* actual = adapt_argument ~expected:formal_type actual in
          Ok ((label, actual) :: actuals))
        (Ok []) call.formal_types actuals
      |> Result.map List.rev
  in
  let actual_types =
    List.map (fun (_, (actual : Sst.expression)) -> actual.typ) actuals
  in
  let* type_arguments =
    Parametric_lowering_private.infer_labeled_type_arguments_for_logical_call
      ~binders:call.type_binders ~formal_types:call.formal_types
      ~formal_labels:call.formal_labels ~actual_types
      ~actual_labels:(List.map fst actuals) ~formal_result:call.formal_result
      ~actual_result:result_type
    |> Result.map_error invalid
  in
  let* instantiated_formals =
    List.fold_left
      (fun result formal ->
        let* formals = result in
        let* formal =
          Parametric_lowering_private.instantiate ~binders:call.type_binders
            ~arguments:type_arguments formal
          |> Result.map_error invalid
        in
        Ok (formal :: formals))
      (Ok []) (List.rev call.formal_types)
  in
  let[@log_value.trace] preadapted_types =
    List.map (fun (_, (actual : Sst.expression)) -> actual.typ) actuals
  in
  let* actuals =
    List.fold_left2
      (fun result expected (label, actual) ->
        let* actuals = result in
        let* actual = adapt_argument ~expected actual in
        Ok ((label, actual) :: actuals))
      (Ok []) instantiated_formals actuals
    |> Result.map List.rev
  in
  let[@log_value.trace] semantic_adaptation_count =
    List.fold_left2
      (fun count before (_, (after : Sst.expression)) ->
        if Parametric_type.equal before after.typ then count else count + 1)
      0 (preadapted_types [@log_value.trace]) actuals
  in
  let* semantic_result =
    Parametric_lowering_private.instantiate ~binders:call.type_binders
      ~arguments:type_arguments call.formal_result
    |> Result.map_error invalid
  in
  let* result_type =
    Parametric_lowering_private.reconcile_authenticated_result ~binders:[]
      ~semantic:semantic_result ~compiler:result_type
    |> Result.map_error invalid
  in
  [%log.trace "reconciled direct specification-call semantic result"
    ~stage:(Delator.Field.string "specification-call-lowering")
    ~function_name:(Delator.Field.string call.function_id.function_name)
    ~type_argument_count:(Delator.Field.int (List.length type_arguments))
    ~semantic_argument_adaptation_count:
      (Delator.Field.int
         (semantic_adaptation_count [@log_value.trace]))
    ~result_sort:
      (Delator.Field.string (Parametric_type.to_string result_type))
    ~decision:(Delator.Field.string "accepted")];
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
  direct_call ~invalid
    ~adapt_argument:(fun ~expected:_ argument -> Ok argument)
    ~span ~result_type ~actuals:[]
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

let source_signature ?semantic_signature ?explicit_parameter_type
    ?explicit_result_type ~lower ~optional_carrier ~parameter_error
    (expression : Typedtree.expression) =
  let[@log_value.debug] log_signature decision signature =
    let[@log_value.debug] signature_value =
      let bounded values =
        let rec collect count kept = function
          | [] -> (List.rev kept, 0)
          | _ :: _ as values when count = 16 ->
              (List.rev kept, List.length values)
          | value :: values -> collect (count + 1) (value :: kept) values
        in
        collect 0 [] values
      in
      let formal_types, dropped =
        bounded
          (List.map
             (fun typ -> Delator.Field.string (Parametric_type.to_string typ))
             signature.formal_types)
      in
      let formal_labels, _ =
        bounded
          (List.map
             (fun label ->
               Delator.Field.string (Option.value ~default:"_" label))
             signature.formal_labels)
      in
      Delator.Field.map ~dropped:0
        [ ("formal_types", Delator.Field.seq ~dropped formal_types);
          ("formal_labels", Delator.Field.seq ~dropped formal_labels);
          ( "formal_result",
            Delator.Field.string
              (Parametric_type.to_string signature.formal_result) ) ]
    in
    [%log.debug "reconstructed callable semantic signature"
      ~decision:(Delator.Field.string decision)
      ~formal_count:(Delator.Field.int (List.length signature.formal_types))
      ~signature:(signature_value [@log_value.debug])]
  in
  match expression.exp_desc with
  | Texp_function { params; body = Tfunction_body body; _ } ->
      Some
        (match semantic_signature with
        | Some signature ->
            let[@log_value.debug] _logged_signature =
              (log_signature [@log_value.debug]) "semantic-sst" signature
            in
            Ok
              ( signature.formal_types,
                signature.formal_labels,
                signature.formal_result )
        | None ->
            let explicit_parameter_type =
              Option.value explicit_parameter_type ~default:(fun _ -> Ok None)
            in
            let explicit_result_type =
              Option.value explicit_result_type ~default:(fun _ -> Ok None)
            in
            let* domains =
              Parametric_lowering_private.compiler_parameter_domains
                expression.exp_type params
              |> Result.map_error (fun (location, message) ->
                     parameter_error location message)
            in
            let lower_parameter (parameter : Typedtree.pattern) source_type =
              let* explicit = explicit_parameter_type parameter in
              match explicit with
              | Some typ -> Ok typ
              | None -> lower parameter.pat_loc source_type
            in
            let rec lower_formals formal_types formal_labels parameters domains =
              match parameters, domains with
              | [], [] -> Ok (List.rev formal_types, List.rev formal_labels)
              | (parameter : Typedtree.function_param) :: parameters,
                domain :: domains -> (
                  match parameter.fp_kind with
                  | Tparam_pat pattern ->
                      let* typ = lower_parameter pattern pattern.pat_type in
                      lower_formals (typ :: formal_types)
                        (Parametric_lowering_private.formal_label
                           parameter.fp_arg_label
                        :: formal_labels)
                        parameters domains
                  | Tparam_optional_default (pattern, _, _) ->
                      let* payload = lower_parameter pattern pattern.pat_type in
                      let* carrier = lower pattern.pat_loc domain in
                      let* typ =
                        optional_carrier pattern.pat_loc carrier payload
                      in
                      lower_formals (typ :: formal_types)
                        (Parametric_lowering_private.formal_label
                           parameter.fp_arg_label
                        :: formal_labels)
                        parameters domains)
              | [], _ :: _ | _ :: _, [] -> assert false
            in
            let* formal_types, formal_labels =
              lower_formals [] [] params domains
            in
            let* explicit_result = explicit_result_type body in
            let* result_type =
              match explicit_result with
              | Some typ -> Ok typ
              | None -> lower body.exp_loc body.exp_type
            in
            let[@log_value.debug] _logged_signature =
              (log_signature [@log_value.debug]) "explicit-constraints"
                { formal_types; formal_labels; formal_result = result_type }
            in
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
          if named_stage then
            Option.bind direct (fun candidate ->
                if first_class candidate then Some candidate else None)
          else
            match direct with
            | Some candidate when first_class candidate -> direct
            | Some _ | None -> None ))
    | _ -> (true, false, None)
  in
  (local_function || named_stage, direct)
