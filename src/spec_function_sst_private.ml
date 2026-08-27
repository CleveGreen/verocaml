type lambda = {
  lambda_site : string;
  lambda_arrow : Sst.typ;
  lambda_parameter : Sst.binding;
  lambda_body : Sst.expression;
  lambda_captures : Sst.binding list;
}

type application = {
  application_arrow : Sst.typ;
  application_function : Sst.expression;
  application_argument : Sst.expression;
  application_label : string option;
  application_result : Sst.typ;
}

let validate_lambda lambda =
  match Spec_function_type_private.classify lambda.lambda_arrow with
  | None -> Error "lambda has a non-canonical arrow type"
  | Some arrow
    when Parametric_type.equal lambda.lambda_parameter.typ arrow.domain
         && Parametric_type.equal lambda.lambda_body.typ arrow.range ->
      if
        List.exists
          (fun capture -> capture.Sst.id = lambda.lambda_parameter.id)
          lambda.lambda_captures
      then Error "lambda parameter appears in its capture vector"
      else Ok ()
  | Some _ -> Error "lambda parameter/body type disagrees with its arrow"

let capture_ids lambda =
  List.map (fun binding -> binding.Sst.id) lambda.lambda_captures

let synthetic_prefix = "$verocaml.spec-lambda:"
let synthetic_name ~site = synthetic_prefix ^ Digest.to_hex (Digest.string site)
let is_synthetic_name = String.starts_with ~prefix:synthetic_prefix
let lambda_index = -111

let variable binding =
  {
    Sst.expression_desc =
      Sst.Variable { binding; use_uniqueness = Sst.Definitely_aliased };
    typ = binding.Sst.typ;
    span = binding.span;
  }

let is_bare_carrier (definition : Sst.function_definition) =
  match (definition.parameters, definition.body) with
  | ( [
        Sst.Value_parameter
          { pattern = { pattern_desc = Sst.Bind parameter; _ }; _ };
      ],
      Sst.Spec_definition
        {
          expression =
            { expression_desc = Sst.Variable { binding = result; _ }; _ };
          _;
        } ) ->
      Parametric_type.is_spec_function definition.result_type
      && parameter.id = result.id
  | _ -> false

let make_lambda lambda =
  match validate_lambda lambda with
  | Error _ as error -> error
  | Ok () ->
      let arguments =
        variable lambda.lambda_parameter
        :: List.map variable lambda.lambda_captures
        @ [ lambda.lambda_body ]
      in
      Ok
        {
          Sst.expression_desc =
            Sst.Direct_call
              {
                call_form = Sst.Specification_call;
                callee =
                  {
                    Sst.function_index = lambda_index;
                    function_name = synthetic_name ~site:lambda.lambda_site;
                  };
                type_arguments = [ lambda.lambda_arrow ];
                arguments =
                  List.map
                    (fun value -> Sst.Value_argument { label = None; value })
                    arguments;
                recursive = false;
              };
          typ = lambda.lambda_arrow;
          span = lambda.lambda_body.span;
        }

let lambda expression =
  match expression.Sst.expression_desc with
  | Sst.Direct_call
      {
        call_form = Sst.Specification_call;
        callee;
        type_arguments = [ arrow ];
        arguments;
        recursive = false;
      }
    when callee.function_index = lambda_index
         && is_synthetic_name callee.function_name
         && Parametric_type.equal arrow expression.typ -> (
      let arguments =
        List.map
          (fun argument -> snd (Sst.require_value_argument argument))
          arguments
      in
      match arguments with
      | { Sst.expression_desc = Sst.Variable { binding = parameter; _ }; _ }
        :: arguments -> (
          match List.rev arguments with
          | body :: reversed_captures ->
              let captures =
                List.rev reversed_captures
                |> List.filter_map (fun expression ->
                    match expression.Sst.expression_desc with
                    | Sst.Variable { binding; _ } -> Some binding
                    | _ -> None)
              in
              if List.length captures <> List.length reversed_captures then None
              else
                let lambda =
                  {
                    lambda_site = callee.function_name;
                    lambda_arrow = expression.typ;
                    lambda_parameter = parameter;
                    lambda_body = body;
                    lambda_captures = captures;
                  }
                in
                if Result.is_ok (validate_lambda lambda) then Some lambda
                else None
          | [] -> None)
      | _ :: _ | [] -> None)
  | _ -> None

let apply_index = -110

let apply_id arrow =
  {
    Sst.function_index = apply_index;
    function_name =
      "$verocaml.spec-apply:" ^ Parametric_type.structural_identity_digest arrow;
  }

let is_apply_id callee =
  callee.Sst.function_index = apply_index
  && String.starts_with ~prefix:"$verocaml.spec-apply:" callee.function_name

let is_application_id = is_apply_id

let make_application ~arrow ~function_ ~argument ~label ~span =
  match Spec_function_type_private.classify arrow with
  | None -> Error "specification-function application has no canonical arrow"
  | Some view
    when Parametric_type.equal function_.Sst.typ arrow
         && Parametric_type.equal argument.Sst.typ view.domain
         && Option.equal String.equal label view.label ->
      Ok
        {
          Sst.expression_desc =
            Sst.Direct_call
              {
                call_form = Sst.Specification_call;
                callee = apply_id arrow;
                type_arguments = [ arrow ];
                arguments =
                  [
                    Sst.Value_argument { label = None; value = function_ };
                    Sst.Value_argument { label; value = argument };
                  ];
                recursive = false;
              };
          typ = view.range;
          span;
        }
  | Some _ -> Error "specification-function application type mismatch"

let application expression =
  match expression.Sst.expression_desc with
  | Sst.Direct_call
      {
        call_form = Sst.Specification_call;
        callee;
        type_arguments = [ arrow ];
        arguments =
          [
            Sst.Value_argument { label = None; value = function_ };
            Sst.Value_argument { label; value = argument };
          ];
        recursive = false;
      }
    when is_apply_id callee -> (
      match Spec_function_type_private.classify arrow with
      | Some view
        when Parametric_type.equal function_.typ arrow
             && Parametric_type.equal argument.Sst.typ view.domain
             && Parametric_type.equal expression.Sst.typ view.range
             && Option.equal String.equal label view.label ->
          Some
            {
              application_arrow = arrow;
              application_function = function_;
              application_argument = argument;
              application_label = label;
              application_result = expression.typ;
            }
      | Some _ | None -> None)
  | _ -> None

let rec application_has_lambda_head expression =
  match application expression with
  | Some application ->
      if lambda application.application_function <> None then true
      else application_has_lambda_head application.application_function
  | None -> false

let is_reference expression =
  match expression.Sst.expression_desc with
  | Sst.Direct_call
      { call_form = Sst.Specification_call; callee; arguments = _; _ } ->
      callee.function_index <> apply_index
      && callee.function_index <> lambda_index
      && Parametric_type.is_spec_function expression.typ
  | _ -> false

let semantic_children expression =
  match lambda expression with
  | Some lambda ->
      Some (List.map variable lambda.lambda_captures @ [ lambda.lambda_body ])
  | None -> (
      match application expression with
      | Some application ->
          Some
            [
              application.application_function; application.application_argument;
            ]
      | None when is_reference expression -> (
          match expression.Sst.expression_desc with
          | Sst.Direct_call { arguments; _ } ->
              Some
                (List.filter_map
                   (function
                     | Sst.Value_argument { value; _ } -> Some value
                     | Sst.Callback_argument _ -> None)
                   arguments)
          | _ -> assert false)
      | None -> None)

let validate_instance_lambda ~issue_pattern ~issue_expression expression =
  match lambda expression with
  | None -> None
  | Some lambda ->
      let ( let* ) result continuation =
        match result with
        | Ok value -> continuation value
        | Error _ as error -> error
      in
      let parameter_pattern =
        {
          Sst.pattern_desc = Sst.Bind lambda.lambda_parameter;
          typ = lambda.lambda_parameter.typ;
          span = lambda.lambda_parameter.span;
        }
      in
      let rec captures = function
        | [] -> Ok ()
        | binding :: rest ->
            let* () = issue_expression (variable binding) in
            captures rest
      in
      Some
        (let* () = issue_pattern parameter_pattern in
         let* () = captures lambda.lambda_captures in
         issue_expression lambda.lambda_body)

let exact_owned_contents_construction ~owned_tree_prerequisite
    (transition : Sst.owned_tree_transition) value =
  let rec inspect owned destination reads (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant -> Some reads
    | Sst.Variable { binding; _ }
      when binding.typ = expression.typ
           &&
           match expression.typ with
           | Sst.Unit | Sst.Bool | Sst.Int -> true
           | Sst.Tuple _ | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _
             ->
               false ->
        Some reads
    | Sst.Field_read
        {
          record = { expression_desc = Sst.Variable { binding = root; _ }; _ };
          field;
        }
      when root == transition.root
           && field = transition.target_field
           &&
           match destination with
           | Some destination -> List.mem destination owned.Sst.recursive_edges
           | None -> false ->
        Some (reads + 1)
    | Sst.Constructor_value { arguments; _ } ->
        List.fold_left
          (fun reads argument ->
            Option.bind reads (fun reads -> inspect owned None reads argument))
          (Some reads) arguments
    | Sst.Record_value { fields; _ } ->
        List.fold_left
          (fun reads (field, argument) ->
            Option.bind reads (fun reads ->
                inspect owned (Some field) reads argument))
          (Some reads) fields
    | Sst.Tuple_value values ->
        List.fold_left
          (fun reads (_, argument) ->
            Option.bind reads (fun reads ->
                inspect owned destination reads argument))
          (Some reads) values
    | Sst.Variable _ | Sst.Mutable_read _ | Sst.Direct_call _
    | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
    | Sst.Forall _ | Sst.Exists _ | Sst.Symbolic_application _
    | Sst.Field_read _ | Sst.Match _ | Sst.Let _ | Sst.If _ | Sst.Compare _
    | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Checked_arithmetic _
    | Sst.Sequence _ | Sst.Old _ | Sst.Field_write _
    | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
    | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_write _
    | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
    | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Optional_absent
    | Sst.Optional_present _ | Sst.Optional_forward _ ->
        None
  in
  match
    (owned_tree_prerequisite transition.root.typ, value.Sst.expression_desc)
  with
  | Some owned, Sst.Constructor_value _ -> inspect owned None 0 value = Some 1
  | Some _, _ | None, _ -> false

let authenticate_rank1_recursion ~descriptor
    (definition : Sst.function_definition) measure =
  match measure.Sst.typ with
  | Sst.Application (constructor, arguments)
    when List.length arguments < List.length definition.type_binders
         && List.for_all
              (function Sst.Parameter _ -> true | _ -> false)
              arguments
         && List.exists
              (fun parameter ->
                let parameter = Sst.require_value_parameter parameter in
                Parametric_type.is_spec_function parameter.pattern.typ)
              definition.parameters ->
      let exact_arguments =
        List.map (fun binder -> Sst.Parameter binder) definition.type_binders
      in
      let expand = function
        | Sst.Application (candidate, candidate_arguments)
          when Parametric_type.compare_constructor candidate constructor = 0
               && List.equal Parametric_type.equal candidate_arguments arguments
          ->
            Sst.Application (candidate, exact_arguments)
        | typ -> typ
      in
      let parameters =
        List.map
          (function
            | Sst.Value_parameter parameter ->
                Sst.Value_parameter
                  {
                    parameter with
                    Sst.pattern = Sst.map_pattern_types expand parameter.pattern;
                    optional_default =
                      Option.map
                        (fun default ->
                          {
                            Sst.optional_pattern =
                              Sst.map_pattern_types expand
                                default.Sst.optional_pattern;
                            optional_expression =
                              Sst.map_expression_types expand
                                default.optional_expression;
                          })
                        parameter.optional_default;
                  }
            | Sst.Callback_parameter _ as parameter -> parameter)
          definition.parameters
      in
      let body =
        match definition.body with
        | Sst.Recursive_spec_definition recursive ->
            Sst.Recursive_spec_definition
              {
                recursive with
                body =
                  {
                    recursive.body with
                    expression =
                      Sst.map_expression_types expand recursive.body.expression;
                  };
              }
        | body -> body
      in
      Parametric_adt_lowering_private.authenticate_direct_recursion ~descriptor
        ~definition:{ definition with parameters; body }
        ~measure:(Sst.map_expression_types expand measure)
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
  | Sst.Parameter _ | Sst.Application _ ->
      Error "not a rank-1 function-parametric recursive specification"

type ('bound, 'error) validation_services = {
  recurse : 'bound -> Sst.expression -> ('bound, 'error) result;
  add_binding : 'bound -> Sst.binding -> ('bound, 'error) result;
  validate_type : Sst.typ -> (unit, 'error) result;
  find_definition : Sst.function_id -> Sst.function_definition option;
  invalid : Diagnostic.span -> string -> 'error;
}

let validate_special services bound expression =
  let ( let* ) result continuation =
    match result with
    | Ok value -> continuation value
    | Error _ as error -> error
  in
  let rec validate_captures = function
    | [] -> Ok ()
    | binding :: rest ->
        let capture = variable binding in
        let* _ = services.recurse bound capture in
        validate_captures rest
  in
  match lambda expression with
  | Some lambda ->
      Some
        (let* lambda_bound =
           services.add_binding bound lambda.lambda_parameter
         in
         let* () = validate_captures lambda.lambda_captures in
         let* _ = services.recurse lambda_bound lambda.lambda_body in
         Ok bound)
  | None -> (
      match application expression with
      | Some application ->
          Some
            (let* bound =
               services.recurse bound application.application_function
             in
             services.recurse bound application.application_argument)
      | None when is_reference expression -> (
          match expression.Sst.expression_desc with
          | Sst.Direct_call { call_form; callee; type_arguments; arguments; _ }
            ->
              Some
                (let rec validate_types = function
                   | [] -> Ok ()
                   | typ :: rest ->
                       let* () = services.validate_type typ in
                       validate_types rest
                 in
                 let* () = validate_types type_arguments in
                 let* definition =
                   match services.find_definition callee with
                   | Some definition -> Ok definition
                   | None ->
                       Error
                         (services.invalid expression.span
                            "function reference has no same-unit definition")
                 in
                 let* () =
                   if
                     call_form = Sst.Specification_call
                     && definition.mode = Sst.Spec
                   then Ok ()
                   else
                     Error
                       (services.invalid expression.span
                          "function reference must target a same-unit Spec")
                 in
                 let rec validate_arguments bound = function
                   | [] -> Ok bound
                   | Sst.Value_argument { value; _ } :: rest ->
                       let* bound = services.recurse bound value in
                       validate_arguments bound rest
                   | Sst.Callback_argument _ :: _ ->
                       Error
                         (services.invalid expression.span
                            "function reference cannot carry a callback")
                 in
                 validate_arguments bound arguments)
          | _ -> assert false)
      | None -> None)

type 'error logical_validation_services = {
  recurse : Sst.expression -> (unit, 'error) result;
  position : Sst.function_id -> int option;
  current_position : int option;
  invalid : Diagnostic.span -> string -> 'error;
}

let validate_logical_special services expression =
  let ( let* ) result continuation =
    match result with
    | Ok value -> continuation value
    | Error _ as error -> error
  in
  let rec captures = function
    | [] -> Ok ()
    | binding :: rest ->
        let* () = services.recurse (variable binding) in
        captures rest
  in
  match lambda expression with
  | Some lambda ->
      Some
        (let* () = captures lambda.lambda_captures in
         services.recurse lambda.lambda_body)
  | None -> (
      match application expression with
      | Some application ->
          Some
            (let* () = services.recurse application.application_function in
             services.recurse application.application_argument)
      | None when is_reference expression -> (
          match expression.Sst.expression_desc with
          | Sst.Direct_call { callee; arguments; _ } ->
              Some
                (let* () =
                   match
                     (services.current_position, services.position callee)
                   with
                   | Some current, Some target when target < current -> Ok ()
                   | _ ->
                       Error
                         (services.invalid expression.span
                            "function references require an earlier same-unit \
                             Spec")
                 in
                 let rec values = function
                   | [] -> Ok ()
                   | Sst.Value_argument { value; _ } :: rest ->
                       let* () = services.recurse value in
                       values rest
                   | Sst.Callback_argument _ :: _ ->
                       Error
                         (services.invalid expression.span
                            "function reference cannot carry a callback")
                 in
                 values arguments)
          | _ -> assert false)
      | None -> None)
