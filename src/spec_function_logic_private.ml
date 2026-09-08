let arrow_digest = Parametric_type.structural_identity_digest
let binder = Spec_function_type_private.binder
let is_binder_for = Spec_function_type_private.is_binder_for

let of_symbol ~arrow symbol =
  match symbol.Vir.sort with
  | Vir.Parametric candidate when is_binder_for arrow candidate ->
      Ok
        {
          Vir.parametric_sort = candidate;
          parametric_desc = Vir.Parametric_symbol symbol;
        }
  | Vir.Integer | Vir.Boolean | Vir.Bit_vector _ | Vir.Aggregate _
  | Vir.Parametric _ ->
      Error "specification-function symbol has the wrong exact arrow sort"

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

type value =
  | Unit_value
  | Integer_value of Vir.integer_term
  | Boolean_value of Vir.boolean_term
  | Bit_vector_value of Vir.bit_vector_term
  | Tuple_value of value list
  | Aggregate_value of Vir.aggregate_term
  | Parametric_value of Vir.parametric_term
  | Function_value of function_value

and function_value = {
  function_term : Vir.spec_function_term;
  function_arrow : Sst.typ;
  function_closure : function_closure;
}

and function_closure =
  | Abstract_function
  | Lambda_function of {
      lambda : Spec_function_sst_private.lambda;
      lambda_environment : (int * value) list;
    }
  | Named_function of {
      definition : Sst.function_definition;
      named_arguments : (string option * value) list;
      named_argument_types : Sst.typ list;
      named_type_arguments : Sst.typ list;
    }

let conditional condition consequent alternative =
  if
    not
      (Parametric_type.equal consequent.function_arrow
         alternative.function_arrow)
  then Error "specification-function conditional crosses exact arrow types"
  else
    let arrow = consequent.function_arrow in
    if
      not
        (is_binder_for arrow consequent.function_term.Vir.parametric_sort
        && is_binder_for arrow alternative.function_term.Vir.parametric_sort)
    then Error "specification-function conditional has the wrong arrow sort"
    else
      Ok
        {
          function_term =
            {
              Vir.parametric_sort = binder arrow;
              parametric_desc =
                Vir.Parametric_conditional
                  (condition, consequent.function_term, alternative.function_term);
            };
          function_arrow = arrow;
          function_closure = Abstract_function;
        }

let recursive_argument ~error ~span = function
  | Integer_value term -> Ok (Vir.Recursive_integer_argument term)
  | Boolean_value term -> Ok (Vir.Recursive_boolean_argument term)
  | Bit_vector_value term -> Ok (Vir.Recursive_bv_argument term)
  | Aggregate_value term -> Ok (Vir.Recursive_aggregate_argument term)
  | Parametric_value term -> Ok (Vir.Recursive_parametric_argument term)
  | Function_value function_ ->
      Ok (Vir.Recursive_parametric_argument function_.function_term)
  | Unit_value ->
      Ok (Vir.Recursive_boolean_argument (Vir.Boolean_constant true))
  | Tuple_value _ ->
      Error (error span "value escaped the recursive logical term ABI")

let application_term ~error ~span = function
  | Integer_value term -> Ok (Vir.Integer_application term)
  | Boolean_value term -> Ok (Vir.Boolean_application term)
  | Bit_vector_value term -> Ok (Vir.Bv_application term)
  | Aggregate_value term -> Ok (Vir.Aggregate_application term)
  | Parametric_value term -> Ok (Vir.Parametric_application term)
  | Function_value function_ ->
      Ok (Vir.Parametric_application function_.function_term)
  | Unit_value | Tuple_value _ ->
      Error (error span "trigger escaped the first-order application ABI")

let value_of_application ~aggregate_type ~error ~span typ application =
  match typ with
  | Sst.Unit -> Ok Unit_value
  | Sst.Int | Sst.Mathematical_int ->
      Ok (Integer_value (Vir.Integer_symbolic_application application))
  | Sst.Bool ->
      Ok (Boolean_value (Vir.Boolean_symbolic_application application))
  | Sst.Bit_vector width ->
      Ok
        (Bit_vector_value
           { Vir.bit_vector_width = width;
             bit_vector_desc = Vir.Bv_symbolic_application application })
  | Sst.Parameter binder ->
      Ok
        (Parametric_value
           {
             Vir.parametric_sort = binder;
             parametric_desc = Vir.Parametric_symbolic_application application;
           })
  | Sst.Application _ when Parametric_type.is_spec_function typ ->
      Ok
        (Function_value
           {
             function_term =
               {
                 Vir.parametric_sort = binder typ;
                 parametric_desc =
                   Vir.Parametric_symbolic_application application;
               };
             function_arrow = typ;
             function_closure = Abstract_function;
           })
  | (Sst.Application _ | Sst.Aggregate _) as typ -> (
      match aggregate_type typ with
      | Some aggregate_type ->
          Ok
            (Aggregate_value
               {
                 Vir.aggregate_type;
                 aggregate_desc = Vir.Aggregate_symbolic_application application;
               })
      | None -> Error (error span "function result lacks an aggregate sort"))
  | Sst.Tuple _ -> Error (error span "function result escaped first-order logic")

let sort_of_type ~aggregate_type = function
  | Parametric_type.Int | Parametric_type.Mathematical_int -> Ok Vir.Integer
  | Bool -> Ok Vir.Boolean
  | Bit_vector width -> Ok (Vir.Bit_vector width)
  | Parameter binder -> Ok (Vir.Parametric binder)
  | Application _ as typ when Parametric_type.is_spec_function typ ->
      Ok (Vir.Parametric (binder typ))
  | (Application _ | Aggregate _) as typ -> (
      match aggregate_type typ with
      | Some aggregate -> Ok (Vir.Aggregate aggregate)
      | None -> Error "application sort is unavailable")
  | Unit | Tuple _ -> Error "unsupported first-order sort"

let quantified_value ~aggregate_type ~error (binding : Sst.binding) =
  let make sort value =
    let symbol : Vir.symbol =
      {
        symbol_id = -1 - binding.id;
        source_name = binding.name;
        sort;
        role = Vir.Local;
        span = binding.span;
      }
    in
    (symbol, value symbol)
  in
  let lower = function
    | Logic_quantifier_private.Integer_binder ->
        Some
          (make Vir.Integer (fun symbol ->
               Integer_value (Vir.Integer_symbol symbol)))
    | Boolean_binder ->
        Some
          (make Vir.Boolean (fun symbol ->
               Boolean_value (Vir.Boolean_symbol symbol)))
    | Bit_vector_binder width ->
        Some
          (make (Vir.Bit_vector width) (fun symbol ->
               Bit_vector_value (Result.get_ok (Vir.bv_symbol symbol))))
    | Parameter_binder parameter ->
        Some
          (make (Vir.Parametric parameter) (fun symbol ->
               Parametric_value
                 {
                   Vir.parametric_sort = parameter;
                   parametric_desc = Vir.Parametric_symbol symbol;
                 }))
    | Application_binder typ ->
        if Parametric_type.is_spec_function typ then
          Some
            (make
               (Vir.Parametric (binder typ))
               (fun symbol ->
                 Function_value
                   {
                     function_term = Result.get_ok (of_symbol ~arrow:typ symbol);
                     function_arrow = typ;
                     function_closure = Abstract_function;
                   }))
        else
          Option.map
            (fun aggregate ->
              make (Vir.Aggregate aggregate) (fun symbol ->
                  Aggregate_value
                    {
                      Vir.aggregate_type = aggregate;
                      aggregate_desc = Vir.Aggregate_symbol symbol;
                    }))
            (aggregate_type typ)
  in
  Quantifier_validation_private.quantifier_value
    { lower; value_error = error }
    binding

type ('context, 'state, 'error) axiom_services = {
  evaluate :
    'context -> Sst.expression -> 'state -> (value * 'state, 'error) result;
  aggregate_type : Sst.typ -> Vir.aggregate_type option;
  environment : 'state -> (int * value) list;
  with_environment : 'state -> (int * value) list -> 'state;
  assume : 'state -> Vir.boolean_term list -> 'state;
  enter_definition : 'context -> Sst.function_definition -> 'context;
  bind_pattern :
    (int * value) list ->
    Sst.pattern ->
    value ->
    ((int * value) list, 'error) result;
  equality : value -> value -> Vir.boolean_term option;
  error : Diagnostic.span -> string -> 'error;
}

let quantifier ~services ~owner ~binding ~body ~trigger =
  let metadata =
    Logic_quantifier_private.create ~kind:Logic_quantifier_private.Forall ~owner
      ~binder_index:binding.Sst.id ~binder_type:binding.typ ~span:binding.span
  in
  let* schema =
    Logic_quantifier_private.vector [ metadata ]
    |> Result.map_error (services.error binding.span)
  in
  Vir.make_boolean_quantifier
    ~sort_of_type:(sort_of_type ~aggregate_type:services.aggregate_type)
    ~schema
    ~binders:[ fst trigger ]
    ~body
    ~trigger:(Some (snd trigger))
  |> Result.map_error (services.error binding.span)

let create_term ~arrow ~identity ~name ~span ~arguments ~argument_types =
  let result_type = Parametric_type.Parameter (binder arrow) in
  let* declaration =
    Spec_function_type_private.declaration ~identity ~name ~span
      ~parameter_types:argument_types ~result_type
  in
  let* application =
    Symbolic_application_private.create declaration ~type_arguments:[]
      ~arguments ~argument_types ~result_type ~span
  in
  Ok
    {
      Vir.parametric_sort = binder arrow;
      parametric_desc = Vir.Parametric_symbolic_application application;
    }

let closure ~(lambda : Spec_function_sst_private.lambda) ~captures =
  let* () = Spec_function_sst_private.validate_lambda lambda in
  let identity =
    "closure:"
    ^ arrow_digest lambda.lambda_arrow
    ^ ":"
    ^ Digest.to_hex (Digest.string lambda.lambda_site)
  in
  create_term ~arrow:lambda.lambda_arrow ~identity
    ~name:(Spec_function_sst_private.synthetic_name ~site:lambda.lambda_site)
    ~span:lambda.lambda_body.span ~arguments:captures
    ~argument_types:
      (List.map
         (fun (binding : Sst.binding) -> binding.typ)
         lambda.lambda_captures)

let named ~arrow ~(function_id : Sst.function_id) ~type_arguments ~arguments
    ~argument_types ~span =
  let identity =
    String.concat ":"
      [
        "named";
        string_of_int function_id.function_index;
        Digest.to_hex (Digest.string function_id.function_name);
        Parametric_type.structural_vector_digest type_arguments;
        arrow_digest arrow;
        string_of_int (List.length arguments);
      ]
  in
  create_term ~arrow ~identity
    ~name:("$verocaml.spec-named:" ^ identity)
    ~span ~arguments ~argument_types

let application ~arrow ~function_ ~argument ~result_type ~span =
  match Spec_function_type_private.classify arrow with
  | None -> Error "specification-function application has a non-arrow type"
  | Some view
    when Parametric_type.compare_binder function_.Vir.parametric_sort
           (binder arrow)
         = 0 ->
      let identity = "apply:" ^ arrow_digest arrow in
      let* declaration =
        Spec_function_type_private.declaration ~identity
          ~name:("$verocaml.spec-apply:" ^ arrow_digest arrow)
          ~span ~parameter_types:[ arrow; view.domain ] ~result_type
      in
      Symbolic_application_private.create declaration ~type_arguments:[]
        ~arguments:[ Vir.Recursive_parametric_argument function_; argument ]
        ~argument_types:[ arrow; view.domain ] ~result_type ~span
  | Some _ -> Error "specification-function value has the wrong arrow sort"

let application_backend_head ~arrow ~result_type ~span =
  let* view = Spec_function_type_private.require arrow in
  let identity = "apply:" ^ arrow_digest arrow in
  let* declaration =
    Spec_function_type_private.declaration ~identity
      ~name:("$verocaml.spec-apply:" ^ arrow_digest arrow)
      ~span ~parameter_types:[ arrow; view.domain ] ~result_type
  in
  let* backend_head =
    Symbolic_application_private.backend_head_for_instantiation declaration
      ~type_arguments:[] ~argument_types:[ arrow; view.domain ] ~result_type
  in
  [%log.trace "correlated specification-function apply backend identity"
    ~stage:(Delator.Field.string "spec-function-apply-identity")
    ~route:(Delator.Field.string "recursive")
    ~correlation:(Delator.Field.string (arrow_digest arrow))
    ~decision:(Delator.Field.string "correlated")];
  Ok backend_head

let is_application application =
  let declaration = Symbolic_application_private.declaration application in
  let marker = Symbolic_application_private.marker_id declaration in
  if String.starts_with ~prefix:"$verocaml.spec-function:apply:" marker then
    match Symbolic_application_private.parameter_types declaration with
    | arrow :: _ when Parametric_type.is_spec_function arrow -> Some arrow
    | _ -> None
  else None

let lambda_value ~environment ~error ~span
    (lambda : Spec_function_sst_private.lambda) =
  let rec captures values arguments = function
    | [] -> Ok (List.rev values, List.rev arguments)
    | (binding : Sst.binding) :: rest -> (
        match List.assoc_opt binding.id environment with
        | None -> Error (error span ("unbound lambda capture " ^ binding.name))
        | Some value ->
            let* argument = recursive_argument ~error ~span value in
            captures
              ((binding.id, value) :: values)
              (argument :: arguments) rest)
  in
  let* lambda_environment, capture_arguments =
    captures [] [] lambda.lambda_captures
  in
  let* function_term =
    closure ~lambda ~captures:capture_arguments |> Result.map_error (error span)
  in
  Ok
    (Function_value
       {
         function_term;
         function_arrow = lambda.lambda_arrow;
         function_closure = Lambda_function { lambda; lambda_environment };
       })

let materialize_lambda_axiom services ~context ~span function_ state =
  match function_.function_closure with
  | Lambda_function { lambda; lambda_environment } ->
      let owner =
        "$verocaml.spec-lambda-axiom:"
        ^ Parametric_type.structural_identity_digest lambda.lambda_arrow
        ^ ":"
        ^ Digest.to_hex (Digest.string lambda.lambda_site)
      in
      let binding =
        {
          Sst.id = -1 - abs (Hashtbl.hash owner);
          name = "$spec_lambda_arg";
          typ = lambda.lambda_parameter.typ;
          uniqueness = Sst.Definitely_aliased;
          span;
        }
      in
      let* binder_symbol, argument =
        quantified_value ~aggregate_type:services.aggregate_type
          ~error:services.error binding
      in
      let caller_environment = services.environment state in
      let environment =
        (lambda.lambda_parameter.id, argument) :: lambda_environment
      in
      let* right, state =
        services.evaluate context lambda.lambda_body
          (services.with_environment state environment)
      in
      let state = services.with_environment state caller_environment in
      let* arrow =
        Spec_function_type_private.require lambda.lambda_arrow
        |> Result.map_error (services.error span)
      in
      let* argument_term =
        recursive_argument ~error:services.error ~span argument
      in
      let* application =
        application ~arrow:lambda.lambda_arrow
          ~function_:function_.function_term ~argument:argument_term
          ~result_type:arrow.range ~span
        |> Result.map_error (services.error span)
      in
      let* left =
        value_of_application ~aggregate_type:services.aggregate_type
          ~error:services.error ~span arrow.range application
      in
      let* equation =
        match services.equality left right with
        | Some equation -> Ok equation
        | None -> Error (services.error span "lambda axiom result mismatch")
      in
      let* trigger = application_term ~error:services.error ~span left in
      let* quantified =
        quantifier ~services ~owner ~binding ~body:equation
          ~trigger:(binder_symbol, trigger)
      in
      Ok (services.assume state [ Vir.Forall_term quantified ])
  | Abstract_function | Named_function _ -> Ok state

let evaluate_named_result services ~context ~span ~definition ~type_arguments
    ~parameters ~values ~value_types ~result_type state =
  let caller_environment = services.environment state in
  let* environment =
    List.fold_left2
      (fun result parameter (_, actual) ->
        let* environment = result in
        services.bind_pattern environment parameter.Sst.pattern actual)
      (Ok []) parameters values
  in
  match definition.Sst.body with
  | Sst.Spec_definition body ->
      let substitute =
        Parametric_type.substitute
          (List.combine definition.type_binders type_arguments)
      in
      let* value, state =
        services.evaluate
          (services.enter_definition context definition)
          (Sst.map_expression_types substitute body.expression)
          (services.with_environment state environment)
      in
      Ok (value, services.with_environment state caller_environment)
  | Sst.Symbolic_declaration declaration ->
      let* arguments =
        List.fold_left
          (fun result (_, value) ->
            let* arguments = result in
            let* argument =
              recursive_argument ~error:services.error ~span value
            in
            Ok (argument :: arguments))
          (Ok []) values
        |> Result.map List.rev
      in
      let* application =
        Symbolic_application_private.create declaration ~type_arguments
          ~arguments ~argument_types:value_types ~result_type ~span
        |> Result.map_error (services.error span)
      in
      let* value =
        value_of_application ~aggregate_type:services.aggregate_type
          ~error:services.error ~span result_type application
      in
      Ok (value, services.with_environment state caller_environment)
  | Sst.Checked_exec _ | Sst.Recursive_spec_definition _ | Sst.Proof_body _
  | Sst.External_specification _ | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _ ->
      Error (services.error span "named function lacks a logical body")

let rec materialize_named_stages services ~context ~span
    ~(definition : Sst.function_definition) ~named_type_arguments ~parameters
    ~owner state binders metadata values value_types current_term current_arrow
    ordinal remaining =
  match remaining with
  | [] -> Ok state
  | _ :: rest ->
      let* arrow =
        Spec_function_type_private.require current_arrow
        |> Result.map_error (services.error span)
      in
      let binding =
        {
          Sst.id =
            -1
            - abs
                (Hashtbl.hash
                   ( owner,
                     ordinal,
                     Parametric_type.structural_identity_digest arrow.domain ));
          name = "$spec_arg_" ^ string_of_int ordinal;
          typ = arrow.domain;
          uniqueness = Sst.Definitely_aliased;
          span;
        }
      in
      let* binder_symbol, argument =
        quantified_value ~aggregate_type:services.aggregate_type
          ~error:services.error binding
      in
      let* argument_term =
        recursive_argument ~error:services.error ~span argument
      in
      let* applied =
        application ~arrow:current_arrow ~function_:current_term
          ~argument:argument_term ~result_type:arrow.range ~span
        |> Result.map_error (services.error span)
      in
      let* left =
        value_of_application ~aggregate_type:services.aggregate_type
          ~error:services.error ~span arrow.range applied
      in
      let values = values @ [ (arrow.label, argument) ] in
      let value_types = value_types @ [ arrow.domain ] in
      let* right, state =
        match rest with
        | _ :: _ ->
            let* arguments =
              List.fold_left
                (fun result (_, value) ->
                  let* arguments = result in
                  let* argument =
                    recursive_argument ~error:services.error ~span value
                  in
                  Ok (argument :: arguments))
                (Ok []) values
              |> Result.map List.rev
            in
            let* function_term =
              named ~arrow:arrow.range ~function_id:definition.function_id
                ~type_arguments:named_type_arguments ~arguments
                ~argument_types:value_types ~span
              |> Result.map_error (services.error span)
            in
            Ok
              ( Function_value
                  {
                    function_term;
                    function_arrow = arrow.range;
                    function_closure = Abstract_function;
                  },
                state )
        | [] ->
            evaluate_named_result services ~context ~span ~definition
              ~type_arguments:named_type_arguments ~parameters ~values
              ~value_types ~result_type:arrow.range state
      in
      let* equation =
        match services.equality left right with
        | Some equation -> Ok equation
        | None ->
            Error
              (services.error span
                 "named specification axiom has a mismatched result")
      in
      let* trigger = application_term ~error:services.error ~span left in
      let binders = binders @ [ binder_symbol ] in
      let metadata =
        metadata
        @ [
            Logic_quantifier_private.create
              ~kind:Logic_quantifier_private.Forall ~owner
              ~binder_index:binding.id ~binder_type:binding.typ ~span;
          ]
      in
      let* schema =
        Logic_quantifier_private.vector metadata
        |> Result.map_error (services.error span)
      in
      let* quantified =
        Vir.make_boolean_quantifier
          ~sort_of_type:(sort_of_type ~aggregate_type:services.aggregate_type)
          ~schema ~binders ~body:equation ~trigger:(Some trigger)
        |> Result.map_error (services.error span)
      in
      let state = services.assume state [ Vir.Forall_term quantified ] in
      let next_term =
        match right with
        | Function_value function_ -> function_.function_term
        | Unit_value | Integer_value _ | Boolean_value _ | Bit_vector_value _
        | Tuple_value _
        | Aggregate_value _ | Parametric_value _ ->
            current_term
      in
      materialize_named_stages services ~context ~span ~definition
        ~named_type_arguments ~parameters ~owner state binders metadata values
        value_types next_term arrow.range (ordinal + 1) rest

let materialize_named_axioms services ~context ~span function_ state =
  match function_.function_closure with
  | Abstract_function | Lambda_function _ -> Ok state
  | Named_function
      {
        definition;
        named_arguments;
        named_argument_types;
        named_type_arguments;
      } ->
      let substitute =
        Parametric_type.substitute
          (List.combine definition.type_binders named_type_arguments)
      in
      let parameters =
        List.map Sst.require_value_parameter definition.parameters
        |> List.map (fun (parameter : Sst.value_parameter) ->
            {
              parameter with
              Sst.pattern = Sst.map_pattern_types substitute parameter.pattern;
            })
      in
      let consumed = List.length named_arguments in
      let remaining =
        List.filteri (fun index _ -> index >= consumed) parameters
      in
      let owner =
        "$verocaml.spec-function-axiom:"
        ^ string_of_int definition.function_id.function_index
        ^ ":"
        ^ Parametric_type.structural_identity_digest function_.function_arrow
      in
      materialize_named_stages services ~context ~span ~definition
        ~named_type_arguments ~parameters ~owner state [] [] named_arguments
        named_argument_types function_.function_term function_.function_arrow 0
        remaining

type ('value, 'error) application_value_services = {
  integer :
    Vir.recursive_spec_argument Symbolic_application_private.t -> 'value;
  boolean :
    Vir.recursive_spec_argument Symbolic_application_private.t -> 'value;
  bit_vector :
    Bv_width.t ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'value;
  parametric :
    Parametric_type.binder ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'value;
  function_ :
    Sst.typ ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'value;
  aggregate :
    Vir.aggregate_type ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'value;
  aggregate_type : Sst.typ -> Vir.aggregate_type option;
  invalid : string -> 'error;
}

let value_of_logic_application services typ application =
  match typ with
  | Sst.Int | Sst.Mathematical_int -> Ok (services.integer application)
  | Sst.Bool -> Ok (services.boolean application)
  | Sst.Bit_vector width -> Ok (services.bit_vector width application)
  | Sst.Parameter binder -> Ok (services.parametric binder application)
  | Sst.Application _ when Parametric_type.is_spec_function typ ->
      Ok (services.function_ typ application)
  | Sst.Application _ -> (
      match services.aggregate_type typ with
      | Some aggregate -> Ok (services.aggregate aggregate application)
      | None -> Error (services.invalid "symbolic result sort is unavailable"))
  | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _ ->
      Error (services.invalid "symbolic result type escaped validation")
