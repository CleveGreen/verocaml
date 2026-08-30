(** Verification preparation and solver-policy dispatch live in the focused
    private owner.  This module re-exports its authenticated tokens while retaining
    query encoding below; no second verification or descriptor authority is kept
    here.
    The delegated owner is responsible for:
    - validating the SST before recursive-spec preparation;
    - sealing preservation capabilities against the physical program;
    - selecting the accepted solver policy and feature set;
    - checking termination obligations in declaration order;
    - preserving inconclusive outcomes for project preflight; and
    - authenticating exact recursive definition identities.
    The query encoder below is responsible only for:
    - declaring the already-authenticated recursive equations;
    - translating concrete obligations at their exact instantiated sorts;
    - applying prepared activation identities; and
    - returning detached queries and observable counters.
    Keeping these lists explicit makes the wrapper a dispatch surface rather than
    a second verification implementation.  Authentication failures remain owned by
    the delegated verifier and are never reconstructed by the query encoder.
    Solver controls likewise cross this boundary only as authenticated values. *)
type error = Recursive_spec_verification_private.error = {
  span : Diagnostic.span;
  message : string;
}
type prepared = Recursive_spec_verification_private.prepared
type verified = Recursive_spec_verification_private.verified =
  | Verified of {
      prepared : Spec_unfolding_private.prepared;
      preservation_capabilities :
        Recursive_spec_preservation.capability list;
    }
type solver_controls = Recursive_spec_verification_private.solver_controls = {
  force_retry_unknown : bool;
  force_retry_counterexample : bool;
  suppress_original_activation : bool;
  suppress_reached_authority : bool;
}
type verification_outcome =
  Recursive_spec_verification_private.verification_outcome =
  | Verification_verified of verified
  | Verification_inconclusive of Solver_backend.obligation_result
let ( let* ) = Recursive_spec_verification_private.( let* )
let fail =
  Recursive_spec_verification_private.fail
let of_logic =
  Recursive_spec_verification_private.of_logic
let of_backend =
  Recursive_spec_verification_private.of_backend
let same_function_id =
  Recursive_spec_verification_private.same_function_id
let prepare =
  Recursive_spec_verification_private.prepare
let has_definitions =
  Recursive_spec_verification_private.has_definitions
let termination_obligation_count =
  Recursive_spec_verification_private.termination_obligation_count
let required_features =
  Recursive_spec_verification_private.required_features
let resolve_solver_policy =
  Recursive_spec_verification_private.resolve_solver_policy
let verify =
  Recursive_spec_verification_private.verify
let verify_for_preflight =
  Recursive_spec_verification_private.verify_for_preflight
let verify_with_requirements =
  Recursive_spec_verification_private.verify_with_requirements
let prepared =
  Recursive_spec_verification_private.prepared
let preservation_capabilities =
  Recursive_spec_verification_private.preservation_capabilities
let definition =
  Recursive_spec_verification_private.definition
let definition_ids =
  Recursive_spec_verification_private.definition_ids
type query_key = {
  function_id : Sst.function_id;
  type_arguments : Parametric_type.t list;
}
let compare_function_id left right =
  let by_index =
    Int.compare left.Sst.function_index right.Sst.function_index
  in
  if by_index <> 0 then by_index
  else String.compare left.function_name right.function_name
let compare_query_key left right =
  let by_function = compare_function_id left.function_id right.function_id in
  if by_function <> 0 then by_function
  else List.compare Parametric_type.compare left.type_arguments right.type_arguments
type definition_view = {
  key : query_key option;
  definition : Spec_unfolding_private.definition;
  parameters : Sst.parameter list;
  result_type : Sst.typ;
  body : Sst.expression;
  stable : string;
}
let canonical_view definition =
  {
    key = None;
    definition;
    parameters = Spec_unfolding_private.definition_parameters definition;
    result_type = Spec_unfolding_private.definition_result_type definition;
    body = Spec_unfolding_private.definition_body definition;
    stable = Spec_unfolding_private.definition_stable_id definition;
  }
type symbols = {
  builder : Logic_ir.builder;
  fuel : Logic_ir.sort;
  zero : Logic_ir.function_symbol;
  succ : Logic_ir.function_symbol;
  public : Logic_ir.function_symbol;
  helper : Logic_ir.function_symbol;
  enabled : Logic_ir.function_symbol;
  definition : Spec_unfolding_private.definition;
  view : definition_view;
  span : Diagnostic.span;
  sort_registry : Logical_adt_encoding_private.sort_registry;
  functions :
    (string, Logic_ir.sort list * Logic_ir.sort * Logic_ir.function_symbol)
    Hashtbl.t;
  logical_adts : Logical_adt_encoding_private.t option;
}
let a2_builder_constructions = ref 0
let make_sort_registry builder span descriptors =
  let parametric_sorts =
    Parametric_logic_private.create_logic_sort_registry ()
  in
  let parametric_sort =
    Parametric_logic_private.logic_sort parametric_sorts ~builder ~span
  in
  Logical_adt_encoding_private.create_sort_registry ~builder ~descriptors
    ~parametric_sort ~span
let sort symbols =
  Logical_adt_encoding_private.sort_for_sst symbols.sort_registry
let parameter_binding parameter =
  match parameter.Sst.pattern.pattern_desc with
  | Sst.Bind binding -> binding
  | _ -> assert false
let aggregate_closure definition =
  let types = Spec_unfolding_private.definition_types definition in
  let find type_id =
    List.find_opt
      (fun candidate -> candidate.Sst.type_id = type_id)
      types
  in
  let fields = function
    | Sst.Record_definition fields -> fields
    | Sst.Variant_definition constructors ->
        List.concat_map
          (fun (constructor : Sst.constructor_definition) ->
            constructor.constructor_fields)
          constructors
  in
  let rec visit seen = function
    | Sst.Aggregate type_id ->
        if List.exists (fun definition -> definition.Sst.type_id = type_id) seen
        then seen
        else (
          match find type_id with
          | None -> seen
          | Some definition ->
              List.fold_left
                (fun seen (field : Sst.field_definition) ->
                  visit seen field.field_type)
                (definition :: seen) (fields definition.type_kind))
    | Sst.Tuple components ->
        List.fold_left (fun seen (_, typ) -> visit seen typ) seen components
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _ | Sst.Application _ ->
        seen
  in
  let seeds =
    Spec_unfolding_private.definition_result_type definition
    :: List.map
         (fun parameter ->
           (parameter_binding (Sst.require_value_parameter parameter)).Sst.typ)
         (Spec_unfolding_private.definition_parameters definition)
  in
  List.fold_left visit [] seeds
  |> List.sort (fun left right ->
         Int.compare left.Sst.type_id.type_index right.Sst.type_id.type_index)
let declare_symbols_in ?functions ?sort_registry
    ?logical_adts ?view builder ~fuel ~zero ~succ definition =
  let view = Option.value ~default:(canonical_view definition) view in
  let span = Spec_unfolding_private.definition_span definition in
  let sort_registry =
    Option.value
      ~default:
        (make_sort_registry builder span
           (Spec_unfolding_private.definition_parametric_adts definition))
      sort_registry
  in
  let sort = Logical_adt_encoding_private.sort_for_sst sort_registry in
  let declare name domain range =
    of_logic (Logic_ir.declare_function builder ~name ~domain ~range ~span)
  in
  let stable = view.stable in
  let domain =
    List.map
      (fun parameter ->
        sort (parameter_binding (Sst.require_value_parameter parameter)).Sst.typ)
      view.parameters
  in
  let range = sort view.result_type in
  let* public = declare stable domain range in
  let* helper = declare (stable ^ "$fuel") (domain @ [ fuel ]) range in
  let* enabled = declare (stable ^ "$enabled") [ fuel ] Logic_ir.Bool in
  Ok
    {
      builder;
      fuel;
      zero;
      succ;
      public;
      helper;
      enabled;
      definition;
      view;
      span;
      sort_registry;
      functions = Option.value ~default:(Hashtbl.create 16) functions;
      logical_adts;
    }
let declare_fuel builder span =
  let* fuel = of_logic (Logic_ir.declare_sort builder ~name:"Fuel" ~span) in
  let declare name domain range =
    of_logic (Logic_ir.declare_function builder ~name ~domain ~range ~span)
  in
  let* zero = declare "fuel.zero" [] fuel in
  let* succ = declare "fuel.succ" [ fuel ] fuel in
  Ok (fuel, zero, succ)
let declare_symbols definition =
  let builder = Logic_ir.create () in
  let span = Spec_unfolding_private.definition_span definition in
  let* fuel, zero, succ = declare_fuel builder span in
  let parametric_adts =
    Spec_unfolding_private.definition_parametric_adts definition
  in
  let types =
    Spec_unfolding_private.definition_result_type definition
    :: List.map
         (fun parameter ->
           (parameter_binding (Sst.require_value_parameter parameter)).Sst.typ)
         (Spec_unfolding_private.definition_parameters definition)
  in
  let sort_registry = make_sort_registry builder span parametric_adts in
  let* logical_adts =
    Logical_adt_encoding_private.declare_for_types ~registry:sort_registry
      ~descriptors:parametric_adts ~types ~additional_schemas:[]
      ~aggregate_types:[]
    |> Result.map_error (fun message -> { span; message })
  in
  declare_symbols_in ~sort_registry ?logical_adts builder
    ~fuel ~zero ~succ definition
let apply symbols function_ arguments =
  of_logic (Logic_ir.apply ~span:symbols.span function_ arguments)
let fuel_term symbols depth =
  if depth < 0 || depth > 64 then
    fail symbols.span "fuel depth must be in 0..64"
  else
    let* zero = apply symbols symbols.zero [] in
    let rec loop term remaining =
      if remaining = 0 then Ok term
      else
        let* term = apply symbols symbols.succ [ term ] in
        loop term (remaining - 1)
    in
    loop zero depth
let bind_parameters symbols suffix =
  let rec loop binders environment terms = function
    | [] -> Ok (List.rev binders, environment, List.rev terms)
    | parameter :: rest ->
        let binding = parameter_binding (Sst.require_value_parameter parameter) in
        let* binder =
          of_logic
            (Logic_ir.bind symbols.builder
               ~name:(binding.name ^ suffix)
               ~sort:(sort symbols binding.typ)
               ~span:binding.span)
        in
        let term = Logic_ir.bound binder in
        loop (binder :: binders) ((binding.id, term) :: environment)
          (term :: terms) rest
  in
  loop [] [] [] symbols.view.parameters
let lookup symbols environment binding =
  match List.assoc_opt binding.Sst.id environment with
  | Some term -> Ok term
  | None ->
      fail symbols.span "unbound recursive-spec body variable %s#%d"
        binding.name binding.id
let cached_function symbols name domain range span =
  match Hashtbl.find_opt symbols.functions name with
  | Some (existing_domain, existing_range, function_)
    when
      List.length existing_domain = List.length domain
      && List.for_all2 Logic_ir.sort_equal existing_domain domain
      && Logic_ir.sort_equal existing_range range ->
      Ok function_
  | Some _ -> fail span "datatype logic symbol %s has inconsistent sorts" name
  | None ->
      let* function_ =
        of_logic
          (Logic_ir.declare_function symbols.builder ~name ~domain ~range ~span)
      in
      Hashtbl.add symbols.functions name (domain, range, function_);
      Ok function_
let type_suffix typ =
  Parametric_type.to_string typ |> Digest.string |> Digest.to_hex
  |> fun digest -> String.sub digest 0 12
let legacy_range_suffix parametric_adts = function
  | Sst.Int -> "int"
  | Sst.Bool -> "bool"
  | Sst.Aggregate type_id ->
      Printf.sprintf "agg%d_%s" type_id.type_index type_id.type_name
  | Sst.Application _ as application -> (
      match
        Logical_adt_encoding_private.aggregate_type_of_sst parametric_adts
          application
      with
      | Some aggregate ->
          Printf.sprintf "agg%d_%s" aggregate.aggregate_type_index
            aggregate.aggregate_type_name
      | None -> "unsupported")
  | Sst.Unit | Sst.Tuple _ | Sst.Parameter _ -> "unsupported"
let tag_function symbols owner_typ (type_id : Sst.type_id) span =
  let name =
    match owner_typ with
    | Sst.Aggregate _ ->
        Printf.sprintf "verocaml_tag_t%d_%s" type_id.type_index
          type_id.type_name
    | Sst.Application _ ->
        Printf.sprintf "verocaml_tag_t%d_%s_%s" type_id.type_index
          type_id.type_name (type_suffix owner_typ)
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
        assert false
  in
  cached_function symbols name [ sort symbols owner_typ ] Logic_ir.Int span
let selector_function symbols owner_typ (constructor : Sst.constructor_id)
    index typ span =
  match
    Option.bind symbols.logical_adts (fun bindings ->
        Logical_adt_encoding_private.selector_for_constructor bindings
          ~parametric_adts:
            (Spec_unfolding_private.definition_parametric_adts
               symbols.definition)
          ~owner:owner_typ ~constructor ~index
          ~field_name:(Printf.sprintf "$arg%d" index) ~field_type:typ)
  with
  | Some function_ -> Ok function_
  | None ->
      let name =
        match owner_typ with
        | Sst.Aggregate _ ->
            Printf.sprintf "verocaml_sel_t%d_t%d_%s_c%d_%s_i%d_$arg%d_p_r%s"
              constructor.constructor_type.type_index
              constructor.constructor_type.type_index
              constructor.constructor_type.type_name
              constructor.constructor_index constructor.constructor_name index
              index
              (legacy_range_suffix
                 (Spec_unfolding_private.definition_parametric_adts
                    symbols.definition)
                 typ)
        | Sst.Application _ ->
            Printf.sprintf
              "verocaml_sel_t%d_%s_t%d_%s_c%d_%s_i%d_$arg%d_p_r%s"
              constructor.constructor_type.type_index (type_suffix owner_typ)
              constructor.constructor_type.type_index
              constructor.constructor_type.type_name
              constructor.constructor_index constructor.constructor_name index
              index (type_suffix typ)
        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
            assert false
      in
      cached_function symbols name [ sort symbols owner_typ ] (sort symbols typ)
        span
let constructor_function symbols (constructor : Sst.constructor_id)
    result_type argument_types span =
  match
    Option.bind symbols.logical_adts (fun bindings ->
        Option.bind
          (Logical_adt_encoding_private.aggregate_type_of_sst
             (Spec_unfolding_private.definition_parametric_adts
                symbols.definition)
             result_type)
          (fun aggregate ->
            Logical_adt_encoding_private.constructor bindings aggregate
              constructor))
  with
  | Some function_ -> Ok function_
  | None ->
      let name =
        match result_type with
        | Sst.Aggregate _ ->
            Printf.sprintf "verocaml_ctor_t%d_%s_c%d_%s"
              constructor.constructor_type.type_index
              constructor.constructor_type.type_name
              constructor.constructor_index constructor.constructor_name
        | Sst.Application _ ->
            Printf.sprintf "verocaml_ctor_t%d_%s_%s_c%d_%s"
              constructor.constructor_type.type_index
              constructor.constructor_type.type_name (type_suffix result_type)
              constructor.constructor_index constructor.constructor_name
        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
            assert false
      in
      cached_function symbols name (List.map (sort symbols) argument_types)
        (sort symbols result_type) span
let record_constructor_function symbols result_type
    (record_type : Sst.type_id) field_types span =
  let name =
    match result_type with
    | Sst.Aggregate _ ->
        Printf.sprintf "verocaml_record_ctor_t%d_%s" record_type.type_index
          record_type.type_name
    | Sst.Application _ ->
        Printf.sprintf "verocaml_record_ctor_t%d_%s_%s" record_type.type_index
          record_type.type_name (type_suffix result_type)
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
        assert false
  in
  cached_function symbols name (List.map (sort symbols) field_types)
    (sort symbols result_type) span
let record_selector_function symbols owner_typ (record_type : Sst.type_id)
    (field : Sst.field_id) typ span =
  let name =
    match owner_typ with
    | Sst.Aggregate _ ->
        Printf.sprintf "verocaml_sel_t%d_t%d_%s_record_i%d_%s_p_r%s"
          record_type.type_index record_type.type_index record_type.type_name
          field.field_index field.field_name
          (legacy_range_suffix
             (Spec_unfolding_private.definition_parametric_adts
                symbols.definition)
             typ)
    | Sst.Application _ ->
        Printf.sprintf "verocaml_sel_t%d_%s_t%d_%s_record_i%d_%s_p_r%s"
          record_type.type_index (type_suffix owner_typ) record_type.type_index
          record_type.type_name field.field_index field.field_name
          (type_suffix typ)
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
        assert false
  in
  cached_function symbols name [ sort symbols owner_typ ] (sort symbols typ) span
let rec translate_pattern symbols environment scrutinee (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Wildcard -> Ok (environment, Logic_ir.bool ~span:pattern.span true)
  | Sst.Bind binding ->
      Ok
        ( (binding.id, scrutinee) :: environment,
          Logic_ir.bool ~span:pattern.span true )
  | Sst.Constructor_pattern (constructor, arguments) ->
      let* condition =
        let recognizer =
          Option.bind symbols.logical_adts (fun bindings ->
              Logical_adt_encoding_private.recognizer_for_sst bindings
                ~parametric_adts:
                  (Spec_unfolding_private.definition_parametric_adts
                     symbols.definition)
                ~owner:pattern.typ constructor)
        in
        match recognizer with
        | Some recognizer ->
            of_logic
              (Logic_ir.apply ~span:pattern.span recognizer [ scrutinee ])
        | None -> (
            match pattern.typ with
            | Sst.Application _ ->
                fail pattern.span
                  "logical ADT application pattern lacks its native recognizer"
            | Sst.Aggregate _ ->
                let* tag =
                  tag_function symbols pattern.typ constructor.constructor_type
                    pattern.span
                in
                let* tag =
                  of_logic
                    (Logic_ir.apply ~span:pattern.span tag [ scrutinee ])
                in
                of_logic
                  (Logic_ir.equal ~span:pattern.span tag
                     (Logic_ir.int ~span:pattern.span
                        (Z.of_int constructor.constructor_index)))
            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
                assert false)
      in
      let rec loop index environment conditions = function
        | [] ->
            let* combined =
              of_logic
                (Logic_ir.and_ ~span:pattern.span
                   (condition :: List.rev conditions))
            in
            Ok (environment, combined)
        | (argument : Sst.pattern) :: rest ->
            let* selector =
              selector_function symbols pattern.typ constructor index argument.typ
                argument.span
            in
            let* selected =
              of_logic
                (Logic_ir.apply ~span:argument.span selector [ scrutinee ])
            in
            let* environment, nested =
              translate_pattern symbols environment selected argument
            in
            loop (index + 1) environment (nested :: conditions) rest
      in
      loop 0 environment [] arguments
  | Sst.Record_pattern fields ->
      Result.bind
        (List.fold_left
        (fun result (field, (nested : Sst.pattern)) ->
          let* environment, conditions = result in
          let record_type =
            match field.Sst.field_owner with
            | Sst.Record_owner type_id -> type_id
            | Sst.Constructor_owner _ -> assert false
          in
          let* selector =
            record_selector_function symbols pattern.typ record_type field
              nested.Sst.typ
              nested.span
          in
          let* selected =
            of_logic (Logic_ir.apply ~span:nested.span selector [ scrutinee ])
          in
          let* environment, condition =
            translate_pattern symbols environment selected nested
          in
          Ok (environment, condition :: conditions))
        (Ok (environment, [])) fields)
        (fun (environment, conditions) ->
             let* condition =
               of_logic (Logic_ir.and_ ~span:pattern.span (List.rev conditions))
             in
             Ok (environment, condition))
  | Sst.Int_pattern value ->
      let* condition =
        of_logic
          (Logic_ir.equal ~span:pattern.span scrutinee
             (Logic_ir.int ~span:pattern.span value))
      in
      Ok (environment, condition)
  | Sst.Bool_pattern value ->
      let* condition =
        of_logic
          (Logic_ir.equal ~span:pattern.span scrutinee
             (Logic_ir.bool ~span:pattern.span value))
      in
      Ok (environment, condition)
  | Sst.Unit_pattern | Sst.Tuple_pattern _
  | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
      fail pattern.span "pattern escaped the recursive datatype grammar"
let translate_match_pattern translate symbols environment scrutinee pattern =
  match scrutinee.Sst.expression_desc with
  | Sst.Tuple_value _ -> (
      match Recursive_spec_tuple_match_private.plan scrutinee pattern with
      | Error error ->
          fail pattern.Sst.span "%s"
            (Recursive_spec_tuple_match_private.error_to_string error)
      | Ok leaves ->
          let* case_environment, conditions =
            List.fold_left
              (fun result
                   (leaf : Recursive_spec_tuple_match_private.leaf) ->
                let* case_environment, conditions = result in
                let* value = translate environment leaf.expression in
                let* case_environment, condition =
                  translate_pattern symbols case_environment value leaf.pattern
                in
                Ok (case_environment, condition :: conditions))
              (Ok (environment, [])) leaves
          in
          let* condition =
            of_logic
              (Logic_ir.and_ ~span:pattern.Sst.span (List.rev conditions))
          in
          Ok (case_environment, condition))
  | _ ->
      let* scrutinee = translate environment scrutinee in
      translate_pattern symbols environment scrutinee pattern
let translate_match_body translate symbols environment
    (expression : Sst.expression) scrutinee cases =
  let* translated_cases =
    List.fold_left
      (fun result (case : Sst.case) ->
        let* translated = result in
        let* case_environment, condition =
          translate_match_pattern translate symbols environment scrutinee
            case.case_pattern
        in
        let* condition =
          match case.case_guard with
          | None -> Ok condition
          | Some guard ->
              let* guard = translate case_environment guard in
              of_logic
                (Logic_ir.and_ ~span:case.case_span [ condition; guard ])
        in
        let* body = translate case_environment case.case_body in
        Ok ((condition, body) :: translated))
      (Ok []) cases
    |> Result.map List.rev
  in
  match List.rev translated_cases with
  | [] -> fail expression.Sst.span "recursive datatype match has no cases"
  | (_, fallback) :: remaining ->
      List.fold_left
        (fun result (condition, body) ->
          let* else_ = result in
          of_logic
            (Logic_ir.ite ~span:expression.span condition ~then_:body ~else_))
        (Ok fallback) remaining
let rec translate_body symbols environment fuel
    (expression : Sst.expression) =
  let recurse = translate_body symbols environment fuel in
  let binary constructor left right =
    let* left = recurse left in
    let* right = recurse right in
    of_logic (constructor ~span:expression.span left right)
  in
  match expression.expression_desc with
  | Sst.Int_constant value -> Ok (Logic_ir.int ~span:expression.span value)
  | Sst.Bool_constant value -> Ok (Logic_ir.bool ~span:expression.span value)
  | Sst.Variable { binding; _ } -> lookup symbols environment binding
  | Sst.Checked_arithmetic (operation, operands) -> (
      match (operation, operands) with
      | Sst.Add, [ left; right ] -> binary Logic_ir.add left right
      | Sst.Subtract, [ left; right ] -> binary Logic_ir.subtract left right
      | Sst.Negate, [ value ] ->
          let* value = recurse value in
          of_logic (Logic_ir.negate ~span:expression.span value)
      | Sst.Multiply_constant coefficient, [ value ] ->
          let* value = recurse value in
          of_logic (Logic_ir.scale ~span:expression.span coefficient value)
      | Sst.Successor, [ value ] ->
          let* value = recurse value in
          of_logic
            (Logic_ir.add ~span:expression.span value
               (Logic_ir.int ~span:expression.span Z.one))
      | Sst.Predecessor, [ value ] ->
          let* value = recurse value in
          of_logic
            (Logic_ir.subtract ~span:expression.span value
               (Logic_ir.int ~span:expression.span Z.one))
      | Sst.Absolute_value, [ value ] ->
          let* value = recurse value in
          let zero = Logic_ir.int ~span:expression.span Z.zero in
          let* negative =
            of_logic (Logic_ir.less_than ~span:expression.span value zero)
          in
          let* negated =
            of_logic (Logic_ir.negate ~span:expression.span value)
          in
          of_logic
            (Logic_ir.ite ~span:expression.span negative ~then_:negated
               ~else_:value)
      | _ -> assert false)
  | Sst.Compare (comparison, left, right) ->
      let constructor =
        match comparison with
        | Sst.Equal -> Logic_ir.equal
        | Sst.Not_equal -> Logic_ir.distinct
        | Sst.Less_than -> Logic_ir.less_than
        | Sst.Less_or_equal -> Logic_ir.less_or_equal
        | Sst.Greater_than -> Logic_ir.greater_than
        | Sst.Greater_or_equal -> Logic_ir.greater_or_equal
      in
      binary constructor left right
  | Sst.Boolean_not operand ->
      let* operand = recurse operand in
      of_logic (Logic_ir.not_ ~span:expression.span operand)
  | Sst.Boolean_binary (operation, left, right) ->
      let* left = recurse left in
      let* right = recurse right in
      of_logic
        (match operation with
        | Sst.And -> Logic_ir.and_ ~span:expression.span [ left; right ]
        | Sst.Or -> Logic_ir.or_ ~span:expression.span [ left; right ])
  | Sst.Forall quantifier ->
      translate_body_quantifier symbols environment fuel true quantifier
  | Sst.Exists quantifier ->
      translate_body_quantifier symbols environment fuel false quantifier
  | Sst.If (condition, consequent, Some alternative) ->
      let* condition = recurse condition in
      let* then_ = recurse consequent in
      let* else_ = recurse alternative in
      of_logic (Logic_ir.ite ~span:expression.span condition ~then_ ~else_)
  | Sst.Let (bindings, body) ->
      let* environment =
        List.fold_left
          (fun result (pattern, value) ->
            let* environment = result in
            let* value = translate_body symbols environment fuel value in
            let* environment, _condition =
              translate_pattern symbols environment value pattern
            in
            Ok environment)
          (Ok environment) bindings
      in
      translate_body symbols environment fuel body
  | Sst.Constructor_value { constructor; arguments } ->
      let* argument_terms =
        List.fold_left
          (fun result argument ->
            let* terms = result in
            let* term = recurse argument in
            Ok (term :: terms))
          (Ok []) arguments
        |> Result.map List.rev
      in
      let* constructor_function =
        constructor_function symbols constructor expression.typ
          (List.map (fun argument -> argument.Sst.typ) arguments)
          expression.span
      in
      of_logic
        (Logic_ir.apply ~span:expression.span constructor_function
           argument_terms)
  | Sst.Record_value { record_type; fields } ->
      let* terms =
        List.fold_left
          (fun result (_, value) ->
            let* terms = result in
            let* term = recurse value in
            Ok (term :: terms))
          (Ok []) fields
        |> Result.map List.rev
      in
      let* constructor =
        record_constructor_function symbols expression.typ record_type
          (List.map (fun (_, value) -> value.Sst.typ) fields)
          expression.span
      in
      of_logic (Logic_ir.apply ~span:expression.span constructor terms)
  | Sst.Field_read { record; field } ->
      let owner_typ = record.Sst.typ in
      let* record = recurse record in
      let* selector =
        match field.field_owner with
        | Sst.Constructor_owner constructor ->
            selector_function symbols owner_typ constructor field.field_index
              expression.typ expression.span
        | Sst.Record_owner record_type ->
            record_selector_function symbols owner_typ record_type field
              expression.typ expression.span
      in
      of_logic (Logic_ir.apply ~span:expression.span selector [ record ])
  | Sst.Match (scrutinee, cases) ->
      translate_match_body
        (fun environment expression ->
          translate_body symbols environment fuel expression)
        symbols environment expression scrutinee cases
  | Sst.Direct_call _
    when Spec_function_sst_private.application expression <> None ->
      let application =
        Option.get (Spec_function_sst_private.application expression)
      in
      let* function_term = recurse application.application_function in
      let* argument_term = recurse application.application_argument in
      let* name =
        Spec_function_logic_private.application_symbol_name
          ~arrow:application.application_arrow
          ~result_type:application.application_result ~span:expression.span
        |> Result.map_error (fun message ->
               { span = expression.span; message })
      in
      let* function_ =
        cached_function symbols name
          [
            sort symbols application.application_arrow;
            sort symbols application.application_argument.typ;
          ]
          (sort symbols application.application_result)
          expression.span
      in
      apply symbols function_ [ function_term; argument_term ]
  | Sst.Direct_call
      {
        callee;
        arguments;
        recursive = true;
        call_form = Sst.Specification_call;
        _;
      }
    when
      same_function_id callee
        (Spec_unfolding_private.definition_id symbols.definition) ->
      let* arguments =
        List.fold_left
          (fun result argument ->
            let _, argument = Sst.require_value_argument argument in
            let* arguments = result in
            let* argument = recurse argument in
            Ok (argument :: arguments))
          (Ok []) arguments
      in
      apply symbols symbols.helper (List.rev arguments @ [ fuel ])
  | _ ->
      fail expression.span
        "expression escaped the authenticated recursive-spec body grammar"
and translate_body_quantifier symbols environment fuel universal quantifier =
  let metadata = quantifier.Sst.quantifier_metadata in
  let binder = quantifier.quantifier_binder in
  let expected_kind =
    if universal then Logic_quantifier_private.Forall
    else Logic_quantifier_private.Exists
  in
  if Logic_quantifier_private.kind metadata <> expected_kind then
    fail binder.span "recursive-spec quantifier metadata kind is inconsistent"
  else
    let* logic_binder =
      of_logic
        (Logic_ir.bind symbols.builder ~name:binder.name
           ~sort:(sort symbols binder.typ) ~span:binder.span)
    in
    let bound = Logic_ir.bound logic_binder in
    let scoped = (binder.id, bound) :: environment in
    let* body =
      translate_body symbols scoped fuel quantifier.quantifier_body
    in
    let* body =
      if binder.typ <> Sst.Int then Ok body
      else
        let lower = Logic_ir.int ~span:binder.span (Z.of_int min_int)
        and upper = Logic_ir.int ~span:binder.span (Z.of_int max_int) in
        let* above =
          of_logic (Logic_ir.less_or_equal ~span:binder.span lower bound)
        in
        let* below =
          of_logic (Logic_ir.less_or_equal ~span:binder.span bound upper)
        in
        let* range =
          of_logic (Logic_ir.and_ ~span:binder.span [ above; below ])
        in
        if universal then
          of_logic (Logic_ir.implies ~span:binder.span range body)
        else of_logic (Logic_ir.and_ ~span:binder.span [ range; body ])
    in
    let qid = Logic_quantifier_private.qid metadata
    and skid = Logic_quantifier_private.skid metadata in
    if universal then
      match quantifier.quantifier_trigger with
      | None ->
          fail binder.span "recursive-spec universal lost its explicit trigger"
      | Some trigger ->
          let* trigger = translate_body_trigger symbols scoped fuel trigger in
          of_logic
            (Logic_ir.forall_term symbols.builder ~binders:[ logic_binder ] ~body
               ~trigger ~qid ~skid ~span:(Logic_quantifier_private.span metadata))
    else
      match quantifier.quantifier_trigger with
      | Some _ ->
          fail binder.span "recursive-spec existential acquired a trigger"
      | None ->
          of_logic
            (Logic_ir.exists_term symbols.builder ~binders:[ logic_binder ] ~body
               ~qid ~skid ~span:(Logic_quantifier_private.span metadata))
and translate_body_trigger symbols environment fuel expression =
  match expression.Sst.expression_desc with
  | Sst.Direct_call
      {
        call_form = (Sst.Specification_call | Sst.Proof_call);
        callee;
        type_arguments;
        arguments = _ :: _ as arguments;
        _;
      } ->
      let* terms =
        List.fold_left
          (fun result argument ->
            let* terms = result in
            let _, value = Sst.require_value_argument argument in
            let* term = translate_body symbols environment fuel value in
            Ok (term :: terms))
          (Ok []) arguments
        |> Result.map List.rev
      in
      let values =
        List.map
          (fun argument ->
            snd (Sst.require_value_argument argument))
          arguments
      in
      let* function_ =
        cached_function symbols
          (Vir.specification_application_name callee type_arguments [])
          (List.map (fun value -> sort symbols value.Sst.typ) values)
          Logic_ir.Bool expression.span
      in
      of_logic (Logic_ir.apply ~span:expression.span function_ terms)
  | _ ->
      fail expression.span
        "recursive-spec universal trigger is not application-headed"
let make_axioms symbols =
  incr a2_builder_constructions;
  let stable = symbols.view.stable in
  let make_fuel_binder suffix =
    of_logic
      (Logic_ir.bind symbols.builder ~name:("fuel" ^ suffix)
         ~sort:symbols.fuel ~span:symbols.span)
  in
  let* a1_binders, _, a1_arguments = bind_parameters symbols ".a1" in
  let* a1_fuel = make_fuel_binder ".a1" in
  let a1_fuel_term = Logic_ir.bound a1_fuel in
  let* a1_left =
    apply symbols symbols.helper (a1_arguments @ [ a1_fuel_term ])
  in
  let* zero = apply symbols symbols.zero [] in
  let* a1_right = apply symbols symbols.helper (a1_arguments @ [ zero ]) in
  let* a1_body = of_logic (Logic_ir.equal ~span:symbols.span a1_left a1_right) in
  let* a1 =
    of_logic
      (Logic_ir.forall symbols.builder
         ~binders:(a1_binders @ [ a1_fuel ]) ~body:a1_body
         ~patterns:[ [ a1_left ] ] ~qid:(stable ^ ".fuel-invariance")
         ~skid:(stable ^ ".fuel-invariance.skolem") ~span:symbols.span)
  in
  let* a2_binders, a2_environment, a2_arguments =
    bind_parameters symbols ".a2"
  in
  let* a2_fuel = make_fuel_binder ".a2" in
  let a2_fuel_term = Logic_ir.bound a2_fuel in
  let* successor = apply symbols symbols.succ [ a2_fuel_term ] in
  let* a2_left = apply symbols symbols.helper (a2_arguments @ [ successor ]) in
  let* expanded =
    translate_body symbols a2_environment a2_fuel_term symbols.view.body
  in
  let* a2_body = of_logic (Logic_ir.equal ~span:symbols.span a2_left expanded) in
  let* a2 =
    of_logic
      (Logic_ir.forall symbols.builder
         ~binders:(a2_binders @ [ a2_fuel ]) ~body:a2_body
         ~patterns:[ [ a2_left ] ] ~qid:(stable ^ ".fuel-body")
         ~skid:(stable ^ ".fuel-body.skolem") ~span:symbols.span)
  in
  let* a3_binders, _, a3_arguments = bind_parameters symbols ".a3" in
  let* a3_fuel = make_fuel_binder ".a3" in
  let a3_fuel_term = Logic_ir.bound a3_fuel in
  let* enabled = apply symbols symbols.enabled [ a3_fuel_term ] in
  let* public = apply symbols symbols.public a3_arguments in
  let* helper =
    apply symbols symbols.helper (a3_arguments @ [ a3_fuel_term ])
  in
  let* equality = of_logic (Logic_ir.equal ~span:symbols.span public helper) in
  let* a3_body =
    of_logic (Logic_ir.implies ~span:symbols.span enabled equality)
  in
  let* a3 =
    of_logic
      (Logic_ir.forall symbols.builder
         ~binders:(a3_binders @ [ a3_fuel ]) ~body:a3_body
         ~patterns:[ [ public; enabled ] ] ~qid:(stable ^ ".public-link")
         ~skid:(stable ^ ".public-link.skolem") ~span:symbols.span)
  in
  Ok [ a1; a2; a3 ]
let ground_arguments symbols arguments =
  let expected =
    List.map Sst.require_value_parameter symbols.view.parameters
  in
  if List.length arguments <> List.length expected then
    fail symbols.span "recursive-spec ground argument count mismatch"
  else
    List.fold_left2
      (fun result argument parameter ->
        let* terms = result in
        let expected = (parameter_binding parameter).Sst.typ in
        match (expected, argument) with
        | Sst.Int, `Int value ->
            Ok (Logic_ir.int ~span:symbols.span value :: terms)
        | Sst.Bool, `Bool value ->
            Ok (Logic_ir.bool ~span:symbols.span value :: terms)
        | _ -> fail symbols.span "recursive-spec ground argument sort mismatch")
      (Ok []) arguments expected
    |> Result.map List.rev
type extra =
  | No_extra
  | Depth_equality of
      [ `Int of Z.t | `Bool of bool ] list * int * int
  | Public_link of [ `Int of Z.t | `Bool of bool ] list * int
let build_query definition depths extra =
  let* symbols = declare_symbols definition in
  let* axioms = make_axioms symbols in
  let* activations =
    List.fold_left
      (fun result depth ->
        let* activations = result in
        let* fuel = fuel_term symbols depth in
        let* enabled = apply symbols symbols.enabled [ fuel ] in
        Ok (enabled :: activations))
      (Ok []) depths
  in
  let* extra_assertions =
    match extra with
    | No_extra -> Ok []
    | Depth_equality (arguments, left_depth, right_depth) ->
        let* arguments = ground_arguments symbols arguments in
        let* left_fuel = fuel_term symbols left_depth in
        let* right_fuel = fuel_term symbols right_depth in
        let* left =
          apply symbols symbols.helper (arguments @ [ left_fuel ])
        in
        let* right =
          apply symbols symbols.helper (arguments @ [ right_fuel ])
        in
        let* unequal =
          of_logic (Logic_ir.distinct ~span:symbols.span left right)
        in
        Ok [ unequal ]
    | Public_link (arguments, depth) ->
        let* arguments = ground_arguments symbols arguments in
        let* fuel = fuel_term symbols depth in
        let* enabled = apply symbols symbols.enabled [ fuel ] in
        let* public = apply symbols symbols.public arguments in
        let* helper = apply symbols symbols.helper (arguments @ [ fuel ]) in
        let* unequal =
          of_logic (Logic_ir.distinct ~span:symbols.span public helper)
        in
        Ok [ enabled; unequal ]
  in
  of_logic
    (Logic_ir.query symbols.builder ~axioms
       ~assertions:(List.rev activations @ extra_assertions)
       ~requires:required_features ~span:symbols.span)
let base_query verified function_id =
  let* definition = definition verified function_id in
  build_query definition [] No_extra
let activated_query verified function_id ~depths =
  let* definition = definition verified function_id in
  build_query definition depths No_extra
let depth_equality_query verified function_id ~arguments ~left_depth
    ~right_depth =
  let* definition = definition verified function_id in
  build_query definition []
    (Depth_equality (arguments, left_depth, right_depth))
let public_link_query verified function_id ~arguments ~depth =
  let* definition = definition verified function_id in
  build_query definition [] (Public_link (arguments, depth))
let same_vir_function (left : Sst.function_id) (right : Sst.function_id) =
  compare_function_id left right = 0
let vir_aggregate_type (type_id : Sst.type_id) =
  {
    Vir.aggregate_type_index = type_id.type_index;
    aggregate_type_name = type_id.type_name;
    aggregate_type_arguments = [];
  }
type reached_recursive_application = {
  reached_key : query_key;
  reached_argument_types : Sst.typ list;
  reached_result_type : Sst.typ;
  reached_span : Diagnostic.span;
  reached_identity : Recursive_spec_application_identity.t option;
}
let validate_obligation_record_metadata ~(program : Sst.program)
    (obligation : Vir.obligation) =
  let span = obligation.span in
  let reached = ref [] in
  let argument_type expected =
    Recursive_spec_term_private.argument_type
      ~parametric_adts:program.parametric_adts ~expected
  in
  let note callee type_arguments arguments result_type call_span identity =
    let reached_argument_types =
      Recursive_spec_term_private.argument_types ~program ~callee
        ~type_arguments arguments
    in
    reached :=
      {
        reached_key = { function_id = callee; type_arguments };
        reached_argument_types;
        reached_result_type = result_type;
        reached_span = call_span;
        reached_identity = identity;
      }
      :: !reached
  in
  let record_definition record_type =
    match
      List.find_opt
        (fun (definition : Sst.type_definition) ->
          definition.type_id = record_type)
        program.types
    with
    | Some { type_kind = Sst.Record_definition fields; _ } -> Ok fields
    | Some { type_kind = Sst.Variant_definition _; _ } | None ->
        fail span "aggregate record has the wrong nominal program type"
  in
  let rec aggregate (term : Vir.aggregate_term) =
    match term.aggregate_desc with
    | Vir.Aggregate_symbol _ -> Ok ()
    | Vir.Aggregate_imported_model_application _ ->
        fail span
          "retained aggregate model application cannot enter recursive encoding"
    | Vir.Aggregate_selector (_, source) -> aggregate source
    | Vir.Aggregate_constructor { arguments = values; _ } ->
        arguments values
    | Vir.Aggregate_recursive_spec_application
        {
          callee;
          type_arguments;
          arguments = values;
          result_type;
          span = call_span;
          application_identity;
        } ->
        if
          result_type <> term.aggregate_type
          || not
               (Recursive_spec_application_identity.authenticate
                  application_identity)
        then fail call_span "aggregate recursive application identity is forged"
        else (
          note callee type_arguments values
            (argument_type None (Vir.Recursive_aggregate_argument term))
            call_span (Some application_identity);
          arguments values)
    | Vir.Aggregate_record { record_type; fields } ->
        let* expected_fields = record_definition record_type in
        if term.aggregate_type <> vir_aggregate_type record_type then
          fail span "aggregate record has a mismatched result type"
        else if
          List.length
            (List.sort_uniq compare
               (List.map
                  (fun (field, _) -> field.Sst.field_index)
                  fields))
          <> List.length fields
        then fail span "aggregate record has duplicate fields"
        else if List.length fields <> List.length expected_fields then
          fail span "aggregate record has an incomplete field vector"
        else
          List.fold_left2
            (fun result (field, value)
                 (expected : Sst.field_definition) ->
              let* () = result in
              let* () =
                match field.Sst.field_owner with
                | Sst.Record_owner owner when owner = record_type -> Ok ()
                | Sst.Record_owner _ | Sst.Constructor_owner _ ->
                    fail span "aggregate record field has a mismatched owner"
              in
              if field.field_index <> expected.field_id.field_index then
                fail span
                  "aggregate record field has a mismatched ordinal/index"
              else if field <> expected.field_id then
                fail span "aggregate record field metadata is not exact"
              else if argument_type (Some expected.field_type) value
                      <> expected.field_type
              then
                fail span "aggregate record field has a mismatched type/sort"
              else argument value)
            (Ok ()) fields expected_fields
    | Vir.Aggregate_conditional (condition, consequent, alternative) ->
        let* () = boolean condition in
        let* () = aggregate consequent in
        aggregate alternative
    | Vir.Aggregate_symbolic_application application ->
        arguments (Symbolic_application_private.arguments application)
  and argument = function
    | Vir.Recursive_integer_argument term -> integer term
    | Vir.Recursive_boolean_argument term -> boolean term
    | Vir.Recursive_aggregate_argument term -> aggregate term
    | Vir.Recursive_parametric_argument term -> (
        match Parametric_logic_private.validate term with
        | Ok () -> (
            match term.parametric_desc with
            | Vir.Parametric_symbolic_application application ->
                arguments
                  (Symbolic_application_private.arguments application)
            | Parametric_symbol _ | Parametric_selector _
            | Parametric_conditional _ ->
                Ok ())
        | Error message -> fail span "%s" message)
  and arguments values =
    List.fold_left
      (fun result value ->
        let* () = result in
        argument value)
      (Ok ()) values
  and integer = function
    | Vir.Integer_add (left, right) | Vir.Integer_subtract (left, right) ->
        let* () = integer left in
        integer right
    | Vir.Integer_negate value
    | Vir.Integer_multiply_constant (_, value)
    | Vir.Integer_absolute_value value ->
        integer value
    | Vir.Integer_conditional (condition, consequent, alternative) ->
        let* () = boolean condition in
        let* () = integer consequent in
        integer alternative
    | Vir.Integer_rank_project (_, value)
    | Vir.Aggregate_tag (_, value)
    | Vir.Integer_selector (_, value) ->
        aggregate value
    | Vir.Integer_recursive_spec_application
        { callee; type_arguments; arguments = values; span = call_span } ->
        note callee type_arguments values Sst.Int call_span None;
        arguments values
    | Vir.Integer_symbolic_application application ->
        arguments (Symbolic_application_private.arguments application)
    | Vir.Integer_constant _ | Vir.Integer_symbol _ -> Ok ()
  and boolean = function
    | Vir.Forall_term quantifier | Vir.Exists_term quantifier ->
        let* () = boolean quantifier.boolean_quantifier_body in
        (match quantifier.boolean_quantifier_trigger with
        | None -> Ok ()
        | Some trigger -> application trigger)
    | Vir.Boolean_not value -> boolean value
    | Vir.Boolean_and (left, right)
    | Vir.Boolean_or (left, right)
    | Vir.Boolean_equal (left, right)
    | Vir.Boolean_not_equal (left, right) ->
        let* () = boolean left in
        boolean right
    | Vir.Integer_compare (_, left, right) ->
        let* () = integer left in
        integer right
    | Vir.Boolean_selector (_, value)
    | Vir.Boolean_invariant_application { value; _ } ->
        aggregate value
    | Vir.Aggregate_equal (left, right) ->
        let* () = aggregate left in
        aggregate right
    | Vir.Boolean_recursive_spec_application
        { callee; type_arguments; arguments = values; span = call_span } ->
        note callee type_arguments values Sst.Bool call_span None;
        arguments values
    | Vir.Boolean_specification_application { arguments = values; _ } ->
        arguments values
    | Vir.Boolean_symbolic_application application ->
        arguments (Symbolic_application_private.arguments application)
    | Vir.Callback_requires application ->
        arguments application.arguments
    | Vir.Callback_ensures { application; result } ->
        arguments (application.arguments @ [ result ])
    | Vir.Parametric_equal (left, right) ->
        Parametric_logic_private.fold_conditions_result (Ok ()) boolean left
          right
    | Vir.Boolean_constant _ | Vir.Boolean_symbol _
    | Vir.Logical_adt_schema _ ->
        Ok ()
  and application = function
    | Vir.Integer_application term -> integer term
    | Boolean_application term -> boolean term
    | Aggregate_application term -> aggregate term
    | Parametric_application term ->
        argument (Vir.Recursive_parametric_argument term)
  in
  let* () =
    match obligation.kind with
    | Vir.Arithmetic_safety { mathematical_result; _ } ->
        integer mathematical_result
    | Vir.Assertion _ | Vir.Local_assertion _ | Vir.Postcondition _
    | Vir.Call_precondition _ | Vir.Callback_precondition _
    | Vir.Invariant_validity _ | Vir.Entry_measure_nonnegative _
    | Vir.Recursive_call_measure_nonnegative _
    | Vir.Recursive_call_strict_descent _ ->
        Ok ()
  in
  let* () =
    List.fold_left
      (fun result term ->
        let* () = result in
        boolean term)
      (Ok ())
      (obligation.assumptions @ obligation.required_preceding_safety
     @ obligation.path_condition @ [ obligation.goal ])
  in
  Ok (List.rev !reached)
let proof_query_constructions = ref 0
let suppress_reached_aggregate_authority_for_testing = ref false
type nullary_branch_specialization = {
  application : Vir.aggregate_term;
  result_constructor : Sst.constructor_id;
}
let specialize_definition_view ~program ~definitions ~applications ordinal key =
  let application_span = (List.hd applications).reached_span in
  let* definition =
    match
      List.find_opt
        (fun definition ->
          same_vir_function
            (Spec_unfolding_private.definition_id definition)
            key.function_id)
        definitions
    with
    | Some definition -> Ok definition
    | None ->
        fail application_span
          "recursive specification %s#%d is absent from verified authority"
          key.function_id.function_name key.function_id.function_index
  in
  let* source =
    match
      List.find_opt
        (fun (source : Sst.function_definition) ->
          same_vir_function source.function_id key.function_id)
        program.Sst.functions
    with
    | Some ({ body = Sst.Recursive_spec_definition _; _ } as source) ->
        Ok source
    | Some _ | None ->
        fail application_span
          "recursive specification source identity is stale or foreign"
  in
  let instantiate typ =
    Parametric_lowering_private.instantiate ~binders:source.type_binders
      ~arguments:key.type_arguments typ
    |> Result.map_error (fun message -> { span = application_span; message })
  in
  let* result_type =
    instantiate (Spec_unfolding_private.definition_result_type definition)
  in
  let substitutions = List.combine source.type_binders key.type_arguments in
  let substitute = Parametric_type.substitute substitutions in
  let* parameters =
    List.fold_left
      (fun result parameter ->
        let* parameters = result in
        match parameter with
        | Sst.Value_parameter value ->
            let* _ = instantiate value.pattern.typ in
            let optional_default =
              Option.map
                (fun default ->
                  {
                    Sst.optional_pattern =
                      Sst.map_pattern_types substitute
                        default.Sst.optional_pattern;
                    optional_expression =
                      Sst.map_expression_types substitute
                        default.optional_expression;
                  })
                value.optional_default
            in
            Ok
              (Sst.Value_parameter
                 {
                   value with
                   pattern = Sst.map_pattern_types substitute value.pattern;
                   optional_default;
                 }
              :: parameters)
        | Sst.Callback_parameter _ ->
            fail application_span
              "recursive specification application has a callback parameter")
      (Ok []) (Spec_unfolding_private.definition_parameters definition)
    |> Result.map List.rev
  in
  let expected_arguments =
    List.map
      (fun parameter ->
        (parameter_binding (Sst.require_value_parameter parameter)).Sst.typ)
      parameters
  in
  let* () =
    List.fold_left
      (fun result application ->
        let* () = result in
        if
          List.length application.reached_argument_types
            = List.length expected_arguments
          && List.for_all2 Parametric_type.equal
               application.reached_argument_types expected_arguments
          && Parametric_type.equal application.reached_result_type result_type
        then Ok ()
        else
          fail application.reached_span
            "recursive specification application has an exact domain/range \
             mismatch")
      (Ok ()) applications
  in
  let stable =
    let base = Spec_unfolding_private.definition_stable_id definition in
    if source.type_binders = [] then base
    else Printf.sprintf "%s$app%d" base ordinal
  in
  Ok
    {
      key = Some key;
      definition;
      parameters;
      result_type;
      body =
        Sst.map_expression_types substitute
          (Spec_unfolding_private.definition_body definition);
      stable;
    }
type proof_query_preparation = {
  definitions : Spec_unfolding_private.definition list;
  instances : definition_view list;
  program : Sst.program;
  span : Diagnostic.span;
  reached_application_identities : Recursive_spec_application_identity.t list;
  activations : Spec_unfolding.activation list;
}
type proof_query_sort_environment = {
  builder : Logic_ir.builder;
  span : Diagnostic.span;
  aggregate_sort : Vir.aggregate_type -> Logic_ir.sort;
  parametric_sort : Parametric_type.binder -> Logic_ir.sort;
  logical_adts : Logical_adt_encoding_private.t option;
  sort_registry : Logical_adt_encoding_private.sort_registry;
  datatype_functions :
    ( string,
      Logic_ir.sort list * Logic_ir.sort * Logic_ir.function_symbol )
    Hashtbl.t;
  fuel : Logic_ir.sort;
  zero : Logic_ir.function_symbol;
  succ : Logic_ir.function_symbol;
}
type proof_query_translation_environment = {
  builder : Logic_ir.builder;
  span : Diagnostic.span;
  reached_application_identities : Recursive_spec_application_identity.t list;
  aggregate_sort : Vir.aggregate_type -> Logic_ir.sort;
  parametric_sort : Parametric_type.binder -> Logic_ir.sort;
  find_symbols :
    Sst.function_id ->
    Parametric_type.t list ->
    (symbols, error) result;
  axioms : Logic_ir.axiom list;
  activation_terms : Logic_ir.term list;
  rank_domains : (string, Logic_ir.rank_domain) Hashtbl.t;
  rank_axioms : Logic_ir.axiom list ref;
  bound_symbols : (int * Logic_ir.binder) list;
  declare_vir_symbol :
    Vir.sort -> Vir.symbol -> (Logic_ir.function_symbol, error) result;
  declare_aggregate_function :
    string ->
    Logic_ir.sort list ->
    Logic_ir.sort ->
    Diagnostic.span ->
    (Logic_ir.function_symbol, error) result;
  logical_selector_function :
    Vir.selector ->
    Logic_ir.sort ->
    Logic_ir.sort ->
    (Logic_ir.function_symbol, error) result;
  logical_constructor_function :
    Vir.aggregate_type ->
    Sst.constructor_id ->
    Logic_ir.sort list ->
    (Logic_ir.function_symbol, error) result;
  aggregate_tag :
    Vir.aggregate_type -> Logic_ir.term -> (Logic_ir.term, error) result;
}
let prepare_proof_query verified ~activations (obligation : Vir.obligation)
    solver_controls =
  let definitions = Spec_unfolding_private.definitions (prepared verified) in
  let span = obligation.span in
  let program =
    Spec_unfolding_private.validated_program (prepared verified)
    |> Sst_validation.program
  in
  let* reached_applications =
    validate_obligation_record_metadata ~program obligation
  in
  let reached_application_identities =
    if
      Option.fold
        ~none:!suppress_reached_aggregate_authority_for_testing
        ~some:(fun controls -> controls.suppress_reached_authority)
        solver_controls
    then []
    else
      List.filter_map
        (fun application -> application.reached_identity)
        reached_applications
  in
  let* reached_activations =
    List.fold_left
      (fun result application ->
        let* reached = result in
        match application.reached_identity with
        | None -> Ok reached
        | Some _ ->
            let function_id = application.reached_key.function_id in
            if
              List.exists
                (fun definition ->
                  same_vir_function
                    (Spec_unfolding_private.definition_id definition)
                    function_id)
                definitions
            then
              Ok
                ({
                   Spec_unfolding.function_id;
                   depth = 1;
                   span = application.reached_span;
                 }
                :: reached)
            else
              fail application.reached_span
                "aggregate recursive application lacks verified totality authority")
      (Ok []) reached_applications
  in
  let activations =
    List.sort_uniq
      (fun (left : Spec_unfolding.activation) right ->
        let by_function =
          compare_function_id left.function_id right.function_id
        in
        if by_function <> 0 then by_function
        else Int.compare left.depth right.depth)
      (List.filter
         (fun (activation : Spec_unfolding.activation) -> activation.depth > 0)
         (activations @ reached_activations))
  in
  let exact_program_aggregate_types =
    List.map
      (fun (definition : Sst.type_definition) ->
        vir_aggregate_type definition.type_id)
      program.Sst.types
  in
  let exact_program_aggregate aggregate =
    List.exists (( = ) aggregate) exact_program_aggregate_types
    ||
    match aggregate.Vir.aggregate_type_arguments with
    | [] -> false
    | arguments ->
        List.exists
          (fun descriptor ->
            let type_id = Parametric_adt.type_id descriptor in
            type_id.type_index = aggregate.aggregate_type_index
            &&
            let application =
              Parametric_type.Application
                (Parametric_adt.type_constructor descriptor, arguments)
            in
            Parametric_adt.same_application descriptor application)
          program.parametric_adts
  in
  let* () =
    List.fold_left
      (fun result aggregate ->
        let* () = result in
        if exact_program_aggregate aggregate then Ok ()
        else
          fail span "missing exact proof aggregate sort %s#%d"
            aggregate.Vir.aggregate_type_name aggregate.aggregate_type_index)
      (Ok ())
      (Vir.obligation_aggregate_types obligation)
  in
  let keys =
    List.map (fun application -> application.reached_key) reached_applications
    |> List.sort_uniq compare_query_key
  in
  let* instances =
    List.fold_left
      (fun result (ordinal, key) ->
        let* instances = result in
        let applications =
          List.filter
            (fun application ->
              compare_query_key application.reached_key key = 0)
            reached_applications
        in
        let* view =
          specialize_definition_view ~program ~definitions ~applications
            ordinal key
        in
        Ok (view :: instances))
      (Ok []) (List.mapi (fun ordinal key -> (ordinal, key)) keys)
    |> Result.map List.rev
  in
  Ok
    {
      definitions;
      instances;
      program;
      span;
      reached_application_identities;
      activations;
    }
let declare_proof_query_sorts (preparation : proof_query_preparation)
    (obligation : Vir.obligation) =
  let builder = Logic_ir.create () in
  let span = preparation.span in
  let program = preparation.program in
  let* fuel, zero, succ = declare_fuel builder span in
  let definition_components =
    preparation.definitions
    |> List.concat_map aggregate_closure
    |> List.map (fun definition -> vir_aggregate_type definition.Sst.type_id)
  in
  let program_components =
    program.Sst.types
    |> List.map (fun (definition : Sst.type_definition) ->
           vir_aggregate_type definition.type_id)
  in
  let aggregate_components =
    definition_components @ program_components
    @ Vir.obligation_aggregate_types obligation
    |> List.sort_uniq (fun left right ->
        let by_index =
          Int.compare left.Vir.aggregate_type_index right.aggregate_type_index
        in
        if by_index <> 0 then by_index
        else String.compare left.aggregate_type_name right.aggregate_type_name)
  in
  let sort_registry = make_sort_registry builder span program.parametric_adts in
  let definition_types =
    preparation.instances
    |> List.concat_map (fun view ->
           view.result_type
           :: List.map
                (fun parameter ->
                  (parameter_binding (Sst.require_value_parameter parameter))
                    .Sst.typ)
                view.parameters)
  in
  let additional_schemas = Logical_adt_encoding_private.schemas obligation in
  let* logical_adts =
    Logical_adt_encoding_private.declare_for_types ~registry:sort_registry
      ~descriptors:program.parametric_adts ~types:definition_types
      ~additional_schemas ~aggregate_types:aggregate_components
    |> Result.map_error (fun message -> { span; message })
  in
  let aggregate_sort =
    Logical_adt_encoding_private.sort_for_aggregate sort_registry
  and parametric_sort =
    Logical_adt_encoding_private.parametric_sort sort_registry
  in
  Ok
    {
      builder;
      span;
      aggregate_sort;
      parametric_sort;
      logical_adts;
      sort_registry;
      datatype_functions = Hashtbl.create 32;
      fuel;
      zero;
      succ;
    }
let declare_proof_query_environment (preparation : proof_query_preparation)
    (sorts : proof_query_sort_environment) (obligation : Vir.obligation) =
  let* symbols =
    List.fold_left
      (fun result view ->
        let* symbols = result in
        let* declared =
          declare_symbols_in ~functions:sorts.datatype_functions
            ~sort_registry:sorts.sort_registry ?logical_adts:sorts.logical_adts
            ~view sorts.builder
            ~fuel:sorts.fuel ~zero:sorts.zero ~succ:sorts.succ view.definition
        in
        Ok (declared :: symbols))
      (Ok []) preparation.instances
    |> Result.map List.rev
  in
  let find_symbols function_id type_arguments =
    let key = { function_id; type_arguments } in
    match
      List.find_opt
        (fun symbols ->
          match symbols.view.key with
          | Some candidate -> compare_query_key candidate key = 0
          | None -> false)
        symbols
    with
    | Some symbols -> Ok symbols
    | None ->
        fail preparation.span
          "recursive specification %s#%d is absent from verified authority"
          function_id.function_name function_id.function_index
  in
  let* axioms =
    List.fold_left
      (fun result symbols ->
        let* axioms = result in
        let function_id =
          Spec_unfolding_private.definition_id symbols.definition
        in
        if
          List.exists
            (fun (activation : Spec_unfolding.activation) ->
              same_vir_function activation.function_id function_id)
            preparation.activations
        then
          let* next = make_axioms symbols in
          Ok (axioms @ next)
        else Ok axioms)
      (Ok []) symbols
  in
  let* activation_terms =
    List.fold_left
      (fun result (activation : Spec_unfolding.activation) ->
        let* terms = result in
        List.fold_left
          (fun result symbols ->
            let* terms = result in
            if
              same_vir_function
                (Spec_unfolding_private.definition_id symbols.definition)
                activation.function_id
            then
              let* fuel = fuel_term symbols activation.depth in
              let* enabled = apply symbols symbols.enabled [ fuel ] in
              Ok (enabled :: terms)
            else Ok terms)
          (Ok terms) symbols)
      (Ok []) preparation.activations
    |> Result.map List.rev
  in
  let declared_symbols = Hashtbl.create 32 in
  let declare_vir_symbol expected_sort (symbol : Vir.symbol) =
    if symbol.sort <> expected_sort then
      fail preparation.span "VIR symbol %s#%d has inconsistent scalar sort"
        symbol.source_name symbol.symbol_id
    else
      let key = (symbol.symbol_id, expected_sort) in
      match Hashtbl.find_opt declared_symbols key with
      | Some function_ -> Ok function_
      | None ->
          let range =
            match expected_sort with
            | Vir.Integer -> Logic_ir.Int
            | Vir.Boolean -> Logic_ir.Bool
            | Vir.Aggregate aggregate -> sorts.aggregate_sort aggregate
            | Vir.Parametric binder -> sorts.parametric_sort binder
          in
          let* function_ =
            of_logic
              (Logic_ir.declare_function sorts.builder
                 ~name:
                   (Printf.sprintf "f%d_s%d"
                      obligation.function_ref.function_index symbol.symbol_id)
                 ~domain:[] ~range ~span:symbol.span)
          in
          Hashtbl.add declared_symbols key function_;
          Ok function_
  in
  let declare_aggregate_function name domain range function_span =
    match Hashtbl.find_opt sorts.datatype_functions name with
    | Some (existing_domain, existing_range, function_)
      when List.length existing_domain = List.length domain
           && List.for_all2 Logic_ir.sort_equal existing_domain domain
           && Logic_ir.sort_equal existing_range range ->
        Ok function_
    | Some _ ->
        fail function_span "aggregate proof symbol %s has inconsistent sorts"
          name
    | None ->
        let* function_ =
          of_logic
            (Logic_ir.declare_function sorts.builder ~name ~domain ~range
               ~span:function_span)
        in
        Hashtbl.add sorts.datatype_functions name (domain, range, function_);
        Ok function_
  in
  let routing =
    Logical_adt_encoding_private.routing ~bindings:sorts.logical_adts
      ~span:sorts.span ~aggregate_sort:sorts.aggregate_sort
      ~error:(fun message -> { span = sorts.span; message })
      ~fallback:declare_aggregate_function
  in
  let logical_selector_function =
    Logical_adt_encoding_private.routed_selector routing
  and logical_constructor_function =
    Logical_adt_encoding_private.routed_constructor routing
  in
  let aggregate_tag = Logical_adt_encoding_private.routed_tag routing in
  Ok
    {
      builder = sorts.builder;
      span = sorts.span;
      reached_application_identities =
        preparation.reached_application_identities;
      aggregate_sort = sorts.aggregate_sort;
      parametric_sort = sorts.parametric_sort;
      find_symbols;
      axioms;
      activation_terms;
      rank_domains = Hashtbl.create 4;
      rank_axioms = ref [];
      bound_symbols = [];
      declare_vir_symbol;
      declare_aggregate_function;
      logical_selector_function;
      logical_constructor_function;
      aggregate_tag;
    }
let rec symbol_term environment expected_sort symbol =
  match List.assoc_opt symbol.Vir.symbol_id environment.bound_symbols with
  | Some binder ->
      if
        Logic_ir.sort_equal
          (logic_sort environment expected_sort)
          (Logic_ir.View.binder_sort binder)
      then Ok (Logic_ir.bound binder)
      else
        fail symbol.span "quantifier binder %s#%d has inconsistent sort"
          symbol.source_name symbol.symbol_id
  | None ->
      let* function_ = environment.declare_vir_symbol expected_sort symbol in
      of_logic (Logic_ir.apply ~span:symbol.Vir.span function_ [])
and logic_sort environment = function
  | Vir.Integer -> Logic_ir.Int
  | Vir.Boolean -> Logic_ir.Bool
  | Vir.Aggregate aggregate -> environment.aggregate_sort aggregate
  | Vir.Parametric binder -> environment.parametric_sort binder
let rank_member_name aggregate =
  Printf.sprintf "type_%d" aggregate.Vir.aggregate_type_index
let rank_owner_type domain (constructor : Sst.constructor_id) =
  Vir.rank_domain_component domain
  |> List.find_opt (fun aggregate ->
      aggregate.Vir.aggregate_type_index
      = constructor.constructor_type.type_index)
  |> Option.value
       ~default:
         {
           Vir.aggregate_type_index = constructor.constructor_type.type_index;
           aggregate_type_name = constructor.constructor_type.type_name;
           aggregate_type_arguments = [];
         }
let rank_project environment declared aggregate value =
  of_logic
    (Logic_ir.rank_project ~span:environment.span declared
       ~member:(rank_member_name aggregate)
       value)
let rank_component_axioms environment declared digest_prefix domain =
  List.fold_left
    (fun result aggregate ->
      let* axioms = result in
      let* binder =
        of_logic
          (Logic_ir.bind environment.builder
             ~name:
               (Printf.sprintf "rank_value_t%d"
                  aggregate.Vir.aggregate_type_index)
             ~sort:(environment.aggregate_sort aggregate)
             ~span:environment.span)
      in
      let* projected =
        rank_project environment declared aggregate (Logic_ir.bound binder)
      in
      let* body =
        of_logic
          (Logic_ir.greater_or_equal ~span:environment.span projected
             (Logic_ir.int ~span:environment.span Z.zero))
      in
      let qid =
        Printf.sprintf "verocaml.rank.%s.t%d.nonnegative" digest_prefix
          aggregate.aggregate_type_index
      in
      let* axiom =
        of_logic
          (Logic_ir.forall environment.builder ~binders:[ binder ] ~body
             ~patterns:[ [ projected ] ] ~qid ~skid:(qid ^ ".skolem")
             ~span:environment.span)
      in
      Ok (axiom :: axioms))
    (Ok [])
    (Vir.rank_domain_component domain)
let rank_fact_axiom environment declared digest_prefix domain fact_index fact =
  match fact with
  | Vir.Ground_rank_base _ -> Ok None
  | Vir.Constructor_rank_nonnegative { constructor } ->
      let owner = rank_owner_type domain constructor in
      let* binder =
        of_logic
          (Logic_ir.bind environment.builder
             ~name:
               (Printf.sprintf "rank_parent_t%d_c%d"
                  constructor.constructor_type.type_index
                  constructor.constructor_index)
             ~sort:(environment.aggregate_sort owner)
             ~span:environment.span)
      in
      let value = Logic_ir.bound binder in
      let* tag = environment.aggregate_tag owner value in
      let* is_constructor =
        of_logic
          (Logic_ir.equal ~span:environment.span tag
             (Logic_ir.int ~span:environment.span
                (Z.of_int constructor.constructor_index)))
      in
      let* projected = rank_project environment declared owner value in
      let* nonnegative =
        of_logic
          (Logic_ir.greater_or_equal ~span:environment.span projected
             (Logic_ir.int ~span:environment.span Z.zero))
      in
      let* body =
        of_logic
          (Logic_ir.implies ~span:environment.span is_constructor nonnegative)
      in
      let qid =
        Printf.sprintf "verocaml.rank.%s.t%d.c%d.nonnegative" digest_prefix
          constructor.constructor_type.type_index constructor.constructor_index
      in
      let* axiom =
        of_logic
          (Logic_ir.forall environment.builder ~binders:[ binder ] ~body
             ~patterns:[ [ projected ] ] ~qid ~skid:(qid ^ ".skolem")
             ~span:environment.span)
      in
      Ok (Some axiom)
  | Vir.Positive_child_rank_smaller
      { constructor; field; child_path; child_type } ->
      let owner = rank_owner_type domain constructor in
      let* binder =
        of_logic
          (Logic_ir.bind environment.builder
             ~name:
               (Printf.sprintf "rank_parent_t%d_c%d_f%d_%d"
                  constructor.constructor_type.type_index
                  constructor.constructor_index field.field_index fact_index)
             ~sort:(environment.aggregate_sort owner)
             ~span:environment.span)
      in
      let value = Logic_ir.bound binder in
      let* tag = environment.aggregate_tag owner value in
      let* is_constructor =
        of_logic
          (Logic_ir.equal ~span:environment.span tag
             (Logic_ir.int ~span:environment.span
                (Z.of_int constructor.constructor_index)))
      in
      let selector =
        {
          Vir.selector_domain = owner;
          selector_range = Vir.Aggregate child_type;
          selector_namespace =
            Printf.sprintf "t%d_%s_c%d_%s"
              constructor.constructor_type.type_index
              constructor.constructor_type.type_name
              constructor.constructor_index constructor.constructor_name;
          selector_index = field.field_index;
          selector_name = Printf.sprintf "$arg%d" field.field_index;
          selector_path = child_path;
        }
      in
      let* child_function =
        environment.logical_selector_function selector
          (environment.aggregate_sort owner)
          (environment.aggregate_sort child_type)
      in
      let* child =
        of_logic
          (Logic_ir.apply ~span:environment.span child_function [ value ])
      in
      let* parent_rank = rank_project environment declared owner value in
      let* child_rank = rank_project environment declared child_type child in
      let* smaller =
        of_logic
          (Logic_ir.less_than ~span:environment.span child_rank parent_rank)
      in
      let* body =
        of_logic
          (Logic_ir.implies ~span:environment.span is_constructor smaller)
      in
      let qid =
        Printf.sprintf "verocaml.rank.%s.t%d.c%d.f%d.%d.child" digest_prefix
          constructor.constructor_type.type_index constructor.constructor_index
          field.field_index fact_index
      in
      let* axiom =
        of_logic
          (Logic_ir.forall environment.builder ~binders:[ binder ] ~body
             ~patterns:[ [ child ] ] ~qid ~skid:(qid ^ ".skolem")
             ~span:environment.span)
      in
      Ok (Some axiom)
let rank_fact_axioms environment declared digest_prefix domain =
  List.fold_left
    (fun result (fact_index, fact) ->
      let* axioms = result in
      let* axiom =
        rank_fact_axiom environment declared digest_prefix domain fact_index
          fact
      in
      Ok (Option.fold ~none:axioms ~some:(fun axiom -> axiom :: axioms) axiom))
    (Ok [])
    (List.mapi (fun index fact -> (index, fact)) (Vir.rank_domain_facts domain))
let ensure_rank_domain environment domain =
  let domain_id = Vir.rank_domain_id domain in
  match Hashtbl.find_opt environment.rank_domains domain_id with
  | Some declared -> Ok declared
  | None ->
      let members =
        Vir.rank_domain_component domain
        |> List.map (fun aggregate ->
            (rank_member_name aggregate, environment.aggregate_sort aggregate))
      in
      let* declared =
        of_logic
          (Logic_ir.declare_rank_domain environment.builder ~domain_id ~members
             ~span:environment.span)
      in
      Hashtbl.add environment.rank_domains domain_id declared;
      let digest = Vir.rank_domain_digest domain in
      let digest_prefix = String.sub digest 0 (min 8 (String.length digest)) in
      let* component_axioms =
        rank_component_axioms environment declared digest_prefix domain
      in
      let* fact_axioms =
        rank_fact_axioms environment declared digest_prefix domain
      in
      environment.rank_axioms :=
        List.rev_append component_axioms
          (List.rev_append fact_axioms !(environment.rank_axioms));
      Ok declared
let rec translate_integer environment term =
  let* core =
    Vir_logic_ir_translation_private.translate_integer_core
      {
        span = environment.span;
        symbol = symbol_term environment Vir.Integer;
        translate_integer = translate_integer environment;
        translate_boolean = translate_boolean environment;
        translate_arguments = translate_recursive_arguments environment;
        recursive_function =
          (fun callee type_arguments ->
            let* symbols = environment.find_symbols callee type_arguments in
            Ok symbols.public);
        term_result = of_logic;
      }
      term
  in
  match core with
  | Some term -> Ok term
  | None -> (
  match term with
  | Vir.Aggregate_tag (aggregate_type, source) ->
      if aggregate_type <> source.aggregate_type then
        fail environment.span "aggregate tag has a mismatched domain"
      else
        Vir_logic_ir_translation_private.translate_normalized
          {
            span = environment.span;
            normalize =
              (fun source ->
                Logical_aggregate_term_normalization_private.tag aggregate_type
                  source);
            translate_boolean = translate_boolean environment;
            translate_exact = translate_integer environment;
            translate_opaque =
              (fun source ->
            let* source = translate_aggregate environment source in
                environment.aggregate_tag aggregate_type source);
            term_result = of_logic;
            malformed = (fun message -> { span = environment.span; message });
          }
          source
  | Vir.Integer_selector (selector, source) ->
      translate_integer_selector environment selector source
  | Vir.Integer_rank_project (domain, source) ->
      if
        not
          (List.exists
             (( = ) source.Vir.aggregate_type)
             (Vir.rank_domain_component domain))
      then
        fail environment.span
          "structural rank domain does not contain its aggregate"
      else
        let aggregate_type = source.Vir.aggregate_type in
        let* declared = ensure_rank_domain environment domain in
        let* source = translate_aggregate environment source in
        of_logic
          (Logic_ir.rank_project ~span:environment.span declared
             ~member:(rank_member_name aggregate_type)
             source)
  | Vir.Integer_symbolic_application application ->
      translate_symbolic_application environment Logic_ir.Int application
  | Vir.Integer_constant _ | Vir.Integer_symbol _ | Vir.Integer_add _
  | Vir.Integer_subtract _ | Vir.Integer_negate _
  | Vir.Integer_multiply_constant _ | Vir.Integer_absolute_value _
  | Vir.Integer_conditional _ | Vir.Integer_recursive_spec_application _ ->
      assert false)
and translate_boolean environment term =
  let* core =
    Vir_logic_ir_translation_private.translate_boolean_core
      {
        span = environment.span;
        symbol = symbol_term environment Vir.Boolean;
        translate_boolean = translate_boolean environment;
        translate_integer = translate_integer environment;
        term_result = of_logic;
      }
      term
  in
  match core with
  | Some term -> Ok term
  | None -> (
  match term with
  | Vir.Forall_term quantifier ->
      translate_user_quantifier environment true quantifier
  | Vir.Exists_term quantifier ->
      translate_user_quantifier environment false quantifier
  | Vir.Parametric_equal (left, right) ->
      if
        Parametric_type.compare_binder left.parametric_sort right.parametric_sort
        <> 0
      then fail environment.span "parametric equality crosses named sorts"
      else
        let* left = translate_parametric environment left in
        let* right = translate_parametric environment right in
        of_logic (Logic_ir.equal ~span:environment.span left right)
  | Vir.Boolean_recursive_spec_application
      { callee; type_arguments; arguments; span = call_span } ->
      let* symbols = environment.find_symbols callee type_arguments in
      let* arguments = translate_recursive_arguments environment arguments in
      of_logic (Logic_ir.apply ~span:call_span symbols.public arguments)
  | Vir.Boolean_specification_application
      { callee; type_arguments; arguments; span = call_span } ->
      translate_callback_relation environment
        (Vir.specification_application_name callee type_arguments arguments)
        arguments call_span
  | Vir.Boolean_symbolic_application application ->
      translate_symbolic_application environment Logic_ir.Bool application
  | Vir.Callback_requires application ->
      translate_callback_relation environment
        (Sst_callback_private.requires_relation application.callback)
        application.arguments application.call_span
  | Vir.Callback_ensures { application; result } ->
      translate_callback_relation environment
        (Sst_callback_private.ensures_relation application.callback)
        (application.arguments @ [ result ])
        application.call_span
  | Vir.Boolean_selector (selector, source) ->
      translate_boolean_selector environment selector source
  | Vir.Aggregate_equal (left, right) ->
      if left.aggregate_type <> right.aggregate_type then
        fail environment.span "aggregate equality crosses exact types"
      else
        Vir_logic_ir_translation_private.translate_normalized
          {
            span = environment.span;
            normalize =
              (fun (left, right) ->
                Logical_aggregate_term_normalization_private.equal left right);
            translate_boolean = translate_boolean environment;
            translate_exact = translate_boolean environment;
            translate_opaque =
              (fun (left, right) ->
            let* left = translate_aggregate environment left in
            let* right = translate_aggregate environment right in
                of_logic
                  (Logic_ir.equal ~span:environment.span left right));
            term_result = of_logic;
            malformed = (fun message -> { span = environment.span; message });
          }
          (left, right)
  | Vir.Boolean_invariant_application { invariant_id; value; _ } ->
      translate_callback_relation environment
        ("verocaml_invariant_" ^ Digest.to_hex (Digest.string invariant_id))
        [ Vir.Recursive_aggregate_argument value ]
        environment.span
  | Vir.Boolean_constant _ | Vir.Logical_adt_schema _ | Vir.Boolean_symbol _
  | Vir.Boolean_not _ | Vir.Boolean_and _ | Vir.Boolean_or _
  | Vir.Integer_compare _ | Vir.Boolean_equal _ | Vir.Boolean_not_equal _ ->
      assert false)
and translate_user_quantifier environment universal quantifier =
  Vir_logic_ir_translation_private.translate_user_quantifier
    {
      quantifier_builder = environment.builder;
      quantifier_sort = logic_sort environment;
      translate =
        (fun binders term ->
          translate_boolean
            {
              environment with
              bound_symbols = binders @ environment.bound_symbols;
            }
            term);
      translate_application =
        (fun binders term ->
          translate_application
            {
              environment with
              bound_symbols = binders @ environment.bound_symbols;
            }
            term);
      bind_result = of_logic;
      term_result = of_logic;
      malformed = (fun span message -> { span; message });
    }
    ~universal quantifier
and translate_aggregate environment (term : Vir.aggregate_term) =
  match term.aggregate_desc with
  | Vir.Aggregate_symbol symbol ->
      symbol_term environment (Vir.Aggregate term.aggregate_type) symbol
  | Vir.Aggregate_imported_model_application _ ->
      fail environment.span
        "retained aggregate model application cannot enter a recursive \
         definition query"
  | Vir.Aggregate_selector (selector, source) ->
      if selector.selector_domain <> source.aggregate_type then
        fail environment.span "aggregate selector has a mismatched owner"
      else if selector.selector_range <> Vir.Aggregate term.aggregate_type then
        fail environment.span "aggregate selector has a mismatched result type"
      else
        Vir_logic_ir_translation_private.translate_normalized
          {
            span = environment.span;
            normalize =
              Logical_aggregate_term_normalization_private.aggregate_selector
                selector;
            translate_boolean = translate_boolean environment;
            translate_exact = translate_aggregate environment;
            translate_opaque =
              (fun source ->
            let* function_ =
              environment.logical_selector_function selector
                (environment.aggregate_sort selector.selector_domain)
                (environment.aggregate_sort term.aggregate_type)
            in
            let* source = translate_aggregate environment source in
            of_logic
                  (Logic_ir.apply ~span:environment.span function_ [ source ]));
            term_result = of_logic;
            malformed = (fun message -> { span = environment.span; message });
          }
          source
  | Vir.Aggregate_constructor { constructor; arguments } ->
      if
        term.aggregate_type.aggregate_type_index
        <> constructor.constructor_type.type_index
      then
        fail environment.span
          "aggregate constructor has a mismatched result type"
      else
        let* arguments = translate_recursive_arguments environment arguments in
        let* function_ =
          environment.logical_constructor_function term.aggregate_type
            constructor
            (List.map Logic_ir.term_sort arguments)
        in
        of_logic (Logic_ir.apply ~span:environment.span function_ arguments)
  | Vir.Aggregate_record { record_type; fields } ->
      if term.aggregate_type.aggregate_type_index <> record_type.type_index then
        fail environment.span "aggregate record has a mismatched result type"
      else
        translate_callback_relation
          ~range:(environment.aggregate_sort term.aggregate_type)
          environment
          (Aggregate_logic_symbol_private.record_constructor
             term.aggregate_type record_type)
          (List.map snd fields) environment.span
  | Vir.Aggregate_conditional (condition, consequent, alternative) ->
      if
        consequent.aggregate_type <> term.aggregate_type
        || alternative.aggregate_type <> term.aggregate_type
      then
        fail environment.span "aggregate conditional crosses exact result types"
      else
        let* condition = translate_boolean environment condition in
        let* then_ = translate_aggregate environment consequent in
        let* else_ = translate_aggregate environment alternative in
        of_logic (Logic_ir.ite ~span:environment.span condition ~then_ ~else_)
  | Vir.Aggregate_recursive_spec_application
      {
        callee;
        type_arguments;
        arguments;
        result_type;
        span = call_span;
        application_identity;
      }
    ->
      if
        result_type <> term.aggregate_type
        || (not
              (Recursive_spec_application_identity.authenticate
                 application_identity))
        || not
             (List.exists
                (fun reached ->
                  Recursive_spec_application_identity.same reached
                    application_identity)
                environment.reached_application_identities)
      then fail call_span "aggregate recursive application identity is forged"
      else
        let* symbols = environment.find_symbols callee type_arguments in
        let* arguments = translate_recursive_arguments environment arguments in
        let expected_range =
          sort symbols symbols.view.result_type
        in
        if
          not
            (Logic_ir.sort_equal expected_range
               (environment.aggregate_sort result_type))
        then fail call_span "aggregate recursive application range mismatch"
        else of_logic (Logic_ir.apply ~span:call_span symbols.public arguments)
  | Vir.Aggregate_symbolic_application application ->
      translate_symbolic_application environment
        (environment.aggregate_sort term.aggregate_type)
        application
and scalar_selector_services environment :
    error Vir_logic_ir_translation_private.scalar_selector_services =
    {
      span = environment.span;
      logic_sort = logic_sort environment;
      aggregate_sort = environment.aggregate_sort;
      selector_function = environment.logical_selector_function;
      translate_boolean = translate_boolean environment;
      translate_integer = translate_integer environment;
      translate_aggregate = translate_aggregate environment;
      translate_symbol =
        (fun symbol -> symbol_term environment symbol.Vir.sort symbol);
      translate_symbolic_application =
        (fun binder application ->
          translate_symbolic_application environment
            (environment.parametric_sort binder)
            application);
      term_result = of_logic;
      malformed = (fun message -> { span = environment.span; message });
    }
and translate_integer_selector environment selector source =
  Vir_logic_ir_translation_private.translate_integer_selector
    (scalar_selector_services environment) selector source
and translate_boolean_selector environment selector source =
  Vir_logic_ir_translation_private.translate_boolean_selector
    (scalar_selector_services environment) selector source
and translate_parametric environment term =
  [%log.debug "translating recursive-spec parametric term"
    ~sort:
      (Delator.Field.string
         (Parametric_logic_private.sort_name term.Vir.parametric_sort))];
  Vir_logic_ir_translation_private.translate_parametric
    (scalar_selector_services environment) term
and translate_application environment = function
  | Vir.Integer_application term -> translate_integer environment term
  | Boolean_application term -> translate_boolean environment term
  | Aggregate_application term -> translate_aggregate environment term
  | Parametric_application term -> translate_parametric environment term
and translate_recursive_arguments environment arguments =
  Vir_logic_ir_translation_private.translate_arguments
    ~integer:(translate_integer environment)
    ~boolean:(translate_boolean environment)
    ~aggregate:(translate_aggregate environment)
    ~parametric:(translate_parametric environment)
    arguments
and translate_callback_relation ?(range = Logic_ir.Bool) environment name
    arguments call_span =
  Vir_logic_ir_translation_private.translate_relation
    {
      aggregate_sort = environment.aggregate_sort;
      parametric_sort = environment.parametric_sort;
      declare = environment.declare_aggregate_function;
      translate_arguments = translate_recursive_arguments environment;
      apply =
        (fun span function_ arguments ->
          Logic_ir.apply ~span function_ arguments |> of_logic);
    }
    ~name ~range ~arguments ~span:call_span
and translate_symbolic_application environment range application =
  translate_callback_relation ~range environment
    (Symbolic_application_private.symbol_name application)
    (Symbolic_application_private.arguments application)
    (Symbolic_application_private.span application)
let translate_boolean_terms environment terms =
  translate_recursive_arguments environment
    (List.map (fun term -> Vir.Recursive_boolean_argument term) terms)
let specialization_assertions environment specialization =
  match specialization with
  | None -> Ok []
  | Some { application; result_constructor } ->
      let result_aggregate = application.Vir.aggregate_type in
      let* application = translate_aggregate environment application in
      let* constructor =
        environment.logical_constructor_function result_aggregate
          result_constructor []
      in
      let* constructor =
        of_logic (Logic_ir.apply ~span:environment.span constructor [])
      in
      let* exact_result =
        of_logic (Logic_ir.equal ~span:environment.span application constructor)
      in
      let* observed_tag =
        environment.aggregate_tag result_aggregate application
      in
      let* exact_tag =
        of_logic
          (Logic_ir.equal ~span:environment.span observed_tag
             (Logic_ir.int ~span:environment.span
                (Z.of_int result_constructor.constructor_index)))
      in
      let* fact =
        of_logic
          (Logic_ir.and_ ~span:environment.span [ exact_result; exact_tag ])
      in
      Ok [ fact ]
let finish_proof_query environment specialization (obligation : Vir.obligation)
    =
  let* assumptions =
    translate_boolean_terms environment
      (obligation.assumptions @ obligation.required_preceding_safety
     @ obligation.path_condition)
  in
  let* goal = translate_boolean environment obligation.goal in
  let* negated_goal = of_logic (Logic_ir.not_ ~span:environment.span goal) in
  let* specialization_assertions =
    specialization_assertions environment specialization
  in
  of_logic
    (Logic_ir.query environment.builder
       ~axioms:(environment.axioms @ List.rev !(environment.rank_axioms))
       ~assertions:
         (environment.activation_terms @ assumptions @ specialization_assertions
        @ [ negated_goal ])
       ~requires:required_features ~span:environment.span)
let build_proof_obligation_query ?specialization ?solver_controls
    ?(record_construction = true) verified ~activations
    (obligation : Vir.obligation) =
  let* preparation =
    prepare_proof_query verified ~activations obligation solver_controls
  in
  match preparation.definitions with
  | [] ->
      fail preparation.span
        "verified recursive authority contains no definitions"
  | _first :: _ ->
      let* sorts = declare_proof_query_sorts preparation obligation in
      let* environment =
        declare_proof_query_environment preparation sorts obligation
      in
      let* query = finish_proof_query environment specialization obligation in
      if record_construction then incr proof_query_constructions;
      Ok query
let proof_obligation_query verified ~activations obligation =
  build_proof_obligation_query verified ~activations obligation
type nullary_branch_retry_outcome =
  | Nullary_branch_verified
  | Nullary_branch_not_verified
  | Nullary_branch_inconclusive of Z3_bridge.inconclusive_reason
  | Nullary_branch_abstain
type nullary_branch_retry_counters : value mod contended portable = {
  attempts : int;
  queries : int;
  facts : int;
  verified : int;
  counterexamples : int;
  inconclusives : int;
  abstentions : int;
}
let nullary_branch_retry_attempts = ref 0
let nullary_branch_retry_queries = ref 0
let nullary_branch_retry_facts = ref 0
let nullary_branch_retry_verified = ref 0
let nullary_branch_retry_counterexamples = ref 0
let nullary_branch_retry_inconclusives = ref 0
let nullary_branch_retry_abstentions = ref 0
let last_nullary_branch_retry_query_for_testing = ref None
let force_nullary_branch_retry_unknown_for_testing = ref false
let force_nullary_branch_retry_counterexample_for_testing = ref false
let suppress_original_nullary_branch_activation_for_testing = ref false
let snapshot_solver_controls () = { force_retry_unknown = !force_nullary_branch_retry_unknown_for_testing;
  force_retry_counterexample = !force_nullary_branch_retry_counterexample_for_testing;
  suppress_original_activation = !suppress_original_nullary_branch_activation_for_testing;
  suppress_reached_authority = !suppress_reached_aggregate_authority_for_testing }
let exact_constructor_definition verified constructor =
  let program =
    Spec_unfolding_private.validated_program (prepared verified)
    |> Sst_validation.program
  in
  program.Sst.types
  |> List.find_map (fun (type_definition : Sst.type_definition) ->
         if type_definition.type_id <> constructor.Sst.constructor_type then
           None
         else
           match type_definition.type_kind with
           | Sst.Record_definition _ -> None
           | Sst.Variant_definition constructors ->
               List.find_opt
                 (fun (candidate : Sst.constructor_definition) ->
                   candidate.constructor_id = constructor)
                 constructors)
let exact_nullary_constructor_metadata verified constructor =
  match exact_constructor_definition verified constructor with
  | Some { Sst.constructor_fields = []; _ } -> true
  | Some { constructor_fields = _ :: _; _ } | None -> false
let rec nullary_scalar_integer_argument = function
  | Vir.Integer_constant _ | Vir.Integer_symbol _ -> true
  | Vir.Integer_add (left, right) | Vir.Integer_subtract (left, right) ->
      nullary_scalar_integer_argument left
      && nullary_scalar_integer_argument right
  | Vir.Integer_negate value
  | Vir.Integer_multiply_constant (_, value)
  | Vir.Integer_absolute_value value ->
      nullary_scalar_integer_argument value
  | Vir.Integer_conditional (condition, consequent, alternative) ->
      nullary_scalar_boolean_argument condition
      && nullary_scalar_integer_argument consequent
      && nullary_scalar_integer_argument alternative
  | Vir.Integer_recursive_spec_application _
  | Vir.Integer_symbolic_application _
  | Vir.Integer_rank_project _
  | Vir.Aggregate_tag _
  | Vir.Integer_selector _ ->
      false
and nullary_scalar_boolean_argument = function
  | Vir.Forall_term _ | Vir.Exists_term _ -> false
  | Vir.Boolean_constant _ | Vir.Boolean_symbol _
  | Vir.Logical_adt_schema _ ->
      true
  | Vir.Boolean_not value -> nullary_scalar_boolean_argument value
  | Vir.Boolean_and (left, right)
  | Vir.Boolean_or (left, right)
  | Vir.Boolean_equal (left, right)
  | Vir.Boolean_not_equal (left, right) ->
      nullary_scalar_boolean_argument left
      && nullary_scalar_boolean_argument right
  | Vir.Integer_compare (_, left, right) ->
      nullary_scalar_integer_argument left
      && nullary_scalar_integer_argument right
  | Vir.Parametric_equal (left, right) ->
      Parametric_logic_private.for_all_conditions
        nullary_scalar_boolean_argument left right
  | Vir.Boolean_recursive_spec_application _
  | Vir.Boolean_symbolic_application _
  | Vir.Boolean_specification_application _
  | Vir.Callback_requires _ | Vir.Callback_ensures _
  | Vir.Boolean_selector _
  | Vir.Aggregate_equal _
  | Vir.Boolean_invariant_application _ ->
      false
let nullary_branch_argument_supported = function
  | Vir.Recursive_integer_argument term ->
      nullary_scalar_integer_argument term
  | Vir.Recursive_boolean_argument term ->
      nullary_scalar_boolean_argument term
  | Vir.Recursive_aggregate_argument
      { Vir.aggregate_desc = Vir.Aggregate_symbol { role = Vir.Input; _ }; _ } ->
      true
  | Vir.Recursive_aggregate_argument _
  | Vir.Recursive_parametric_argument _ -> false
let nullary_branch_specialization ?solver_controls verified ~activations
    ~(ground_constructors : (Vir.symbol * Sst.constructor_id) list)
    (obligation : Vir.obligation) =
  let abstain () = Ok None in
  match obligation.kind with
  | Vir.Local_assertion _ -> (
      match Recursive_spec_retry_demand_private.of_goal obligation.goal with
      | Error (`Malformed (span, message)) -> fail span "%s" message
      | Ok None -> abstain ()
      | Ok (Some demand) ->
          let application = Recursive_spec_retry_demand_private.application demand in
          let callee = Recursive_spec_retry_demand_private.callee demand in
          let arguments = Recursive_spec_retry_demand_private.arguments demand in
          let result_type = Recursive_spec_retry_demand_private.result_type demand in
          if not (List.for_all nullary_branch_argument_supported arguments)
          then abstain ()
          else
            let original_activations =
              if Option.fold
                   ~none:!suppress_original_nullary_branch_activation_for_testing
                   ~some:(fun controls -> controls.suppress_original_activation)
                   solver_controls
              then []
              else activations
            in
            let* definition = definition verified callee in
            if
              not
                (List.exists
                   (fun (activation : Spec_unfolding.activation) ->
                     same_function_id activation.function_id callee
                     && activation.depth >= 1 && activation.depth <= 64
                     && activation.span
                        <> Spec_unfolding_private.definition_span definition)
                   original_activations)
            then abstain ()
            else
              let expected_result =
                Spec_unfolding_private.definition_result_type definition
              in
              if
                Logical_adt_encoding_private.aggregate_type_of_sst
                  (Spec_unfolding_private.definition_parametric_adts
                     definition)
                  expected_result
                <> Some result_type
              then
                fail obligation.span
                  "aggregate recursive application range lost exact definition identity"
              else
                let parameters =
                  List.map Sst.require_value_parameter
                    (Spec_unfolding_private.definition_parameters definition)
                in
                if List.length parameters <> List.length arguments then
                  fail obligation.span
                    "aggregate recursive application argument count mismatch"
                else
                  match
                    (Spec_unfolding_private.definition_body definition)
                      .expression_desc
                  with
                  | Sst.Match
                      ( {
                          expression_desc =
                            Sst.Variable { binding = scrutinee; _ };
                          _;
                        },
                        cases ) -> (
                      let rec parameter_index index = function
                        | [] -> None
                        | parameter :: rest ->
                            if (parameter_binding parameter).id = scrutinee.id
                            then Some index
                            else parameter_index (index + 1) rest
                      in
                      match parameter_index 0 parameters with
                      | None -> abstain ()
                      | Some index -> (
                          match List.nth_opt arguments index with
                          | Some
                              (Vir.Recursive_aggregate_argument
                                ({
                                   aggregate_desc =
                                     Vir.Aggregate_symbol symbol;
                                   aggregate_type = actual_type;
                                 } as _actual))
                            when symbol.role = Vir.Input
                                 && symbol.sort = Vir.Aggregate actual_type ->
                              let witnesses =
                                List.filter
                                  (fun (candidate, _) -> candidate = symbol)
                                  ground_constructors
                              in
                              (match witnesses with
                              | [] | _ :: _ :: _ -> abstain ()
                              | [ (_, constructor) ] ->
                                  if
                                    constructor.constructor_type.type_index
                                    <> actual_type.aggregate_type_index
                                  then
                                    fail obligation.span
                                      "routed nullary constructor witness has a mismatched aggregate type"
                                  else if
                                    not
                                      (exact_nullary_constructor_metadata
                                         verified constructor)
                                  then
                                    fail obligation.span
                                      "routed constructor witness is not an exact nullary constructor"
                                  else
                                    let matching_cases =
                                      List.filter
                                        (fun (case : Sst.case) ->
                                          match
                                            case.case_pattern.pattern_desc
                                          with
                                          | Sst.Constructor_pattern
                                              (candidate, []) ->
                                              candidate = constructor
                                          | Sst.Constructor_pattern
                                              (_, _ :: _)
                                          | Sst.Wildcard | Sst.Bind _
                                          | Sst.Owned_tree_cursor_pattern _
                                          | Sst.Int_pattern _
                                          | Sst.Bool_pattern _
                                          | Sst.Unit_pattern
                                          | Sst.Tuple_pattern _
                                          | Sst.Record_pattern _
                                          | Sst.Or_pattern _ ->
                                              false)
                                        cases
                                    in
                                    (match matching_cases with
                                    | [
                                     {
                                       case_guard = None;
                                       case_body =
                                         {
                                           expression_desc =
                                             Sst.Constructor_value
                                               {
                                                 constructor =
                                                   result_constructor;
                                                 arguments = [];
                                               };
                                           typ = result_constructor_type;
                                           _;
                                         };
                                       _;
                                     };
                                    ]
                                      when
                                        result_constructor.constructor_type
                                          .type_index
                                        = result_type.aggregate_type_index
                                        && Logical_adt_encoding_private
                                           .aggregate_type_of_sst
                                             (Spec_unfolding_private
                                              .definition_parametric_adts
                                                definition)
                                             result_constructor_type
                                           = Some result_type
                                        && exact_nullary_constructor_metadata
                                             verified result_constructor ->
                                        Ok
                                          (Some
                                             {
                                               application;
                                               result_constructor;
                                             })
                                    | [] | [ _ ] | _ :: _ :: _ ->
                                        abstain ()))
                          | Some
                              (Vir.Recursive_integer_argument _
                              | Vir.Recursive_boolean_argument _
                              | Vir.Recursive_aggregate_argument _
                              | Vir.Recursive_parametric_argument _)
                          | None ->
                              abstain ()))
                  | Sst.Int_constant _ | Sst.Bool_constant _
                  | Sst.Unit_constant | Sst.Variable _ | Sst.Tuple_value _
                  | Sst.Record_value _ | Sst.Constructor_value _
                  | Sst.Field_read _ | Sst.Field_write _
                  | Sst.Shared_scalar_field_write _
                  | Sst.Owned_tree_nested_write _
                  | Sst.Owned_tree_rebase _ | Sst.Let_mutable _
                  | Sst.Mutable_read _ | Sst.Mutable_write _ | Sst.Let _
                  | Sst.Sequence _ | Sst.If _ | Sst.Match _
                  | Sst.Checked_arithmetic _
                  | Sst.Compare _ | Sst.Boolean_not _ | Sst.Boolean_binary _
                  | Sst.Optional_absent | Sst.Optional_present _
                  | Sst.Optional_forward _ | Sst.Direct_call _
                  | Sst.Callback_call _ | Sst.Callback_requires _
                  | Sst.Callback_ensures _ | Sst.Reveal _
                  | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
                  | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _
                  | Sst.Forall _ | Sst.Exists _
                  | Sst.Symbolic_application _ ->
                      abstain ())
  | Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Postcondition _
  | Vir.Call_precondition _ | Vir.Callback_precondition _
  | Vir.Invariant_validity _
  | Vir.Entry_measure_nonnegative _
  | Vir.Recursive_call_measure_nonnegative _
  | Vir.Recursive_call_strict_descent _ ->
      abstain ()
type ground_value =
  | Ground_unit
  | Ground_integer of Z.t
  | Ground_boolean of bool
  | Ground_aggregate of Sst.constructor_id
type ground_counterexample_outcome : value mod contended portable =
  | Ground_complete_violation
  | Ground_abstain
type ground_counterexample_counters : value mod contended portable = {
  attempts : int;
  complete_violations : int;
  abstentions : int;
  antecedents_checked : int;
}
type solver_definition = { solver_id : Sst.function_id; solver_parameters : Sst.parameter list;
  solver_body : Sst.expression }
type solver_source = { solver_definitions : solver_definition list; solver_types : Sst.type_definition list }
type ground_payload = { ground_source : solver_source;
  ground_activations : Spec_unfolding.activation list;
  ground_constructors : (Vir.symbol * Sst.constructor_id) list;
  ground_obligation : Vir.obligation }
type prepared_query = Prepared_query of string
and prepared_proof = { initial_query : prepared_query; retry_query : prepared_query option; retry_eligible : bool;
  ground_payload : ground_payload option; solver_controls : solver_controls }
let last_prepared_nullary_branch_retry_query_for_testing = ref None
let detach_query query = Prepared_query (Marshal.to_string query [])
let solve_prepared_query_local ~controlled ~rlimit config (Prepared_query encoded) =
  Z3_bridge.solve_query_local ~controlled ~rlimit config
    (Marshal.from_string encoded 0 : Logic_ir.query)
let solver_source verified =
  let prepared = prepared verified in
  let program = Spec_unfolding_private.validated_program prepared |> Sst_validation.program in
  { solver_definitions = List.map (fun definition ->
        { solver_id = Spec_unfolding_private.definition_id definition;
          solver_parameters = Spec_unfolding_private.definition_parameters definition;
          solver_body = Spec_unfolding_private.definition_body definition })
      (Spec_unfolding_private.definitions prepared);
    solver_types = List.map (fun definition ->
      { definition with Sst.representation = Sst.Revealed }) program.Sst.types }
let ground_counterexample_attempts = ref 0
let ground_counterexample_complete_violations = ref 0
let ground_counterexample_abstentions = ref 0
let ground_counterexample_antecedents_checked = ref 0
let same_constructor_id (left : Sst.constructor_id)
    (right : Sst.constructor_id) =
  left.constructor_type.type_index = right.constructor_type.type_index
  && String.equal left.constructor_type.type_name
       right.constructor_type.type_name
  && left.constructor_index = right.constructor_index
  && String.equal left.constructor_name right.constructor_name
let ground_value_equal left right =
  match (left, right) with
  | Ground_unit, Ground_unit -> true
  | Ground_integer left, Ground_integer right -> Z.equal left right
  | Ground_boolean left, Ground_boolean right -> Bool.equal left right
  | Ground_aggregate left, Ground_aggregate right ->
      same_constructor_id left right
  | ( Ground_unit,
      (Ground_integer _ | Ground_boolean _ | Ground_aggregate _) )
  | ( Ground_integer _,
      (Ground_unit | Ground_boolean _ | Ground_aggregate _) )
  | ( Ground_boolean _,
      (Ground_unit | Ground_integer _ | Ground_aggregate _) )
  | ( Ground_aggregate _,
      (Ground_unit | Ground_integer _ | Ground_boolean _) ) ->
      false
let checked_integer value =
  if
    Z.compare value Int_bounds.minimum >= 0
    && Z.compare value Int_bounds.maximum <= 0
  then Some value
  else None
let ground_sst_comparison comparison left right =
  match (comparison, left, right) with
  | Sst.Equal, _, _ -> Some (ground_value_equal left right)
  | Sst.Not_equal, _, _ -> Some (not (ground_value_equal left right))
  | (Sst.Less_than | Sst.Less_or_equal | Sst.Greater_than
    | Sst.Greater_or_equal), Ground_integer left, Ground_integer right ->
      let order = Z.compare left right in
      Some
        (match comparison with
        | Sst.Less_than -> order < 0
        | Sst.Less_or_equal -> order <= 0
        | Sst.Greater_than -> order > 0
        | Sst.Greater_or_equal -> order >= 0
        | Sst.Equal | Sst.Not_equal -> assert false)
  | (Sst.Less_than | Sst.Less_or_equal | Sst.Greater_than
    | Sst.Greater_or_equal), _, _ ->
      None
type ground_pattern_result =
  | Ground_pattern_match of (int * ground_value) list
  | Ground_pattern_mismatch
  | Ground_pattern_unsupported
let ground_bind_pattern environment (pattern : Sst.pattern) value =
  match (pattern.pattern_desc, value) with
  | Sst.Wildcard, _ -> Ground_pattern_match environment
  | Sst.Bind binding, value ->
      Ground_pattern_match ((binding.id, value) :: environment)
  | Sst.Unit_pattern, Ground_unit -> Ground_pattern_match environment
  | Sst.Int_pattern expected, Ground_integer actual ->
      if Z.equal expected actual
      then Ground_pattern_match environment
      else Ground_pattern_mismatch
  | Sst.Bool_pattern expected, Ground_boolean actual ->
      if Bool.equal expected actual
      then Ground_pattern_match environment
      else Ground_pattern_mismatch
  | Sst.Constructor_pattern (expected, []), Ground_aggregate actual ->
      if same_constructor_id expected actual
      then Ground_pattern_match environment
      else Ground_pattern_mismatch
  | Sst.Constructor_pattern (_, _ :: _), Ground_aggregate _ ->
      Ground_pattern_unsupported
  | Sst.Tuple_pattern _, _ | Sst.Record_pattern _, _
  | Sst.Or_pattern _, _ | Sst.Owned_tree_cursor_pattern _, _ ->
      Ground_pattern_unsupported
  | ( Sst.Unit_pattern | Sst.Int_pattern _ | Sst.Bool_pattern _
    | Sst.Constructor_pattern _ ),
    _ ->
      Ground_pattern_mismatch
let ground_bind_tuple_pattern evaluate environment scrutinee pattern =
  match Recursive_spec_tuple_match_private.plan scrutinee pattern with
  | Error _ -> Ground_pattern_unsupported
  | Ok leaves ->
      List.fold_left
        (fun result (leaf : Recursive_spec_tuple_match_private.leaf) ->
          match result with
          | Ground_pattern_mismatch | Ground_pattern_unsupported -> result
          | Ground_pattern_match case_environment -> (
              match evaluate leaf.expression with
              | None -> Ground_pattern_unsupported
              | Some value ->
                  ground_bind_pattern case_environment leaf.pattern value))
        (Ground_pattern_match environment) leaves
let rec ground_sst_expression fuel environment
    (expression : Sst.expression) =
  let evaluate = ground_sst_expression fuel environment in
  match expression.expression_desc with
  | Sst.Int_constant value -> Some (Ground_integer value)
  | Sst.Bool_constant value -> Some (Ground_boolean value)
  | Sst.Unit_constant -> Some Ground_unit
  | Sst.Variable { binding; _ } -> List.assoc_opt binding.id environment
  | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _ ->
      None
  | Sst.Let (bindings, body) ->
      let rec bind environment = function
        | [] -> ground_sst_expression fuel environment body
        | (pattern, value) :: rest -> (
            match ground_sst_expression fuel environment value with
            | None -> None
            | Some value -> (
                match ground_bind_pattern environment pattern value with
                | Ground_pattern_match environment -> bind environment rest
                | Ground_pattern_mismatch | Ground_pattern_unsupported ->
                    None))
      in
      bind environment bindings
  | Sst.Sequence (first, second) -> (
      match evaluate first with
      | None -> None
      | Some _ -> evaluate second)
  | Sst.If (condition, consequent, alternative) -> (
      match evaluate condition with
      | Some (Ground_boolean true) -> evaluate consequent
      | Some (Ground_boolean false) ->
          Option.bind alternative evaluate
      | Some (Ground_unit | Ground_integer _ | Ground_aggregate _) | None ->
          None)
  | Sst.Match (scrutinee, cases) ->
      let rec choose bind = function
        | [] -> None
        | case :: rest -> (
            match bind case.Sst.case_pattern with
            | Ground_pattern_mismatch -> choose bind rest
            | Ground_pattern_unsupported -> None
            | Ground_pattern_match environment -> (
                match case.case_guard with
                | None ->
                    ground_sst_expression fuel environment case.case_body
                | Some guard -> (
                    match ground_sst_expression fuel environment guard with
                    | Some (Ground_boolean true) ->
                        ground_sst_expression fuel environment case.case_body
                    | Some (Ground_boolean false) -> choose bind rest
                    | Some
                        (Ground_unit | Ground_integer _ | Ground_aggregate _)
                    | None ->
                        None)))
      in
      (match scrutinee.expression_desc with
      | Sst.Tuple_value _ ->
          choose
            (ground_bind_tuple_pattern evaluate environment scrutinee)
            cases
      | _ -> (
          match evaluate scrutinee with
          | None -> None
          | Some scrutinee ->
              choose
                (fun pattern ->
                  ground_bind_pattern environment pattern scrutinee)
                cases))
  | Sst.Checked_arithmetic (operation, arguments) ->
      let rec integers values = function
        | [] -> Some (List.rev values)
        | argument :: rest -> (
            match evaluate argument with
            | Some (Ground_integer value) -> integers (value :: values) rest
            | Some
                (Ground_unit | Ground_boolean _ | Ground_aggregate _)
            | None ->
                None)
      in
      Option.bind (integers [] arguments) (fun arguments ->
          let result =
            match (operation, arguments) with
            | Sst.Add, [ left; right ] -> Some (Z.add left right)
            | Sst.Subtract, [ left; right ] -> Some (Z.sub left right)
            | Sst.Negate, [ value ] -> Some (Z.neg value)
            | Sst.Multiply_constant constant, [ value ] ->
                Some (Z.mul constant value)
            | Sst.Successor, [ value ] -> Some (Z.succ value)
            | Sst.Predecessor, [ value ] -> Some (Z.pred value)
            | Sst.Absolute_value, [ value ] -> Some (Z.abs value)
            | _ -> None
          in
          Option.bind result checked_integer
          |> Option.map (fun value -> Ground_integer value))
  | Sst.Compare (comparison, left, right) -> (
      match (evaluate left, evaluate right) with
      | Some left, Some right ->
          Option.map
            (fun result -> Ground_boolean result)
            (ground_sst_comparison comparison left right)
      | None, _ | _, None -> None)
  | Sst.Boolean_not operand -> (
      match evaluate operand with
      | Some (Ground_boolean value) -> Some (Ground_boolean (not value))
      | Some (Ground_unit | Ground_integer _ | Ground_aggregate _) | None ->
          None)
  | Sst.Boolean_binary (operation, left, right) -> (
      match (evaluate left, evaluate right) with
      | Some (Ground_boolean left), Some (Ground_boolean right) ->
          Some
            (Ground_boolean
               (match operation with
               | Sst.And -> left && right
               | Sst.Or -> left || right))
      | ( Some
            (Ground_unit | Ground_integer _ | Ground_aggregate _),
          _ )
      | (_, Some (Ground_unit | Ground_integer _ | Ground_aggregate _))
      | None, _ | _, None ->
          None)
  | Sst.Constructor_value { constructor; arguments = [] } ->
      Some (Ground_aggregate constructor)
  | Sst.Direct_call { recursive = true; _ } when fuel <= 0 -> None
  | Sst.Direct_call _ | Sst.Callback_call _ | Sst.Callback_requires _
  | Sst.Callback_ensures _ | Sst.Tuple_value _ | Sst.Record_value _
  | Sst.Constructor_value { arguments = _ :: _; _ } | Sst.Field_read _
  | Sst.Field_write _ | Sst.Shared_scalar_field_write _
  | Sst.Owned_tree_nested_write _
  | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
  | Sst.Mutable_write _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
  | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Proof_region _
  | Sst.Old _ | Sst.Forall _ | Sst.Exists _
  | Sst.Symbolic_application _ ->
      None
let exact_nullary_constructor source constructor =
  List.exists
    (fun (type_definition : Sst.type_definition) ->
      type_definition.type_id = constructor.Sst.constructor_type
      &&
      match type_definition.type_kind with
      | Sst.Variant_definition constructors ->
          List.exists (fun (candidate : Sst.constructor_definition) ->
              same_constructor_id candidate.constructor_id constructor
              && candidate.constructor_fields = []) constructors
      | Sst.Record_definition _ -> false)
    source.solver_types
type ground_vir_context = {
  ground_source : solver_source;
  ground_activations : Spec_unfolding.activation list;
  ground_symbols : (Vir.symbol * Sst.constructor_id) list;
}
let ground_symbol context symbol =
  List.find_map
    (fun (candidate, constructor) ->
      if candidate = symbol then Some constructor else None)
    context.ground_symbols
let ground_recursive_application context callee arguments =
  let depths =
    context.ground_activations
    |> List.filter_map (fun (activation : Spec_unfolding.activation) ->
           if same_function_id activation.function_id callee
              && activation.depth > 0
           then Some activation.depth
           else None)
    |> List.sort_uniq Int.compare
  in
  match depths with
  | [] -> None
  | depths -> (
      match List.find_opt
              (fun definition -> same_function_id definition.solver_id callee)
              context.ground_source.solver_definitions with
      | None -> None
      | Some definition ->
          let parameters =
            List.map Sst.require_value_parameter definition.solver_parameters
          in
          if List.length parameters <> List.length arguments
          then None
          else
            let bind arguments =
              List.fold_left2
                (fun environment parameter argument ->
                  Option.bind environment (fun environment ->
                      match
                        ground_bind_pattern environment
                          parameter.Sst.pattern argument
                      with
                      | Ground_pattern_match environment ->
                          Some environment
                      | Ground_pattern_mismatch
                      | Ground_pattern_unsupported ->
                          None))
                (Some []) parameters arguments
            in
            Option.bind (bind arguments) (fun environment ->
                let body = definition.solver_body in
                let results =
                  List.map
                    (fun depth ->
                      ground_sst_expression depth environment body)
                    depths
                in
                match results with
                | Some first :: rest
                  when List.for_all
                         (function
                           | Some value -> ground_value_equal first value
                           | None -> false)
                         rest ->
                    Some first
                | [] | None :: _ | Some _ :: _ -> None))
let rec ground_vir_integer context = function
  | Vir.Integer_constant value -> Some value
  | Vir.Integer_symbol _ -> None
  | Vir.Integer_symbolic_application _ -> None
  | Vir.Integer_add (left, right) -> (
      match
        (ground_vir_integer context left, ground_vir_integer context right)
      with
      | Some left, Some right -> checked_integer (Z.add left right)
      | None, _ | _, None -> None)
  | Vir.Integer_subtract (left, right) -> (
      match
        (ground_vir_integer context left, ground_vir_integer context right)
      with
      | Some left, Some right -> checked_integer (Z.sub left right)
      | None, _ | _, None -> None)
  | Vir.Integer_negate value ->
      Option.bind (ground_vir_integer context value) (fun value ->
          checked_integer (Z.neg value))
  | Vir.Integer_multiply_constant (constant, value) ->
      Option.bind (ground_vir_integer context value) (fun value ->
          checked_integer (Z.mul constant value))
  | Vir.Integer_absolute_value value ->
      Option.bind (ground_vir_integer context value) (fun value ->
          checked_integer (Z.abs value))
  | Vir.Aggregate_tag (_, aggregate) ->
      Option.map
        (fun constructor -> Z.of_int constructor.Sst.constructor_index)
        (ground_vir_aggregate context aggregate)
  | Vir.Integer_recursive_spec_application { callee; arguments; _ } ->
      Option.bind
        (ground_vir_arguments context arguments)
        (fun arguments ->
          match ground_recursive_application context callee arguments with
          | Some (Ground_integer value) -> Some value
          | Some
              (Ground_unit | Ground_boolean _ | Ground_aggregate _)
          | None ->
              None)
  | Vir.Integer_conditional _ -> None
  | Vir.Integer_rank_project _ | Vir.Integer_selector _ -> None
and ground_vir_aggregate context aggregate =
  match aggregate.Vir.aggregate_desc with
  | Vir.Aggregate_symbol symbol -> ground_symbol context symbol
  | Vir.Aggregate_constructor { constructor; arguments = [] } ->
      if exact_nullary_constructor context.ground_source constructor
      then Some constructor
      else None
  | Vir.Aggregate_conditional (condition, consequent, alternative) -> (
      match ground_vir_boolean context condition with
      | Some true -> ground_vir_aggregate context consequent
      | Some false -> ground_vir_aggregate context alternative
      | None -> None)
  | Vir.Aggregate_selector _
  | Vir.Aggregate_symbolic_application _
  | Vir.Aggregate_imported_model_application _
  | Vir.Aggregate_constructor { arguments = _ :: _; _ }
  | Vir.Aggregate_record _ | Vir.Aggregate_recursive_spec_application _ ->
      None
and ground_vir_argument context = function
  | Vir.Recursive_integer_argument term ->
      Option.map
        (fun value -> Ground_integer value)
        (ground_vir_integer context term)
  | Vir.Recursive_boolean_argument term ->
      Option.map
        (fun value -> Ground_boolean value)
        (ground_vir_boolean context term)
  | Vir.Recursive_aggregate_argument term ->
      Option.map
        (fun value -> Ground_aggregate value)
        (ground_vir_aggregate context term)
  | Vir.Recursive_parametric_argument _ -> None
and ground_vir_arguments context arguments =
  let rec evaluate values = function
    | [] -> Some (List.rev values)
    | argument :: rest ->
        Option.bind (ground_vir_argument context argument) (fun value ->
            evaluate (value :: values) rest)
  in
  evaluate [] arguments
and ground_vir_boolean context = function
  | Vir.Forall_term _ | Vir.Exists_term _ -> None
  | Vir.Boolean_constant value -> Some value
  | Vir.Logical_adt_schema _ -> Some true
  | Vir.Boolean_symbol _ -> None
  | Vir.Boolean_symbolic_application _ -> None
  | Vir.Boolean_not operand ->
      Option.map not (ground_vir_boolean context operand)
  | Vir.Boolean_and (left, right) -> (
      match
        (ground_vir_boolean context left, ground_vir_boolean context right)
      with
      | Some left, Some right -> Some (left && right)
      | None, _ | _, None -> None)
  | Vir.Boolean_or (left, right) -> (
      match
        (ground_vir_boolean context left, ground_vir_boolean context right)
      with
      | Some left, Some right -> Some (left || right)
      | None, _ | _, None -> None)
  | Vir.Integer_compare (comparison, left, right) -> (
      match
        (ground_vir_integer context left, ground_vir_integer context right)
      with
      | Some left, Some right ->
          let order = Z.compare left right in
          Some
            (match comparison with
            | Vir.Equal -> order = 0
            | Vir.Not_equal -> order <> 0
            | Vir.Less_than -> order < 0
            | Vir.Less_or_equal -> order <= 0
            | Vir.Greater_than -> order > 0
            | Vir.Greater_or_equal -> order >= 0)
      | None, _ | _, None -> None)
  | Vir.Boolean_equal (left, right) -> (
      match
        (ground_vir_boolean context left, ground_vir_boolean context right)
      with
      | Some left, Some right -> Some (Bool.equal left right)
      | None, _ | _, None -> None)
  | Vir.Boolean_not_equal (left, right) -> (
      match
        (ground_vir_boolean context left, ground_vir_boolean context right)
      with
      | Some left, Some right -> Some (not (Bool.equal left right))
      | None, _ | _, None -> None)
  | Vir.Aggregate_equal (left, right) -> (
      match
        ( ground_vir_aggregate context left,
          ground_vir_aggregate context right )
      with
      | Some left, Some right -> Some (same_constructor_id left right)
      | None, _ | _, None -> None)
  | Vir.Boolean_recursive_spec_application { callee; arguments; _ } ->
      Option.bind
        (ground_vir_arguments context arguments)
        (fun arguments ->
          match ground_recursive_application context callee arguments with
          | Some (Ground_boolean value) -> Some value
          | Some
              (Ground_unit | Ground_integer _ | Ground_aggregate _)
          | None ->
              None)
  | Vir.Boolean_specification_application _
  | Vir.Parametric_equal _ | Vir.Boolean_selector _
  | Vir.Boolean_invariant_application _
  | Vir.Callback_requires _ | Vir.Callback_ensures _ ->
      None
let ground_bindings source bindings =
  let rec bind bound = function
    | [] -> if bound = [] then None else Some (List.rev bound)
    | (symbol, constructor) :: rest ->
        if symbol.Vir.role <> Vir.Input
           || not (exact_nullary_constructor source constructor)
        then None
        else
          match
            List.find_opt
              (fun (candidate, _) -> candidate = symbol)
              bound
          with
          | None -> bind ((symbol, constructor) :: bound) rest
          | Some (_, existing) when same_constructor_id existing constructor ->
              bind bound rest
          | Some _ -> None
  in
  bind [] bindings
let classify_ground payload =
  let abstain antecedents_checked = (Ground_abstain,
    { attempts = 1; complete_violations = 0; abstentions = 1; antecedents_checked }) in
  let obligation = payload.ground_obligation in
  match (obligation.kind,
         ground_bindings payload.ground_source payload.ground_constructors) with
  | Vir.Local_assertion _, Some ground_symbols ->
      let context = { ground_source = payload.ground_source;
        ground_activations = payload.ground_activations; ground_symbols } in
      let antecedents = obligation.assumptions @ obligation.required_preceding_safety
                        @ obligation.path_condition in
      let evaluated = List.map (ground_vir_boolean context) antecedents in
      let antecedents_checked = List.fold_left2 (fun count antecedent -> function
          | Some _ when
              (match antecedent with Vir.Logical_adt_schema _ -> true | _ -> false)
            -> count
          | Some _ when Immutable_aggregate_fact_relevance_private
                          .is_exact_aggregate_construction_equality antecedent -> count
          | Some _ -> count + 1 | None -> count) 0 antecedents evaluated in
      let goal = ground_vir_boolean context obligation.goal in
      if List.for_all (function Some true -> true | Some false | None -> false)
           evaluated && goal = Some false
      then (Ground_complete_violation, { attempts = 1; complete_violations = 1;
        abstentions = 0; antecedents_checked })
      else abstain antecedents_checked
  | ( (Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Postcondition _
      | Vir.Call_precondition _ | Vir.Callback_precondition _
      | Vir.Invariant_validity _
      | Vir.Entry_measure_nonnegative _
      | Vir.Recursive_call_measure_nonnegative _
      | Vir.Recursive_call_strict_descent _),
      (None | Some _) )
  | Vir.Local_assertion _, None ->
      abstain 0
let commit_ground_counterexample_counters counters =
  ground_counterexample_attempts := !ground_counterexample_attempts + counters.attempts;
  ground_counterexample_complete_violations := !ground_counterexample_complete_violations + counters.complete_violations;
  ground_counterexample_abstentions := !ground_counterexample_abstentions + counters.abstentions;
  ground_counterexample_antecedents_checked := !ground_counterexample_antecedents_checked + counters.antecedents_checked
let ground_counterexample verified ~activations ~ground_constructors obligation =
  let outcome, counters = classify_ground {
      ground_source = solver_source verified; ground_activations = activations;
      ground_constructors; ground_obligation = obligation } in
  commit_ground_counterexample_counters counters; outcome
let prepare_proof ~controls verified ~activations ~ground_constructors ~routed obligation =
  let* initial_query = build_proof_obligation_query ~solver_controls:controls ~record_construction:false verified ~activations obligation in
  let initial_query = detach_query initial_query in
  let* retry_query =
    if not routed then Ok None else
      let* specialization = nullary_branch_specialization ~solver_controls:controls verified ~activations ~ground_constructors obligation in
      match specialization with
      | None -> Ok None
      | Some specialization -> build_proof_obligation_query ~specialization ~solver_controls:controls
          ~record_construction:false verified ~activations obligation
          |> Result.map (fun query -> Some (detach_query query))
  in
  let ground_payload = if routed then Some { ground_source = solver_source verified; ground_activations = activations;
      ground_constructors; ground_obligation = obligation } else None in
  let retry_eligible = match obligation.Vir.kind with Vir.Local_assertion _ -> routed
    | Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Postcondition _
    | Vir.Call_precondition _ | Vir.Callback_precondition _
    | Vir.Invariant_validity _ | Vir.Entry_measure_nonnegative _
    | Vir.Recursive_call_measure_nonnegative _ | Vir.Recursive_call_strict_descent _ -> false in
  Ok { initial_query; retry_query; retry_eligible; ground_payload; solver_controls = controls }
let prepared_initial_query prepared = prepared.initial_query and prepared_retry_query prepared = prepared.retry_query
let detach_prepared_query (Prepared_query encoded) =
  Z3_bridge.detach_query (Marshal.from_string encoded 0 : Logic_ir.query)
let prepared_retry_eligible prepared = prepared.retry_eligible and prepared_ground_payload prepared = prepared.ground_payload
let prepared_ground_classification prepared =
  Option.map classify_ground prepared.ground_payload
let prepared_retry_control prepared = if prepared.solver_controls.force_retry_counterexample then `Counterexample
  else if prepared.solver_controls.force_retry_unknown then `Unknown else `Real
let commit_prepared_observations ~proof_queries ~(retry : nullary_branch_retry_counters)
    ~(ground : ground_counterexample_counters) ~last_retry_query =
  proof_query_constructions := !proof_query_constructions + proof_queries;
  nullary_branch_retry_attempts := !nullary_branch_retry_attempts + retry.attempts;
  nullary_branch_retry_queries := !nullary_branch_retry_queries + retry.queries;
  nullary_branch_retry_facts := !nullary_branch_retry_facts + retry.facts;
  nullary_branch_retry_verified := !nullary_branch_retry_verified + retry.verified;
  nullary_branch_retry_counterexamples := !nullary_branch_retry_counterexamples + retry.counterexamples;
  nullary_branch_retry_inconclusives := !nullary_branch_retry_inconclusives + retry.inconclusives;
  nullary_branch_retry_abstentions := !nullary_branch_retry_abstentions + retry.abstentions;
  Option.iter (fun query -> last_prepared_nullary_branch_retry_query_for_testing := Some query)
    last_retry_query;
  commit_ground_counterexample_counters ground
let solve_proof_obligation ?rlimit ~timeout_ms verified ~activations obligation =
  let* solver_policy = resolve_solver_policy obligation.Vir.span ?rlimit ~timeout_ms () in
  let* query = proof_obligation_query verified ~activations obligation in
  let config = { Z3_bridge.timeout_ms = Solver_policy_private.timeout_ms solver_policy;
                 model = false } in
  of_backend obligation.Vir.span (Z3_bridge.solve_query
    ~rlimit:(Solver_policy_private.rlimit solver_policy) config query)
let retry_nullary_branch ?rlimit ~timeout_ms verified ~activations
    ~ground_constructors obligation =
  let* solver_policy = resolve_solver_policy obligation.Vir.span ?rlimit ~timeout_ms () in
  incr nullary_branch_retry_attempts;
  let* specialization = nullary_branch_specialization verified ~activations ~ground_constructors obligation in
  match specialization with
  | None -> incr nullary_branch_retry_abstentions; Ok Nullary_branch_abstain
  | Some specialization ->
      let* query = build_proof_obligation_query ~specialization verified ~activations obligation in
      incr nullary_branch_retry_queries; incr nullary_branch_retry_facts;
      last_nullary_branch_retry_query_for_testing := Some query;
      let config = { Z3_bridge.timeout_ms = Solver_policy_private.timeout_ms solver_policy;
                     model = false } in
      let rlimit = Solver_policy_private.rlimit solver_policy in
      let result = if !force_nullary_branch_retry_counterexample_for_testing then
          Ok (Z3_bridge.Counterexample []) else if !force_nullary_branch_retry_unknown_for_testing
        then Z3_bridge.solve_query ~controlled:Z3_bridge.Force_unknown ~rlimit config query
        else Z3_bridge.solve_query ~rlimit config query in
      let* outcome = of_backend obligation.Vir.span result in
      (match outcome with
      | Z3_bridge.Verified -> incr nullary_branch_retry_verified; Ok Nullary_branch_verified
      | Z3_bridge.Counterexample _ -> incr nullary_branch_retry_counterexamples;
          Ok Nullary_branch_not_verified
      | Z3_bridge.Inconclusive reason -> incr nullary_branch_retry_inconclusives;
          Ok (Nullary_branch_inconclusive reason))
let proof_entry_activations verified function_id =
  Spec_unfolding_private.proof_entry_activations (prepared verified) function_id
let error_to_string error =
  Printf.sprintf "%s at %s:%d:%d-%d:%d" error.message
    (Filename.basename error.span.Diagnostic.file)
    error.span.start_pos.line error.span.start_pos.column error.span.end_pos.line
    error.span.end_pos.column
module For_testing = struct
  let termination_obligations = Spec_unfolding_private.termination_obligations
  let verify_with_requirements = verify_with_requirements
  let proof_obligation_query = proof_obligation_query
  let proof_query_construction_count () = !proof_query_constructions
  let reset_proof_query_construction_count () =
    proof_query_constructions := 0
  let ground_counterexample_counters () =
    {
      attempts = !ground_counterexample_attempts;
      complete_violations =
        !ground_counterexample_complete_violations;
      abstentions = !ground_counterexample_abstentions;
      antecedents_checked = !ground_counterexample_antecedents_checked;
    }
  let reset_ground_counterexample_counters () =
    ground_counterexample_attempts := 0;
    ground_counterexample_complete_violations := 0;
    ground_counterexample_abstentions := 0;
    ground_counterexample_antecedents_checked := 0
  let nullary_branch_retry_counters () =
    {
      attempts = !nullary_branch_retry_attempts;
      queries = !nullary_branch_retry_queries;
      facts = !nullary_branch_retry_facts;
      verified = !nullary_branch_retry_verified;
      counterexamples = !nullary_branch_retry_counterexamples;
      inconclusives = !nullary_branch_retry_inconclusives;
      abstentions = !nullary_branch_retry_abstentions;
    }
  let reset_nullary_branch_retry_counters () =
    nullary_branch_retry_attempts := 0;
    nullary_branch_retry_queries := 0;
    nullary_branch_retry_facts := 0;
    nullary_branch_retry_verified := 0;
    nullary_branch_retry_counterexamples := 0;
    nullary_branch_retry_inconclusives := 0;
    nullary_branch_retry_abstentions := 0;
    last_nullary_branch_retry_query_for_testing := None; last_prepared_nullary_branch_retry_query_for_testing := None;
    force_nullary_branch_retry_unknown_for_testing := false;
    force_nullary_branch_retry_counterexample_for_testing := false;
    suppress_original_nullary_branch_activation_for_testing := false
  let last_nullary_branch_retry_query () =
    match !last_prepared_nullary_branch_retry_query_for_testing with
    | Some (Prepared_query encoded) -> Some (Marshal.from_string encoded 0 : Logic_ir.query)
    | None -> !last_nullary_branch_retry_query_for_testing
  let force_nullary_branch_retry_unknown force =
    force_nullary_branch_retry_unknown_for_testing := force
  let force_nullary_branch_retry_counterexample force =
    force_nullary_branch_retry_counterexample_for_testing := force
  let nullary_branch_argument_supported =
    nullary_branch_argument_supported
  let suppress_original_nullary_branch_activation suppress =
    suppress_original_nullary_branch_activation_for_testing := suppress
  let suppress_reached_aggregate_authority suppress =
    suppress_reached_aggregate_authority_for_testing := suppress
  let a2_builder_construction_count () = !a2_builder_constructions
  let reset_a2_builder_construction_count () =
    a2_builder_constructions := 0
  let helper_expansion_count =
    Typedtree_adapter_private.Public.Recursive_helper_for_testing.expansion_count
  let reset_helper_expansion_count =
    Typedtree_adapter_private.Public.Recursive_helper_for_testing
    .reset_expansion_count
  let reordered_helper_closure_copy_rejected ~program ~definition =
    match
      Typedtree_adapter_private.Public.Recursive_helper_for_testing
      .with_reordered_closure_copy ~program ~definition (fun () ->
        prepare program)
    with
    | Some (Error _) -> true
    | None | Some (Ok _) -> false
  let raw_helper_copy_rejected ~program ~definition =
    match
      Typedtree_adapter_private.Public.Recursive_helper_for_testing
      .with_raw_helper_copy ~program ~definition (fun () -> prepare program)
    with
    | Some (Error _) -> true
    | None | Some (Ok _) -> false
  let partial_helper_call_rejected ~program ~definition =
    match
      Typedtree_adapter_private.Public.Recursive_helper_for_testing
      .with_partial_helper_call ~program ~definition (fun () ->
        prepare program)
    with
    | Some (Error _) -> true
    | None | Some (Ok _) -> false
  let recursive_lowering_count =
    Spec_unfolding_private.For_testing.recursive_lowering_count
  let reset_recursive_lowering_count =
    Spec_unfolding_private.For_testing.reset_recursive_lowering_count
end
