type parameter_kind =
  | Positional_parameter
  | Labelled_parameter
  | Optional_parameter
  | Default_parameter

type recursive_evidence = {
  provider_completion : Verified_provider_completion_private.t;
  source_definition_digest : string;
}

type formal = {
  ordinal : int;
  label : string option;
  kind : parameter_kind;
  mode : Sst.instance_mode;
  typ : Parametric_type.t;
  default_digest : string option;
}

type t = {
  binders : Parametric_type.binder list;
  formals : formal list;
  result_type : Parametric_type.t;
  result_mode : Sst.instance_mode;
  contracts : Sst.contracts;
  recursive : bool;
  recursive_evidence : recursive_evidence option;
  source_definition : Sst.function_definition;
  definition_digest : string;
  semantic_fingerprint : string;
  semantic_dump : string;
}

type instantiated = {
  type_arguments : Parametric_type.t list;
  parameters : Sst.parameter list;
  parameter_types : Parametric_type.t list;
  result_type : Parametric_type.t;
  contracts : Sst.contracts;
  semantic_fingerprint : string;
  semantic_dump : string;
}

let digest value = Digest.string value |> Digest.to_hex

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let definition_digest (definition : Sst.function_definition) =
  digest
    (Sst.to_string
       {
         Sst.policy = definition.Sst.policy;
         parametric_adts = [];
         types = [];
         functions = [ definition ];
       })

let verified_recursion ~definition ~provider_completion =
  if
    Verified_provider_completion_private.authenticates_recursive_definition
      provider_completion definition
  then
    Ok
      {
        provider_completion;
        source_definition_digest = definition_digest definition;
      }
  else Error "retained recursive provider completion is unauthenticated"

let mode_name = function
  | Sst.Exec_instance -> "exec"
  | Sst.Tracked_instance -> "tracked"
  | Sst.Ghost_instance -> "ghost"

let kind_name = function
  | Positional_parameter -> "positional"
  | Labelled_parameter -> "labelled"
  | Optional_parameter -> "optional"
  | Default_parameter -> "default"

let canonical_substitutions binders =
  let owner = Parametric_type.owner ~index:(-1) ~name:"signature" in
  List.mapi
    (fun ordinal binder ->
      (binder, Parametric_type.Parameter (Parametric_type.binder owner ~ordinal)))
    binders

let canonical_type substitutions typ =
  Parametric_type.substitute substitutions typ |> Parametric_type.to_string

let field_name field =
  match field.Sst.field_owner with
  | Sst.Record_owner owner ->
      Printf.sprintf "%s#%d.%s#%d" owner.type_name owner.type_index
        field.field_name field.field_index
  | Sst.Constructor_owner constructor ->
      Printf.sprintf "%s#%d.%s#%d.%s#%d" constructor.constructor_type.type_name
        constructor.constructor_type.type_index constructor.constructor_name
        constructor.constructor_index field.field_name field.field_index

let constructor_name constructor =
  Printf.sprintf "%s#%d.%s#%d" constructor.Sst.constructor_type.type_name
    constructor.constructor_type.type_index constructor.constructor_name
    constructor.constructor_index

let rec pattern_dump substitutions (pattern : Sst.pattern) =
  let typ = canonical_type substitutions pattern.Sst.typ in
  let desc =
    match pattern.pattern_desc with
    | Sst.Wildcard -> "_"
    | Sst.Bind binding -> Printf.sprintf "bind#%d" binding.id
    | Sst.Owned_tree_cursor_pattern cursor ->
        Printf.sprintf "cursor#%d" cursor.cursor_binding.id
    | Sst.Int_pattern value -> "int:" ^ Z.to_string value
    | Sst.Bool_pattern value -> "bool:" ^ string_of_bool value
    | Sst.Unit_pattern -> "unit"
    | Sst.Tuple_pattern components ->
        components
        |> List.map (fun (label, nested) ->
            Option.value ~default:"_" label
            ^ "="
            ^ pattern_dump substitutions nested)
        |> String.concat "," |> Printf.sprintf "tuple[%s]"
    | Sst.Record_pattern fields ->
        fields
        |> List.map (fun (field, nested) ->
            field_name field ^ "=" ^ pattern_dump substitutions nested)
        |> String.concat ","
        |> Printf.sprintf "record[%s]"
    | Sst.Constructor_pattern (constructor, arguments) ->
        Printf.sprintf "constructor[%s](%s)"
          (constructor_name constructor)
          (String.concat "," (List.map (pattern_dump substitutions) arguments))
    | Sst.Or_pattern (left, right) ->
        "or("
        ^ pattern_dump substitutions left
        ^ ","
        ^ pattern_dump substitutions right
        ^ ")"
  in
  desc ^ ":" ^ typ

let rec expression_dump substitutions (expression : Sst.expression) =
  let recurse = expression_dump substitutions in
  let children values = String.concat "," (List.map recurse values) in
  let desc =
    match expression.Sst.expression_desc with
    | Sst.Int_constant value -> "int:" ^ Z.to_string value
    | Sst.Bool_constant value -> "bool:" ^ string_of_bool value
    | Sst.Unit_constant -> "unit"
    | Sst.Variable { binding; _ } -> Printf.sprintf "var#%d" binding.id
    | Sst.Tuple_value values -> "tuple(" ^ children (List.map snd values) ^ ")"
    | Sst.Record_value { record_type; fields } ->
        Printf.sprintf "record:%s#%d(%s)" record_type.type_name
          record_type.type_index
          (fields
          |> List.map (fun (field, value) ->
              field_name field ^ "=" ^ recurse value)
          |> String.concat ",")
    | Sst.Constructor_value { constructor; arguments } ->
        "constructor:"
        ^ constructor_name constructor
        ^ "(" ^ children arguments ^ ")"
    | Sst.Field_read { record; field } ->
        "field:" ^ field_name field ^ "(" ^ recurse record ^ ")"
    | Sst.Field_write { field; value; _ }
    | Sst.Shared_scalar_field_write { field; value; _ } ->
        "write:" ^ field_name field ^ "(" ^ recurse value ^ ")"
    | Sst.Owned_tree_nested_write { value; _ } | Sst.Mutable_write { value; _ }
      ->
        "write(" ^ recurse value ^ ")"
    | Sst.Owned_tree_rebase _ -> "rebase"
    | Sst.Let_mutable (binding, initial, body) ->
        Printf.sprintf "letmut#%d(%s,%s)" binding.id (recurse initial)
          (recurse body)
    | Sst.Mutable_read binding -> Printf.sprintf "read#%d" binding.id
    | Sst.Let (bindings, body) ->
        "let("
        ^ (bindings
          |> List.map (fun (pattern, value) ->
              pattern_dump substitutions pattern ^ "=" ^ recurse value)
          |> String.concat ",")
        ^ ";" ^ recurse body ^ ")"
    | Sst.Sequence (left, right) -> "sequence(" ^ children [ left; right ] ^ ")"
    | Sst.If (condition, yes, no) ->
        "if(" ^ children (condition :: yes :: Option.to_list no) ^ ")"
    | Sst.Match (scrutinee, cases) ->
        "match(" ^ recurse scrutinee ^ ";"
        ^ (cases
          |> List.map (fun case ->
              pattern_dump substitutions case.Sst.case_pattern
              ^ "=>"
              ^ Option.fold ~none:""
                  ~some:(fun guard -> recurse guard ^ "?")
                  case.case_guard
              ^ recurse case.case_body)
          |> String.concat "|")
        ^ ")"
    | Sst.Checked_arithmetic (_, operands) ->
        "arithmetic(" ^ children operands ^ ")"
    | Sst.Compare (_, left, right) ->
        "compare(" ^ children [ left; right ] ^ ")"
    | Sst.Boolean_not operand -> "not(" ^ recurse operand ^ ")"
    | Sst.Boolean_binary (_, left, right) ->
        "boolean(" ^ children [ left; right ] ^ ")"
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        let metadata = quantifier.quantifier_metadata in
        Printf.sprintf "%s#%d<%s>{owner=%s;qid=%s;skid=%s}(%s;%s)"
          (Logic_quantifier_private.kind_to_string
             (Logic_quantifier_private.kind metadata))
          quantifier.quantifier_binder.id
          (canonical_type substitutions quantifier.quantifier_binder.typ)
          (Logic_quantifier_private.owner metadata)
          (Logic_quantifier_private.qid metadata)
          (Logic_quantifier_private.skid metadata)
          (recurse quantifier.quantifier_body)
          (Option.fold ~none:""
             ~some:(fun trigger -> "trigger=" ^ recurse trigger)
             quantifier.quantifier_trigger)
    | Sst.Direct_call
        { call_form = _; callee; type_arguments; arguments; recursive } ->
        Printf.sprintf "call:%s#%d:%b<%s>(%s)" callee.function_name
          callee.function_index recursive
          (String.concat ","
             (List.map (canonical_type substitutions) type_arguments))
          (children
             (List.map
                (fun argument -> snd (Sst.require_value_argument argument))
                arguments))
    | Sst.Reveal id ->
        Printf.sprintf "reveal:%s#%d" id.function_name id.function_index
    | Sst.Reveal_with_fuel { function_id; literal_depth } ->
        Printf.sprintf "reveal:%s#%d:%s" function_id.function_name
          function_id.function_index literal_depth
    | Sst.Use_type_invariant { use_id; value } ->
        "invariant:" ^ use_id ^ "(" ^ recurse value ^ ")"
    | Sst.Local_assert { assertion_ordinal; predicate } ->
        Printf.sprintf "assert:%d(%s)" assertion_ordinal (recurse predicate)
    | Sst.Proof_region body -> "proof(" ^ recurse body ^ ")"
    | Sst.Old body -> "old(" ^ recurse body ^ ")"
    | Sst.Optional_absent -> "optional-absent"
    | Sst.Optional_present payload ->
        "optional-present(" ^ recurse payload ^ ")"
    | Sst.Optional_forward payload ->
        "optional-forward(" ^ recurse payload ^ ")"
    | Sst.Callback_call application ->
        "callback-call#" ^ string_of_int application.callback.Sst.callback_id
    | Sst.Callback_requires application ->
        "callback-requires#"
        ^ string_of_int application.callback.Sst.callback_id
    | Sst.Callback_ensures { application; result } ->
        "callback-ensures#"
        ^ string_of_int application.callback.Sst.callback_id
        ^ "(" ^ recurse result ^ ")"
    | Sst.Symbolic_application application ->
        "symbolic:"
        ^ Symbolic_application_private.identity_digest application
        ^ "("
        ^ children (Symbolic_application_private.arguments application)
        ^ ")"
  in
  desc ^ ":" ^ canonical_type substitutions expression.typ

let contracts_dump substitutions (contracts : Sst.contracts) =
  let predicates name clauses =
    clauses
    |> List.map (fun (clause : Sst.predicate_clause) ->
        Printf.sprintf "%s#%d=%s" name clause.Sst.clause_index
          (expression_dump substitutions clause.predicate.expression))
  in
  let ensures =
    contracts.Sst.ensures
    |> List.map (fun (clause : Sst.ensures_clause) ->
        Printf.sprintf "ensures#%d:%s=%s" clause.clause_index
          (Option.fold ~none:"_"
             ~some:(pattern_dump substitutions)
             clause.binder)
          (expression_dump substitutions clause.predicate.expression))
  in
  String.concat ";"
    (predicates "requires" contracts.requires
    @ ensures
    @ predicates "decreases" contracts.decreases
    @ predicates "assertions" contracts.assertions)

let semantic_dump_of_parts ~binders ~formals ~result_type ~result_mode
    ~contracts ~recursive ~recursive_evidence =
  let substitutions = canonical_substitutions binders in
  let binders_dump =
    binders
    |> List.mapi (fun ordinal _ -> Printf.sprintf "'%d" ordinal)
    |> String.concat ","
  in
  let formals_dump =
    formals
    |> List.map (fun formal ->
        Printf.sprintf "%d:%s:%s:%s:%s:%s" formal.ordinal
          (Option.value ~default:"_" formal.label)
          (kind_name formal.kind) (mode_name formal.mode)
          (canonical_type substitutions formal.typ)
          (Option.value ~default:"none" formal.default_digest))
    |> String.concat ";"
  in
  String.concat "|"
    [
      "parametric-signature-v1";
      "binders=" ^ binders_dump;
      "formals=" ^ formals_dump;
      "result=" ^ canonical_type substitutions result_type;
      "result-mode=" ^ mode_name result_mode;
      "contracts=" ^ contracts_dump substitutions contracts;
      "recursive=" ^ string_of_bool recursive;
      "recursive-evidence="
      ^ Option.fold ~none:"none" ~some:(fun _ -> "verified") recursive_evidence;
    ]

let default_digest substitutions = function
  | None -> None
  | Some default ->
      Some
        (digest
           (pattern_dump substitutions default.Sst.optional_pattern
           ^ "="
           ^ expression_dump substitutions default.optional_expression))

let expected_kind parameter =
  let parameter = Sst.require_value_parameter parameter in
  match (parameter.optional_default, parameter.label) with
  | Some _, Some label when String.starts_with ~prefix:"?" label ->
      Default_parameter
  | Some _, (None | Some _) -> Default_parameter
  | None, Some label when String.starts_with ~prefix:"?" label ->
      Optional_parameter
  | None, Some _ -> Labelled_parameter
  | None, None -> Positional_parameter

let binders_are_canonical (definition : Sst.function_definition) =
  let owner =
    Parametric_type.owner ~index:definition.function_id.function_index
      ~name:definition.function_id.function_name
  in
  let rec loop ordinal = function
    | [] -> true
    | (binder : Parametric_type.binder) :: rest ->
        binder.ordinal = ordinal
        && Parametric_type.compare_owner binder.owner owner = 0
        && loop (ordinal + 1) rest
  in
  loop 0 definition.type_binders

let create ~definition ~parameter_kinds ~parameter_modes ~result_mode
    ~recursive_evidence =
  let binders = definition.Sst.type_binders in
  let substitutions = canonical_substitutions binders in
  if not (binders_are_canonical definition) then
    Error "retained parametric signature binder owner/order mismatch"
  else if
    List.length parameter_kinds <> List.length definition.parameters
    || List.length parameter_modes <> List.length definition.parameters
  then Error "retained parametric signature formal vector length mismatch"
  else if parameter_kinds <> List.map expected_kind definition.parameters then
    Error "retained parametric signature label/default vector mismatch"
  else if definition.recursive <> Option.is_some recursive_evidence then
    Error "retained recursive provider verification evidence mismatch"
  else if
    Option.fold ~none:false
      ~some:(fun evidence ->
        (not
           (String.equal evidence.source_definition_digest
              (definition_digest definition)))
        || not
             (Verified_provider_completion_private
              .authenticates_recursive_definition evidence.provider_completion
                definition))
      recursive_evidence
  then Error "retained recursive provider verification evidence is stale"
  else
    let formals =
      List.mapi
        (fun ordinal parameter ->
          let parameter = Sst.require_value_parameter parameter in
          let kind = List.nth parameter_kinds ordinal
          and mode = List.nth parameter_modes ordinal in
          {
            ordinal;
            label = parameter.Sst.label;
            kind;
            mode;
            typ = parameter.pattern.typ;
            default_digest =
              default_digest substitutions parameter.optional_default;
          })
        definition.parameters
    in
    let dump =
      semantic_dump_of_parts ~binders ~formals
        ~result_type:definition.result_type ~result_mode
        ~contracts:definition.contracts ~recursive:definition.recursive
        ~recursive_evidence
    in
    Ok
      {
        binders;
        formals;
        result_type = definition.result_type;
        result_mode;
        contracts = definition.contracts;
        recursive = definition.recursive;
        recursive_evidence;
        source_definition = definition;
        definition_digest = definition_digest definition;
        semantic_fingerprint = digest dump;
        semantic_dump = dump;
      }

let rebind_definition signature definition =
  if String.equal signature.definition_digest (definition_digest definition)
  then Ok signature
  else if signature.recursive then
    Error "retained recursive signature definition binding is stale"
  else
    create ~definition
      ~parameter_kinds:(List.map (fun formal -> formal.kind) signature.formals)
      ~parameter_modes:(List.map (fun formal -> formal.mode) signature.formals)
      ~result_mode:signature.result_mode ~recursive_evidence:None

let binders (signature : t) = signature.binders
let formals (signature : t) = signature.formals
let result_type (signature : t) = signature.result_type
let result_mode (signature : t) = signature.result_mode

let modes (signature : t) =
  (List.map (fun formal -> formal.mode) signature.formals, signature.result_mode)

let contracts (signature : t) = signature.contracts
let recursive (signature : t) = signature.recursive

let recursive_verified (signature : t) =
  Option.is_some signature.recursive_evidence

let source_definition (signature : t) = signature.source_definition
let semantic_fingerprint (signature : t) = signature.semantic_fingerprint
let semantic_dump (signature : t) = signature.semantic_dump

let infer_type_arguments signature ~actual_types ~actual_labels ~actual_result =
  Parametric_lowering_private.infer_labeled_type_arguments
    ~binders:signature.binders
    ~formal_types:(List.map (fun formal -> formal.typ) signature.formals)
    ~formal_labels:(List.map (fun formal -> formal.label) signature.formals)
    ~actual_types ~actual_labels ~formal_result:signature.result_type
    ~actual_result

let infer_partial_type_arguments signature ~actual_types ~actual_labels
    ~actual_result =
  if
    List.length actual_types <> List.length signature.formals
    || actual_labels
       <> List.map (fun (formal : formal) -> formal.label) signature.formals
  then Error "retained call formal label/default vector mismatch"
  else
    let formals, actuals =
      List.fold_left2
        (fun (formals, actuals) (formal : formal) actual ->
          match actual with
          | None -> (formals, actuals)
          | Some actual -> (formal.typ :: formals, actual :: actuals))
        ([], []) signature.formals actual_types
    in
    Parametric_lowering_private.infer_type_arguments ~binders:signature.binders
      ~formals:(List.rev formals) ~actuals:(List.rev actuals)
      ~formal_result:signature.result_type ~actual_result

let map_pattern_types = Sst.map_pattern_types
let map_expression_types = Sst.map_expression_types
let substitute_contracts = Sst.substitute_contracts

let rebase_definition signature ~source_definition ~function_id definition =
  let* _ = rebind_definition signature source_definition in
  let owner =
    Parametric_type.owner ~index:function_id.Sst.function_index
      ~name:function_id.function_name
  in
  let binders =
    List.mapi
      (fun ordinal _ -> Parametric_type.binder owner ~ordinal)
      signature.binders
  in
  let substitutions =
    List.map2
      (fun old replacement -> (old, Parametric_type.Parameter replacement))
      signature.binders binders
  in
  let substitute typ = Parametric_type.substitute substitutions typ in
  let parameter = function
    | Sst.Callback_parameter _ as parameter -> parameter
    | Sst.Value_parameter value ->
        Sst.Value_parameter
          {
            value with
            Sst.pattern = map_pattern_types substitute value.pattern;
            optional_default =
              Option.map
                (fun (default : Sst.optional_default) ->
                  {
                    Sst.optional_pattern =
                      map_pattern_types substitute default.optional_pattern;
                    optional_expression =
                      map_expression_types substitute
                        default.optional_expression;
                  })
                value.optional_default;
          }
  in
  let staged (staged : Sst.staged_expression) =
    {
      staged with
      Sst.expression = map_expression_types substitute staged.expression;
    }
  in
  let body =
    match definition.Sst.body with
    | Sst.Checked_exec body ->
        Sst.Checked_exec { body with body = staged body.body }
    | Sst.Spec_definition body -> Sst.Spec_definition (staged body)
    | Sst.Recursive_spec_definition body ->
        Sst.Recursive_spec_definition { body with body = staged body.body }
    | Sst.Proof_body body ->
        Sst.Proof_body { body with body = staged body.body }
    | ( Sst.External_specification _ | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ) as body ->
        body
  in
  let* contracts =
    substitute_contracts signature.binders
      (List.map (fun binder -> Parametric_type.Parameter binder) binders)
      definition.contracts
  in
  let definition =
    {
      definition with
      Sst.function_id;
      type_binders = binders;
      parameters = List.map parameter definition.parameters;
      contracts;
      body;
      result_type = substitute definition.result_type;
    }
  in
  let parameter_kinds = List.map (fun formal -> formal.kind) signature.formals
  and parameter_modes =
    List.map (fun formal -> formal.mode) signature.formals
  in
  let definition =
    if signature.recursive then
      {
        definition with
        Sst.recursive = false;
        contracts = { definition.contracts with decreases = [] };
      }
    else definition
  in
  let* rebased =
    create ~definition ~parameter_kinds ~parameter_modes
      ~result_mode:signature.result_mode ~recursive_evidence:None
  in
  Ok (definition, rebased)

let instantiate signature type_arguments =
  let* substitutions =
    if List.length signature.binders = List.length type_arguments then
      Ok (List.combine signature.binders type_arguments)
    else Error "retained call type-argument arity mismatch"
  in
  let substitute typ = Parametric_type.substitute substitutions typ in
  let parameter = function
    | Sst.Callback_parameter _ as parameter -> parameter
    | Sst.Value_parameter value ->
        Sst.Value_parameter
          {
            value with
            Sst.pattern = map_pattern_types substitute value.pattern;
            optional_default =
              Option.map
                (fun (default : Sst.optional_default) ->
                  {
                    Sst.optional_pattern =
                      map_pattern_types substitute default.optional_pattern;
                    optional_expression =
                      map_expression_types substitute
                        default.optional_expression;
                  })
                value.optional_default;
          }
  in
  let* parameter_types =
    let rec loop instantiated = function
      | [] -> Ok (List.rev instantiated)
      | formal :: rest ->
          let* typ =
            Parametric_type.instantiate signature.binders type_arguments
              formal.typ
          in
          loop (typ :: instantiated) rest
    in
    loop [] signature.formals
  in
  let* result_type =
    Parametric_type.instantiate signature.binders type_arguments
      signature.result_type
  in
  let* contracts =
    substitute_contracts signature.binders type_arguments signature.contracts
  in
  let dump =
    String.concat "|"
      [
        signature.semantic_dump;
        "arguments="
        ^ String.concat "," (List.map Parametric_type.to_string type_arguments);
        "instantiated-result=" ^ Parametric_type.to_string result_type;
      ]
  in
  Ok
    {
      type_arguments;
      parameters = List.map parameter signature.source_definition.parameters;
      parameter_types;
      result_type;
      contracts;
      semantic_fingerprint = digest dump;
      semantic_dump = dump;
    }

let validate_call signature ~type_arguments ~actual_result ~arguments =
  let* instantiated = instantiate signature type_arguments in
  if not (Parametric_type.equal instantiated.result_type actual_result) then
    Error "retained call result differs from its complete substitution"
  else if List.length signature.formals <> List.length arguments then
    Error "retained call is not saturated"
  else
    let rec loop formals expected actuals =
      match (formals, expected, actuals) with
      | [], [], [] -> Ok instantiated
      | formal :: formals, typ :: expected, (label, actual) :: actuals ->
          if formal.label <> label then
            Error "retained call formal label/default vector mismatch"
          else if not (Parametric_type.equal typ actual.Sst.typ) then
            Error
              "retained call argument differs from its complete substitution"
          else loop formals expected actuals
      | _ -> Error "retained call formal vector mismatch"
    in
    loop signature.formals instantiated.parameter_types arguments

module For_testing = struct
  let forged_recursive_signature ~definition ~parameter_kinds ~parameter_modes
      ~result_mode =
    let provider_completion =
      Verified_provider_completion_private.For_testing.forged ~definition
    in
    let recursive_evidence =
      Some
        {
          provider_completion;
          source_definition_digest = definition_digest definition;
        }
    in
    create ~definition ~parameter_kinds ~parameter_modes ~result_mode
      ~recursive_evidence

  let stale_recursive_body signature =
    match (signature.recursive_evidence, signature.source_definition.body) with
    | Some evidence, Sst.Checked_exec body ->
        let expression = body.body.expression in
        let stale =
          {
            signature.source_definition with
            Sst.body =
              Sst.Checked_exec
                {
                  body with
                  body =
                    {
                      body.body with
                      expression =
                        {
                          expression with
                          expression_desc = Sst.Let ([], expression);
                        };
                    };
                };
          }
        in
        if
          Verified_provider_completion_private
          .authenticates_recursive_definition evidence.provider_completion stale
        then Ok ()
        else Error "retained recursive provider verification evidence is stale"
    | Some _, _ -> Error "retained recursive test source is not executable"
    | None, _ -> Error "retained signature has no recursive evidence"
end

let remap_descriptor_type_id descriptor (type_id : Parametric_type.type_id) =
  let old_binders = Parametric_adt.binders descriptor in
  let owner =
    Parametric_type.owner ~index:type_id.type_index ~name:type_id.type_name
  in
  let binders =
    List.mapi
      (fun ordinal _ -> Parametric_type.binder owner ~ordinal)
      old_binders
  in
  let substitutions =
    List.map2
      (fun old binder -> (old, Parametric_type.Parameter binder))
      old_binders binders
  in
  let field (field : Parametric_adt.field) =
    {
      field with
      Parametric_adt.field_type =
        Parametric_type.substitute substitutions field.field_type;
    }
  in
  let kind =
    match Parametric_adt.kind descriptor with
    | Parametric_adt.Record fields ->
        Parametric_adt.Record (List.map field fields)
    | Parametric_adt.Variant constructors ->
        Parametric_adt.Variant
          (List.map
             (fun (constructor : Parametric_adt.constructor) ->
               {
                 constructor with
                 Parametric_adt.constructor_fields =
                   List.map field constructor.constructor_fields;
               })
             constructors)
  in
  match
    Parametric_adt.create ~type_id
      ~type_constructor:(Parametric_adt.type_constructor descriptor)
      ~binders
      ~provenance:(Parametric_adt.provenance descriptor)
      ~kind
  with
  | Ok descriptor -> Ok descriptor
  | Error error -> Error error.Parametric_adt.message
