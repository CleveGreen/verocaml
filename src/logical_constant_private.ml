let defined_semantic_class = "logical-constant-definition-v1"
let symbolic_semantic_class = "symbolic-origin-v1"

let digest value = Digest.string value |> Digest.to_hex

let framed tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let same_origin (left : Sst.logical_constant_origin)
    (right : Sst.logical_constant_origin) =
  String.equal left.semantic_class right.semantic_class
  && String.equal left.provider_unit right.provider_unit
  && String.equal left.provider_interface right.provider_interface
  && String.equal left.value_uid right.value_uid
  && String.equal left.declaration_marker right.declaration_marker
  && String.equal left.canonical_path right.canonical_path
  && String.equal left.origin_digest right.origin_digest

let same_id (left : Sst.logical_constant_id)
    (right : Sst.logical_constant_id) =
  left.constant_index = right.constant_index
  && String.equal left.constant_name right.constant_name
  && same_origin left.constant_origin right.constant_origin

let origin_of_source source =
  let provider_unit = Typedtree_logical_constant_private.provider_unit source in
  let provider_interface =
    Typedtree_logical_constant_private.provider_interface source
  in
  let value_uid = Typedtree_logical_constant_private.value_uid source in
  let declaration_marker =
    Typedtree_logical_constant_private.marker_id source
  in
  let canonical_path =
    Typedtree_logical_constant_private.canonical_path source
  in
  let origin_digest =
    digest
      (framed defined_semantic_class
         [ provider_unit; provider_interface; value_uid; declaration_marker;
           canonical_path ])
  in
  {
    Sst.semantic_class = defined_semantic_class;
    provider_unit;
    provider_interface;
    value_uid;
    declaration_marker;
    canonical_path;
    origin_digest;
  }

let issue_id ~source ~constant_index ~constant_name =
  let constant_origin = origin_of_source source in
  [%log.trace "issued logical constant identity"
    ~stage:(Delator.Field.string "logical-constant-issuance")
    ~constant_name:(Delator.Field.string constant_name)
    ~constant_index:(Delator.Field.int constant_index)
    ~canonical_path:(Delator.Field.string constant_origin.canonical_path)
    ~origin_digest:(Delator.Field.string constant_origin.origin_digest)
    ~decision:(Delator.Field.string "issued")];
  { Sst.constant_index; constant_name; constant_origin }

let canonical_substitutions binders =
  let owner = Parametric_type.owner ~index:(-1) ~name:"logical-constant" in
  List.mapi
    (fun ordinal binder ->
      (binder, Parametric_type.Parameter (Parametric_type.binder owner ~ordinal)))
    binders

let canonical_type substitutions typ =
  Parametric_type.substitute substitutions typ

let descriptor_digest ~constant_id ~binders ~declared_type =
  let substitutions = canonical_substitutions binders in
  digest
    (Marshal.to_string
       ( "logical-constant-descriptor-v1",
         constant_id.Sst.constant_origin.semantic_class,
         constant_id.constant_origin.provider_unit,
         constant_id.constant_origin.provider_interface,
         constant_id.constant_origin.value_uid,
         constant_id.constant_origin.declaration_marker,
         constant_id.constant_origin.canonical_path,
         constant_id.constant_origin.origin_digest,
         List.length binders,
         canonical_type substitutions declared_type )
       [ Marshal.No_sharing ])

let interface_abi ~path ~uid definition =
  Symbolic_application_private.declaration_abi_material ~canonical_path:path
    ~value_uid:uid ~type_binders:definition.Sst.constant_type_binders
    ~parameter_labels:[] ~parameter_types:[]
    ~result_type:definition.constant_declared_type

let canonical_body binders (body : Sst.staged_expression) =
  let substitutions = canonical_substitutions binders in
  {
    body with
    Sst.expression =
      Sst.map_expression_types (canonical_type substitutions) body.expression;
  }

let body_digest ~source_body_digest ~binders ~declared_type ~trust_dependencies
    body =
  let substitutions = canonical_substitutions binders in
  digest
    (Marshal.to_string
       ( defined_semantic_class,
         source_body_digest,
         List.length binders,
         canonical_type substitutions declared_type,
         List.sort_uniq String.compare trust_dependencies,
         canonical_body binders body )
       [ Marshal.No_sharing ])

let expression_children expression =
  match expression.Sst.expression_desc with
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      quantifier.quantifier_body
      :: Option.to_list quantifier.quantifier_trigger
  | _ -> Sst_callback_private.expression_children expression

let dependency_receipt ~binders body =
  let substitutions = canonical_substitutions binders in
  let rec collect dependencies expression =
    let dependencies =
      match expression.Sst.expression_desc with
      | Sst.Logical_constant_reference { constant; type_arguments } ->
          framed "defined-constant"
            [ constant.constant_origin.origin_digest;
              Parametric_type.structural_vector_material
                (List.map (canonical_type substitutions) type_arguments) ]
          :: dependencies
      | Sst.Symbolic_application application ->
          framed "symbolic"
            [ Symbolic_application_private.identity_digest application ]
          :: dependencies
      | Sst.Direct_call
          { call_form; callee; type_arguments; recursive; _ } ->
          framed "logical-definition"
            [ string_of_int callee.function_index; callee.function_name;
              (match call_form with
              | Sst.Unclassified_call -> "unclassified"
              | Specification_call -> "spec"
              | Proof_call -> "proof"
              | Exec_call -> "exec");
              string_of_bool recursive;
              Parametric_type.structural_vector_material
                (List.map (canonical_type substitutions) type_arguments) ]
          :: dependencies
      | _ -> dependencies
    in
    List.fold_left collect dependencies (expression_children expression)
  in
  collect [] body.Sst.expression
  |> List.sort_uniq String.compare
  |> framed "logical-constant-dependencies-v1"
  |> digest

type pending = {
  source : Typedtree_logical_constant_private.declaration;
  definition : Sst.logical_constant_definition;
}

type issuance = {
  program : Sst.program Weak.t;
  definitions : Sst.logical_constant_definition list;
}

let pending = ref []
let issuances = ref []

let validate_binders (definition : Sst.logical_constant_definition) =
  let id = definition.constant_id in
  let expected_owner =
    Parametric_type.owner ~index:id.constant_index ~name:id.constant_name
  in
  let rec loop ordinal = function
    | [] -> Ok ()
    | (binder : Parametric_type.binder) :: rest ->
        if Parametric_type.compare_owner binder.owner expected_owner <> 0 then
          Error "logical constant type binder owner differs from its identity"
        else if binder.ordinal <> ordinal then
          Error
            "logical constant type binders have forged, duplicate, or reordered ordinals"
        else loop (ordinal + 1) rest
  in
  loop 0 definition.constant_type_binders

let rec first_order_type = function
  | Sst.Bool | Int | Mathematical_int | Bit_vector _ | Aggregate _
  | Parameter _ -> true
  | Application (_, arguments) as typ ->
      (not (Parametric_type.is_spec_function typ))
      && List.for_all first_order_type arguments
  | Unit | Tuple _ -> false

let validate_instance_type typ =
  if first_order_type typ then Ok ()
  else (
    [%log.debug "rejected unsupported logical constant instance type"
      ~stage:(Delator.Field.string "logical-constant-instance-validation")
      ~result_type:(Delator.Field.string (Parametric_type.to_string typ))
      ~reason_class:(Delator.Field.string "non-first-order-logical-type")
      ~decision:(Delator.Field.string "rejected")];
    Error "logical constant instance is not a supported first-order logical type")
[@@delator.instrument] [@@delator.level debug]

let make_definition ~source ~constant_id ~type_binders ~declared_type ~body
    ~span =
  let[@log_value.debug] reference_count expression =
    let rec count total expression =
      let total =
        match expression.Sst.expression_desc with
        | Sst.Logical_constant_reference _ -> total + 1
        | _ -> total
      in
      List.fold_left count total (expression_children expression)
    in
    count 0 expression
  in
  let expected_id =
    issue_id ~source ~constant_index:constant_id.Sst.constant_index
      ~constant_name:constant_id.constant_name
  in
  if not (same_id expected_id constant_id) then
    Error "logical constant identity differs from its authenticated source"
  else if body.Sst.stage <> Sst.Logical then
    Error "logical constant definition is not in the logical stage"
  else if not (Parametric_type.equal body.expression.typ declared_type) then
    Error "logical constant body type differs from its declared type"
  else
    let* () = validate_instance_type declared_type in
    let constant_source_body_digest =
      Typedtree_logical_constant_private.source_body_digest source
    in
    let constant_body_digest =
      body_digest ~source_body_digest:constant_source_body_digest
        ~binders:type_binders ~declared_type ~trust_dependencies:[] body
    in
    let constant_dependency_receipt =
      dependency_receipt ~binders:type_binders body
    in
    let constant_descriptor_digest =
      descriptor_digest ~constant_id ~binders:type_binders ~declared_type
    in
    let definition =
      {
        Sst.constant_id;
        constant_type_binders = type_binders;
        constant_declared_type = declared_type;
        constant_descriptor_digest;
        constant_provenance = Sst.Verified_definitional_equation;
        constant_equation =
          Some
            {
              Sst.constant_body = body;
              constant_source_body_digest;
              constant_body_digest;
              constant_dependency_receipt;
              constant_trust_dependencies = [];
            };
        constant_span = span;
      }
    in
    let* () = validate_binders definition in
    pending := { source; definition } :: !pending;
    [%log.debug "issued logical constant definition"
      ~stage:(Delator.Field.string "logical-constant-issuance")
      ~constant_name:(Delator.Field.string constant_id.constant_name)
      ~constant_index:(Delator.Field.int constant_id.constant_index)
      ~type_binder_count:(Delator.Field.int (List.length type_binders))
      ~defined_constant_dependency_count:
        (Delator.Field.int
           ((reference_count [@log_value.debug]) body.expression))
      ~body_digest:(Delator.Field.string constant_body_digest)
      ~dependency_receipt:(Delator.Field.string constant_dependency_receipt)
      ~decision:(Delator.Field.string "issued")];
    Ok definition

let find_definition program id =
  List.find_opt
    (fun definition -> same_id definition.Sst.constant_id id)
    program.Sst.logical_constants

let validate_reference ~program ~logical ~expression_type constant
    ~type_arguments =
  if not logical then Error "logical constants are unavailable to executable code"
  else
    match find_definition program constant with
    | None -> Error "logical constant reference has no exact definition"
    | Some definition ->
        let* inferred =
          Parametric_lowering_private.infer_type_arguments
            ~binders:definition.constant_type_binders ~formals:[] ~actuals:[]
            ~formal_result:definition.constant_declared_type
            ~actual_result:expression_type
        in
        let* () =
          if
            List.length inferred = List.length type_arguments
            && List.for_all2 Parametric_type.equal inferred type_arguments
          then Ok ()
          else
            Error
              "logical constant type vector differs from canonical result inference"
        in
        let* () =
          List.fold_left
            (fun result argument ->
              let* () = result in
              validate_instance_type argument)
            (Ok ()) type_arguments
        in
        let* instantiated =
          Parametric_type.instantiate definition.constant_type_binders
            type_arguments definition.constant_declared_type
        in
        let* () = validate_instance_type instantiated in
        let* () = validate_instance_type expression_type in
        if Parametric_type.equal instantiated expression_type then (
          [%log.trace "validated logical constant reference"
            ~stage:(Delator.Field.string "logical-constant-validation")
            ~constant_name:(Delator.Field.string constant.constant_name)
            ~constant_index:(Delator.Field.int constant.constant_index)
            ~type_argument_count:
              (Delator.Field.int (List.length type_arguments))
            ~result_type:
              (Delator.Field.string (Parametric_type.to_string expression_type))
            ~decision:(Delator.Field.string "accepted")];
          Ok ())
        else Error "logical constant reference result type is stale"

let definition_valid (definition : Sst.logical_constant_definition) =
  let id = definition.constant_id in
  let expected_origin_digest =
    if String.equal id.constant_origin.semantic_class defined_semantic_class then
      digest
        (framed defined_semantic_class
           [ id.constant_origin.provider_unit;
             id.constant_origin.provider_interface;
             id.constant_origin.value_uid;
             id.constant_origin.declaration_marker;
             id.constant_origin.canonical_path ])
    else
      digest
        (framed symbolic_semantic_class
           [ id.constant_origin.provider_unit;
             id.constant_origin.provider_interface;
             id.constant_origin.value_uid;
             id.constant_origin.declaration_marker;
             id.constant_origin.canonical_path ])
  in
  let* () = validate_binders definition in
  if
    not
      (String.equal id.constant_origin.semantic_class defined_semantic_class
      || String.equal id.constant_origin.semantic_class symbolic_semantic_class)
  then Error "logical constant semantic class is unsupported"
  else if
    String.equal id.constant_origin.provider_unit ""
    || String.equal id.constant_origin.provider_interface ""
    || String.equal id.constant_origin.value_uid ""
    || String.equal id.constant_origin.declaration_marker ""
    || String.equal id.constant_origin.canonical_path ""
  then Error "logical constant origin identity is incomplete"
  else if not (String.equal id.constant_origin.origin_digest expected_origin_digest)
  then Error "logical constant origin digest is stale"
  else
    let* () = validate_instance_type definition.constant_declared_type in
    if
    not
      (String.equal definition.constant_descriptor_digest
         (descriptor_digest ~constant_id:id
            ~binders:definition.constant_type_binders
            ~declared_type:definition.constant_declared_type))
    then Error "logical constant descriptor digest is stale"
    else
    match (definition.constant_provenance, definition.constant_equation) with
    | Sst.Uninterpreted_symbolic, None
      when String.equal id.constant_origin.semantic_class symbolic_semantic_class ->
        Ok ()
    | Sst.Opaque_defined_identity, None
      when String.equal id.constant_origin.semantic_class defined_semantic_class ->
        Ok ()
    | Sst.Verified_definitional_equation, Some equation
      when String.equal id.constant_origin.semantic_class defined_semantic_class ->
        if equation.constant_body.stage <> Sst.Logical then
          Error "logical constant body is not logical"
        else if
          not
            (Parametric_type.equal equation.constant_body.expression.typ
               definition.constant_declared_type)
        then Error "logical constant body and declared types differ"
        else if String.equal equation.constant_source_body_digest "" then
          Error "logical constant source body receipt is missing"
        else if
          not
            (String.equal equation.constant_body_digest
               (body_digest
                  ~source_body_digest:equation.constant_source_body_digest
                  ~binders:definition.constant_type_binders
                  ~declared_type:definition.constant_declared_type
                  ~trust_dependencies:equation.constant_trust_dependencies
                  equation.constant_body))
        then Error "logical constant body digest is stale"
        else if
          not
            (String.equal equation.constant_dependency_receipt
               (dependency_receipt ~binders:definition.constant_type_binders
                  equation.constant_body))
        then Error "logical constant dependency receipt is stale"
        else if
          List.exists (String.equal "") equation.constant_trust_dependencies
          || List.length equation.constant_trust_dependencies
             <> List.length
                  (List.sort_uniq String.compare
                     equation.constant_trust_dependencies)
        then Error "logical constant trust dependency receipt is malformed"
        else Ok ()
    | Sst.Uninterpreted_symbolic, (Some _ | None)
    | Sst.Opaque_defined_identity, (Some _ | None)
    | Sst.Verified_definitional_equation, (Some _ | None) ->
        Error "logical constant provenance and equation availability disagree"

let remap_authenticated ~definition ~constant_id ~type_binders
    ~trust_dependencies ~map_type ~map_expression =
  let* () = definition_valid definition in
  let declared_type = map_type definition.Sst.constant_declared_type in
  let equation =
    Option.map
      (fun source ->
        let constant_body =
          { source.Sst.constant_body with
            expression = map_expression source.constant_body.expression }
        in
        let constant_dependency_receipt =
          dependency_receipt ~binders:type_binders constant_body
        in
        let constant_body_digest =
          body_digest ~source_body_digest:source.constant_source_body_digest
            ~binders:type_binders ~declared_type
            ~trust_dependencies constant_body
        in
        { source with Sst.constant_body; constant_body_digest;
          constant_dependency_receipt;
          constant_trust_dependencies = trust_dependencies })
      definition.constant_equation
  in
  let mapped =
    { definition with
      Sst.constant_id;
      constant_type_binders = type_binders;
      constant_declared_type = declared_type;
      constant_descriptor_digest =
        descriptor_digest ~constant_id ~binders:type_binders ~declared_type;
      constant_equation = equation }
  in
  let* () = definition_valid mapped in
  [%log.debug "remapped authenticated logical-value descriptor"
    ~stage:(Delator.Field.string "logical-value-import-remap")
    ~semantic_class:
      (Delator.Field.string constant_id.constant_origin.semantic_class)
    ~provenance:
      (Delator.Field.string
         (match mapped.constant_provenance with
         | Sst.Uninterpreted_symbolic -> "uninterpreted-symbolic"
         | Sst.Opaque_defined_identity -> "opaque-defined-identity"
         | Sst.Verified_definitional_equation ->
             "verified-definitional-equation"))
    ~equation_available:(Delator.Field.bool (Option.is_some equation))
    ~decision:(Delator.Field.string "accepted")];
  Ok mapped
[@@delator.instrument] [@@delator.level debug]

let authenticate_definition = definition_valid

let dependency_closure ~program definition =
  let node_digest tag value =
    framed tag
      [
        Digest.to_hex
          (Digest.string (Marshal.to_string value [ Marshal.No_sharing ]));
      ]
  in
  let materials = ref [] in
  let trust_dependencies = ref [] in
  let visited_constants = ref [] in
  let visited_functions = ref [] in
  let visited_types = ref [] in
  let add material = materials := material :: !materials in
  let add_trust material =
    trust_dependencies := material :: !trust_dependencies;
    add material
  in
  let same_function_id left right =
    left.Sst.function_index = right.Sst.function_index
    && String.equal left.function_name right.function_name
  in
  let rec visit_type = function
    | Sst.Unit | Bool | Int | Mathematical_int | Bit_vector _ | Parameter _ -> Ok ()
    | Tuple components ->
        List.fold_left
          (fun result (_, typ) ->
            let* () = result in
            visit_type typ)
          (Ok ()) components
    | Aggregate id ->
        if List.mem id !visited_types then Ok ()
        else (
          visited_types := id :: !visited_types;
          match
            List.find_opt
              (fun candidate -> candidate.Sst.type_id = id)
              program.Sst.types
          with
          | Some type_definition ->
              add (node_digest "logical-type-definition-v1" type_definition);
              Ok ()
          | None ->
              Error
                "logical constant dependency closure has no exact aggregate type")
    | Application (constructor, arguments) as typ ->
        add
          (framed "logical-type-application-v1"
             [ Parametric_type.structural_identity_material typ ]);
        let* () =
          if Parametric_type.is_spec_function typ then Ok ()
          else
            match Parametric_adt.find program.Sst.parametric_adts constructor with
            | Some descriptor ->
                add (node_digest "logical-type-constructor-v1" descriptor);
                Ok ()
            | None ->
                Error
                  "logical constant dependency closure has no exact type-constructor authority"
        in
        List.fold_left
          (fun result argument ->
            let* () = result in
            visit_type argument)
          (Ok ()) arguments
  and visit_expression active expression =
    let* () = visit_type expression.Sst.typ in
    let* () =
      match expression.Sst.expression_desc with
      | Sst.Logical_constant_reference { constant; type_arguments } ->
          let* () =
            List.fold_left
              (fun result typ ->
                let* () = result in
                visit_type typ)
              (Ok ()) type_arguments
          in
          visit_constant active constant
      | Sst.Symbolic_application application ->
          add
            (node_digest "symbolic-logical-declaration-v1"
               (Symbolic_application_private.declaration application));
          Ok ()
      | Sst.Direct_call { call_form = Sst.Specification_call; callee; _ }
        when Spec_function_sst_private.is_reference expression ->
          [%log.trace
            "included first-class specification reference in logical constant dependency closure"
            ~stage:(Delator.Field.string "logical-value-dependency-closure")
            ~callee_name:(Delator.Field.string callee.function_name)
            ~decision:(Delator.Field.string "included")];
          visit_function active callee
      | Sst.Direct_call { call_form = Sst.Specification_call; callee; _ }
        when Spec_function_sst_private.lambda expression = None
             && Spec_function_sst_private.application expression = None
             && not (Spec_function_sst_private.is_reference expression) ->
          visit_function active callee
      | _ -> Ok ()
    in
    List.fold_left
      (fun result child ->
        let* () = result in
        visit_expression active child)
      (Ok ()) (expression_children expression)
  and visit_constant active id =
    let key = id.Sst.constant_origin.origin_digest in
    if List.mem key active then Error "cyclic logical constant dependency closure"
    else if List.mem key !visited_constants then Ok ()
    else
      match find_definition program id with
      | None ->
          Error
            "logical constant dependency closure has no authenticated logical value"
      | Some dependency ->
          let* () = definition_valid dependency in
          visited_constants := key :: !visited_constants;
          add
            (framed "logical-constant-authority-v1"
               [ dependency.constant_id.constant_origin.origin_digest;
                 dependency.constant_descriptor_digest;
                 (match dependency.constant_provenance with
                 | Sst.Uninterpreted_symbolic -> "uninterpreted-symbolic"
                 | Sst.Opaque_defined_identity -> "opaque-defined-identity"
                 | Sst.Verified_definitional_equation ->
                     "verified-definitional-equation");
                 Option.fold ~none:"equationless"
                   ~some:(fun equation -> equation.Sst.constant_body_digest)
                   dependency.constant_equation ]);
          (match dependency.constant_equation with
          | None -> Ok ()
          | Some equation ->
              List.iter add_trust equation.constant_trust_dependencies;
              visit_expression (key :: active) equation.constant_body.expression)
  and visit_function active id =
    let key = Printf.sprintf "function:%d:%s" id.Sst.function_index id.function_name in
    if List.mem key active || List.mem key !visited_functions then Ok ()
    else
      match
        List.find_opt
          (fun dependency -> same_function_id dependency.Sst.function_id id)
          program.Sst.functions
      with
      | None ->
          Error
            "logical constant dependency closure has no authenticated specification"
      | Some dependency ->
          visited_functions := key :: !visited_functions;
          add (node_digest "logical-function-authority-v1" dependency);
          let trust = node_digest "trusted-logical-function-v1" dependency in
          let* () =
            match dependency.Sst.body with
            | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
            | Sst.External_specification _ ->
                add_trust trust;
                Ok ()
            | Sst.Spec_definition body
            | Sst.Recursive_spec_definition { body; _ } ->
                visit_expression (key :: active) body.expression
            | Sst.Symbolic_declaration declaration ->
                add
                  (node_digest "symbolic-logical-declaration-v1" declaration);
                Ok ()
            | Sst.Checked_exec _ | Sst.Proof_body _ ->
                Error
                  "logical constant dependency closure reached a non-specification function"
          in
          let contract_expressions =
            List.map
              (fun (clause : Sst.predicate_clause) ->
                clause.Sst.predicate.expression)
              dependency.contracts.requires
            @ List.map
                (fun (clause : Sst.ensures_clause) ->
                  clause.Sst.predicate.expression)
                dependency.contracts.ensures
            @ List.map
                (fun (clause : Sst.predicate_clause) ->
                  clause.Sst.predicate.expression)
                dependency.contracts.decreases
            @ List.map
                (fun (clause : Sst.predicate_clause) ->
                  clause.Sst.predicate.expression)
                dependency.contracts.assertions
          in
          List.fold_left
            (fun result expression ->
              let* () = result in
              visit_expression (key :: active) expression)
            (Ok ()) contract_expressions
  in
  let* () = definition_valid definition in
  let* () = visit_constant [] definition.constant_id in
  let materials = List.sort_uniq String.compare !materials in
  let trust_dependencies =
    List.sort_uniq String.compare !trust_dependencies
  in
  let receipt =
    digest (framed "logical-constant-closed-dependencies-v1" materials)
  in
  [%log.debug "closed authenticated logical constant dependency graph"
    ~stage:(Delator.Field.string "logical-value-dependency-closure")
    ~constant_name:
      (Delator.Field.string definition.constant_id.constant_name)
    ~authority_count:(Delator.Field.int (List.length materials))
    ~trust_dependency_count:
      (Delator.Field.int (List.length trust_dependencies))
    ~closure_receipt:(Delator.Field.string receipt)
    ~decision:(Delator.Field.string "authenticated")];
  Ok (receipt, trust_dependencies)

let unique_definitions definitions =
  let rec loop indices origins provider_paths = function
    | [] ->
        [%log.trace "validated logical constant identity uniqueness"
          ~stage:(Delator.Field.string "logical-constant-validation")
          ~constant_count:(Delator.Field.int (List.length definitions))
          ~origin_count:(Delator.Field.int (List.length origins))
          ~provider_path_count:
            (Delator.Field.int (List.length provider_paths))
          ~decision:(Delator.Field.string "accepted")];
        Ok ()
    | (definition : Sst.logical_constant_definition) :: rest ->
        let id = definition.constant_id in
        let provider_path =
          ( id.constant_origin.provider_unit,
            id.constant_origin.provider_interface,
            id.constant_origin.canonical_path )
        in
        if List.mem id.constant_index indices then
          Error "logical constant declaration index is duplicated"
        else if List.mem id.constant_origin.origin_digest origins then
          Error "logical constant origin identity is duplicated"
        else if List.mem provider_path provider_paths then
          Error "logical constant canonical path is duplicated within its provider"
        else
          loop (id.constant_index :: indices)
            (id.constant_origin.origin_digest :: origins)
            (provider_path :: provider_paths) rest
  in
  loop [] [] [] definitions

let seal ~imported_definitions ~program =
  let definitions = program.Sst.logical_constants in
  let* () = unique_definitions definitions in
  let* () =
    List.fold_left
      (fun result definition ->
        let* () = result in
        match
          List.find_opt
            (fun issued -> issued.definition == definition)
            !pending
        with
        | None
          when List.exists (fun imported -> imported == definition)
                 imported_definitions ->
            definition_valid definition
        | None ->
            Error
              "logical constant definition was not issued by Typedtree lowering or an authenticated import"
        | Some issued ->
            let expected_id =
              issue_id ~source:issued.source
                ~constant_index:definition.constant_id.constant_index
                ~constant_name:definition.constant_id.constant_name
            in
            if not (same_id expected_id definition.constant_id) then
              Error "logical constant definition source identity is stale"
            else definition_valid definition)
      (Ok ()) definitions
  in
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some program);
  issuances :=
    { program = weak; definitions }
    :: List.filter
         (fun issuance ->
           match Weak.get issuance.program 0 with
           | Some candidate -> candidate != program
           | None -> false)
         !issuances;
  pending :=
    List.filter
      (fun issued ->
        not
          (List.exists
             (fun definition -> definition == issued.definition)
             definitions))
      !pending;
  [%log.debug "sealed logical constant program"
    ~stage:(Delator.Field.string "logical-constant-validation")
    ~definition_count:(Delator.Field.int (List.length definitions))
    ~imported_count:(Delator.Field.int (List.length imported_definitions))
    ~decision:(Delator.Field.string "accepted")];
  Ok ()

let authenticate_program program =
  let live, exact =
    List.fold_left
      (fun (live, exact) issuance ->
        match Weak.get issuance.program 0 with
        | None -> (live, exact)
        | Some candidate ->
            let exact_definitions =
              List.length issuance.definitions
              = List.length program.Sst.logical_constants
              && List.for_all2 ( == ) issuance.definitions
                   program.logical_constants
            in
            ( issuance :: live,
              exact || (candidate == program && exact_definitions) ))
      ([], false) !issuances
  in
  issuances := List.rev live;
  if not exact then Error "logical constant program lacks exact issuance"
  else
    let* () = unique_definitions program.Sst.logical_constants in
    List.fold_left
      (fun result definition ->
        let* () = result in
        definition_valid definition)
      (Ok ()) program.logical_constants

let destroy program =
  issuances :=
    List.filter
      (fun issuance ->
        match Weak.get issuance.program 0 with
        | None -> false
        | Some candidate -> candidate != program)
      !issuances

module For_testing = struct
  let definition_rejected ~program definition =
    let _ = program in
    Result.is_error (definition_valid definition)
end
