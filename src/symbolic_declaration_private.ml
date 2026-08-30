type pending = {
  pending_declaration : Symbolic_application_private.declaration;
  pending_definition : Sst.function_definition;
}

type issuance = {
  program : Sst.program Weak.t;
  definitions :
    (Sst.function_definition * Symbolic_application_private.declaration) list;
  imported_definitions :
    (Sst.function_definition * Symbolic_application_private.declaration) list;
}

let pending = ref []
let issuances = ref []

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let value_parameter_types parameters =
  let rec collect types = function
    | [] -> Ok (List.rev types)
    | Sst.Value_parameter { pattern; optional_default = None; _ } :: rest ->
        collect (pattern.typ :: types) rest
    | Sst.Value_parameter { optional_default = Some _; _ } :: _ ->
        Error "symbolic declarations do not support optional defaults"
    | Sst.Callback_parameter _ :: _ ->
        Error "symbolic declarations do not support callback parameters"
  in
  collect [] parameters

let declaration_of_definition definition =
  match definition.Sst.body with
  | Sst.Symbolic_declaration declaration -> Some declaration
  | Sst.Checked_exec _ | Sst.Spec_definition _
  | Sst.Recursive_spec_definition _ | Sst.Proof_body _
  | Sst.External_specification _ | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _ ->
      None

let valid_definition definition declaration =
  let* parameter_types = value_parameter_types definition.Sst.parameters in
  if
    definition.function_id.function_index
    <> Symbolic_application_private.declaration_index declaration
    || not
         (String.equal definition.function_id.function_name
            (Symbolic_application_private.declaration_name declaration))
  then Error "symbolic declaration function identity is stale"
  else if definition.mode <> Sst.Spec then
    Error "symbolic declaration is not in Spec mode"
  else if definition.recursive then
    Error "symbolic declaration is recursive"
  else if definition.contracts <> Sst.empty_contracts then
    Error "symbolic declaration carries contracts"
  else if definition.returns_unique_parameter <> None then
    Error "symbolic declaration carries executable result ownership"
  else if
    not
      (List.equal
         (fun left right -> Parametric_type.compare_binder left right = 0)
         definition.type_binders
         (Symbolic_application_private.type_binders declaration))
  then Error "symbolic declaration type binders are stale"
  else if
    not
      (List.equal Parametric_type.equal parameter_types
         (Symbolic_application_private.parameter_types declaration))
  then Error "symbolic declaration parameter types are stale"
  else if
    not
      (Parametric_type.equal definition.result_type
         (Symbolic_application_private.declaration_result_type declaration))
  then Error "symbolic declaration result type is stale"
  else Ok ()

let authenticate_imported ~canonical_path ~value_uid definition =
  match declaration_of_definition definition with
  | None -> Error "imported definition is not a symbolic declaration"
  | Some declaration ->
      let* () = valid_definition definition declaration in
      if
        not
          (String.equal canonical_path
             (Symbolic_application_private.canonical_path declaration))
      then Error "imported symbolic declaration path is stale"
      else if
        not
          (String.equal value_uid
             (Symbolic_application_private.value_uid declaration))
      then Error "imported symbolic declaration compiler UID is stale"
      else if
        String.equal (Symbolic_application_private.canonical_path declaration) ""
        || String.equal (Symbolic_application_private.value_uid declaration) ""
        || String.equal (Symbolic_application_private.source_file declaration) ""
        || String.equal
             (Symbolic_application_private.compilation_identity declaration)
             ""
      then Error "imported symbolic declaration identity is incomplete"
      else Ok declaration

let make_definition ~source ~function_id ~type_binders ~parameters ~result_type
    ~span =
  let* parameter_types = value_parameter_types parameters in
  let* declaration =
    Symbolic_application_private.declare
      ~marker_id:(Typedtree_symbolic_private.marker_id source)
      ~declaration_index:function_id.Sst.function_index
      ~declaration_name:function_id.Sst.function_name
      ~canonical_path:(Typedtree_symbolic_private.canonical_path source)
      ~value_uid:(Typedtree_symbolic_private.value_uid source)
      ~source_file:(Typedtree_symbolic_private.source_file source)
      ~compilation_identity:
        (Typedtree_symbolic_private.compilation_identity source)
      ~declaration_span:span ~type_binders ~parameter_types ~result_type
  in
  let definition =
    {
      Sst.function_id;
      type_binders;
      mode = Sst.Spec;
      recursive = false;
      parameters;
      contracts = Sst.empty_contracts;
      body = Sst.Symbolic_declaration declaration;
      policy = Sst.Default_linear_z3;
      result_type;
      returns_unique_parameter = None;
      span;
    }
  in
  pending := { pending_declaration = declaration; pending_definition = definition } :: !pending;
  Ok definition

let declaration_key declaration =
  ( Symbolic_application_private.canonical_path declaration,
    Symbolic_application_private.value_uid declaration )

let unique_declaration_keys definitions =
  let keys =
    List.map (fun (_, declaration) -> declaration_key declaration) definitions
  in
  List.length keys = List.length (List.sort_uniq compare keys)

let seal ~imported_definitions ~program =
  let definitions =
    program.Sst.functions
    |> List.filter_map (fun definition ->
           match declaration_of_definition definition with
           | None -> None
           | Some declaration ->
               Some (definition, declaration))
  in
  let* () =
    List.fold_left
      (fun result (definition, declaration) ->
        let* () = result in
        if
          List.exists
            (fun issued ->
              issued.pending_definition == definition
              && issued.pending_declaration == declaration)
            !pending
        then valid_definition definition declaration
        else Error "symbolic declaration was not issued by Typedtree lowering")
      (Ok ()) definitions
  in
  let* imported_definitions =
    let rec authenticate authenticated = function
      | [] -> Ok (List.rev authenticated)
      | definition :: rest -> (
          match declaration_of_definition definition with
          | None ->
              Error "imported symbolic issuance contains a nonsymbolic definition"
          | Some declaration ->
              let* declaration =
                authenticate_imported
                  ~canonical_path:
                    (Symbolic_application_private.canonical_path declaration)
                  ~value_uid:
                    (Symbolic_application_private.value_uid declaration)
                  definition
              in
              authenticate ((definition, declaration) :: authenticated) rest)
    in
    authenticate [] imported_definitions
  in
  if not (unique_declaration_keys (definitions @ imported_definitions)) then
    Error "symbolic declaration identity is ambiguous"
  else
    let weak = Weak.create 1 in
    Weak.set weak 0 (Some program);
    let other_live_issuances =
      List.filter
        (fun issuance ->
          match Weak.get issuance.program 0 with
          | None -> false
          | Some candidate -> candidate != program)
        !issuances
    in
    issuances :=
      { program = weak; definitions; imported_definitions }
      :: other_live_issuances;
    pending :=
      List.filter
        (fun issued ->
          not
            (List.exists
               (fun (definition, _) -> definition == issued.pending_definition)
               definitions))
        !pending;
    Ok ()

let authenticate ~program ~definition =
  match declaration_of_definition definition with
  | None -> Error "definition is not a symbolic declaration"
  | Some declaration ->
      let live, found =
        List.fold_left
          (fun (live, found) issuance ->
            match Weak.get issuance.program 0 with
            | None -> (live, found)
            | Some candidate ->
                let exact =
                  candidate == program
                  && List.exists
                       (fun (issued, claim) ->
                         issued == definition && claim == declaration)
                       issuance.definitions
                in
                (issuance :: live, found || exact))
          ([], false) !issuances
      in
      issuances := List.rev live;
      if not found then
        Error "symbolic declaration lacks exact program issuance"
      else
        let* () = valid_definition definition declaration in
        Ok declaration

let authenticate_application_declaration ~program declaration =
  let live, matches =
    List.fold_left
      (fun (live, matches) issuance ->
        match Weak.get issuance.program 0 with
        | None -> (live, matches)
        | Some candidate ->
            let matches =
              if candidate != program then matches
              else
                List.fold_left
                  (fun matches (definition, claim) ->
                    if claim == declaration then (definition, claim) :: matches
                    else matches)
                  matches
                  (issuance.definitions @ issuance.imported_definitions)
            in
            (issuance :: live, matches))
      ([], []) !issuances
  in
  issuances := List.rev live;
  match matches with
  | [ (definition, authenticated) ] ->
      let* () = valid_definition definition authenticated in
      Ok authenticated
  | [] -> Error "symbolic application lacks exact consumer issuance"
  | _ :: _ :: _ -> Error "symbolic application identity is ambiguous"

let validate_application ~program ~logical ~expression_type application =
  let declaration = Symbolic_application_private.declaration application in
  let* authenticated =
    authenticate_application_declaration ~program declaration
  in
  if
    not
      (Symbolic_application_private.same_declaration declaration authenticated)
  then Error "symbolic application identity differs from its declaration"
  else if not logical then
    Error "symbolic declarations are unavailable to executable code"
  else
    let* () =
      Symbolic_application_private.validate
        ~argument_type:(fun argument -> argument.Sst.typ)
        application
    in
    if
      Parametric_type.equal expression_type
        (Symbolic_application_private.result_type application)
    then Ok (Symbolic_application_private.arguments application)
    else Error "symbolic application carries a stale result type"

let validate_definition ~program ~supported definition declaration =
  let* authenticated = authenticate ~program ~definition in
  if
    not
      (Symbolic_application_private.same_declaration declaration authenticated)
  then Error "symbolic declaration identity is stale"
  else if
    List.exists
      (function
        | Sst.Value_parameter
            {
              label = _;
              pattern = { pattern_desc = Sst.Wildcard; _ };
              optional_default = None;
            } ->
            false
        | Sst.Value_parameter _ | Sst.Callback_parameter _ -> true)
      definition.Sst.parameters
  then
    Error
      "symbolic declaration parameters must be bodyless first-order value \
       descriptions"
  else if
    List.for_all supported
      (Symbolic_application_private.parameter_types declaration)
    && supported
         (Symbolic_application_private.declaration_result_type declaration)
  then Ok ()
  else
    Error
      "symbolic declaration type is not a canonical immutable first-order \
       logical type"

let application ~declaration ~type_arguments ~arguments ~result_type ~span =
  Symbolic_application_private.create declaration ~type_arguments ~arguments
    ~argument_types:(List.map (fun expression -> expression.Sst.typ) arguments)
    ~result_type ~span

let is_symbolic definition =
  Option.is_some (declaration_of_definition definition)

let destroy program =
  issuances :=
    List.filter
      (fun issuance ->
        match Weak.get issuance.program 0 with
        | None -> false
        | Some candidate -> candidate != program)
      !issuances
