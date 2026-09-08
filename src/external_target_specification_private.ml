open Typedtree
let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let digest value = Digest.string value |> Digest.to_hex
let issuer = ref ()
let live_registration_count = ref 0
let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid
let span_string (span : Diagnostic.span) =
  Printf.sprintf "%s:%d:%d-%d:%d" span.file span.start_pos.line
    span.start_pos.column span.end_pos.line span.end_pos.column

let artifact_digest (implementation : Cmt_input.implementation) =
  let imports =
    implementation.imports
    |> Array.to_list
    |> List.map (fun (import : Cmt_input.import) ->
           import.unit_name ^ "=" ^ Option.value ~default:"<none>" import.crc)
    |> List.sort String.compare |> String.concat ";"
  in
  digest (String.concat "\000"
       [
         "external-target-consumer-v1";
         implementation.unit_name;
         Option.value ~default:"<none>" implementation.interface_digest;
         Option.value ~default:"<none>" implementation.source_digest;
         imports;
         String.concat ";" implementation.implementation_family_markers;
         String.concat ";" implementation.verification_scope_markers;
       ])
let interface_digest implementation =
  match implementation.Cmt_input.interface_digest with
  | Some value -> Ok value
  | None ->
      Error
        ("imported external target " ^ implementation.unit_name
       ^ " has no exact CMI identity")

let mode_for_path implementation canonical_path =
  let prefix = implementation.Cmt_input.unit_name ^ "." in
  let local_path =
    if String.starts_with ~prefix canonical_path then
      String.sub canonical_path (String.length prefix)
        (String.length canonical_path - String.length prefix)
    else canonical_path
  in
  List.assoc_opt ("value:" ^ local_path)
    implementation.Cmt_input.interface_mode_signatures
let default_mode_signature ~slots = function
  | Some (Some signature) ->
      let defaults =
        String.split_on_char ';' signature
        |> List.mapi (fun index value ->
               String.equal value (Printf.sprintf "%d=default" index))
      in
      List.length defaults = slots && List.for_all Fun.id defaults
  | Some None | None -> false

type compiler_callable_modes = {
  parameter_modes : Mode.Alloc.lr list;
  result_modes : Mode.Alloc.lr list;
}

let compiler_modes parameter_modes result_modes = { parameter_modes; result_modes }
let empty_compiler_callable_modes = compiler_modes [] []
let copy_compiler_modes value = Marshal.from_bytes (Marshal.to_bytes value []) 0

let rec callable_shape typ =
  match Types.get_desc typ with
  | Types.Tpoly (body, _) | Types.Tlink body | Types.Tsubst (body, _) ->
      callable_shape body
  | Types.Tarrow ((label, parameter_mode, result_mode), domain, codomain, _) ->
      if not (Parametric_lowering_private.first_order_source_type domain) then
        Error "imported external target has a higher-order parameter"
      else
        let* formals, result, modes = callable_shape codomain in
        Ok
          ( (Parametric_lowering_private.formal_label label, domain) :: formals,
            result,
            {
              parameter_modes = parameter_mode :: modes.parameter_modes;
              result_modes = result_mode :: modes.result_modes;
            } )
  | _ when Parametric_lowering_private.first_order_source_type typ ->
      Ok ([], typ, empty_compiler_callable_modes)
  | _ -> Error "imported external target has a higher-order result"

let all_compiler_mode_slots modes = modes.parameter_modes @ modes.result_modes

let compiler_target_modes_are_exact_default modes =
  let floors = copy_compiler_modes modes
  and ceilings = copy_compiler_modes modes in
  List.for_all
    (fun mode ->
      Mode.Alloc.Const.equal
        (Mode.Alloc.zap_to_floor mode)
        Mode.Alloc.Const.legacy)
    (all_compiler_mode_slots floors)
  && List.for_all
       (fun mode ->
         Mode.Alloc.Const.equal
           (Mode.Alloc.zap_to_ceil mode)
           Mode.Alloc.Const.legacy)
       (all_compiler_mode_slots ceilings)

let stable_type_variables typ =
  Parametric_lowering_private.source_type_variable_ids typ
  |> List.fold_left
       (fun variables id ->
         if List.mem id variables then variables else variables @ [ id ])
       []

let lower_target_type binders ~resolve_application typ =
  let rec lower typ =
    Parametric_lowering_private.lower_source_type ~substitutions:[] ~binders
      ~application:(fun path arguments ->
        match resolve_application path with
        | None -> Error Parametric_lowering_private.Unsupported_source_type
        | Some descriptor ->
            let rec lower_arguments lowered = function
              | [] -> Ok (List.rev lowered)
              | argument :: rest ->
                  let* argument = lower argument in
                  lower_arguments (argument :: lowered) rest
            in
            let* arguments = lower_arguments [] arguments in
            Parametric_adt.application descriptor arguments
            |> Result.map_error (fun _ ->
                   Parametric_lowering_private.Unsupported_source_type))
      typ
  in
  lower typ
  |> Result.map_error (function
       | Parametric_lowering_private.Polymorphic_source_type ->
           "imported external target has an unsupported polymorphic ABI"
       | Higher_order_source_type ->
           "imported external target has a higher-order ABI"
       | Unsupported_source_type ->
           "imported external target has an unsupported first-order ABI")

type target = {
  implementation : Cmt_input.implementation;
  interface_digest : string;
  import_crc : string;
}

type target_identity = {
  identity_unit : string;
  identity_interface : string;
  identity_path : string;
  identity_uid : string;
}

type environment = {
  token : unit ref;
  consumer : Cmt_input.implementation;
  consumer_digest : string;
  targets : target list;
  mutable reserved_targets : target_identity list;
  mutable summaries : summary list;
}

and candidate = {
  candidate_token : unit ref;
  candidate_environment : environment;
  target : target;
  canonical_path : string;
  value_uid : string;
  labels : string option list;
  compiler_modes : compiler_callable_modes;
  type_binders : (int * Parametric_type.binder) list;
  formal_source_types : Types.type_expr list;
  result_source_type : Types.type_expr;
}

and sealed_summary = {
  sealed_token : unit ref;
  sealed_environment : environment;
  sealed_candidate : candidate;
  sealed_definition : Sst.function_definition;
  sealed_signature : Parametric_signature_private.t;
  sealed_link : Sst.target_link;
  mutable sealed_bound : bool;
}

and summary_origin = Local_summary | Imported_provider_summary

and summary = {
  summary_token : unit ref;
  summary_environment : environment;
  summary_candidate : candidate;
  summary_definition : Sst.function_definition;
  summary_signature : Parametric_signature_private.t;
  summary_link : Sst.target_link;
  summary_origin : summary_origin;
}

type registration = {
  registration_token : unit ref;
  registration_environment : environment;
  registration_program : Sst.program;
  registration_snapshot : string;
  registration_summaries : summary list;
  mutable valid : bool;
}

let direct_imports consumer target =
  Array.to_list consumer.Cmt_input.imports
  |> List.filter
    (fun (import : Cmt_input.import) ->
      String.equal import.unit_name target.Cmt_input.unit_name)

let target consumer implementation =
  let* digest = interface_digest implementation in
  if implementation.implementation_family_markers <> [ "ordinary-v1" ] then
    Error
      ("imported external target " ^ implementation.unit_name
     ^ " is not one exact ordinary artifact")
  else if
    implementation.interface_family_markers <> []
    && implementation.interface_family_markers <> [ "ordinary-v1" ]
  then
    Error
      ("imported external target " ^ implementation.unit_name
     ^ " has a nonordinary target CMI family")
  else
    match direct_imports consumer implementation with
    | [] -> Error "target is not a direct compiler import of the consumer"
    | [ { crc = Some import_crc; _ } ] when String.equal import_crc digest ->
        Ok { implementation; interface_digest = digest; import_crc }
    | [ _ ] ->
        Error
          ("consumer import CRC does not match imported external target "
         ^ implementation.unit_name)
    | _ ->
        Error
          ("consumer has duplicate import slots for external target "
         ^ implementation.unit_name)

let environment ~consumer ~targets =
  let direct =
    List.filter
      (fun implementation ->
        direct_imports consumer implementation <> []
        && implementation.Cmt_input.implementation_family_markers
           = [ "ordinary-v1" ]
        && (implementation.interface_family_markers = []
           || implementation.interface_family_markers = [ "ordinary-v1" ]))
      targets
  in
  let rec collect result = function
    | [] -> Result.map List.rev result
    | implementation :: rest ->
        let* targets = result in
        let* target = target consumer implementation in
        collect (Ok (target :: targets)) rest
  in
  let* targets = collect (Ok []) direct in
  Ok
    {
      token = issuer;
      consumer;
      consumer_digest = artifact_digest consumer;
      targets;
      reserved_targets = [];
      summaries = [];
    }

let cmi_values target =
  match target.implementation.Cmt_input.embedded_interface_metadata with
  | None -> []
  | Some interface ->
      Subst.Lazy.force_signature interface.Cmi_format.cmi_sign
      |> List.filter_map (function
           | Types.Sig_value (ident, description, _)
             when
               (match description.Types.val_kind with
               | Types.Val_reg _ -> true
               | Val_mut _ | Val_prim _ | Val_ivar _ | Val_self _ | Val_anc _ ->
                   false) ->
               Some
                 ( target.implementation.unit_name ^ "." ^ Ident.name ident,
                   compiler_uid description.val_uid,
                   description.val_type )
           | Types.Sig_value _ | Types.Sig_type _ | Types.Sig_typext _
           | Types.Sig_module _ | Types.Sig_modtype _ | Types.Sig_class _
           | Types.Sig_class_type _ ->
               None)

let resolve_candidate environment ~canonical_path ~value_uid =
  if environment.token != issuer then Error "foreign target-specification environment"
  else
    let matches =
      environment.targets
      |> List.concat_map (fun target ->
             cmi_values target
             |> List.filter_map (fun (path, uid, typ) ->
                    if
                      String.equal path canonical_path
                      && String.equal uid value_uid
                    then Some (target, typ)
                    else None))
    in
    match matches with
    | [ (target, typ) ] ->
        let* formals, result, compiler_modes = callable_shape typ in
        let owner = Parametric_type.owner ~index:0 ~name:canonical_path in
        let binders =
          stable_type_variables typ
          |> List.mapi (fun ordinal id ->
                 (id, Parametric_type.binder owner ~ordinal))
        in
        let labels = List.map fst formals in
        if
          not (compiler_target_modes_are_exact_default compiler_modes)
          || not
            (default_mode_signature ~slots:(List.length labels + 1)
               (mode_for_path target.implementation canonical_path))
        then
          Error
            "imported external target is mode-bearing or lacks its exact default ABI"
        else
          Ok
            {
              candidate_token = issuer;
              candidate_environment = environment;
              target;
              canonical_path;
              value_uid;
              labels;
              compiler_modes = copy_compiler_modes compiler_modes;
              type_binders = binders;
              formal_source_types = List.map snd formals;
              result_source_type = result;
            }
    | [] ->
        Error
          ("selected target CMI has no exact value identity "
         ^ canonical_path)
    | _ -> Error "competing target CMI value identities"

let candidate_path candidate = candidate.canonical_path
let candidate_uid candidate = candidate.value_uid

let same_compiler_mode_shape left right =
  List.length left.parameter_modes = List.length right.parameter_modes
  && List.length left.result_modes = List.length right.result_modes

let compiler_callable_modes_are_compatible target wrapper =
  if not (same_compiler_mode_shape target wrapper) then false
  else
    (* One pair copy; OxCaml alone tests whether each exact target is admitted. *)
    let target, wrapper = copy_compiler_modes (target, wrapper) in
    List.combine target.parameter_modes wrapper.parameter_modes
    @ List.combine target.result_modes wrapper.result_modes
    |> List.for_all (fun (target, wrapper) ->
        let target_compatible, _ = Mode.Alloc.newvar_above target in
        Result.is_ok (Mode.Alloc.equate target_compatible wrapper))

let candidate_has_compatible_compiler_modes candidate typ =
  match callable_shape typ with
  | Ok (_, _, wrapper_modes) ->
      compiler_target_modes_are_exact_default candidate.compiler_modes
      && compiler_callable_modes_are_compatible candidate.compiler_modes
           wrapper_modes
  | Error _ -> false

let kind_for_label = function
  | None -> Parametric_signature_private.Positional_parameter
  | Some label when String.starts_with ~prefix:"?" label ->
      Parametric_signature_private.Optional_parameter
  | Some _ -> Parametric_signature_private.Labelled_parameter

let same_link left right =
  match (left, right) with
  | Sst.Imported_unverified_target _, Sst.Imported_unverified_target _ ->
      left = right
  | (Sst.Same_unit_target _ | Sst.Unresolved_target _), _
  | _, (Sst.Same_unit_target _ | Sst.Unresolved_target _) ->
      false

let target_identity candidate =
  {
    identity_unit = candidate.target.implementation.unit_name;
    identity_interface = candidate.target.interface_digest;
    identity_path = candidate.canonical_path;
    identity_uid = candidate.value_uid;
  }

let same_target_identity left right =
  String.equal left.identity_unit right.identity_unit
  && String.equal left.identity_interface right.identity_interface
  && (String.equal left.identity_path right.identity_path
     || String.equal left.identity_uid right.identity_uid)

let target_is_reserved environment candidate =
  let identity = target_identity candidate in
  List.exists (same_target_identity identity) environment.reserved_targets

let target_has_summary environment candidate =
  let identity = target_identity candidate in
  List.exists
    (fun summary ->
      same_target_identity identity (target_identity summary.summary_candidate))
    environment.summaries

let seal environment ~candidate ~resolve_application ~wrapper ~signature
    ~target_span ~declaration_span ~witness_span ~wrapper_is_unannotated_exec =
  if
    environment.token != issuer
    || candidate.candidate_token != issuer
    || candidate.candidate_environment != environment
  then Error "raw or copied external target candidate"
  else if
    target_is_reserved environment candidate
    || target_has_summary environment candidate
  then Error "duplicate or competing verified external specification"
  else if not wrapper_is_unannotated_exec then
    Error "mode-bearing verified external specifications are unsupported"
  else if
    wrapper.Sst.mode <> Sst.Exec || wrapper.recursive
    || wrapper.contracts.decreases <> []
    || wrapper.contracts.assertions <> []
    || wrapper.returns_unique_parameter <> None
  then Error "verified external specification has unsupported semantic authority"
  else
    let formals = Parametric_signature_private.formals signature in
    let labels =
      List.map
        (fun (formal : Parametric_signature_private.formal) -> formal.label)
        formals
    and kinds =
      List.map
        (fun (formal : Parametric_signature_private.formal) -> formal.kind)
        formals
    in
    let expected_kinds = List.map kind_for_label candidate.labels in
    let* target_formals =
      let rec lower lowered = function
        | [] -> Ok (List.rev lowered)
        | typ :: rest ->
            let* typ =
              lower_target_type candidate.type_binders ~resolve_application typ
            in
            lower (typ :: lowered) rest
      in
      lower [] candidate.formal_source_types
    in
    let* target_result =
      lower_target_type candidate.type_binders ~resolve_application
        candidate.result_source_type
    in
    let signature_binders = Parametric_signature_private.binders signature
    and signature_formals = Parametric_signature_private.formals signature
    and signature_result = Parametric_signature_private.result_type signature in
    let formal_modes, result_mode =
      Parametric_signature_private.modes signature
    in
    if labels <> candidate.labels || kinds <> expected_kinds then
      Error "verified external specification differs from the complete compiler ABI"
    else if
      List.length signature_binders <> List.length candidate.type_binders
      || List.length signature_formals <> List.length target_formals
      || not
           (List.for_all2
              (fun (formal : Parametric_signature_private.formal) typ ->
                Parametric_type.alpha_equal formal.typ typ)
              signature_formals target_formals)
      || not (Parametric_type.alpha_equal signature_result target_result)
    then
      Error
        "verified external specification type/binder ABI differs from its target CMI"
    else if
      List.exists (( <> ) Sst.Exec_instance) formal_modes
      || result_mode <> Sst.Exec_instance
    then Error "verified external specification attempted to mint mode authority"
    else
      let callable_abi_digest =
        Parametric_signature_private.semantic_fingerprint signature
      in
      let summary_digest =
        digest
          (String.concat "\000"
             [
               "external-target-specification-v1";
               environment.consumer_digest;
               candidate.target.implementation.unit_name;
               candidate.target.interface_digest;
               candidate.target.import_crc;
               candidate.canonical_path;
               candidate.value_uid;
               callable_abi_digest;
               span_string target_span;
               span_string declaration_span;
               span_string witness_span;
             ])
      in
      let sealed_link =
        Sst.Imported_unverified_target
          {
            wrapper = wrapper.function_id;
            consumer_artifact_digest = environment.consumer_digest;
            target_unit = candidate.target.implementation.unit_name;
            target_interface_digest = candidate.target.interface_digest;
            import_crc = candidate.target.import_crc;
            canonical_path = candidate.canonical_path;
            value_uid = candidate.value_uid;
            callable_abi_digest;
            summary_digest;
            target_span;
            declaration_span;
            witness_span;
          }
      in
      let sealed_definition =
        {
          wrapper with
          Sst.body = Sst.External_specification sealed_link;
        }
      in
      let* sealed_signature =
        Parametric_signature_private.rebind_definition signature
          sealed_definition
      in
      let sealed =
        {
          sealed_token = issuer;
          sealed_environment = environment;
          sealed_candidate = candidate;
          sealed_definition;
          sealed_signature;
          sealed_link;
          sealed_bound = false;
        }
      in
      environment.reserved_targets <-
        target_identity candidate :: environment.reserved_targets;
      Ok sealed

let link sealed = sealed.sealed_link

let bind_definition environment sealed ~definition =
  if
    sealed.sealed_token != issuer || sealed.sealed_environment != environment
  then Error "raw or replayed verified external specification"
  else if sealed.sealed_bound then
    Error "verified external specification seal was already bound"
  else if definition <> sealed.sealed_definition then
    Error "verified external specification definition differs from its exact seal"
  else
    let summary =
      {
        summary_token = issuer;
        summary_environment = environment;
        summary_candidate = sealed.sealed_candidate;
        summary_definition = definition;
        summary_signature = sealed.sealed_signature;
        summary_link = sealed.sealed_link;
        summary_origin = Local_summary;
      }
    in
    sealed.sealed_bound <- true;
    environment.summaries <- summary :: environment.summaries;
    Ok summary

let complete_summary environment ~candidate ~resolve_application ~wrapper_id ~type_binders
    ~parameter_nodes ~parameters ~contracts ~terminal_arguments ~result_type
    ~target_span ~declaration_span ~witness_span
    ~wrapper_is_unannotated_exec =
  let exact_argument parameter (label, argument) =
    let parameter = Sst.require_value_parameter parameter in
    let variable =
      match argument.Sst.expression_desc with
      | Sst.Variable { binding; _ } -> Some binding
      | Sst.Optional_forward
          { expression_desc = Sst.Variable { binding; _ }; _ } ->
          Some binding
      | _ -> None
    in
    match
      ( parameter.Sst.pattern.pattern_desc,
        variable,
        argument.Sst.expression_desc )
    with
    | Sst.Bind formal, Some actual, _ ->
        label = parameter.label && formal.id = actual.id
    | Sst.Unit_pattern, None, Sst.Unit_constant -> label = parameter.label
    | _ -> false
  in
  if
    List.length terminal_arguments <> List.length parameters
    || not (List.for_all2 exact_argument parameters terminal_arguments)
  then Error "verified external specification does not forward its exact ABI"
  else
    let provisional =
      {
        Sst.function_id = wrapper_id;
        type_binders;
        mode = Sst.Exec;
        recursive = false;
        parameters;
        contracts;
        body =
          Sst.External_specification
            (Sst.Unresolved_target
               { target_name = candidate.canonical_path; witness_span });
        policy = Sst.Default_linear_z3;
        result_type;
        returns_unique_parameter = None;
        span = declaration_span;
      }
    in
    let parameter_kinds =
      List.map
        (fun parameter ->
          match parameter.fp_kind, parameter.fp_arg_label with
          | Tparam_optional_default _, _ ->
              Parametric_signature_private.Default_parameter
          | Tparam_pat _, Nolabel ->
              Parametric_signature_private.Positional_parameter
          | Tparam_pat _, Optional _ ->
              Parametric_signature_private.Optional_parameter
          | Tparam_pat _, (Labelled _ | Position _) ->
              Parametric_signature_private.Labelled_parameter)
        parameter_nodes
    in
    let* signature =
      Parametric_signature_private.create ~definition:provisional
        ~parameter_kinds
        ~parameter_modes:(List.map (fun _ -> Sst.Exec_instance) parameters)
        ~result_mode:Sst.Exec_instance ~recursive_evidence:None
    in
    let* sealed =
      seal environment ~candidate ~resolve_application ~wrapper:provisional
        ~signature ~target_span
        ~declaration_span ~witness_span ~wrapper_is_unannotated_exec
    in
    let definition =
      {
        provisional with
        Sst.body = Sst.External_specification (link sealed);
      }
    in
    let* _ = bind_definition environment sealed ~definition in
    Ok definition

let find_summary environment ~canonical_path ~value_uid =
  List.find_opt
    (fun summary ->
      summary.summary_token == issuer
      && summary.summary_environment == environment
      && String.equal summary.summary_candidate.canonical_path canonical_path
      && String.equal summary.summary_candidate.value_uid value_uid)
    environment.summaries

let definition summary = summary.summary_definition
let signature summary = summary.summary_signature
let canonical_path summary = summary.summary_candidate.canonical_path
let value_uid summary = summary.summary_candidate.value_uid

let witness_span summary =
  match summary.summary_link with
  | Sst.Imported_unverified_target { witness_span; _ } -> witness_span
  | Sst.Same_unit_target _ | Sst.Unresolved_target _ -> assert false

let position_before (left : Diagnostic.span) (right : Diagnostic.span) =
  String.equal left.file right.file
  &&
  (left.end_pos.line < right.start_pos.line
  || (left.end_pos.line = right.start_pos.line
     && left.end_pos.column <= right.start_pos.column))

let call_is_after_summary summary call_span =
  match summary.summary_origin with
  | Local_summary -> position_before (witness_span summary) call_span
  | Imported_provider_summary -> true

let mode_prefix = "verocaml.internal.instance_mode."
let compiler_mode_marker = "verocaml.internal.compiler_mode_syntax"
let has_mode_bearing_syntax binding =
  let found = ref false in
  let inspect attributes =
    if
      List.exists
        (fun attribute ->
          String.starts_with ~prefix:mode_prefix
            attribute.Parsetree.attr_name.txt
          || String.equal compiler_mode_marker
               attribute.Parsetree.attr_name.txt)
        attributes
    then found := true
  in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      value_binding =
        (fun self value ->
          inspect value.Typedtree.vb_attributes;
          default.value_binding self value);
      expr =
        (fun self expression ->
          inspect expression.Typedtree.exp_attributes;
          default.expr self expression);
      pat =
        (fun (type k) self (pattern : k Typedtree.general_pattern) ->
          inspect pattern.Typedtree.pat_attributes;
          default.pat self pattern);
    }
  in
  iterator.value_binding iterator binding;
  !found

let adopt_imported environment ~provider_unit ~provider_interface ~definition
    ~signature ~target_link =
  if environment.token != issuer then
    Error "foreign target-specification environment"
  else if
    not
      (Array.exists
         (fun (import : Cmt_input.import) ->
           String.equal import.unit_name provider_unit
           && import.crc = Some provider_interface)
         environment.consumer.imports)
  then Error "consumer does not directly import the external-specification provider"
  else
    match target_link with
    | Sst.Same_unit_target _ | Sst.Unresolved_target _ ->
        Error "provider external specification has a nonportable target link"
    | Sst.Imported_unverified_target source ->
        let* candidate =
          resolve_candidate environment ~canonical_path:source.canonical_path
            ~value_uid:source.value_uid
        in
        if
          not
            (String.equal candidate.target.implementation.unit_name
               source.target_unit
            && String.equal candidate.target.interface_digest
                 source.target_interface_digest
            && String.equal candidate.target.import_crc source.import_crc)
        then Error "consumer target identity differs from the provider specification"
        else if
          definition.Sst.body <> Sst.External_specification target_link
        then Error "provider external specification body does not match its descriptor"
        else
          let formals = Parametric_signature_private.formals signature in
          let labels =
            List.map
              (fun (formal : Parametric_signature_private.formal) -> formal.label)
              formals
          and kinds =
            List.map
              (fun (formal : Parametric_signature_private.formal) -> formal.kind)
              formals
          in
          let formal_modes, result_mode =
            Parametric_signature_private.modes signature
          in
          let fingerprint =
            Parametric_signature_private.semantic_fingerprint signature
          in
          if
            labels <> candidate.labels
            || kinds <> List.map kind_for_label candidate.labels
            || List.length (Parametric_signature_private.binders signature)
               <> List.length candidate.type_binders
            || List.exists (( <> ) Sst.Exec_instance) formal_modes
            || result_mode <> Sst.Exec_instance
          then Error "provider external specification ABI differs from the target"
          else if
            target_is_reserved environment candidate
            || target_has_summary environment candidate
          then Error "overlapping external function specifications"
          else
            let summary_digest =
              digest
                (String.concat "\000"
                   [
                     "imported-provider-external-target-specification-v1";
                     environment.consumer_digest;
                     provider_unit;
                     provider_interface;
                     candidate.target.implementation.unit_name;
                     candidate.target.interface_digest;
                     candidate.canonical_path;
                     candidate.value_uid;
                     fingerprint;
                     source.summary_digest;
                   ])
            in
            let link =
              Sst.Imported_unverified_target
                {
                  wrapper = definition.function_id;
                  consumer_artifact_digest = environment.consumer_digest;
                  target_unit = candidate.target.implementation.unit_name;
                  target_interface_digest = candidate.target.interface_digest;
                  import_crc = candidate.target.import_crc;
                  canonical_path = candidate.canonical_path;
                  value_uid = candidate.value_uid;
                  callable_abi_digest = fingerprint;
                  summary_digest;
                  target_span = source.target_span;
                  declaration_span = source.declaration_span;
                  witness_span = source.witness_span;
                }
            in
            let definition =
              { definition with Sst.body = Sst.External_specification link }
            in
            let* signature =
              Parametric_signature_private.rebind_definition signature definition
            in
            environment.summaries <-
              {
                summary_token = issuer;
                summary_environment = environment;
                summary_candidate = candidate;
                summary_definition = definition;
                summary_signature = signature;
                summary_link = link;
                summary_origin = Imported_provider_summary;
              }
              :: environment.summaries;
            Ok ()

let adopted_definitions registration =
  List.filter_map
    (fun summary ->
      match summary.summary_origin with
      | Imported_provider_summary -> Some summary.summary_definition
      | Local_summary -> None)
    registration.registration_summaries

let rebind_program_summary program summary =
  let matches =
    List.filter
      (fun definition -> definition = summary.summary_definition)
      program.Sst.functions
  in
  match matches with
  | [ definition ] ->
      let* signature =
        Parametric_signature_private.rebind_definition
          summary.summary_signature definition
      in
      Ok
        {
          summary with
          summary_definition = definition;
          summary_signature = signature;
        }
  | [] -> Error "verified external specification definition is absent from the program"
  | _ -> Error "competing verified external specification definitions"

let register environment ~consumer ~program =
  if environment.consumer != consumer then
    Error "verified external specifications were replayed for another consumer"
  else if environment.summaries = [] then Ok None
  else
    let* summaries =
      let rec rebind rebound = function
        | [] -> Ok (List.rev rebound)
        | summary :: rest ->
            let* summary =
              match summary.summary_origin with
              | Local_summary -> rebind_program_summary program summary
              | Imported_provider_summary -> Ok summary
            in
            rebind (summary :: rebound) rest
      in
      rebind [] environment.summaries
    in
    environment.summaries <- summaries;
    let registration =
      {
        registration_token = issuer;
        registration_environment = environment;
        registration_program = program;
        registration_snapshot = Sst.to_string program;
        registration_summaries = summaries;
        valid = true;
      }
    in
    incr live_registration_count;
    Ok (Some registration)

let invalidate registration =
  if registration.valid then (
    registration.valid <- false;
    decr live_registration_count)

let authentic registration program =
  registration.registration_token == issuer && registration.valid
  && registration.registration_program == program
  && String.equal registration.registration_snapshot (Sst.to_string program)

let find_definition_summary registration definition =
  List.find_opt
    (fun summary ->
      summary.summary_token == issuer
      && summary.summary_definition == definition
      && summary.summary_environment == registration.registration_environment)
    registration.registration_summaries

let authenticates_definition registration ~program ~definition link =
  authentic registration program
  &&
  match find_definition_summary registration definition with
  | Some summary -> same_link summary.summary_link link
  | None -> false

let authenticate_call registration ~program ~callee
    ~(expression : Sst.expression) =
  if not (authentic registration program) then
    Error "external target specification registration is stale"
  else
    match find_definition_summary registration callee with
    | None -> Error "call has no exact verified external specification"
    | Some summary ->
        if not (call_is_after_summary summary expression.Sst.span) then
          Error "external target call occurs before its summary"
        else
          match expression.expression_desc with
          | Sst.Direct_call { callee = id; type_arguments; arguments; _ }
            when id = callee.function_id ->
              let rec value_arguments = function
                | [] -> Ok []
                | Sst.Value_argument { label; value } :: rest ->
                    let* rest = value_arguments rest in
                    Ok ((label, value) :: rest)
                | Sst.Callback_argument _ :: _ ->
                    Error
                      "external target callback actuals are unsupported"
              in
              let* arguments = value_arguments arguments in
              Parametric_signature_private.validate_call
                summary.summary_signature ~type_arguments
                ~actual_result:expression.typ ~arguments
          | _ -> Error "external target call is not direct and saturated"

let validate_call registration ~program ~functions
    ~(caller_id : Sst.function_id) ~(callee : Sst.function_definition)
    ~expression =
  let* registration =
    Option.to_result
      ~none:"external target call has no private authentication"
      registration
  in
  let caller_kind =
    if
      List.exists
        (fun definition -> definition.Sst.function_id = caller_id)
        functions
    then Some "function"
    else if
      List.exists
        (fun definition ->
          definition.Sst.constant_id.constant_index = caller_id.function_index
          && String.equal definition.constant_id.constant_name
               caller_id.function_name)
        program.Sst.logical_constants
    then Some "logical-constant"
    else None
  in
  let* (caller_kind [@log_value.trace]) =
    Option.to_result ~none:"external target caller is absent from the program"
      caller_kind
  in
  [%log.trace "authenticated external target semantic caller"
    ~stage:(Delator.Field.string "external-target-call-validation")
    ~caller_kind:
      (Delator.Field.string (caller_kind [@log_value.trace]))
    ~caller_name:(Delator.Field.string caller_id.function_name)
    ~callee_name:(Delator.Field.string callee.function_id.function_name)
    ~decision:(Delator.Field.string "accepted")];
  let* _ =
    authenticate_call registration ~program ~callee ~expression
  in
  Ok ()

let validate_definition registration ~program ~definition ~expected_wrapper
    link =
  match link with
  | Sst.Imported_unverified_target { wrapper; _ } ->
      if not expected_wrapper then
        Error
          "external target links require a verified external-specification declaration"
      else if wrapper <> definition.Sst.function_id then
        Error "verified external specification is attached to the wrong wrapper"
      else
        (match registration with
        | Some registration
          when authenticates_definition registration ~program ~definition link ->
            Ok ()
        | Some _ | None ->
            Error
              "verified external specification lacks exact private authentication")
  | Sst.Same_unit_target _ | Sst.Unresolved_target _ ->
      Error "non-imported target reached external-target validation"

let signature_for_definition registration ~program definition =
  if not (authentic registration program) then None
  else
    Option.map
      (fun summary -> summary.summary_signature)
      (find_definition_summary registration definition)

let external_target_identity registration ~program definition =
  if not (authentic registration program) then None
  else
    Option.map
      (fun summary -> summary.summary_link)
      (find_definition_summary registration definition)

module For_testing = struct
  let compiler_target_modes_are_exact_default ~parameter_modes ~result_modes =
    compiler_target_modes_are_exact_default
      (compiler_modes parameter_modes result_modes)
  let compiler_callable_modes_are_compatible ~target_parameter_modes
      ~target_result_modes ~wrapper_parameter_modes ~wrapper_result_modes =
    let target = compiler_modes target_parameter_modes target_result_modes
    and wrapper = compiler_modes wrapper_parameter_modes wrapper_result_modes
    in
    compiler_callable_modes_are_compatible target wrapper

  let live_registrations () = !live_registration_count
  let reset_live_registrations () = live_registration_count := 0
end
