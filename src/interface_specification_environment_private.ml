(* Private capability, environment, staging, and provider owner. *)
type visibility = Revealed | Abstract

type public_clause = {
  clause_index : int;
  clause_span : Diagnostic.span;
  clause_expression : Sst.expression;
}

type public_contract = {
  requires : public_clause list;
  ensures : public_clause list;
  decreases : public_clause list;
}

type public_callable = {
  definition : Sst.function_definition;
  callable_id : Sst.function_id;
  callable_mode : Sst.verification_mode;
  parameter_types : Sst.typ list;
  parameter_modes : Sst.instance_mode list;
  finite_formals : int list;
  result_type : Sst.typ;
  result_mode : Sst.instance_mode;
  contract : public_contract;
}

type public_type = {
  definition : Sst.type_definition;
  parametric_descriptor : Parametric_adt.t option;
  type_id : Sst.type_id;
  source_name : string;
  visibility : visibility;
  kind : Sst.type_kind option;
  logical : bool;
  field_modes : (Sst.field_id * Sst.instance_mode) list;
}

type public_model = {
  callable : public_callable;
  domain : Sst.type_id;
  result_type : Sst.typ;
}

type public_invariant = {
  invariant_id : string;
  abstract_type : Sst.type_id;
  certificate_id : string;
  model_callable : Sst.function_id;
  model_snapshot_type : Sst.typ;
  predicate_callable : Sst.function_id;
  predicate_digest : string;
  public_operations :
    (Sst.function_id * Sst.abstract_operation_role) list;
}

type handle = {
  issuer : unit ref;
  nonserializable : unit -> unit;
  unit_name : string;
  interface_digest : string;
  mode_signature_digest : string;
  types : public_type list;
  callables : public_callable list;
  external_specifications : public_callable list;
  models : public_model list;
  invariants : public_invariant list;
  direct_dependencies : handle list;
  transitive_dependencies : handle list;
  semantic_snapshot : Sst_validation.validated_program;
  private_driver_completion : Verification_driver_private.completion;
  private_implementation : Cmt_input.implementation;
  source_digest : string;
  family_digest : string;
  import_digest : string;
}

type environment = {
  issuer : unit ref;
  handles : handle list;
}

type verification_report = {
  report_driver : Verification_driver_private.report;
}
[@@warning "-69"]

type provenance = {
  unit_name : string;
  interface_digest : string;
  direct_dependencies : (string * string) list;
  transitive_dependencies : (string * string) list;
}

type error = {
  unit_name : string option;
  message : string;
}

module Error_identity = struct
  type t = error

  let equal left right = left == right
  let hash error = Hashtbl.hash error
end

module Error_diagnostics = Ephemeron.K1.Make (Error_identity)

let process_issuer = ref ()
let error_diagnostics = Error_diagnostics.create 8
let error_diagnostics_lock = Mutex.create ()

let with_error_diagnostics action =
  Mutex.lock error_diagnostics_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock error_diagnostics_lock)
    action

let error ?unit_name ?diagnostic message =
  let result = { unit_name; message } in
  Option.iter
    (fun diagnostic ->
      with_error_diagnostics (fun () ->
          Error_diagnostics.replace error_diagnostics result diagnostic))
    diagnostic;
  Error result

let error_to_string error =
  Option.fold ~none:error.message
    ~some:(fun unit_name -> Printf.sprintf "unit %s: %s" unit_name error.message)
    error.unit_name

let error_diagnostic error =
  with_error_diagnostics (fun () ->
      Error_diagnostics.find_opt error_diagnostics error)

let rec handle_is_authentic (handle : handle) =
  handle.issuer == process_issuer
  && Verification_driver_private.completion_matches
       handle.private_driver_completion
       ~implementation:handle.private_implementation
       ~validated:handle.semantic_snapshot
  && List.for_all handle_is_authentic handle.transitive_dependencies

let require_handle (handle : handle) =
  ignore handle.semantic_snapshot;
  ignore handle.nonserializable;
  if not (handle_is_authentic handle) then
    invalid_arg "unauthenticated verified-interface handle"

let require_environment environment =
  if
    environment.issuer != process_issuer
    || not (List.for_all handle_is_authentic environment.handles)
  then invalid_arg "unauthenticated verified-interface environment"

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

type staged_dependency = {
  candidate : Cmt_input.implementation;
  interface_digest : string;
  mode_signature_digest : string;
  types : public_type list;
  callables : public_callable list;
  external_specifications : public_callable list;
  models : public_model list;
  invariants : public_invariant list;
  semantic_snapshot : Sst_validation.validated_program;
  private_driver_completion : Verification_driver_private.completion;
  direct_dependencies : staged_dependency list;
}

let unique_handles (handles : handle list) =
  List.fold_left
    (fun unique (handle : handle) ->
      if
        List.exists
          (fun (existing : handle) ->
            String.equal existing.unit_name handle.unit_name)
          unique
      then unique
      else unique @ [ handle ])
    [] handles

let candidate_source_digest candidate =
  Option.fold ~none:"<missing>" ~some:Digest.to_hex
    candidate.Cmt_input.source_digest

let candidate_family_digest candidate =
  Digest.string
    (String.concat "|" candidate.Cmt_input.interface_family_markers)
  |> Digest.to_hex

let candidate_import_digest candidate =
  candidate.Cmt_input.imports
  |> Array.to_list
  |> List.map (fun (import : Cmt_input.import) ->
         import.unit_name ^ "=" ^ Option.value ~default:"<missing>" import.crc)
  |> String.concat "|" |> Digest.string |> Digest.to_hex

let construct_handle staged direct_dependencies transitive_dependencies =
  let source_digest = candidate_source_digest staged.candidate in
  let family_digest = candidate_family_digest staged.candidate in
  let import_digest = candidate_import_digest staged.candidate in
  {
    issuer = process_issuer;
    nonserializable = (fun () -> ());
    unit_name = staged.candidate.unit_name;
    interface_digest = staged.interface_digest;
    mode_signature_digest = staged.mode_signature_digest;
    types = staged.types;
    callables = staged.callables;
    external_specifications = staged.external_specifications;
    models = staged.models;
    invariants = staged.invariants;
    direct_dependencies;
    transitive_dependencies;
    semantic_snapshot = staged.semantic_snapshot;
    private_driver_completion = staged.private_driver_completion;
    private_implementation = staged.candidate;
    source_digest;
    family_digest;
    import_digest;
  }

type provider_root =
  | Handle_root of handle
  | Staged_root of staged_dependency

let require_root = function
  | Handle_root handle -> require_handle handle
  | Staged_root staged ->
      if
        not
          (Verification_driver_private.completion_matches
             staged.private_driver_completion
             ~implementation:staged.candidate
             ~validated:staged.semantic_snapshot)
      then invalid_arg "stale staged private-driver completion"

let root_implementation = function
  | Handle_root handle -> handle.private_implementation
  | Staged_root staged -> staged.candidate
and root_snapshot = function
  | Handle_root handle -> handle.semantic_snapshot
  | Staged_root staged -> staged.semantic_snapshot
and root_completion = function
  | Handle_root handle -> handle.private_driver_completion
  | Staged_root staged -> staged.private_driver_completion
and root_callables = function
  | Handle_root handle -> handle.callables
  | Staged_root staged -> staged.callables
and root_external_specifications = function
  | Handle_root handle -> handle.external_specifications
  | Staged_root staged -> staged.external_specifications
and root_types = function
  | Handle_root handle -> handle.types
  | Staged_root staged -> staged.types
and root_models = function
  | Handle_root handle -> handle.models
  | Staged_root staged -> staged.models
and root_unit_name = function
  | Handle_root handle -> handle.unit_name
  | Staged_root staged -> staged.candidate.unit_name
and root_interface_digest = function
  | Handle_root handle -> handle.interface_digest
  | Staged_root staged -> staged.interface_digest
and root_source_digest = function
  | Handle_root handle -> handle.source_digest
  | Staged_root staged -> candidate_source_digest staged.candidate
and root_family_digest = function
  | Handle_root handle -> handle.family_digest
  | Staged_root staged -> candidate_family_digest staged.candidate
and root_import_digest = function
  | Handle_root handle -> handle.import_digest
  | Staged_root staged -> candidate_import_digest staged.candidate
and root_dependencies = function
  | Handle_root handle ->
      List.map (fun dependency -> Handle_root dependency)
        handle.direct_dependencies
  | Staged_root staged ->
      List.map (fun dependency -> Staged_root dependency)
        staged.direct_dependencies

let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid
let interface_uids (implementation : Cmt_input.implementation) =
  match implementation.embedded_interface_metadata with
  | None -> []
  | Some interface ->
      let root =
        Subst.Lazy.force_signature interface.Cmi_format.cmi_sign
      in
      let rec signature prefix items =
        let module_types =
          List.filter_map
            (function
              | Types.Sig_modtype (ident, declaration, _) ->
                  Option.map (fun typ -> (ident, typ))
                    declaration.Types.mtd_type
              | _ -> None)
            items
        in
        let rec module_signature seen = function
          | Types.Mty_signature nested -> Some nested
          | Mty_strengthen (nested, _, _) ->
              module_signature seen nested
          | Mty_ident (Path.Pident ident) -> (
              if List.exists (Ident.same ident) seen then None
              else
                match
                  List.find_opt
                    (fun (candidate, _) -> Ident.same candidate ident)
                    module_types
                with
                | Some (_, nested) ->
                    module_signature (ident :: seen) nested
                | None -> None)
          | Mty_ident _ | Mty_functor _ | Mty_alias _ -> None
        in
        let path ident =
          String.concat "."
            (implementation.unit_name
            :: List.rev (Ident.name ident :: prefix))
        in
        List.concat_map
          (function
            | Types.Sig_value (ident, description, _) ->
                [
                  ( `Value,
                    path ident,
                    compiler_uid description.Types.val_uid );
                ]
            | Types.Sig_type (ident, declaration, _, _) ->
                [
                  ( `Type,
                    path ident,
                    compiler_uid declaration.Types.type_uid );
                ]
            | Types.Sig_module (ident, _, declaration, _, _) -> (
                match module_signature [] declaration.Types.md_type with
                | Some nested ->
                    signature (Ident.name ident :: prefix) nested
                | None -> [])
            | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
            | Types.Sig_class_type _ ->
                [])
          items
      in
      signature [] root

let binding_uid implementation kind path =
  interface_uids implementation
  |> List.find_map (fun (candidate_kind, candidate_path, uid) ->
         if candidate_kind = kind && String.equal candidate_path path then
           Some uid
         else None)

let parameter_kinds implementation ~canonical_path
    (definition : Sst.function_definition) =
  let open Typedtree in
  let unit_prefix = implementation.Cmt_input.unit_name ^ "." in
  let relative_path =
    if String.starts_with ~prefix:unit_prefix canonical_path then
      String.sub canonical_path (String.length unit_prefix)
        (String.length canonical_path - String.length unit_prefix)
    else canonical_path
  in
  let kind (parameter : function_param) =
    match parameter.fp_kind with
    | Tparam_optional_default _ -> Imported_callable.Default_parameter
    | Tparam_pat _ -> (
        match parameter.fp_arg_label with
        | Nolabel -> Imported_callable.Positional_parameter
        | Labelled _ | Position _ ->
            Imported_callable.Labelled_parameter
        | Optional _ -> Imported_callable.Optional_parameter)
  in
  let expression = function
    | {
        exp_desc =
          Texp_function { params; body = Tfunction_body _; _ };
        _;
      } ->
        Some (List.map kind params)
    | _ -> None
  in
  let binding prefix (binding : value_binding) =
    match binding.vb_pat.pat_desc with
    | Tpat_var (_, name, _, _, _)
      when String.equal
             (String.concat "." (prefix @ [ name.txt ]))
             relative_path ->
        expression binding.vb_expr
    | _ -> None
  in
  let rec module_expression prefix expression =
    match expression.mod_desc with
    | Tmod_structure structure -> items prefix structure.str_items
    | Tmod_constraint (inner, _, _, _) -> module_expression prefix inner
    | Tmod_ident _ | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _
    | Tmod_unpack _ -> None
  and items prefix = function
    | [] -> None
    | { str_desc = Tstr_value (_, bindings); _ } :: rest -> (
        match List.find_map (binding prefix) bindings with
        | Some _ as kinds -> kinds
        | None -> items prefix rest)
    | { str_desc = Tstr_module binding; _ } :: rest -> (
        let nested =
          Option.bind binding.mb_name.txt (fun name ->
              module_expression (prefix @ [ name ]) binding.mb_expr)
        in
        match nested with
        | Some _ as kinds -> kinds
        | None -> items prefix rest)
    | _ :: rest -> items prefix rest
  in
  Option.value
    ~default:
      (List.map
         (fun parameter ->
           let parameter = Sst.require_value_parameter parameter in
           match parameter.label with
           | None -> Imported_callable.Positional_parameter
           | Some label when String.starts_with ~prefix:"?" label ->
               Imported_callable.Optional_parameter
           | Some _ -> Imported_callable.Labelled_parameter)
         definition.parameters)
    (items [] implementation.Cmt_input.structure.str_items)

let snapshot_requirement validated descriptor ordinal =
  match
    Sst_validation.finite_formal_requirement validated descriptor ordinal
  with
  | None -> None
  | Some requirement ->
      let rank = Sst_validation.finite_formal_rank_domain requirement in
      let component_snapshot =
        Sst_validation.rank_component rank
        |> List.map (fun (identity : Typedtree_adapter.rank_type_identity) ->
               Printf.sprintf "%s#%d|%s|%s"
                 identity.rank_type_id.type_name
                 identity.rank_type_id.type_index identity.rank_path
                 identity.rank_uid)
      in
      Some
        {
          Imported_callable.ordinal;
          label = Sst_validation.finite_formal_label requirement;
          pattern_digest =
            Sst_validation.finite_formal_pattern_digest requirement;
          binding_ids =
            Sst_validation.finite_formal_binding_ids requirement;
          mode = Sst_validation.finite_formal_mode requirement;
          typ = Sst_validation.finite_formal_type requirement;
          rank_domain_id = Sst_validation.rank_domain_id rank;
          rank_domain_version = Sst_validation.rank_domain_version rank;
          rank_domain_digest = Sst_validation.rank_snapshot_digest rank;
          rank_component_snapshot = component_snapshot;
          profile_actual_snapshot =
            ("authenticated-retained-profile:"
            ^ Sst_validation.rank_snapshot_digest rank)
            :: component_snapshot;
          requirement_digest =
            Sst_validation.finite_formal_digest requirement;
          rank_component = Sst_validation.rank_component rank;
          rank_positive_children =
            Sst_validation.rank_positive_children rank;
          rank_ground_witnesses =
            Sst_validation.rank_ground_witnesses rank;
        }

let rec provider_of_root root =
  require_root root;
  ignore (root_completion root);
  let implementation = root_implementation root
  and semantic_snapshot = root_snapshot root
  and unit_name = root_unit_name root in
  let provider_completion =
    Verification_driver_private.provider_completion (root_completion root)
    |> Option.get
  in
  let callable_descriptors =
    Sst_validation.callable_descriptors semantic_snapshot
  and public_models = root_models root in
  let callables =
    root_callables root
    |> List.filter_map (fun (callable : public_callable) ->
           let definition = callable.definition in
           let binding_path =
             unit_name ^ "." ^ definition.function_id.function_name
           in
           let callable_binding_uid =
             Option.value ~default:""
               (binding_uid implementation `Value binding_path)
           in
           let kinds =
             parameter_kinds implementation
               ~canonical_path:definition.function_id.function_name definition
           in
           let simple pattern =
             match pattern.Sst.pattern_desc with
             | Sst.Bind _ -> true
             | Sst.Wildcard | Sst.Owned_tree_cursor_pattern _
             | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern
             | Sst.Tuple_pattern _ | Sst.Record_pattern _
             | Sst.Constructor_pattern _ | Sst.Or_pattern _ ->
                 false
           in
           let simple_parameter = function
             | Sst.Callback_parameter _ -> false
             | Sst.Value_parameter parameter -> match parameter.optional_default with
               | None ->
                   simple parameter.pattern
                   || parameter.pattern.pattern_desc = Sst.Unit_pattern
               | Some default ->
                   parameter.pattern.pattern_desc = Sst.Wildcard
                   && simple default.optional_pattern
           in
           let symbolic_parameter = function
             | Sst.Value_parameter
                 { pattern = { pattern_desc = Sst.Wildcard; _ };
                   optional_default = None;
                   _ } ->
                 true
             | Sst.Value_parameter _ | Sst.Callback_parameter _ -> false
           in
           let symbolic =
             match definition.body with
             | Sst.Symbolic_declaration _ -> true
             | _ -> false
           in
           let eligible_abi =
             (if symbolic then
                List.for_all symbolic_parameter definition.parameters
              else List.for_all simple_parameter definition.parameters)
             && List.for_all
                  (fun clause ->
                    Option.fold ~none:true ~some:simple clause.Sst.binder)
                  definition.contracts.ensures
           in
           match
             (eligible_abi, definition.mode, definition.recursive,
              definition.body)
           with
           | false, _, _, _ -> None
           | true, Sst.Exec, _, Sst.Checked_exec _
           | true, Sst.Proof, _, Sst.Proof_body _
           | true, (Sst.Exec | Sst.Proof), false, Sst.Trusted_external_body _
           | true, Sst.Spec, false, Sst.Spec_definition _
           | true, Sst.Spec, false, Sst.Symbolic_declaration _
           | true, Sst.Spec, true, Sst.Recursive_spec_definition _ ->
               let descriptor =
                 List.find_opt
                   (fun descriptor ->
                     same_function_id
                       (Sst_validation.callable_id descriptor)
                       definition.function_id)
                   callable_descriptors
               in
               Option.bind descriptor
                 (fun descriptor ->
                   let resolved_path =
                     unit_name ^ "."
                     ^ definition.function_id.function_name
                   in
                   match
                     Parametric_interface_provider_private.signature
                       ~definition ~parameter_kinds:kinds
                       ~parameter_modes:callable.parameter_modes
                       ~result_mode:callable.result_mode
                       ~provider_completion
                   with
                   | Error _ -> None
                   | Ok signature ->
                   Some {
                     Imported_callable.resolved_path;
                     binding_uid = callable_binding_uid;
                     definition;
                     signature;
                     finite_requirements =
                       (List.mapi
                          (fun ordinal _ ->
                            snapshot_requirement semantic_snapshot descriptor
                              ordinal)
                          definition.parameters
                       |> List.filter_map Fun.id);
                     model =
                       public_models
                       |> List.find_opt (fun (model : public_model) ->
                              same_function_id model.callable.callable_id
                                definition.function_id)
                       |> Option.map (fun (model : public_model) ->
                              {
                                Imported_callable.domain = model.domain;
                                result_type = model.result_type;
                                closure_type_ids = [];
                                closure_digest = "";
                              });
                   })
           | true, _, _, _ -> None)
  in
  let external_specifications =
    root_external_specifications root
    |> List.filter_map (fun (callable : public_callable) ->
           let definition = callable.definition in
           match definition.Sst.body with
           | Sst.External_specification
               (Sst.Imported_unverified_target _ as target_link) -> (
               match
                 Parametric_interface_provider_private.signature ~definition
                   ~parameter_kinds:
                     (parameter_kinds implementation
                        ~canonical_path:definition.function_id.function_name
                        definition)
                   ~parameter_modes:callable.parameter_modes
                   ~result_mode:callable.result_mode ~provider_completion
               with
               | Error _ -> None
               | Ok signature ->
                   Some
                     {
                       Imported_callable.definition;
                       signature;
                       target_link;
                     })
           | Sst.Checked_exec _ | Sst.Spec_definition _
           | Sst.Recursive_spec_definition _ | Sst.Proof_body _
           | Sst.External_specification _ | Sst.Trusted_external_spec_target _
           | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
               None)
  in
  let types =
    let rank_domains =
      Typedtree_adapter.issued_rank_domains
        (Sst_validation.program semantic_snapshot)
    in
    let rank_profile_digest type_id =
      rank_domains
      |> List.find_opt (fun domain ->
             Typedtree_adapter.rank_component domain
             |> List.exists
                  (fun (identity : Typedtree_adapter.rank_type_identity) ->
                    identity.rank_type_id = type_id))
      |> Option.map Typedtree_adapter.rank_snapshot_digest
    in
    root_types root
    |> List.map (fun (typ : public_type) ->
           let resolved_path =
             unit_name ^ "." ^ typ.type_id.type_name
           in
           let binding_path =
             unit_name ^ "." ^ typ.source_name
           in
           let revealed = typ.visibility = Revealed in
           let rank_profile_digest =
             if revealed then rank_profile_digest typ.type_id else None
           in
           let definition =
             if revealed then typ.definition
             else
               {
                 typ.definition with
                 Sst.type_kind = Sst.Record_definition [];
                 representation = Sst.Revealed;
               }
           in
           {
             Imported_callable.resolved_path;
             source_path = binding_path;
             source_name = typ.source_name;
             binding_uid =
               Option.value ~default:""
                 (binding_uid implementation `Type binding_path);
             definition;
             parametric_descriptor = typ.parametric_descriptor;
             revealed;
             logical =
               revealed
               && (typ.logical || Option.is_some rank_profile_digest);
             field_modes = if revealed then typ.field_modes else [];
             rank_profile_digest;
           })
  in
  let description =
  {
    Imported_callable.unit_name = unit_name;
    interface_digest = root_interface_digest root;
    source_digest = root_source_digest root;
    family_digest = root_family_digest root;
    import_digest = root_import_digest root;
    callables;
    types;
    external_specifications;
  }
  in
  let rec seal_dependencies sealed = function
    | [] -> Ok (List.rev sealed)
    | dependency :: rest -> (
        match provider_of_root dependency with
        | Error _ as error -> error
        | Ok provider -> seal_dependencies (provider :: sealed) rest)
  in
  match seal_dependencies [] (root_dependencies root) with
  | Error _ as error -> error
  | Ok direct_dependencies ->
      Imported_callable.seal_provider
        ~provider_completion ~implementation
        ~program:(Sst_validation.program semantic_snapshot)
        ~direct_dependencies description

let rec seal_roots sealed = function
  | [] -> Imported_callable.create (List.rev sealed)
  | root :: rest -> (
      match provider_of_root root with
      | Error _ as error -> error
      | Ok provider -> seal_roots (provider :: sealed) rest)

let imported_environment_of_roots roots = seal_roots [] roots

let imported_environment (environment : environment) =
  require_environment environment;
  imported_environment_of_roots
    (List.map (fun handle -> Handle_root handle) environment.handles)

let imported_environment_of_staged staged =
  imported_environment_of_roots
    (List.map (fun dependency -> Staged_root dependency) staged)


let provenance environment =
  List.map
    (fun (handle : handle) ->
      require_handle handle;
      {
        unit_name = handle.unit_name;
        interface_digest = handle.interface_digest;
        direct_dependencies =
          List.map
            (fun (dependency : handle) ->
              require_handle dependency;
              (dependency.unit_name, dependency.interface_digest))
            handle.direct_dependencies;
        transitive_dependencies =
          List.map
            (fun (dependency : handle) ->
              require_handle dependency;
              (dependency.unit_name, dependency.interface_digest))
            handle.transitive_dependencies;
      })
    (require_environment environment; environment.handles)
