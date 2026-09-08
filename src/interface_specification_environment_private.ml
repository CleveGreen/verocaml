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

type public_logical_constant =
  | Public_defined_logical_value of {
      logical_constant : Sst.logical_constant_definition;
      logical_constant_path : string;
      logical_constant_uid : string;
      logical_constant_dependency_closure : string;
      logical_constant_trust_dependencies : string list;
    }
  | Public_symbolic_logical_value of {
      symbolic_callable : public_callable;
      logical_constant_path : string;
      logical_constant_uid : string;
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
  authentication_index : int;
  nonserializable : unit -> unit;
  unit_name : string;
  interface_digest : string;
  mode_signature_digest : string;
  types : public_type list;
  callables : public_callable list;
  logical_constants : public_logical_constant list;
  external_specifications : public_callable list;
  models : public_model list;
  invariants : public_invariant list;
  direct_dependencies : handle list;
  transitive_dependencies : handle list;
  semantic_snapshot : Sst_validation.validated_program;
  private_driver_completion : Verification_driver_private.completion;
  numeric_provider : Numeric_provider_private.t;
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
module Internal_errors = Ephemeron.K1.Make (Error_identity)

module Handle_identity = struct
  type t = handle

  let equal left right = left == right
  let hash handle = handle.authentication_index
end

module Issued_handles = Ephemeron.K1.Make (Handle_identity)

let process_issuer = ref ()
let error_diagnostics = Error_diagnostics.create 8
let internal_errors = Internal_errors.create 8
let error_diagnostics_lock = Mutex.create ()
let issued_handles = Issued_handles.create 32
let issued_handles_lock = Mutex.create ()
let next_handle_authentication_index = ref 0

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let with_error_diagnostics action =
  Mutex.lock error_diagnostics_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock error_diagnostics_lock)
    action

let with_issued_handles action =
  Mutex.lock issued_handles_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock issued_handles_lock) action

let issue_handle make =
  with_issued_handles (fun () ->
      let authentication_index = !next_handle_authentication_index in
      next_handle_authentication_index := authentication_index + 1;
      let handle = make authentication_index in
      Issued_handles.replace issued_handles handle ();
      handle)

let handle_was_issued handle =
  with_issued_handles (fun () -> Issued_handles.mem issued_handles handle)

let rec public_dependency_message message =
  let prefix = "[VERO_DEPENDENCY] " in
  if String.starts_with ~prefix message then
    public_dependency_message
      (String.sub message (String.length prefix)
         (String.length message - String.length prefix))
  else message

let broadcast_artifact_failure reason =
  let reason = String.lowercase_ascii reason in
  let contains fragment =
    let fragment_length = String.length fragment in
    let rec search offset =
      offset + fragment_length <= String.length reason
      &&
      (String.equal (String.sub reason offset fragment_length) fragment
      || search (offset + 1))
    in
    fragment_length = 0 || search 0
  in
  if contains "missing" || contains "unknown" || contains "no " || contains "lacks" then
    Diagnostic.Missing_provider_artifact
  else if contains "malformed" || contains "invalid" then
    Diagnostic.Malformed_provider_artifact
  else if contains "stale" then Diagnostic.Stale_provider_artifact
  else if contains "conflict" || contains "ambiguous" || contains "duplicate" then
    Diagnostic.Conflicting_provider_artifact
  else Diagnostic.Mismatched_provider_artifact

let _broadcast_artifact_failure_name = function
  | Diagnostic.Missing_provider_artifact -> "missing"
  | Diagnostic.Malformed_provider_artifact -> "malformed"
  | Diagnostic.Stale_provider_artifact -> "stale"
  | Diagnostic.Mismatched_provider_artifact -> "mismatched"
  | Diagnostic.Conflicting_provider_artifact -> "conflicting"

let error ?unit_name ?diagnostic ?(internal = false) message =
  let message = public_dependency_message message in
  let result = { unit_name; message } in
  with_error_diagnostics (fun () ->
      Option.iter
        (fun diagnostic ->
          Error_diagnostics.replace error_diagnostics result diagnostic)
        diagnostic;
      if internal then Internal_errors.replace internal_errors result ());
  Error result

let broadcast_dependency_error ~unit_name ~source_file ~stage:_stage ~route:_route
    reason =
  let failure = broadcast_artifact_failure reason in
  [%log.debug "classified broadcast provider artifact rejection"
    ~provider:(Delator.Field.string unit_name)
    ~stage:(Delator.Field.string _stage)
    ~route:(Delator.Field.string _route)
    ~failure_class:(Delator.Field.string "artifact")
    ~cause_class:
      (Delator.Field.string (_broadcast_artifact_failure_name failure))
    ~correlation:
      (Delator.Field.string
         (Digest.string
            (unit_name ^ ":" ^ _stage ^ ":"
           ^ _broadcast_artifact_failure_name failure)
         |> Digest.to_hex))
    ~decision:(Delator.Field.string "rejected")
    ~remedy_class:(Delator.Field.string "rebuild-provider-consumer")];
  let diagnostic =
    Diagnostic.make
      (Diagnostic.Invalid_broadcast_dependency { provider = unit_name; failure })
      (Diagnostic.file_span source_file)
  in
  error ~unit_name ~diagnostic diagnostic.message

let internal_error ?unit_name _message =
  [%log.error "classified internal verification error"
    ~unit_name:
      (Delator.Field.string (Option.value ~default:"<unknown>" unit_name))
    ~stage:(Delator.Field.string "error-routing")
    ~reason_class:(Delator.Field.string "internal-verifier")];
  error ?unit_name ~internal:true
    "verification could not complete because an internal consistency check failed"

let error_to_string error =
  Option.fold ~none:error.message
    ~some:(fun unit_name -> Printf.sprintf "unit %s: %s" unit_name error.message)
    error.unit_name

let error_diagnostic error =
  with_error_diagnostics (fun () ->
      Error_diagnostics.find_opt error_diagnostics error)

let error_is_internal error =
  with_error_diagnostics (fun () -> Internal_errors.mem internal_errors error)

let retained_authority_identity_is_exact =
  Cmt_input.retained_authority_identity_is_exact

let exact_import = Cmt_input.exact_import
let exact_imports = Cmt_input.exact_imports

let rec handle_is_authentic (handle : handle) =
  let issued = handle_was_issued handle in
  let completion_matches =
    issued && handle.issuer == process_issuer
    && Verification_driver_private.completion_matches
         handle.private_driver_completion
         ~implementation:handle.private_implementation
         ~validated:handle.semantic_snapshot
  in
  let dependencies_authentic =
    completion_matches
    && List.for_all handle_is_authentic handle.transitive_dependencies
  in
  let authentic = completion_matches && dependencies_authentic in
  if not authentic then
    [%log.warn "rejected unissued or stale verified-interface handle"
      ~provider:(Delator.Field.string handle.unit_name)
      ~stage:(Delator.Field.string "handle-authentication")
      ~issued:(Delator.Field.bool issued)
      ~completion_matches:(Delator.Field.bool completion_matches)
      ~dependencies_authentic:(Delator.Field.bool dependencies_authentic)
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "capability-authentication")];
  authentic

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
  logical_constants : public_logical_constant list;
  external_specifications : public_callable list;
  models : public_model list;
  invariants : public_invariant list;
  semantic_snapshot : Sst_validation.validated_program;
  private_driver_completion : Verification_driver_private.completion;
  numeric_provider : Numeric_provider_private.t;
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
  candidate.Cmt_input.interface_imports
  |> Array.to_list
  |> List.map (fun (import : Cmt_input.import) ->
         import.unit_name ^ "=" ^ Option.value ~default:"<missing>" import.crc)
  |> String.concat "|" |> Digest.string |> Digest.to_hex

let construct_handle staged direct_dependencies transitive_dependencies =
  let source_digest = candidate_source_digest staged.candidate in
  let family_digest = candidate_family_digest staged.candidate in
  let import_digest = candidate_import_digest staged.candidate in
  let handle =
    issue_handle (fun authentication_index ->
        {
          issuer = process_issuer;
          authentication_index;
          nonserializable = (fun () -> ());
          unit_name = staged.candidate.unit_name;
          interface_digest = staged.interface_digest;
          mode_signature_digest = staged.mode_signature_digest;
          types = staged.types;
          callables = staged.callables;
          logical_constants = staged.logical_constants;
          external_specifications = staged.external_specifications;
          models = staged.models;
          invariants = staged.invariants;
          direct_dependencies;
          transitive_dependencies;
          semantic_snapshot = staged.semantic_snapshot;
          private_driver_completion = staged.private_driver_completion;
          numeric_provider = staged.numeric_provider;
          private_implementation = staged.candidate;
          source_digest;
          family_digest;
          import_digest;
        })
  in
  [%log.trace "issued verified-interface handle capability"
    ~provider:(Delator.Field.string handle.unit_name)
    ~stage:(Delator.Field.string "handle-authentication")
    ~direct_dependency_count:
      (Delator.Field.int (List.length direct_dependencies))
    ~transitive_dependency_count:
      (Delator.Field.int (List.length transitive_dependencies))
    ~decision:(Delator.Field.string "issued")];
  handle

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
and root_logical_constants = function
  | Handle_root handle -> handle.logical_constants
  | Staged_root staged -> staged.logical_constants
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
      let rec signature enclosing_module_types prefix items =
        let local_module_types =
          List.filter_map
            (function
              | Types.Sig_modtype (ident, declaration, _) ->
                  Option.map (fun typ -> (ident, typ))
                    declaration.Types.mtd_type
              | _ -> None)
            items
        in
        let module_types = local_module_types @ enclosing_module_types in
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
                    signature module_types (Ident.name ident :: prefix) nested
                | None -> [])
            | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
            | Types.Sig_class_type _ ->
                [])
          items
      in
      signature [] [] root

let binding_uid implementation kind path =
  let candidates =
    interface_uids implementation
    |> List.filter_map (fun (candidate_kind, candidate_path, uid) ->
           if candidate_kind = kind && String.equal candidate_path path then
             Some uid
           else None)
    |> List.sort_uniq String.compare
  in
  [%log.trace "evaluated unique compiler interface binding identity"
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~stage:(Delator.Field.string "interface-binding-correlation")
    ~member_kind:
      (Delator.Field.string (match kind with `Value -> "value" | `Type -> "type"))
    ~candidate_count:(Delator.Field.int (List.length candidates))
    ~decision:
      (Delator.Field.string
         (match candidates with
         | [ _ ] -> "correlated"
         | [] -> "unresolved"
         | _ :: _ :: _ -> "rejected"))];
  match candidates with [ uid ] -> Some uid | [] | _ :: _ :: _ -> None

let broadcast_identities implementation =
  List.map
    (fun member -> member.Retained_broadcast_private.identity)
    implementation.Cmt_input.interface_broadcasts

let preflight_broadcast_candidate ~dependencies implementation =
  let unit_name = implementation.Cmt_input.unit_name in
  let broadcast_error reason =
    broadcast_dependency_error ~unit_name ~source_file:implementation.source_file
      ~stage:"pre-solver-provider-reconciliation"
      ~route:"typed-interface" reason
  in
  let imported_identities =
    dependencies
    |> List.filter (exact_imports implementation)
    |> List.concat_map broadcast_identities
    |> List.sort_uniq Retained_broadcast_private.compare_identity
  in
  let imported_declaration_identities, imported_group_identities =
    List.partition
      (fun (identity : Retained_broadcast_private.identity) ->
        identity.kind = Retained_broadcast_private.Declaration)
      imported_identities
  in
  let* broadcast_scan =
    match
      Typedtree_adapter_private.Public.Broadcast.authenticate_typedtree
        ~imported_declarations:imported_declaration_identities
        ~imported_groups:imported_group_identities
        ~source_file:implementation.source_file ~imports:implementation.imports
        ~artifact:
          (Some
             (Typedtree_adapter_private.Public.proof_capture_artifact
                implementation))
        implementation.structure
    with
    | Ok scan -> Ok scan
    | Error diagnostic ->
        error ~unit_name ~diagnostic diagnostic.Diagnostic.message
  in
  let interface_members = implementation.Cmt_input.interface_broadcasts in
  let local_identities = broadcast_identities implementation in
  let available_identities =
    local_identities @ imported_identities
    |> List.sort_uniq Retained_broadcast_private.compare_identity
  in
  let identity_for_source
      (source : Retained_broadcast_private.source_reference) =
    let candidates =
      List.filter
        (fun (identity : Retained_broadcast_private.identity) ->
          String.equal identity.provider_origin source.member_provider_origin
          && String.equal identity.interface_digest
               source.member_interface_receipt
          && String.equal identity.dependency_receipt
               source.member_dependency_receipt
          && identity.kind = source.member_kind
          && String.equal identity.compiler_uid source.member_compiler_uid
          && String.equal identity.canonical_path source.member_canonical_path)
        available_identities
    in
    match candidates with
    | [ identity ] -> Ok identity
    | [] -> Error "broadcast group has an unknown compiler-resolved retained member"
    | _ :: _ :: _ ->
        Error "broadcast group has an ambiguous compiler-resolved retained member"
  in
  let implementation_triggers =
    Typedtree_broadcast_private.declaration_triggers broadcast_scan
  and implementation_groups =
    Typedtree_broadcast_private.groups broadcast_scan
  in
  let identity_for_target target =
    let kind =
      if target.Typedtree_broadcast_private.target_group then
        Retained_broadcast_private.Group
      else Retained_broadcast_private.Declaration
    in
    let local_path =
      if kind = Retained_broadcast_private.Declaration then
        let prefix = "broadcast:" in
        if
          String.starts_with ~prefix target.target_id
          && List.mem_assoc target.target_id implementation_triggers
        then
          Some
            (unit_name ^ "."
            ^ String.sub target.target_id (String.length prefix)
                (String.length target.target_id - String.length prefix))
        else None
      else
        implementation_groups
        |> List.find_opt (fun group ->
               String.equal group.Typedtree_broadcast_private.group_id
                 target.target_id)
        |> Option.map (fun (group : Typedtree_broadcast_private.group) ->
               unit_name ^ "." ^ group.group_path)
    in
    let candidates =
      match local_path with
      | Some canonical_path ->
          List.filter
            (fun (identity : Retained_broadcast_private.identity) ->
              identity.kind = kind
              && String.equal identity.canonical_path canonical_path
              && target.target_interface_uid = Some identity.compiler_uid)
            local_identities
      | None ->
          List.filter
            (fun (identity : Retained_broadcast_private.identity) ->
              identity.kind = kind
              && String.equal identity.compiler_uid target.target_uid
              && String.equal identity.canonical_path target.target_path)
            imported_identities
    in
    match candidates with
    | [ identity ] -> Ok identity
    | [] -> Error "implementation broadcast group has an unknown retained member"
    | _ :: _ :: _ ->
        Error "implementation broadcast group has an ambiguous retained member"
  in
  let declarations, groups =
    List.partition
      (fun member ->
        member.Retained_broadcast_private.identity.kind
        = Retained_broadcast_private.Declaration)
      interface_members
  in
  let rec reconcile_declarations = function
    | [] -> Ok ()
    | member :: rest ->
        let identity = member.Retained_broadcast_private.identity in
        let prefix = unit_name ^ "." in
        if not (String.starts_with ~prefix identity.canonical_path) then
          Error "retained broadcast declaration has a foreign provider path"
        else
          let declaration_id =
            "broadcast:"
            ^ String.sub identity.canonical_path (String.length prefix)
                (String.length identity.canonical_path - String.length prefix)
          in
          (match
             ( Typedtree_broadcast_private.declaration_identity broadcast_scan
                 declaration_id,
               List.assoc_opt declaration_id implementation_triggers )
           with
          | Some (_, Some interface_uid), Some [ _ ]
            when String.equal interface_uid identity.compiler_uid ->
              [%log.trace "reconciled exact interface/implementation declaration identity"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "pre-solver-authority")
                ~route:
                  (Delator.Field.string "typedtree-interface-reconciliation")
                ~correlation:
                  (Delator.Field.string
                     (Retained_broadcast_private.correlation identity))
                ~decision:(Delator.Field.string "accepted")];
              reconcile_declarations rest
          | (Some _ | None), (Some _ | None) ->
              Error
                "retained broadcast declaration lacks exactly one completed implementation trigger")
  in
  let rec reconcile_groups = function
    | [] -> Ok ()
    | member :: rest ->
        let identity = member.Retained_broadcast_private.identity in
        let matching =
          List.filter
            (fun group ->
              String.equal identity.canonical_path
                (unit_name ^ "."
                ^ group.Typedtree_broadcast_private.group_path)
              && group.group_interface_uid = Some identity.compiler_uid)
            implementation_groups
        in
        let* implementation_group =
          match matching with
          | [ group ] -> Ok group
          | [] -> Error "retained broadcast group has no implementation group"
          | _ :: _ :: _ ->
              Error "retained broadcast group has ambiguous implementation authority"
        in
        let* interface_members =
          List.fold_left
            (fun result source ->
              let* identities = result in
              let* identity = identity_for_source source in
              Ok (identity :: identities))
            (Ok []) member.source_members
        in
        let* interface_set =
          match Retained_broadcast_private.canonical_set interface_members with
          | Ok set -> Ok set
          | Error _ ->
              Error "retained broadcast interface group contains a duplicate member"
        in
        let* implementation_members =
          List.fold_left
            (fun result target ->
              let* identities = result in
              let* identity = identity_for_target target in
              Ok (identity :: identities))
            (Ok []) implementation_group.group_targets
        in
        let* implementation_set =
          match
            Retained_broadcast_private.canonical_set implementation_members
          with
          | Ok set -> Ok set
          | Error _ ->
              Error
                "retained broadcast implementation group contains a duplicate member"
        in
        if
          List.compare Retained_broadcast_private.compare_identity interface_set
            implementation_set
          <> 0
        then
          Error
            "retained broadcast interface and implementation group member sets differ"
        else (
          [%log.debug "reconciled pre-solver broadcast group exact set"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "pre-solver-authority")
            ~route:(Delator.Field.string "typedtree-interface-reconciliation")
            ~member_kind:(Delator.Field.string "group")
            ~set_cardinality:(Delator.Field.int (List.length interface_set))
            ~decision:(Delator.Field.string "accepted")];
          reconcile_groups rest)
  in
  match reconcile_declarations declarations with
  | Error reason -> broadcast_error reason
  | Ok () -> (
      match reconcile_groups groups with
      | Error reason -> broadcast_error reason
      | Ok () ->
          [%log.debug "completed pre-solver broadcast authority reconciliation"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "pre-solver-authority")
            ~route:(Delator.Field.string "typedtree-interface-reconciliation")
            ~declaration_count:(Delator.Field.int (List.length declarations))
            ~group_count:(Delator.Field.int (List.length groups))
            ~decision:(Delator.Field.string "accepted")];
          Ok ())

let preflight_broadcast_implementations ~dependencies ~consumer =
  let candidates =
    dependencies @ [ consumer ]
    |> List.fold_left
         (fun unique candidate ->
           if List.exists (( == ) candidate) unique then unique
           else unique @ [ candidate ])
         []
  in
  let rec preflight = function
    | [] -> Ok ()
    | implementation :: rest ->
        let* () =
          preflight_broadcast_candidate ~dependencies implementation
        in
        preflight rest
  in
  preflight candidates

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

let rec provider_of_root_internal root =
  require_root root;
  ignore (root_completion root);
  let implementation = root_implementation root
  and semantic_snapshot = root_snapshot root
  and unit_name = root_unit_name root in
  let provider_completion =
    Verification_driver_private.provider_completion (root_completion root)
    |> Option.get
  in
  let rec seal_dependencies sealed = function
    | [] -> Ok (List.rev sealed)
    | dependency :: rest ->
        let* provider = provider_of_root_internal dependency in
        seal_dependencies (provider :: sealed) rest
  in
  let* direct_dependencies =
    seal_dependencies [] (root_dependencies root)
  in
  let* imported =
    match Imported_callable.create direct_dependencies with
    | Ok imported -> Ok imported
    | Error message -> error ~unit_name message
  in
  let imported_declaration_identities =
    Imported_callable.broadcast_declarations imported
    |> List.map
         (fun (declaration : Imported_callable.broadcast_declaration_snapshot) ->
           declaration.identity)
  and imported_group_identities =
    Imported_callable.broadcast_groups imported
    |> List.map (fun (group : Imported_callable.broadcast_group_snapshot) ->
           group.identity)
  in
  let* broadcast_scan =
    match
      Typedtree_adapter_private.Public.Broadcast.authenticate_typedtree
        ~imported_declarations:imported_declaration_identities
        ~imported_groups:imported_group_identities
        ~source_file:implementation.source_file ~imports:implementation.imports
        ~artifact:
          (Some
             (Typedtree_adapter_private.Public.proof_capture_artifact
                implementation))
        implementation.structure
    with
    | Ok scan -> Ok scan
    | Error diagnostic ->
        error ~unit_name ~diagnostic
          "provider broadcast implementation metadata is invalid"
  in
  let interface_members = implementation.Cmt_input.interface_broadcasts in
  let retained_declaration_paths =
    interface_members
    |> List.filter_map (fun member ->
           let identity = member.Retained_broadcast_private.identity in
           if identity.kind = Retained_broadcast_private.Declaration then
             Some identity.canonical_path
           else None)
  in
  let broadcast_triggers =
    Typedtree_broadcast_private.declaration_triggers broadcast_scan
  in
  [%log.debug "authenticated provider broadcast interface"
    ~unit_name:(Delator.Field.string unit_name)
    ~interface_declarations:
      (Delator.Field.int (List.length retained_declaration_paths))
    ~interface_groups:
      (Delator.Field.int
         (List.length interface_members
         - List.length retained_declaration_paths))
    ~implementation_declarations:
      (Delator.Field.int (List.length broadcast_triggers))];
  let callable_descriptors =
    Sst_validation.callable_descriptors semantic_snapshot
  and public_models = root_models root in
  let logical_value_callables =
    root_logical_constants root
    |> List.filter_map (function
         | Public_symbolic_logical_value { symbolic_callable; _ } ->
             Some symbolic_callable
         | Public_defined_logical_value _ -> None)
  in
  let callables =
    (root_callables root @ logical_value_callables)
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
                 {
                   pattern = { pattern_desc = Sst.Wildcard; _ };
                   optional_default = None;
                   _;
                 } ->
                 true
             | Sst.Value_parameter _ | Sst.Callback_parameter _ -> false
           in
           let symbolic =
             match definition.body with
             | Sst.Symbolic_declaration _ -> true
             | Sst.Checked_exec _ | Sst.Spec_definition _
             | Sst.Recursive_spec_definition _ | Sst.Proof_body _
             | Sst.External_specification _ | Sst.Trusted_external_spec_target _
             | Sst.Trusted_external_body _ ->
                 false
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
                   let broadcast_trigger_span =
                     if
                       List.mem resolved_path retained_declaration_paths
                     then
                       match
                         List.assoc_opt
                           ("broadcast:" ^ definition.function_id.function_name)
                           broadcast_triggers
                       with
                       | Some [ location ] ->
                           Some
                             (Diagnostic.span_of_location
                                ~fallback_file:implementation.source_file
                                location)
                       | Some ([] | _ :: _ :: _) | None -> None
                     else None
                   in
                   [%log.trace "classified provider broadcast callable"
                     ~unit_name:(Delator.Field.string unit_name)
                     ~path:(Delator.Field.string resolved_path)
                     ~correlation:
                       (Delator.Field.string
                          (Digest.string
                             (unit_name ^ ":provider-callable:"
                            ^ callable_binding_uid)
                          |> Digest.to_hex))
                     ~declared:
                       (Delator.Field.bool
                          (List.mem resolved_path retained_declaration_paths))
                     ~authenticated:
                       (Delator.Field.bool
                          (Option.is_some broadcast_trigger_span))];
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
                     broadcast_trigger_span;
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
               | Error _ ->
                   [%log.debug
                     "rejected completed external specification signature"
                     ~provider:(Delator.Field.string unit_name)
                     ~stage:(Delator.Field.string "environment-sealing")
                     ~decision:(Delator.Field.string "rejected")
                     ~reason_class:
                       (Delator.Field.string "external-signature")];
                   None
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
  let broadcast_error reason =
    broadcast_dependency_error ~unit_name ~source_file:implementation.source_file
      ~stage:"provider-environment-reconciliation" ~route:"provider-environment"
      reason
  in
  let local_identities =
    List.map
      (fun member -> member.Retained_broadcast_private.identity)
      interface_members
  in
  let available_identities =
    local_identities @ imported_declaration_identities
    @ imported_group_identities
  in
  let identity_for_source
      (source : Retained_broadcast_private.source_reference) =
    let candidates =
      List.filter
        (fun (identity : Retained_broadcast_private.identity) ->
          String.equal identity.provider_origin source.member_provider_origin
          && String.equal identity.interface_digest
               source.member_interface_receipt
          && String.equal identity.dependency_receipt
               source.member_dependency_receipt
          && identity.kind = source.member_kind
          && String.equal identity.compiler_uid source.member_compiler_uid
          && String.equal identity.canonical_path
               source.member_canonical_path)
        available_identities
    in
    match candidates with
    | [ identity ] -> Ok identity
    | [] ->
        [%log.debug "rejected unknown retained broadcast source identity"
          ~provider:(Delator.Field.string unit_name)
          ~stage:(Delator.Field.string "compiler-identity")
          ~member_kind:
            (Delator.Field.string
               (Retained_broadcast_private.kind_name source.member_kind))
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "unknown-full-identity")];
        Error
          "broadcast group has an unknown compiler-resolved retained member"
    | _ :: _ :: _ ->
        [%log.debug "rejected ambiguous retained broadcast source identity"
          ~provider:(Delator.Field.string unit_name)
          ~stage:(Delator.Field.string "compiler-identity")
          ~member_kind:
            (Delator.Field.string
               (Retained_broadcast_private.kind_name source.member_kind))
          ~candidate_count:(Delator.Field.int (List.length candidates))
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "ambiguous-full-identity")];
        Error
          "broadcast group has an ambiguous compiler-resolved retained member"
  in
  let source_binding source identity =
    if String.equal identity.Retained_broadcast_private.provider_origin unit_name then
      Ok
        {
          Imported_callable.broadcast_source_reference = source;
          broadcast_source_identity = identity;
          broadcast_source_edge = None;
          broadcast_source_authority_index = None;
        }
    else
      let dependencies =
        root_dependencies root
        |> List.filter (fun dependency ->
               let candidate = root_implementation dependency in
               exact_imports implementation candidate
               &&
               (String.equal candidate.unit_name identity.provider_origin
               || List.exists
                    (fun member ->
                      Retained_broadcast_private.equal_identity
                        member.Retained_broadcast_private.identity identity)
                    candidate.interface_broadcasts))
      in
      match (dependencies, implementation.retained_authority) with
      | [ dependency ], Some parent ->
          let candidate = root_implementation dependency in
          let edges =
            List.filter
              (fun edge ->
                String.equal edge.Retained_interface_authority_private.dependency_unit
                  candidate.unit_name
                && edge.dependency_authority_receipt
                   = candidate.retained_authority_receipt)
              parent.dependencies
          in
          (match (edges, candidate.retained_authority_index) with
          | [ edge ], Some index ->
              [%log.trace "bound broadcast source to exact selected authority edge"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "provider-group-source-binding")
                ~route:(Delator.Field.string "selected-authority-closure")
                ~member_kind:
                  (Delator.Field.string
                     (Retained_broadcast_private.kind_name identity.kind))
                ~decision:(Delator.Field.string "accepted")];
              Ok
                {
                  Imported_callable.broadcast_source_reference = source;
                  broadcast_source_identity = identity;
                  broadcast_source_edge = Some edge;
                  broadcast_source_authority_index = Some index;
                }
          | [], (Some _ | None) | _ :: _ :: _, (Some _ | None)
          | [ _ ], None ->
              Error "broadcast source lacks one exact selected authority edge")
      | [], (Some _ | None) | _ :: _ :: _, (Some _ | None) | [ _ ], None ->
          Error "broadcast source does not resolve through one selected authority"
  in
  let implementation_triggers =
    Typedtree_broadcast_private.declaration_triggers broadcast_scan
  and implementation_groups = Typedtree_broadcast_private.groups broadcast_scan in
  let identity_for_target target =
    let kind =
      if target.Typedtree_broadcast_private.target_group then
        Retained_broadcast_private.Group
      else Retained_broadcast_private.Declaration
    in
    let local_path =
      if kind = Retained_broadcast_private.Declaration then
        if List.mem_assoc target.target_id implementation_triggers then
          let prefix = "broadcast:" in
          Some
            (unit_name ^ "."
            ^ String.sub target.target_id (String.length prefix)
                (String.length target.target_id - String.length prefix))
        else None
      else
        implementation_groups
        |> List.find_opt (fun group ->
               String.equal group.Typedtree_broadcast_private.group_id
                 target.target_id)
        |> Option.map (fun (group : Typedtree_broadcast_private.group) ->
               unit_name ^ "." ^ group.group_path)
    in
    let candidates =
      match local_path with
      | Some canonical_path ->
          List.filter
            (fun (identity : Retained_broadcast_private.identity) ->
              identity.kind = kind
              && String.equal identity.canonical_path canonical_path
              && target.target_interface_uid = Some identity.compiler_uid)
            local_identities
      | None ->
          List.filter
            (fun (identity : Retained_broadcast_private.identity) ->
              identity.kind = kind
              && String.equal identity.compiler_uid target.target_uid
              && String.equal identity.canonical_path target.target_path)
            (imported_declaration_identities @ imported_group_identities)
    in
    match candidates with
    | [ identity ] -> Ok identity
    | [] -> Error "implementation broadcast group has an unknown retained member"
    | _ :: _ :: _ ->
        Error "implementation broadcast group has an ambiguous retained member"
  in
  let declaration_members =
    List.filter
      (fun member ->
        member.Retained_broadcast_private.identity.kind
        = Retained_broadcast_private.Declaration)
      interface_members
  in
  let rec prepare_declarations prepared = function
    | [] -> Ok (List.rev prepared)
    | member :: rest ->
        let identity = member.Retained_broadcast_private.identity in
        let matching =
          root_callables root
          |> List.filter (fun (callable : public_callable) ->
                 String.equal identity.canonical_path
                   (unit_name ^ "."
                  ^ callable.definition.Sst.function_id.function_name))
        in
        let* callable =
          match matching with
          | [ callable ] -> Ok callable
          | [] ->
              Error
                "retained broadcast declaration has no completed implementation definition"
          | _ :: _ :: _ ->
              Error
                "retained broadcast declaration has ambiguous implementation authority"
        in
        let definition = callable.definition in
        let declaration_id =
          "broadcast:" ^ definition.Sst.function_id.function_name
        in
        let* trigger_span =
          match List.assoc_opt declaration_id implementation_triggers with
          | Some [ location ] ->
              Ok
                (Diagnostic.span_of_location
                   ~fallback_file:implementation.source_file location)
          | Some ([] | _ :: _ :: _) | None ->
              Error
                "retained broadcast declaration lacks exactly one authenticated trigger"
        in
        let* kind =
          Typedtree_adapter_private.Public.Broadcast.authenticate_definition
            ~theorem_id:declaration_id ~trigger_span definition
        in
        let* signature =
          Parametric_interface_provider_private.signature ~definition
            ~parameter_kinds:
              (parameter_kinds implementation
                 ~canonical_path:identity.canonical_path definition)
            ~parameter_modes:callable.parameter_modes
            ~result_mode:callable.result_mode ~provider_completion
        in
        let* () =
          match
            Typedtree_broadcast_private.declaration_identity broadcast_scan
              declaration_id
          with
          | Some (_, Some uid) when String.equal uid identity.compiler_uid ->
              [%log.trace "correlated provider declaration compiler identity"
                ~provider:(Delator.Field.string unit_name)
                ~route:(Delator.Field.string "provider-interface")
                ~stage:(Delator.Field.string "signature-admission")
                ~correlation:
                  (Delator.Field.string
                     (Retained_broadcast_private.correlation identity))
                ~decision:(Delator.Field.string "correlated")];
              Ok ()
          | Some _ | None ->
              Error
                "retained broadcast declaration compiler identity does not match its implementation"
        in
        let broadcast_trust =
          match kind with
          | Broadcast_declaration_private.Proved_lemma ->
              Retained_broadcast_private.Proved
          | Broadcast_declaration_private.Trusted_axiom ->
              Retained_broadcast_private.Trusted
        in
        [%log.debug "admitted completed provider broadcast declaration"
          ~provider:(Delator.Field.string unit_name)
          ~route:(Delator.Field.string "provider-interface")
          ~stage:(Delator.Field.string "signature-admission")
          ~member_kind:(Delator.Field.string "declaration")
          ~trust_class:
            (Delator.Field.string
               (match broadcast_trust with
               | Retained_broadcast_private.Proved -> "proved"
               | Retained_broadcast_private.Trusted -> "trusted"))
          ~correlation:
            (Delator.Field.string
               (Retained_broadcast_private.correlation identity))
          ~decision:(Delator.Field.string "accepted")];
        prepare_declarations
          ({
             Imported_callable.broadcast_identity = identity;
             broadcast_definition = definition;
             broadcast_signature = signature;
             broadcast_trigger_span = trigger_span;
             broadcast_trust;
           }
          :: prepared)
          rest
  in
  let* broadcast_declarations =
    match prepare_declarations [] declaration_members with
    | Ok declarations -> Ok declarations
    | Error reason -> broadcast_error reason
  in
  let group_members =
    List.filter
      (fun member ->
        member.Retained_broadcast_private.identity.kind
        = Retained_broadcast_private.Group)
      interface_members
  in
  let rec prepare_groups prepared = function
    | [] -> Ok (List.rev prepared)
    | member :: rest ->
        let identity = member.Retained_broadcast_private.identity in
        let matching =
          List.filter
            (fun group ->
              String.equal identity.canonical_path
                (unit_name ^ "." ^ group.Typedtree_broadcast_private.group_path)
              && group.group_interface_uid = Some identity.compiler_uid)
            implementation_groups
        in
        let* implementation_group =
          match matching with
          | [ group ] -> Ok group
          | [] -> Error "retained broadcast group has no implementation group"
          | _ :: _ :: _ ->
              Error "retained broadcast group has ambiguous implementation authority"
        in
        let* () =
          if implementation_group.group_interface_uid = Some identity.compiler_uid
          then Ok ()
          else
              Error
                "retained broadcast group compiler identity does not match its implementation"
        in
        let* interface_sources =
          List.fold_left
            (fun result source ->
              let* sources = result in
              let* identity = identity_for_source source in
              let* binding = source_binding source identity in
              Ok (binding :: sources))
            (Ok []) member.source_members
        in
        let interface_members =
          List.map
            (fun source -> source.Imported_callable.broadcast_source_identity)
            interface_sources
        in
        let* interface_set =
          match Retained_broadcast_private.canonical_set interface_members with
          | Ok set -> Ok set
          | Error _ ->
              Error
                "retained broadcast interface group contains a duplicate member"
        in
        let* implementation_members =
          List.fold_left
            (fun result target ->
              let* identities = result in
              let* identity = identity_for_target target in
              Ok (identity :: identities))
            (Ok []) implementation_group.group_targets
        in
        let* implementation_set =
          match
            Retained_broadcast_private.canonical_set implementation_members
          with
          | Ok set -> Ok set
          | Error _ ->
              Error
                "retained broadcast implementation group contains a duplicate member"
        in
        let* () =
          if
            List.compare Retained_broadcast_private.compare_identity
              interface_set implementation_set
            = 0
          then Ok ()
          else
            ( [%log.debug "rejected retained broadcast exact member set"
                ~provider:(Delator.Field.string unit_name)
                ~route:(Delator.Field.string "provider-interface")
                ~stage:(Delator.Field.string "exact-set-reconciliation")
                ~member_kind:(Delator.Field.string "group")
                ~interface_count:(Delator.Field.int (List.length interface_set))
                ~implementation_count:
                  (Delator.Field.int (List.length implementation_set))
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "full-identity-set-mismatch")];
            Error
              "retained broadcast interface and implementation group member sets differ"
            )
        in
        [%log.debug "reconciled provider broadcast group exact set"
          ~provider:(Delator.Field.string unit_name)
          ~route:(Delator.Field.string "provider-interface")
          ~stage:(Delator.Field.string "exact-set-reconciliation")
          ~member_kind:(Delator.Field.string "group")
          ~set_cardinality:(Delator.Field.int (List.length interface_set))
          ~correlation:
            (Delator.Field.string
               (Retained_broadcast_private.correlation identity))
          ~decision:(Delator.Field.string "accepted")];
        prepare_groups
          ({
             Imported_callable.broadcast_group_identity = identity;
             broadcast_members = interface_set;
             broadcast_sources = List.rev interface_sources;
           }
          :: prepared)
          rest
  in
  let* broadcast_groups =
    match prepare_groups [] group_members with
    | Ok groups -> Ok groups
    | Error reason -> broadcast_error reason
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
  let defined_logical_values =
    root_logical_constants root
    |> List.filter_map (function
         | Public_symbolic_logical_value _ -> None
         | Public_defined_logical_value constant ->
           let interface_receipts =
             implementation.Cmt_input.interface_logical_values
             |> List.filter (fun receipt ->
                    String.equal
                      receipt.Retained_interface_authority_private.logical_value_path
                      constant.logical_constant_path
                    && String.equal
                         receipt.Retained_interface_authority_private.logical_value_uid
                         constant.logical_constant_uid)
           in
           match interface_receipts with
           | [ constant_interface_receipt ] ->
               Some (Imported_callable.Provider_defined_logical_value
                 { constant_path = constant.logical_constant_path;
                   constant_uid = constant.logical_constant_uid;
                   constant_definition = constant.logical_constant;
                   constant_dependency_closure =
                     constant.logical_constant_dependency_closure;
                   constant_trust_dependencies =
                     constant.logical_constant_trust_dependencies;
                   constant_interface_receipt })
           | [] | _ :: _ :: _ ->
               invalid_arg
                 "authenticated logical constant lost its exact interface receipt")
  in
  let symbolic_logical_values =
    implementation.Cmt_input.interface_logical_values
    |> List.filter_map (fun receipt ->
           if
             receipt.Retained_interface_authority_private.logical_value_class
             <> Retained_interface_authority_private.Symbolic_value
           then None
           else
             match
               List.filter
                 (fun (callable : Imported_callable.provider_callable) ->
                   String.equal callable.Imported_callable.resolved_path
                     receipt.logical_value_path
                   && String.equal callable.binding_uid
                        receipt.logical_value_uid)
                 callables
             with
             | [ constant_symbolic ] ->
                 Some
                   (Imported_callable.Provider_symbolic_logical_value
                      { constant_symbolic; constant_interface_receipt = receipt })
             | [] | _ :: _ :: _ ->
                 invalid_arg
                   "authenticated symbolic logical value lost its VERO-115 descriptor")
  in
  let logical_constants =
    defined_logical_values @ symbolic_logical_values
  in
  let callables =
    List.filter
      (fun callable ->
        not
          (List.exists
             (function
               | Imported_callable.Provider_symbolic_logical_value
                   { constant_symbolic; _ } ->
                   constant_symbolic == callable
               | Imported_callable.Provider_defined_logical_value _ -> false)
             symbolic_logical_values))
      callables
  in
  [%log.debug "prepared authenticated provider exports"
    ~provider:(Delator.Field.string unit_name)
    ~stage:(Delator.Field.string "environment-sealing")
    ~callable_count:(Delator.Field.int (List.length callables))
    ~external_specification_count:
      (Delator.Field.int (List.length external_specifications))
    ~broadcast_declaration_count:
      (Delator.Field.int (List.length broadcast_declarations))
    ~broadcast_group_count:(Delator.Field.int (List.length broadcast_groups))
    ~type_count:(Delator.Field.int (List.length types))
    ~logical_constant_count:
      (Delator.Field.int (List.length logical_constants))
    ~decision:(Delator.Field.string "prepared")];
  let description =
  {
    Imported_callable.unit_name = unit_name;
    interface_digest = root_interface_digest root;
    source_digest = root_source_digest root;
    family_digest = root_family_digest root;
    import_digest = root_import_digest root;
    callables;
    broadcast_declarations;
    broadcast_groups;
    types;
    logical_sorts = implementation.Cmt_input.interface_logical_sorts;
    logical_constants;
    external_specifications;
  }
  in
  match
    Imported_callable.seal_provider_with_diagnostic
      ~provider_completion ~implementation
      ~program:(Sst_validation.program semantic_snapshot)
      ~direct_dependencies description
  with
  | Ok provider -> Ok provider
  | Error seal_error ->
      if Imported_callable.seal_error_is_internal seal_error then
        internal_error ~unit_name
          (Imported_callable.seal_error_message seal_error)
      else
        error ~unit_name
          ?diagnostic:(Imported_callable.seal_error_diagnostic seal_error)
          (Imported_callable.seal_error_message seal_error)

let provider_of_root
    (root [@delator.field (fun root -> root_unit_name root)]) =
  let[@log_value.info] _unit_name = root_unit_name root in
  let result = provider_of_root_internal root in
  (match result with
  | Ok _ ->
      [%log.info "completed retained broadcast provider authority"
        ~provider:(Delator.Field.string (_unit_name [@log_value.info]))
        ~stage:(Delator.Field.string "provider-authority")
        ~route:(Delator.Field.string "provider-environment")
        ~direct_dependency_count:
          (Delator.Field.int (List.length (root_dependencies root)))
        ~decision:(Delator.Field.string "accepted")]
  | Error _ ->
      [%log.debug "rejected retained broadcast provider authority"
        ~provider:(Delator.Field.string (_unit_name [@log_value.debug]))
        ~stage:(Delator.Field.string "provider-authority")
        ~route:(Delator.Field.string "provider-environment")
        ~direct_dependency_count:
          (Delator.Field.int (List.length (root_dependencies root)))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "authority-reconciliation")]);
  result
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let rec seal_roots sealed = function
  | [] -> (
      match Imported_callable.create (List.rev sealed) with
      | Ok environment -> Ok environment
      | Error message -> error message)
  | root :: rest -> (
      match provider_of_root root with
      | Error _ as error -> error
      | Ok provider -> seal_roots (provider :: sealed) rest)

let imported_environment_of_roots roots = seal_roots [] roots

let imported_environment_authenticated (environment : environment) =
  require_environment environment;
  imported_environment_of_roots
    (List.map (fun handle -> Handle_root handle) environment.handles)

let imported_environment_of_staged_authenticated staged =
  imported_environment_of_roots
    (List.map (fun dependency -> Staged_root dependency) staged)

let imported_environment environment =
  imported_environment_authenticated environment
  |> Result.map_error (fun error -> error.message)

let imported_environment_of_staged staged =
  imported_environment_of_staged_authenticated staged
  |> Result.map_error (fun error -> error.message)


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
