type import = {
  unit_name : string;
  crc : string option;
}

type interface_broadcast_group = {
  group_path : string;
  group_targets : string list;
}

type interface_symbolic_declaration = {
  symbolic_path : string;
  symbolic_uid : string;
  symbolic_marker : string;
  symbolic_type_digest : string;
  symbolic_typed_abi : string;
}

type numeric_artifact_representation =
  | Artifact_immediate
  | Artifact_boxed

let numeric_artifact_representation_name = function
  | Artifact_immediate -> "immediate"
  | Artifact_boxed -> "boxed"

type interface_numeric_carrier = {
  numeric_carrier_base : Numeric_interface_claim_private.base_reference option;
  numeric_carrier_source : Numeric_source_claim_private.carrier;
  numeric_carrier_path : string;
  numeric_carrier_uid : string;
  numeric_carrier_owner_unit : string;
  numeric_carrier_owner_cmi_full_key : string;
  numeric_carrier_owner_cmi_checked_digest : string;
  numeric_carrier_import_routes : string list;
  numeric_carrier_constructor_abi : string;
  numeric_carrier_binder_abi : string;
  numeric_carrier_compiler_jkind_abi : string;
  numeric_carrier_compiler_representation : numeric_artifact_representation;
}

type interface_numeric_role = {
  numeric_role_callable_shape : Numeric_callable_domain_private.shape option;
  numeric_role_callable_domain : Numeric_callable_domain_private.t;
  numeric_role_source : Numeric_source_claim_private.role;
  numeric_role_callable_path : string;
  numeric_role_callable_uid : string;
  numeric_role_callable_abi : string;
  numeric_role_callable_mode_abi : string;
  numeric_role_callable_owner_unit : string;
  numeric_role_callable_owner_cmi_full_key : string;
  numeric_role_callable_owner_cmi_checked_digest : string;
  numeric_role_callable_import_routes : string list;
  numeric_role_semantics_path : string;
  numeric_role_semantics_uid : string;
  numeric_role_semantics_abi : string;
  numeric_role_semantics_mode_abi : string;
  numeric_role_semantics_owner_unit : string;
  numeric_role_semantics_owner_cmi_full_key : string;
  numeric_role_semantics_owner_cmi_checked_digest : string;
  numeric_role_semantics_import_routes : string list;
  numeric_role_carrier_uid : string;
  numeric_role_carrier_owner_unit : string;
  numeric_role_carrier_owner_cmi_full_key : string;
  numeric_role_carrier_owner_cmi_checked_digest : string;
  numeric_role_carrier_import_routes : string list;
}

type interface_numeric_claims = {
  numeric_carriers : interface_numeric_carrier list;
  numeric_roles : interface_numeric_role list;
  numeric_provenance_nodes : int;
  numeric_provenance_edges : int;
  numeric_provenance_bytes : int;
}

let logical_sort_marker =
  "verocaml.internal.logical_sort.mathematical_int.v1"

let integer_literal_marker =
  "verocaml.internal.logical_sort.integer_literal.v1"

type interface_broadcast_syntax = {
  broadcast_path : string;
  broadcast_uid : string;
  broadcast_kind : Retained_broadcast_private.kind;
  broadcast_member_count : int;
  broadcast_member_paths : string list;
}

type implementation = {
  metadata : Cmt_format.cmt_infos;
  embedded_interface_metadata : Cmi_format.cmi_infos_lazy option;
  raw_artifact_digest : string;
  raw_artifact_receipt : string;
  filename : string;
  source_file : string;
  unit_name : string;
  interface_digest : string option;
  interface_filename : string option;
  retained_authority_filename : string option;
  retained_authority : Retained_interface_authority_private.t option;
  retained_authority_receipt : string option;
  retained_authority_index : string option;
  interface_view_receipts : string list;
  structure : Typedtree.structure;
  imports : import array;
  compiler_arguments : string array;
  source_digest : string option;
  build_directory : string;
  load_path_visible : string list;
  load_path_hidden : string list;
  declared_artifact_directories : string list;
  embedded_interface : bool;
  explicit_interface : bool;
  interface_unit_name : string option;
  interface_implementation_unit_name : string option;
  interface_parameter_count : int;
  interface_imports : import array;
  implementation_family_markers : string list;
  implementation_family_issuers : string list;
  ppxlib_context : bool;
  verification_scope_markers : string list;
  implementation_metadata_valid : bool;
  interface_family_markers : string list;
  interface_family_issuers : string list;
  interface_mode_signatures : (string * string option) list;
  interface_finite_signatures : (string * string option) list;
  interface_broadcast_declarations : string list;
  interface_broadcast_groups : interface_broadcast_group list;
  interface_symbolic_declarations : interface_symbolic_declaration list;
  interface_logical_values : Retained_interface_authority_private.logical_value list;
  interface_logical_sorts : Logical_sort_private.t list;
  interface_numeric_claims : interface_numeric_claims;
  interface_broadcasts : Retained_broadcast_private.interface_member list;
  declaration_dependency_count : int;
  has_implementation_shape : bool;
  identifier_occurrence_count : int;
}

let retained_authority_identity_is_exact
    (candidate : implementation) =
  match
    ( candidate.retained_authority,
      candidate.retained_authority_receipt,
      candidate.retained_authority_index )
  with
  | Some authority, Some _, Some index ->
      String.equal index (Retained_interface_authority_private.index authority)
  | None, None, None -> true
  | (Some _ | None), (Some _ | None), (Some _ | None) -> false

let interface_receipt_matches (dependency : implementation) receipt =
  match dependency.interface_digest with
  | Some retained_receipt when String.equal receipt retained_receipt ->
      Some `Retained_interface
  | Some _ when List.mem receipt dependency.interface_view_receipts ->
      Some `Authenticated_ordinary_view
  | Some _ | None -> None

let retained_authority_import owner (import : import) =
  match (owner.retained_authority, import.crc) with
  | Some authority, Some compiler_receipt ->
      let matching =
        List.filter
          (fun dependency ->
            String.equal
              dependency.Retained_interface_authority_private.dependency_unit
              import.unit_name
            && dependency.dependency_compiler_receipt = Some compiler_receipt)
          authority.dependencies
      in
      let required =
        List.exists
          (fun dependency ->
            Option.is_some
              dependency.Retained_interface_authority_private.dependency_authority_receipt)
          matching
      in
      [%log.trace "classified retained authority compiler import"
        ~stage:(Delator.Field.string "retained-authority-import")
        ~route:(Delator.Field.string "exact-vri-dependency")
        ~unit_name:(Delator.Field.string import.unit_name)
        ~matching_count:(Delator.Field.int (List.length matching))
        ~decision:
          (Delator.Field.string (if required then "required" else "ordinary"))];
      required
  | Some _, None | None, _ -> false

let parent_authority_edge_matches ~owner
    (dependency : implementation) import_receipt =
  match (owner.retained_authority, dependency.retained_authority) with
  | Some parent, Some child -> (
      match dependency.retained_authority_receipt with
      | None -> false
      | Some authority_receipt ->
          let matching =
            List.filter
              (fun edge ->
                String.equal edge.Retained_interface_authority_private.dependency_unit
                  dependency.unit_name
                && edge.dependency_compiler_receipt = Some import_receipt
                && String.equal edge.dependency_interface_receipt child.cmi_receipt
                && edge.dependency_authority_receipt = Some authority_receipt)
              parent.dependencies
          in
          List.length matching = 1)
  | (Some _ | None), None | None, Some _ -> true

let exact_import ~owner ~(dependency : implementation)
    (import : import) =
  let unit_matches = String.equal import.unit_name dependency.unit_name in
  let receipt_class =
    Option.bind import.crc (interface_receipt_matches dependency)
  in
  let authority_identity_matches = retained_authority_identity_is_exact dependency in
  let edge_matches =
    match import.crc with
    | Some receipt -> parent_authority_edge_matches ~owner dependency receipt
    | None -> false
  in
  let accepted =
    unit_matches && Option.is_some receipt_class && authority_identity_matches
    && edge_matches
  in
  (if accepted then
     [%log.trace "accepted exact compiler import authority edge"
       ~stage:(Delator.Field.string "candidate-import-matching")
       ~route:(Delator.Field.string "compiler-import-graph")
       ~receipt_class:
         (Delator.Field.string
            (match receipt_class with
            | Some `Retained_interface -> "retained-interface"
            | Some `Authenticated_ordinary_view -> "authenticated-ordinary-view"
            | None -> "unmatched"))
       ~parent_has_authority:
         (Delator.Field.bool (Option.is_some owner.retained_authority))
       ~child_has_authority:
         (Delator.Field.bool (Option.is_some dependency.retained_authority))
       ~decision:(Delator.Field.string "accepted")]
   else
     [%log.debug "rejected compiler import authority edge"
       ~stage:(Delator.Field.string "candidate-import-matching")
       ~route:(Delator.Field.string "compiler-import-graph")
       ~unit_matches:(Delator.Field.bool unit_matches)
       ~receipt_matches:(Delator.Field.bool (Option.is_some receipt_class))
       ~authority_identity_matches:(Delator.Field.bool authority_identity_matches)
       ~authority_edge_matches:(Delator.Field.bool edge_matches)
       ~decision:(Delator.Field.string "rejected")
       ~reason_class:(Delator.Field.string "exact-authority-mismatch")]);
  accepted

let exact_imports owner dependency =
  Array.exists (exact_import ~owner ~dependency) owner.imports

let argument_after flag arguments =
  let rec loop = function
    | [] | [ _ ] -> []
    | candidate :: value :: rest ->
        if String.equal candidate flag then value :: loop rest
        else loop (value :: rest)
  in
  loop (Array.to_list arguments)

let standalone_ppx_mode command =
  match String.split_on_char ' ' command |> List.filter (( <> ) "") with
  | executable :: arguments ->
      let executable =
        String.trim executable
        |> String.map (fun character ->
               if Char.equal character '\'' || Char.equal character '"' then
                 ' '
               else character)
        |> String.trim
      in
      let basename = Filename.basename executable in
      if
        String.equal basename "vero_ppx.exe"
        || String.equal basename "verocaml-ppx"
      then
        match arguments with
        | [] -> Some `Ordinary
        | [ "--keep-ghost" ] -> Some `Retained
        | _ -> None
      else None
  | [] -> None

let authenticated_ppx_family implementation expected =
  let compiler_ppx = argument_after "-ppx" implementation.compiler_arguments in
  let expected_family =
    match expected with
    | `Ordinary -> "ordinary-v1"
    | `Retained -> "retained-v1"
  in
  let compiler_matches =
    match compiler_ppx with
    | [ command ] -> standalone_ppx_mode command = Some expected
    | [] | _ :: _ :: _ -> false
  in
  let _route =
    match implementation.implementation_family_issuers with
    | [ "standalone-v1" ] -> "standalone-v1"
    | [ "ppxlib-v1" ] -> "ppxlib-v1"
    | [] | [ _ ] | _ :: _ :: _ -> "unrecognized"
  in
  let authenticated =
    implementation.implementation_metadata_valid
    && implementation.implementation_family_markers = [ expected_family ]
    &&
    match implementation.implementation_family_issuers with
    | [ "standalone-v1" ] -> compiler_matches
    | [ "ppxlib-v1" ] ->
        compiler_ppx = [] && implementation.ppxlib_context
    | [] | [ _ ] | _ :: _ :: _ -> false
  in
  [%log.debug "evaluated official PPX artifact authentication"
    ~provider:(Delator.Field.string implementation.unit_name)
    ~route:(Delator.Field.string _route)
    ~family:(Delator.Field.string expected_family)
    ~stage:(Delator.Field.string "artifact-authentication")
    ~decision:
      (Delator.Field.string (if authenticated then "accepted" else "rejected"))
    ~receipt_count:
      (Delator.Field.int
         (List.length implementation.implementation_family_markers))
    ~ppx_context:(Delator.Field.bool implementation.ppxlib_context)
    ~compiler_ppx_count:(Delator.Field.int (List.length compiler_ppx))];
  authenticated

let retained_ppx_artifact implementation =
  authenticated_ppx_family implementation `Retained

let ordinary_ppx_artifact implementation =
  authenticated_ppx_family implementation `Ordinary

let retained_ppx_command command =
  match String.split_on_char ' ' command |> List.filter (( <> ) "") with
  | executable :: arguments ->
      let executable =
        String.trim executable
        |> String.map (fun character ->
               if Char.equal character '\'' || Char.equal character '"' then
                 ' '
               else character)
        |> String.trim
      in
      let basename = Filename.basename executable in
      (String.equal basename "vero_ppx.exe"
      || String.equal basename "verocaml-ppx")
      && List.exists (String.equal "--keep-ghost") arguments
  | [] -> false

let retained_preprocessing implementation =
  let ppx = argument_after "-ppx" implementation.compiler_arguments in
  (List.length ppx = 1 && retained_ppx_command (List.hd ppx))
  ||
  (implementation.implementation_metadata_valid
  && implementation.implementation_family_markers = [ "retained-v1" ]
  && Filename.check_suffix implementation.source_file ".pp.ml"
  && Array.exists (String.equal "-impl") implementation.compiler_arguments)

let advertises_external_type_specification implementation =
  let authenticated = ref 0 in
  let malformed = ref 0 in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      type_declaration =
        (fun self declaration ->
          (match
             External_type_specification_marker_private.classify
               declaration.Typedtree.typ_attributes
           with
          | External_type_specification_marker_private.Ordinary -> ()
          | Authenticated -> incr authenticated
          | Malformed -> incr malformed);
          default.type_declaration self declaration);
    }
  in
  iterator.structure iterator implementation.structure;
  let advertised = !authenticated > 0 || !malformed > 0 in
  [%log.debug "inspected external type specification advertisement"
    ~provider:(Delator.Field.string implementation.unit_name)
    ~stage:(Delator.Field.string "external-type-specification-discovery")
    ~route:(Delator.Field.string "retained-typedtree-marker")
    ~authenticated_marker_count:(Delator.Field.int !authenticated)
    ~malformed_marker_count:(Delator.Field.int !malformed)
    ~decision:
      (Delator.Field.string (if advertised then "advertised" else "absent"))];
  advertised
[@@delator.instrument] [@@delator.level debug]

let diagnostic_for_location classification ~source_file location =
  Diagnostic.make classification
    (Diagnostic.span_of_location ~fallback_file:source_file location)

let classify_annots ~source_file = function
  | Cmt_format.Implementation structure -> Ok structure
  | Interface signature ->
      Error
        (diagnostic_for_location (Unsupported_input Interface) ~source_file
           signature.Typedtree.sig_sloc)
  | Packed _ ->
      Error
        (Diagnostic.make (Unsupported_input Packed)
           (Diagnostic.file_span source_file))
  | Partial_implementation parts ->
      let location =
        if Array.length parts = 0 then Location.none
        else
          match parts.(0) with
          | Partial_structure structure -> (
              match structure.Typedtree.str_items with
              | item :: _ -> item.Typedtree.str_loc
              | [] -> Location.none)
          | Partial_structure_item item -> item.Typedtree.str_loc
          | Partial_expression expression -> expression.Typedtree.exp_loc
          | Partial_pattern (_, pattern) -> pattern.Typedtree.pat_loc
          | Partial_class_expr class_expression ->
              class_expression.Typedtree.cl_loc
          | Partial_signature signature -> signature.Typedtree.sig_sloc
          | Partial_signature_item item -> item.Typedtree.sig_loc
          | Partial_module_type module_type -> module_type.Typedtree.mty_loc
      in
      Error
        (diagnostic_for_location
           (Unsupported_input Partial_implementation)
           ~source_file location)
  | Partial_interface parts ->
      let location =
        if Array.length parts = 0 then Location.none
        else
          match parts.(0) with
          | Partial_structure structure -> (
              match structure.Typedtree.str_items with
              | item :: _ -> item.Typedtree.str_loc
              | [] -> Location.none)
          | Partial_structure_item item -> item.Typedtree.str_loc
          | Partial_expression expression -> expression.Typedtree.exp_loc
          | Partial_pattern (_, pattern) -> pattern.Typedtree.pat_loc
          | Partial_class_expr class_expression ->
              class_expression.Typedtree.cl_loc
          | Partial_signature signature -> signature.Typedtree.sig_sloc
          | Partial_signature_item item -> item.Typedtree.sig_loc
          | Partial_module_type module_type -> module_type.Typedtree.mty_loc
      in
      Error
        (diagnostic_for_location (Unsupported_input Partial_interface)
           ~source_file location)

type magic_classification =
  | Readable_abi
  | Malformed
  | Incompatible

let has_prefix string prefix =
  String.length string >= String.length prefix
  && String.sub string 0 (String.length prefix) = prefix

let classify_magic filename =
  let magic_length = String.length Config.cmt_magic_number in
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let bytes = really_input_string channel magic_length in
      if
        String.equal bytes Config.cmt_magic_number
        || String.equal bytes Config.cmi_magic_number
      then Readable_abi
      else if has_prefix bytes "Caml1999" then Incompatible
      else Malformed)

let artifact_digest filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length |> Digest.string |> Digest.to_hex)

let artifact_content_receipt filename =
  let length = (Unix.stat filename).Unix.st_size in
  Printf.sprintf "%d:%s" length (artifact_digest filename)

let digest_of_content_receipt receipt =
  match String.index_opt receipt ':' with
  | Some separator when separator + 1 < String.length receipt ->
      String.sub receipt (separator + 1)
        (String.length receipt - separator - 1)
  | None | Some _ -> raise (Failure "invalid compiler artifact content receipt")

let read_all filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let stable_file_snapshot filename decode =
  let before_stat = Unix.stat filename in
  let before = read_all filename in
  let snapshot = Filename.temp_file "verocaml-artifact-snapshot-" ".bin" in
  let value, snapshot_after, after, after_stat =
    Fun.protect
      ~finally:(fun () ->
        if Sys.file_exists snapshot then Sys.remove snapshot)
      (fun () ->
        let channel = open_out_bin snapshot in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () -> output_string channel before);
        let value = decode snapshot in
        let snapshot_after = read_all snapshot in
        let after = read_all filename in
        let after_stat = Unix.stat filename in
        (value, snapshot_after, after, after_stat))
  in
  if
    before <> snapshot_after
    || before <> after
    || before_stat.Unix.st_size <> String.length before
    || after_stat.Unix.st_size <> String.length after
  then (
      [%log.warn "rejected unstable compiler artifact snapshot"
      ~stage:(Delator.Field.string "compiler-artifact-stable-snapshot")
      ~route:(Delator.Field.string "stat-bytes-snapshot-decode-bytes-stat")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "artifact-changed-during-decode")];
    raise (Failure "compiler artifact changed while it was being decoded"))
  else (
    let receipt =
      Printf.sprintf "%d:%s" (String.length before)
        (Digest.to_hex (Digest.string before))
    in
    [%log.trace "accepted stable compiler artifact snapshot"
      ~stage:(Delator.Field.string "compiler-artifact-stable-snapshot")
      ~route:(Delator.Field.string "stat-bytes-snapshot-decode-bytes-stat")
      ~artifact_bytes:(Delator.Field.int (String.length before))
      ~decision:(Delator.Field.string "accepted")];
    (value, receipt))

let read_stable_cmi filename =
  stable_file_snapshot filename (fun snapshot ->
      let information = Cmi_format.read_cmi_lazy snapshot in
      ignore (Subst.Lazy.force_signature information.Cmi_format.cmi_sign);
      information)

let read_stable_typed_interface filename =
  let (embedded, info), receipt =
    stable_file_snapshot filename Cmt_format.read
  in
  match (embedded, info) with
  | _, Some info -> (
      match info.Cmt_format.cmt_annots with
      | Cmt_format.Interface signature -> (info, signature, receipt)
      | Implementation _ | Partial_implementation _ | Partial_interface _
      | Packed _ ->
          raise (Failure "compiler interface artifact is not a finalized CMTI"))
  | _, None -> raise (Failure "compiler interface artifact has no CMT metadata")

module For_testing = struct
  let stable_snapshot_uses_receipted_bytes () =
    let filename = Filename.temp_file "verocaml-snapshot-seam-" ".bin" in
    let write value =
      let channel = open_out_bin filename in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel value)
    in
    Fun.protect
      ~finally:(fun () ->
        if Sys.file_exists filename then Sys.remove filename)
      (fun () ->
        let receipted = "alpha" and substituted = "bravo" in
        write receipted;
        let decoded, receipt =
          stable_file_snapshot filename (fun snapshot ->
              write substituted;
              let decoded = read_all snapshot in
              write receipted;
              decoded)
        in
        String.equal decoded receipted
        && String.equal receipt
             (Printf.sprintf "%d:%s" (String.length receipted)
                (Digest.to_hex (Digest.string receipted))))
end

let stable_unique equal values =
  List.fold_left
    (fun unique value ->
      if List.exists (equal value) unique then unique else unique @ [ value ])
    [] values

let source_file filename info =
  match info.Cmt_format.cmt_sourcefile with
  | Some source_file -> source_file
  | None -> filename

let imports_of_array imports =
  Array.map
    (fun import ->
      {
        unit_name =
          Compilation_unit.Name.to_string (Import_info.name import);
        crc = Option.map Digest.to_hex (Import_info.crc import);
      })
    imports

let imports info = imports_of_array info.Cmt_format.cmt_imports

let import_receipt imports =
  imports |> Array.to_list
  |> List.map (fun (import : import) ->
         import.unit_name ^ "=" ^ Option.value ~default:"<missing>" import.crc)
  |> String.concat "|" |> Digest.string |> Digest.to_hex

exception Symbolic_artifact_failure of {
  provider : string;
  reason : string;
}

exception Numeric_claim_failure of {
  provider : string;
  reason : string;
  location : Location.t option;
}

exception Broadcast_artifact_failure of {
  provider : string;
  reason : string;
}

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
  if contains "missing" || contains "no exact" || contains "lacks" then
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

let family_prefix = "verocaml.internal.artifact_family."

let family_payload = function
  | Parsetree.PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( { pexp_desc = Pexp_constant (Pconst_string (text, _, None)); _ },
                [] );
          _;
        };
      ] ->
      Some text
  | PStr _ | PSig _ | PTyp _ | PPat _ -> None

let family_receipt attribute =
  if
    not
      (String.starts_with ~prefix:family_prefix
         attribute.Parsetree.attr_name.txt)
  then Ok None
  else
    let family =
      String.sub attribute.attr_name.txt (String.length family_prefix)
        (String.length attribute.attr_name.txt - String.length family_prefix)
    in
    let expected_family =
      String.equal family "retained-v1" || String.equal family "ordinary-v1"
    in
    let payload =
      Option.map (String.split_on_char '|')
        (family_payload attribute.attr_payload)
    in
    let _safe_family = if expected_family then family else "unrecognized" in
    let _safe_route =
      match payload with
      | Some [ "v1"; "issuer=verocaml.ppx"; "route=standalone-v1"; _ ] ->
          "standalone-v1"
      | Some [ "v1"; "issuer=verocaml.ppx"; "route=ppxlib-v1"; _ ] ->
          "ppxlib-v1"
      | Some _ | None -> "unrecognized"
    in
    let result =
      match payload with
      | Some
          [ "v1"; "issuer=verocaml.ppx"; route; payload_family ]
        when attribute.attr_loc.Location.loc_ghost
             && attribute.attr_name.loc.loc_ghost && expected_family
             && (String.equal route "route=standalone-v1"
                || String.equal route "route=ppxlib-v1")
             && String.equal payload_family ("family=" ^ family) ->
          Ok
            (Some
               ( family,
                 String.sub route (String.length "route=")
                   (String.length route - String.length "route=") ))
      | Some _ | None -> Error ()
    in
    [%log.trace "decoded official PPX artifact-family receipt"
      ~route:(Delator.Field.string _safe_route)
      ~family:(Delator.Field.string _safe_family)
      ~stage:(Delator.Field.string "artifact-family-receipt")
      ~decision:
        (Delator.Field.string
           (match result with Ok _ -> "accepted" | Error () -> "rejected"))];
    result

let authenticated_retained_family_attribute implementation attribute =
  match
    ( implementation.implementation_family_issuers,
      family_receipt attribute )
  with
  | [ implementation_issuer ], Ok (Some ("retained-v1", attribute_issuer)) ->
      String.equal implementation_issuer attribute_issuer
  | ( [], _ )
  | ( [ _ ], (Ok None | Error ()) )
  | ( [ _ ], Ok (Some _) )
  | ( _ :: _ :: _, _ ) ->
      false

let ppxlib_context_attribute attribute =
  let tool_name =
    match attribute.Parsetree.attr_payload with
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval ({ pexp_desc = Pexp_record (fields, None); _ }, []);
            _;
          };
        ] ->
        List.find_map
          (fun
            ( (name : Longident.t Location.loc),
              (expression : Parsetree.expression) ) ->
            match (name.txt, expression.pexp_desc) with
            | ( Longident.Lident "tool_name",
                Pexp_constant (Pconst_string (tool_name, _, None)) ) ->
                Some tool_name
            | _ -> None)
          fields
    | PStr _ | PSig _ | PTyp _ | PPat _ -> None
  in
  String.equal attribute.attr_name.txt "ocaml.ppx.context"
  && attribute.attr_loc.Location.loc_ghost
  && attribute.attr_name.loc.loc_ghost
  && tool_name = Some "ppx_driver"

type implementation_metadata = {
  implementation_family_markers : string list;
  implementation_family_issuers : string list;
  ppxlib_context : bool;
  verification_scope_markers : string list;
  implementation_metadata_valid : bool;
}

let implementation_metadata structure =
  let scope_prefix = "verocaml.internal.verification_scope." in
  let families = ref []
  and issuers = ref []
  and scopes = ref []
  and valid = ref true in
  let collect_family attribute =
    match family_receipt attribute with
    | Ok None -> ()
    | Ok (Some (family, issuer)) ->
        families := family :: !families;
        issuers := issuer :: !issuers
    | Error () -> valid := false
  in
  let collect_scope attribute =
    let prefix = scope_prefix in
    if String.starts_with ~prefix attribute.Parsetree.attr_name.txt then
      if
        attribute.attr_loc.Location.loc_ghost
        && attribute.attr_name.loc.loc_ghost
        && attribute.attr_payload = Parsetree.PStr []
      then
        scopes :=
          String.sub attribute.attr_name.txt (String.length prefix)
            (String.length attribute.attr_name.txt - String.length prefix)
          :: !scopes
      else valid := false
  in
  let ppx_contexts =
    List.filter_map
      (fun item ->
        match item.Typedtree.str_desc with
        | Tstr_attribute attribute
          when String.equal attribute.attr_name.txt "ocaml.ppx.context" ->
            Some attribute
        | _ -> None)
      structure.Typedtree.str_items
  in
  let ppxlib_context =
    match ppx_contexts with
    | [] -> false
    | [ attribute ] when ppxlib_context_attribute attribute -> true
    | [ _ ] | _ :: _ :: _ ->
        valid := false;
        false
  in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      attribute =
        (fun self attribute ->
          collect_family attribute;
          collect_scope attribute;
          default.attribute self attribute);
      value_binding =
        (fun self binding ->
          List.iter collect_family binding.vb_attributes;
          default.value_binding self binding);
      type_declaration =
        (fun self declaration ->
          List.iter collect_family declaration.typ_attributes;
          default.type_declaration self declaration);
    }
  in
  iterator.structure iterator structure;
  {
    implementation_family_markers =
      List.sort_uniq String.compare !families;
    implementation_family_issuers =
      List.sort_uniq String.compare !issuers;
    ppxlib_context;
    verification_scope_markers = List.rev !scopes;
    implementation_metadata_valid = !valid;
  }

let interface_family_receipts interface =
  let provider =
    Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
  in
  let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
  let add_attributes receipts attributes =
    List.fold_left
      (fun receipts attribute ->
        match (receipts, family_receipt attribute) with
        | Ok receipts, Ok None -> Ok receipts
        | Ok receipts, Ok (Some receipt) -> Ok (receipt :: receipts)
        | Error (), _ | _, Error () -> Error ())
      receipts attributes
  in
  let path_key path = Format.asprintf "%a" Path.print path in
  let rec module_signature scope seen module_type =
    match module_type with
    | Types.Mty_signature nested -> Some nested
    | Mty_strengthen (nested, _, _) -> module_signature scope seen nested
    | Mty_ident path -> modtype_path scope seen path
    | Mty_alias _ | Mty_functor _ -> None
  and modtype_path scope seen path =
    let key = "modtype:" ^ path_key path in
    if List.mem key seen then None
    else
      match path with
      | Path.Pident ident ->
          let resolve items =
            items
            |> List.filter_map (function
                 | Types.Sig_modtype (candidate, declaration, _)
                   when Ident.same candidate ident ->
                     Option.bind declaration.Types.mtd_type (fun nested ->
                         module_signature items (key :: seen) nested)
                 | _ -> None)
            |> function [ signature ] -> Some signature | [] | _ :: _ :: _ -> None
          in
          (match resolve scope with Some _ as nested -> nested | None -> resolve root)
      | Pdot (parent, name) ->
          Option.bind (module_path scope (key :: seen) parent) (fun nested ->
              nested
              |> List.filter_map (function
                   | Types.Sig_modtype (ident, declaration, _)
                     when String.equal (Ident.name ident) name ->
                       Option.bind declaration.Types.mtd_type (fun module_type ->
                           module_signature nested (key :: seen) module_type)
                   | _ -> None)
              |> function
              | [ signature ] -> Some signature
              | [] | _ :: _ :: _ -> None)
      | Papply _ | Pextra_ty _ -> None
  and module_path scope seen path =
    let key = "module:" ^ path_key path in
    if List.mem key seen then None
    else
      match path with
      | Path.Pident ident ->
          let resolve items =
            items
            |> List.filter_map (function
                 | Types.Sig_module (candidate, _, declaration, _, _)
                   when Ident.same candidate ident ->
                     module_signature items (key :: seen)
                       declaration.Types.md_type
                 | _ -> None)
            |> function [ signature ] -> Some signature | [] | _ :: _ :: _ -> None
          in
          (match resolve scope with Some _ as nested -> nested | None -> resolve root)
      | Pdot (parent, name) ->
          Option.bind (module_path scope (key :: seen) parent) (fun nested ->
              nested
              |> List.filter_map (function
                   | Types.Sig_module (ident, _, declaration, _, _)
                     when String.equal (Ident.name ident) name ->
                       module_signature nested (key :: seen)
                         declaration.Types.md_type
                   | _ -> None)
              |> function
              | [ signature ] -> Some signature
              | [] | _ :: _ :: _ -> None)
      | Papply _ | Pextra_ty _ -> None
  in
  let rec collect receipts items =
    List.fold_left
      (fun receipts item ->
        match (receipts, item) with
        | Error (), _ -> Error ()
        | Ok _ as receipts, Types.Sig_value (_, description, _) ->
            add_attributes receipts description.Types.val_attributes
        | Ok _ as receipts, Types.Sig_type (_, declaration, _, _) ->
            add_attributes receipts declaration.Types.type_attributes
        | Ok _ as receipts, Types.Sig_module (_, _, declaration, _, _) ->
            let receipts =
              add_attributes receipts declaration.Types.md_attributes
            in
            (match (receipts, declaration.Types.md_type) with
            | Error (), _ -> Error ()
            | Ok _ as receipts, Types.Mty_alias _ -> receipts
            | Ok _ as receipts, module_type -> (
                match module_signature items [] module_type with
                | Some nested -> collect receipts nested
                | None -> receipts))
        | Ok _ as receipts, Types.Sig_modtype (_, declaration, _) ->
            add_attributes receipts declaration.Types.mtd_attributes
        | Ok _ as receipts,
          (Types.Sig_typext _ | Types.Sig_class _ | Types.Sig_class_type _) ->
            receipts)
      receipts items
  in
  collect (Ok []) root
  |> function
  | Ok receipts ->
      let receipts = List.sort_uniq compare receipts in
      [%log.trace "collected recursive compiler artifact-family receipts"
        ~provider:(Delator.Field.string provider)
        ~stage:(Delator.Field.string "artifact-family-receipt")
        ~route:(Delator.Field.string "recursive-cmi-signature")
        ~receipt_count:(Delator.Field.int (List.length receipts))
        ~decision:(Delator.Field.string "accepted")];
      receipts
  | Error () ->
      [%log.debug "rejected recursive compiler artifact-family receipt"
        ~provider:(Delator.Field.string provider)
        ~stage:(Delator.Field.string "artifact-family-receipt")
        ~route:(Delator.Field.string "recursive-cmi-signature")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "malformed-nested-receipt")];
      raise
        (Symbolic_artifact_failure
           { provider; reason = "has a malformed artifact-family receipt" })

let interface_family_markers interface =
  interface_family_receipts interface |> List.map fst
  |> List.sort_uniq String.compare

let interface_family_issuers interface =
  interface_family_receipts interface |> List.map snd
  |> List.sort_uniq String.compare

let string_payload_attribute name attributes =
  List.find_map
    (fun attribute ->
      if
        attribute.Parsetree.attr_loc.Location.loc_ghost
        && attribute.attr_name.loc.loc_ghost
        && String.equal attribute.attr_name.txt name
      then
        match attribute.attr_payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc = Pexp_constant (Pconst_string (value, _, _));
                        _;
                      },
                      [] );
                _;
              };
            ] ->
            Some value
        | _ -> None
      else None)
    attributes

let mode_signature =
  string_payload_attribute "verocaml.internal.mode_signature"

let finite_signature attributes =
  let matching =
    List.filter
      (fun attribute ->
        String.equal attribute.Parsetree.attr_name.txt
          "verocaml.internal.finite_signature")
      attributes
  in
  match matching with
  | [] -> None
  | _ :: _ :: _ -> raise (Failure "duplicate finite signature metadata")
  | [ attribute ] ->
      if
        not
          (attribute.attr_loc.Location.loc_ghost
          && attribute.attr_name.loc.loc_ghost)
      then raise (Failure "non-ghost finite signature metadata")
      else
        let payload =
          match attribute.attr_payload with
          | PStr
              [
                {
                  pstr_desc =
                    Pstr_eval
                      ( {
                          pexp_desc =
                            Pexp_constant (Pconst_string (value, _, _));
                          _;
                        },
                        [] );
                  _;
                };
              ] ->
              value
          | _ -> raise (Failure "malformed finite signature metadata")
        in
        if not (String.starts_with ~prefix:"v1|" payload) then
          raise (Failure "unknown finite signature metadata version")
        else
          let entries =
            String.sub payload 3 (String.length payload - 3)
            |> fun body ->
            if body = "" then [] else String.split_on_char ';' body
          in
          List.iteri
            (fun expected entry ->
              match String.split_on_char '=' entry with
              | [ key; ("finite" | "default") ] -> (
                  match String.index_opt key ':' with
                  | Some separator ->
                      let ordinal =
                        String.sub key 0 separator |> int_of_string_opt
                      in
                      let label =
                        String.sub key (separator + 1)
                          (String.length key - separator - 1)
                      in
                      if ordinal <> Some expected || label = "" then
                        raise (Failure "malformed finite signature vector")
                  | None ->
                      raise (Failure "malformed finite signature vector"))
              | _ -> raise (Failure "malformed finite signature vector"))
            entries;
          Some payload

let interface_signatures attribute interface =
  let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
  let key kind prefix ident =
    String.concat "." (List.rev (Ident.name ident :: prefix)) |> ( ^ ) kind
  in
  let rec signature prefix items =
    let module_types =
      List.filter_map
        (function
          | Types.Sig_modtype (ident, declaration, _) ->
              Option.map (fun typ -> (ident, typ)) declaration.Types.mtd_type
          | _ -> None)
        items
    in
    let rec module_signature seen = function
      | Types.Mty_signature nested -> Some nested
      | Mty_strengthen (nested, _, _) -> module_signature seen nested
      | Mty_ident (Path.Pident ident) -> (
          if List.exists (Ident.same ident) seen then None
          else
            match
              List.find_opt
                (fun (candidate, _) -> Ident.same candidate ident)
                module_types
            with
            | Some (_, nested) -> module_signature (ident :: seen) nested
            | None -> None)
      | Mty_ident _ | Mty_functor _ | Mty_alias _ -> None
    in
    List.concat_map
      (function
        | Types.Sig_value (ident, description, _) ->
            [
              ( key "value:" prefix ident,
                attribute description.Types.val_attributes );
            ]
        | Types.Sig_type (ident, declaration, _, _) ->
            [
              ( key "type:" prefix ident,
                attribute declaration.Types.type_attributes );
            ]
        | Types.Sig_module (ident, _, declaration, _, _) -> (
            match module_signature [] declaration.Types.md_type with
            | Some nested -> signature (Ident.name ident :: prefix) nested
            | None -> [])
        | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
        | Types.Sig_class_type _ ->
            [])
      items
  in
  signature [] root
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let interface_mode_signatures = interface_signatures mode_signature
let interface_finite_signatures interface =
  let signatures = interface_signatures finite_signature interface in
  let rec duplicates = function
    | (left, _) :: ((right, _) :: _ as rest) ->
        if String.equal left right then
          raise (Failure "duplicate finite signature path")
        else duplicates rest
    | [] | [ _ ] -> ()
  in
  duplicates signatures;
  signatures

let broadcast_decode_rejection ~stage:_stage ~reason_class:_reason_class =
  [%log.debug "rejected retained broadcast metadata"
    ~route:(Delator.Field.string "compiler-artifact")
    ~stage:(Delator.Field.string _stage)
    ~decision:(Delator.Field.string "rejected")
    ~reason_class:(Delator.Field.string _reason_class)]

let strict_broadcast_attribute (name [@delator.skip])
    (attributes [@delator.skip]) =
  let matching =
    List.filter
      (fun attribute -> String.equal attribute.Parsetree.attr_name.txt name)
      attributes
  in
  match matching with
  | [] -> None
  | _ :: _ :: _ ->
      broadcast_decode_rejection ~stage:"metadata-marker"
        ~reason_class:"duplicate-marker";
      raise (Failure "duplicate retained broadcast metadata")
  | [ attribute ] ->
      if
        not
          (attribute.attr_loc.Location.loc_ghost
          && attribute.attr_name.loc.loc_ghost)
      then (
        broadcast_decode_rejection ~stage:"metadata-marker"
          ~reason_class:"non-ghost-marker";
        raise (Failure "retained broadcast metadata is not ghost-issued"))
      else
        match attribute.attr_payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc =
                          Pexp_constant (Pconst_string (value, _, None));
                        _;
                      },
                      [] );
                _;
              };
            ] ->
            Some value
          | PStr _ | PSig _ | PTyp _ | PPat _ ->
            broadcast_decode_rejection ~stage:"metadata-marker"
              ~reason_class:"payload-shape";
            raise (Failure "retained broadcast metadata is malformed")
[@@delator.instrument]
[@@delator.level trace]
[@@delator.no_exn_log]

let require_broadcast_family_receipt ~provider:_provider ~member_kind:_member_kind
    attributes =
  let matching =
    List.filter
      (fun attribute ->
        String.starts_with ~prefix:family_prefix
          attribute.Parsetree.attr_name.txt)
      attributes
  in
  let receipt =
    match matching with
    | [ attribute ] -> (
        match family_receipt attribute with
        | Ok (Some ("retained-v1", issuer)) -> Some issuer
        | Ok None | Ok (Some _) | Error () -> None)
    | [] | _ :: _ :: _ -> None
  in
  [%log.trace "authenticated retained broadcast member family receipt"
    ~provider:(Delator.Field.string _provider)
    ~stage:(Delator.Field.string "broadcast-family-receipt")
    ~route:(Delator.Field.string "recursive-cmi-signature")
    ~member_kind:(Delator.Field.string _member_kind)
    ~receipt_count:(Delator.Field.int (List.length matching))
    ~decision:
      (Delator.Field.string
         (if Option.is_some receipt then "accepted" else "rejected"))];
  match receipt with
  | Some issuer -> issuer
  | None ->
      broadcast_decode_rejection ~stage:"broadcast-family-receipt"
        ~reason_class:"missing-or-ambiguous-retained-family";
      raise
        (Failure
           "retained broadcast declaration has no exact artifact-family receipt")

let decode_broadcast_frames (value [@delator.skip]) =
  let total = String.length value in
  let rec colon index =
    if index >= total then (
      broadcast_decode_rejection ~stage:"metadata-frame"
        ~reason_class:"truncated-frame";
      raise (Failure "truncated retained broadcast frame"))
    else
      match value.[index] with
      | ':' -> index
      | '0' .. '9' -> colon (index + 1)
      | _ ->
          broadcast_decode_rejection ~stage:"metadata-frame"
            ~reason_class:"length-prefix";
          raise (Failure "malformed retained broadcast frame")
  in
  let rec loop index values =
    if index = total then List.rev values
    else
      let separator = colon index in
      let field_length =
        String.sub value index (separator - index) |> int_of_string
      in
      let start = separator + 1 in
      if field_length < 0 || start + field_length > total then
        (broadcast_decode_rejection ~stage:"metadata-frame"
           ~reason_class:"frame-bounds";
        raise (Failure "invalid retained broadcast frame length")
        )
      else
        loop (start + field_length)
          (String.sub value start field_length :: values)
  in
  let frames = loop 0 [] in
  [%log.trace "decoded retained broadcast metadata frame"
    ~route:(Delator.Field.string "compiler-artifact")
    ~stage:(Delator.Field.string "metadata-frame")
    ~field_count:(Delator.Field.int (List.length frames))
    ~decision:(Delator.Field.string "accepted")];
  frames
[@@delator.instrument]
[@@delator.level trace]
[@@delator.no_exn_log]
let interface_self_crc interface =
  Array.to_list interface.Cmi_format.cmi_crcs
  |> List.filter (fun imported ->
         Compilation_unit.Name.equal
           (Import_info.name imported)
           interface.Cmi_format.cmi_name)
  |> function
  | [ imported ] -> Option.map Digest.to_hex (Import_info.crc imported)
  | [] | _ :: _ :: _ -> None

type authenticated_typed_interface = {
  typed_metadata : Cmt_format.cmt_infos;
  typed_signature : Typedtree.signature;
  typed_content_receipt : string;
}

let interface_import_identity interface =
  imports_of_array interface.Cmi_format.cmi_crcs |> Array.to_list
  |> List.map (fun (imported : import) -> (imported.unit_name, imported.crc))
  |> List.sort compare

let typed_import_identity information =
  imports information |> Array.to_list
  |> List.map (fun (imported : import) -> (imported.unit_name, imported.crc))
  |> List.sort compare

let authenticate_typed_interface ~unit_name ~interface filename =
  let result =
    if not (Sys.file_exists filename) then
      Error "typed interface artifact is missing"
    else
      try
        let information, signature, typed_content_receipt =
          read_stable_typed_interface filename
        in
        let unit_matches =
          String.equal
            (Compilation_unit.name_as_string information.Cmt_format.cmt_modname)
            unit_name
        and digest_matches =
          Option.map Digest.to_hex information.Cmt_format.cmt_interface_digest
          = interface_self_crc interface
        and imports_match =
          typed_import_identity information = interface_import_identity interface
        in
        if unit_matches && digest_matches && imports_match then
          Ok { typed_metadata = information; typed_signature = signature;
               typed_content_receipt }
        else Error "typed interface does not match the exact compiler interface"
      with
      | Failure reason | Sys_error reason -> Error reason
      | Cmi_format.Error _ | Cmt_format.Error _ | End_of_file ->
          Error "typed interface artifact is malformed"
  in
  (match result with
  | Ok _ ->
      [%log.trace "authenticated typed interface artifact correlation"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "typed-interface-artifact-correlation")
        ~route:(Delator.Field.string "stable-cmi-cmti")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.warn]) ->
      [%log.warn "rejected typed interface artifact correlation"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "typed-interface-artifact-correlation")
        ~route:(Delator.Field.string "stable-cmi-cmti")
        ~reason_class:(Delator.Field.string (reason [@log_value.warn]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let discover_authenticated_typed_interface ?required_receipt ~unit_name
    ~interface ~adjacent ~load_paths () =
  let basenames =
    [ unit_name ^ ".cmti"; String.uncapitalize_ascii unit_name ^ ".cmti" ]
    |> stable_unique String.equal
  in
  let candidates =
    adjacent
    @ List.concat_map
        (fun directory ->
          List.map (fun basename -> Filename.concat directory basename) basenames)
        load_paths
    |> stable_unique String.equal |> List.filter Sys.file_exists
  in
  let matches =
    candidates
    |> List.filter_map (fun filename ->
           match authenticate_typed_interface ~unit_name ~interface filename with
           | Ok artifact
             when Option.fold ~none:true
                    ~some:(String.equal artifact.typed_content_receipt)
                    required_receipt ->
               Some (filename, artifact)
           | Ok _ | Error _ -> None)
  in
  let result =
    match matches with
    | [] -> Error "authenticated typed interface artifact is missing or stale"
    | (filename, artifact) :: rest ->
        if
          List.for_all
            (fun (_, candidate) ->
              String.equal candidate.typed_content_receipt
                artifact.typed_content_receipt)
            rest
        then Ok (filename, artifact)
        else Error "authenticated typed interface candidates conflict"
  in
  (match result with
  | Ok _ ->
      [%log.trace "coalesced authenticated typed interface discovery"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "typed-interface-discovery")
        ~candidate_count:(Delator.Field.int (List.length candidates))
        ~matching_count:(Delator.Field.int (List.length matches))
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.warn]) ->
      [%log.warn "rejected authenticated typed interface discovery"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "typed-interface-discovery")
        ~candidate_count:(Delator.Field.int (List.length candidates))
        ~matching_count:(Delator.Field.int (List.length matches))
        ~reason_class:(Delator.Field.string (reason [@log_value.warn]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

type authenticated_interface_artifact = {
  artifact_filename : string;
  artifact_interface : Cmi_format.cmi_infos_lazy;
  artifact_content_digest : string;
  artifact_self_crc : string;
}

let compiler_normalization_lock = Mutex.create ()

let find_imported_interface_artifact
    ?(admit_candidate = fun _filename -> Ok ())
    ~load_paths:(load_paths [@delator.skip])
    ~unit_name:(unit_name [@delator.field Fun.id])
    ~expected_crc:(expected_crc [@delator.skip]) () =
  let basenames =
    [ unit_name ^ ".cmi"; String.uncapitalize_ascii unit_name ^ ".cmi" ]
    |> stable_unique String.equal
  in
  let candidates =
    List.concat_map
      (fun directory ->
        List.map (fun basename -> Filename.concat directory basename) basenames)
      load_paths
    |> stable_unique String.equal
    |> List.filter Sys.file_exists
  in
  let matches =
    List.filter_map
      (fun filename ->
        try
          (match admit_candidate filename with
          | Ok () -> ()
          | Error reason -> raise (Failure reason));
          let interface, content_receipt = read_stable_cmi filename in
          let unit_matches =
            Compilation_unit.Name.equal interface.cmi_name
              (Compilation_unit.Name.of_string unit_name)
          and self_crc_matches =
            interface_self_crc interface = Some expected_crc
          in
          [%log.trace "inspected retained broadcast dependency CMI candidate"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "imported-cmi-receipt")
            ~route:(Delator.Field.string "interface-load-path")
            ~candidate_count:(Delator.Field.int (List.length candidates))
            ~unit_matches:(Delator.Field.bool unit_matches)
            ~self_crc_matches:(Delator.Field.bool self_crc_matches)
            ~decision:
              (Delator.Field.string
                 (if unit_matches && self_crc_matches then "accepted"
                  else "rejected"))];
          if unit_matches && self_crc_matches then
            Some
              {
                artifact_filename = filename;
                artifact_interface = interface;
                artifact_content_digest = content_receipt;
                artifact_self_crc = expected_crc;
              }
          else None
        with
        | Failure reason -> raise (Failure reason)
        | Sys_error _ ->
            [%log.warn "retained broadcast dependency artifact read issue"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "imported-cmi-receipt")
              ~route:(Delator.Field.string "interface-load-path")
              ~candidate_count:(Delator.Field.int (List.length candidates))
              ~unit_matches:(Delator.Field.bool false)
              ~self_crc_matches:(Delator.Field.bool false)
              ~decision:(Delator.Field.string "unavailable")
              ~reason_class:(Delator.Field.string "artifact-io")];
            None
        | Cmi_format.Error _ | End_of_file ->
            [%log.warn "retained broadcast dependency artifact decode issue"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "imported-cmi-receipt")
              ~route:(Delator.Field.string "interface-load-path")
              ~candidate_count:(Delator.Field.int (List.length candidates))
              ~unit_matches:(Delator.Field.bool false)
              ~self_crc_matches:(Delator.Field.bool false)
              ~decision:(Delator.Field.string "unavailable")
              ~reason_class:(Delator.Field.string "artifact-decode")];
            None)
      candidates
  in
  let matches =
    stable_unique
      (fun left right ->
        String.equal left.artifact_filename right.artifact_filename)
      matches
  in
  let found =
    match matches with
    | [] -> None
    | first :: rest ->
        if
          List.for_all
            (fun candidate ->
              String.equal candidate.artifact_content_digest
                first.artifact_content_digest)
            rest
        then Some first
        else (
          [%log.debug "rejected ambiguous retained broadcast dependency artifacts"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "imported-cmi-receipt")
            ~route:(Delator.Field.string "interface-load-path")
            ~candidate_count:(Delator.Field.int (List.length matches))
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:
              (Delator.Field.string "conflicting-complete-receipts")];
          raise
            (Failure
               "retained broadcast dependency has conflicting complete receipts"))
  in
  [%log.trace "correlated retained broadcast imported CMI receipt"
    ~provider:(Delator.Field.string unit_name)
    ~stage:(Delator.Field.string "imported-cmi-receipt")
    ~candidate_count:(Delator.Field.int (List.length candidates))
    ~decision:
      (Delator.Field.string
         (if Option.is_some found then "accepted" else "rejected"))
    ~reason_class:
      (Delator.Field.string
         (if Option.is_some found then "exact-receipt" else "missing-or-stale"))];
  found
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let find_imported_interface ~load_paths ~unit_name ~expected_crc =
  Option.map
    (fun artifact -> artifact.artifact_interface)
    (find_imported_interface_artifact ~load_paths ~unit_name ~expected_crc ())

let with_authenticated_interfaces artifacts action =
  Mutex.lock compiler_normalization_lock;
  let loader = Persistent_env.Persistent_signature.load in
  let previous = !loader in
  let exact_loader ~allow_hidden:_ ~unit_name =
    let matches =
      List.filter
        (fun artifact ->
          Compilation_unit.Name.equal artifact.artifact_interface.cmi_name
            unit_name)
        artifacts
    in
    match matches with
    | [ artifact ] ->
        Some
          {
            Persistent_env.Persistent_signature.filename =
              artifact.artifact_filename;
            cmi = artifact.artifact_interface;
            visibility = Load_path.Visible;
          }
    | [] | _ :: _ :: _ -> None
  in
  Fun.protect
    ~finally:(fun () ->
      Envaux.reset_cache ~preserve_persistent_env:false;
      loader := previous;
      Mutex.unlock compiler_normalization_lock;
      [%log.trace "restored compiler loader and cleared summary environments"
        ~stage:(Delator.Field.string "compiler-artifact-environment")
        ~decision:(Delator.Field.string "restored")])
    (fun () ->
      Envaux.reset_cache ~preserve_persistent_env:false;
      loader := exact_loader;
      [%log.trace "installed exact compiler loader with fresh summary environments"
        ~stage:(Delator.Field.string "compiler-artifact-environment")
        ~artifact_count:(Delator.Field.int (List.length artifacts))
        ~decision:(Delator.Field.string "installed")];
      action ())

let authenticated_import_interfaces ?admit_candidate ~load_paths interface =
  Array.to_list interface.Cmi_format.cmi_crcs
  |> List.filter_map (fun imported ->
         let name = Import_info.name imported in
         if Compilation_unit.Name.equal name interface.Cmi_format.cmi_name then
           None
         else
           Option.bind (Import_info.crc imported) (fun crc ->
               find_imported_interface_artifact ?admit_candidate ~load_paths
                 ~unit_name:(Compilation_unit.Name.to_string name)
                 ~expected_crc:(Digest.to_hex crc) ()))

let interface_broadcast_syntax ~load_paths:(load_paths [@delator.skip])
    (interface [@delator.skip]) =
      let declaration_name =
        "verocaml.internal.broadcast.interface_declaration.v1"
      and group_name = "verocaml.internal.broadcast.interface_group.v1" in
      let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid in
      let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
      let imported_interfaces = authenticated_import_interfaces ~load_paths interface in
      let imported_signatures =
        ( Compilation_unit.Name.to_string interface.Cmi_format.cmi_name,
          root )
        :: List.map
          (fun artifact ->
            ( Compilation_unit.Name.to_string
                artifact.artifact_interface.Cmi_format.cmi_name,
              Subst.Lazy.force_signature artifact.artifact_interface.cmi_sign ))
          imported_interfaces
      in
      let path_key path = Format.asprintf "%a" Path.print path in
      let unique_signature _reason signatures =
        match signatures with
        | [] -> None
        | [ signature ] -> Some signature
        | _ :: _ :: _ ->
            [%log.warn "rejected ambiguous retained module metadata route"
              ~provider:
                (Delator.Field.string
                   (Compilation_unit.Name.to_string interface.cmi_name))
              ~stage:(Delator.Field.string "module-metadata-routing")
              ~route:(Delator.Field.string "compiler-signature")
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string _reason)];
            raise (Failure "retained module metadata route is ambiguous")
      in
      let rec module_signature items seen = function
        | Types.Mty_signature nested -> Some nested
        | Mty_strengthen (nested, _, _) -> module_signature items seen nested
        | Mty_ident path -> modtype_path items seen path
        | Mty_alias path -> module_path items seen path
        | Mty_functor _ -> None
      and modtype_path items seen path =
        let key = "modtype:" ^ path_key path in
        if List.mem key seen then (
          [%log.warn "rejected retained module-type metadata cycle"
            ~provider:
              (Delator.Field.string
                 (Compilation_unit.Name.to_string interface.cmi_name))
            ~stage:(Delator.Field.string "module-metadata-routing")
            ~route:(Delator.Field.string "compiler-signature")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "module-type-cycle")];
          raise (Failure "retained module-type metadata cycle"))
        else
          match path with
          | Path.Pident ident ->
              let resolve scope =
                scope
                |> List.filter_map (function
                     | Types.Sig_modtype (candidate, declaration, _)
                       when Ident.same candidate ident ->
                         Option.bind declaration.Types.mtd_type (fun nested ->
                             module_signature scope (key :: seen) nested)
                     | _ -> None)
                |> unique_signature "module-type-collision"
              in
              (match resolve items with
              | Some _ as signature -> signature
              | None -> resolve root)
          | Pdot (parent, name) ->
              Option.bind (module_path items (key :: seen) parent) (fun nested ->
                  nested
                  |> List.filter_map (function
                       | Types.Sig_modtype (ident, declaration, _)
                         when String.equal (Ident.name ident) name ->
                           Option.bind declaration.Types.mtd_type (fun module_type ->
                               module_signature nested (key :: seen) module_type)
                       | _ -> None)
                  |> unique_signature "qualified-module-type-collision")
          | Papply _ | Pextra_ty _ -> None
      and module_path items seen path =
        let key = "module:" ^ path_key path in
        if List.mem key seen then (
          [%log.warn "rejected retained module-alias metadata cycle"
            ~provider:
              (Delator.Field.string
                 (Compilation_unit.Name.to_string interface.cmi_name))
            ~stage:(Delator.Field.string "module-metadata-routing")
            ~route:(Delator.Field.string "compiler-signature")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "module-alias-cycle")];
          raise (Failure "retained module alias metadata cycle"))
        else
          match path with
          | Path.Pident ident ->
              let local =
                items
                |> List.filter_map (function
                     | Types.Sig_module (candidate, _, declaration, _, _)
                       when Ident.same candidate ident ->
                         module_signature items (key :: seen)
                           declaration.Types.md_type
                     | _ -> None)
              in
              let persistent =
                match List.assoc_opt (Ident.name ident) imported_signatures with
                | Some signature -> [ signature ]
                | None -> []
              in
              unique_signature "module-alias-collision" (local @ persistent)
          | Pdot (parent, name) ->
              Option.bind (module_path items (key :: seen) parent) (fun nested ->
                  nested
                  |> List.filter_map (function
                       | Types.Sig_module (ident, _, declaration, _, _)
                         when String.equal (Ident.name ident) name ->
                           module_signature nested (key :: seen)
                             declaration.Types.md_type
                       | _ -> None)
                  |> unique_signature "qualified-module-collision")
          | Papply _ | Pextra_ty _ -> None
      in
      let rec module_type_contains_retained items seen = function
        | Types.Mty_functor (parameter, result) ->
            let parameter_contains_retained =
              match parameter with
              | Types.Unit -> false
              | Types.Named (_, argument) ->
                  module_type_contains_retained items seen argument
            in
            parameter_contains_retained
            || module_type_contains_retained items seen result
        | module_type ->
            Option.fold ~none:false ~some:signature_contains_retained
              (module_signature items seen module_type)
      and signature_contains_retained signature =
        List.exists
          (function
            | Types.Sig_value (_, description, _) ->
                Option.is_some
                  (strict_broadcast_attribute declaration_name
                     description.Types.val_attributes)
                || Option.is_some
                     (strict_broadcast_attribute group_name
                        description.Types.val_attributes)
            | Types.Sig_module (_, _, declaration, _, _) ->
                module_type_contains_retained signature []
                  declaration.Types.md_type
            | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
            | Types.Sig_class _ | Types.Sig_class_type _ ->
                false)
          signature
      in
      let reject_retained_functor items module_type =
        let retained = module_type_contains_retained items [] module_type in
        [%log.debug "inspected retained functor authority boundary"
          ~provider:
            (Delator.Field.string
               (Compilation_unit.Name.to_string interface.cmi_name))
          ~stage:(Delator.Field.string "module-metadata-routing")
          ~route:(Delator.Field.string "compiler-signature")
          ~decision:
            (Delator.Field.string (if retained then "rejected" else "ignored"))
          ~reason_class:
            (Delator.Field.string
               (if retained then "functor-boundary"
                else "ordinary-functor"))];
        if retained then
          raise
            (Failure
               "retained broadcast declarations under functors are unsupported")
      in
      let rec retained_uids signature =
        List.concat_map
          (function
            | Types.Sig_value (_, description, _) ->
                if
                  Option.is_some
                    (strict_broadcast_attribute declaration_name
                       description.Types.val_attributes)
                  || Option.is_some
                       (strict_broadcast_attribute group_name
                          description.Types.val_attributes)
                then [ compiler_uid description.Types.val_uid ]
                else []
            | Types.Sig_module (_, _, declaration, _, _) -> (
                match declaration.Types.md_type with
                | Types.Mty_alias _ -> []
                | Types.Mty_functor _ as module_type ->
                    reject_retained_functor signature module_type;
                    []
                | Mty_ident _ | Mty_signature _
                | Mty_strengthen _ -> (
                    match
                      module_signature signature [] declaration.Types.md_type
                    with
                    | Some nested -> retained_uids nested
                    | None -> []))
            | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
            | Types.Sig_class _ | Types.Sig_class_type _ -> [])
          signature
      in
      let imported_uids =
        Array.fold_left
          (fun imported_uids imported ->
            let name = Import_info.name imported in
            if Compilation_unit.Name.equal name interface.Cmi_format.cmi_name then
              imported_uids
            else
              match Import_info.crc imported with
              | None -> imported_uids
              | Some crc ->
                  let unit_name = Compilation_unit.Name.to_string name in
                  (match
                     find_imported_interface ~load_paths ~unit_name
                       ~expected_crc:(Digest.to_hex crc)
                   with
                  | Some dependency ->
                      List.rev_append
                        (retained_uids
                           (Subst.Lazy.force_signature dependency.cmi_sign))
                        imported_uids
                  | None ->
                      [%log.debug "rejected retained broadcast dependency receipt"
                        ~provider:(Delator.Field.string unit_name)
                        ~stage:(Delator.Field.string "artifact-decode")
                        ~decision:(Delator.Field.string "rejected")
                        ~reason_class:
                          (Delator.Field.string "missing-or-stale-dependency")];
                      raise
                        (Failure
                           ("retained broadcast dependency is missing or stale: "
                          ^ unit_name))))
          [] interface.Cmi_format.cmi_crcs
      in
      let rec collect prefix items =
        List.concat_map
          (function
            | Types.Sig_value (ident, description, _) ->
                let declaration =
                  strict_broadcast_attribute declaration_name
                    description.Types.val_attributes
                and group =
                  strict_broadcast_attribute group_name
                    description.Types.val_attributes
                in
                let broadcast_path =
                  String.concat "." (List.rev (Ident.name ident :: prefix))
                and broadcast_uid = compiler_uid description.Types.val_uid in
                if List.mem broadcast_uid imported_uids then (
                  [%log.trace "ignored flattened retained broadcast reexport"
                    ~provider:
                      (Delator.Field.string
                         (Compilation_unit.Name.to_string interface.cmi_name))
                    ~stage:(Delator.Field.string "interface-scope")
                    ~decision:(Delator.Field.string "preserve-origin")];
                  [])
                else (
                  let member_kind =
                    match (declaration, group) with
                    | Some _, None -> "declaration"
                    | None, Some _ -> "group"
                    | None, None -> "ordinary"
                    | Some _, Some _ -> "conflict"
                  in
                  if declaration <> None || group <> None then
                    ignore
                      (require_broadcast_family_receipt
                         ~provider:
                           (Compilation_unit.Name.to_string interface.cmi_name)
                         ~member_kind description.Types.val_attributes);
                  (match (declaration, group) with
                  | None, None -> []
                  | Some "v1", None ->
                      [
                        {
                          broadcast_path;
                          broadcast_uid;
                          broadcast_kind = Retained_broadcast_private.Declaration;
                          broadcast_member_count = 0;
                          broadcast_member_paths = [];
                        };
                      ]
                  | None, Some payload
                    when String.starts_with ~prefix:"v4|" payload ->
                      let receipt_name, member_paths =
                        match
                          String.sub payload 3 (String.length payload - 3)
                          |> decode_broadcast_frames
                        with
                        | receipt_name :: (_ :: _ as member_paths) ->
                            (receipt_name, member_paths)
                        | [] | [ _ ] ->
                            raise
                              (Failure
                                 "retained broadcast group receipt is incomplete")
                      in
                      if
                        not
                          (String.equal receipt_name (Ident.name ident)
                          && member_paths = List.sort String.compare member_paths)
                      then
                        raise
                          (Failure
                             "retained broadcast group receipt is noncanonical");
                      let count = List.length member_paths in
                      if count <= 0 then (
                        [%log.debug "rejected empty retained broadcast group"
                          ~provider:
                            (Delator.Field.string
                               (Compilation_unit.Name.to_string interface.cmi_name))
                          ~stage:(Delator.Field.string "artifact-decode")
                          ~member_kind:(Delator.Field.string "group")
                          ~decision:(Delator.Field.string "rejected")
                          ~reason_class:(Delator.Field.string "empty-group")];
                        raise (Failure "retained broadcast group is empty"))
                      else
                        [
                          {
                            broadcast_path;
                            broadcast_uid;
                            broadcast_kind = Retained_broadcast_private.Group;
                            broadcast_member_count = count;
                            broadcast_member_paths = member_paths;
                          };
                        ]
                  | Some _, None ->
                      raise
                        (Failure "unknown retained broadcast declaration version")
                  | None, Some _ ->
                      raise (Failure "unknown retained broadcast group version")
                  | Some _, Some _ ->
                      raise
                        (Failure "retained broadcast value has conflicting kinds")))
            | Types.Sig_module (ident, _, declaration, _, _) -> (
                match declaration.Types.md_type with
                | Types.Mty_alias _ -> []
                | Types.Mty_functor _ as module_type ->
                    reject_retained_functor items module_type;
                    []
                | _ ->
                match module_signature items [] declaration.Types.md_type with
                | Some nested -> collect (Ident.name ident :: prefix) nested
                | None -> [])
            | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
            | Types.Sig_class _ | Types.Sig_class_type _ -> [])
          items
      in
      let rec value_paths prefix items =
        List.concat_map
          (function
            | Types.Sig_value (ident, _, _) ->
                [ String.concat "." (List.rev (Ident.name ident :: prefix)) ]
            | Types.Sig_module (ident, _, declaration, _, _) -> (
                match declaration.Types.md_type with
                | Types.Mty_alias _ -> []
                | _ ->
                match module_signature items [] declaration.Types.md_type with
                | Some nested -> value_paths (Ident.name ident :: prefix) nested
                | None -> [])
            | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
            | Types.Sig_class _ | Types.Sig_class_type _ -> [])
          items
      in
      let values = collect [] root in
      let paths = List.map (fun value -> value.broadcast_path) values in
      let complete_namespace = value_paths [] root in
      if
        List.length paths <> List.length (List.sort_uniq String.compare paths)
        || List.exists
             (fun broadcast_path ->
               List.length
                 (List.filter (String.equal broadcast_path) complete_namespace)
               <> 1)
             paths
      then (
        [%log.debug "rejected retained broadcast namespace collision"
          ~provider:
            (Delator.Field.string
               (Compilation_unit.Name.to_string interface.cmi_name))
          ~stage:(Delator.Field.string "artifact-decode")
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "namespace-collision")];
        raise (Failure "retained broadcast namespace collision"))
      else (
        [%log.debug "decoded retained broadcast interface artifact"
          ~provider:
            (Delator.Field.string
               (Compilation_unit.Name.to_string interface.cmi_name))
          ~stage:(Delator.Field.string "artifact-decode")
          ~route:(Delator.Field.string "cmi")
          ~set_cardinality:(Delator.Field.int (List.length values))
          ~decision:(Delator.Field.string "accepted")];
        values)
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let typed_interface_broadcast_members
    ~typed_interface:(signature [@delator.skip])
    ~unit_name:(unit_name [@delator.field Fun.id])
    ~interface_digest:(interface_digest [@delator.skip])
    ~load_paths:(load_paths [@delator.skip])
    ~interface:(interface [@delator.skip])
    ~interface_syntax:(interface_syntax [@delator.skip]) =
      let reject _reason_class message =
        [%log.debug "rejected typed retained broadcast witness"
          ~provider:(Delator.Field.string unit_name)
          ~stage:(Delator.Field.string "typed-witness")
          ~route:(Delator.Field.string "cmti-typed-signature")
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string _reason_class)];
        raise (Failure message)
      in
      let member_name =
        "verocaml.internal.broadcast.interface_member.v1"
      and declaration_name =
        "verocaml.internal.broadcast.interface_declaration.v1"
      and group_name = "verocaml.internal.broadcast.interface_group.v1" in
      let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid in
      let imported_interfaces =
        authenticated_import_interfaces ~load_paths interface
      in
      let compiler_interface_root =
        Subst.Lazy.force_signature interface.Cmi_format.cmi_sign
      in
      let member_origins =
        let paths provider interface_receipt dependency_receipt syntax =
          List.map
            (fun member ->
              ( member.broadcast_uid,
                member.broadcast_kind,
                provider ^ "." ^ member.broadcast_path,
                interface_receipt,
                dependency_receipt ))
            syntax
        in
        let local =
          paths unit_name interface_digest
            (import_receipt (imports_of_array interface.Cmi_format.cmi_crcs))
            interface_syntax
        in
        Array.fold_left
          (fun members imported ->
            let name = Import_info.name imported in
            if Compilation_unit.Name.equal name interface.Cmi_format.cmi_name then
              members
            else
              let provider = Compilation_unit.Name.to_string name in
              match
                List.find_opt
                  (fun artifact ->
                    Compilation_unit.Name.equal
                      artifact.artifact_interface.cmi_name name)
                  imported_interfaces
              with
              | None -> members
              | Some dependency ->
                  List.rev_append
                    (paths provider dependency.artifact_self_crc
                       (import_receipt
                          (imports_of_array
                             dependency.artifact_interface.Cmi_format.cmi_crcs))
                       (interface_broadcast_syntax ~load_paths
                          dependency.artifact_interface))
                    members)
          local interface.Cmi_format.cmi_crcs
      in
      let retained_kind description =
        match
          ( strict_broadcast_attribute declaration_name
              description.Types.val_attributes,
            strict_broadcast_attribute group_name
              description.Types.val_attributes )
        with
        | Some "v1", None -> Some Retained_broadcast_private.Declaration
        | None, Some payload when String.starts_with ~prefix:"v4|" payload ->
            Some Retained_broadcast_private.Group
        | None, None -> None
        | Some _, None | None, Some _ | Some _, Some _ ->
            [%log.debug "rejected typed retained broadcast kind metadata"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "compiler-lookup")
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "kind-metadata-invariant")];
            raise
              (Failure "typed retained broadcast target kind is malformed")
      in
      let target_kind description =
        match retained_kind description with
        | Some kind -> kind
        | None ->
            [%log.debug "rejected typed retained broadcast member kind"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "compiler-lookup")
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "non-authoritative-target")];
            raise
              (Failure
                 "typed retained broadcast target is not an authoritative declaration or group")
      in
      let compiler_paths root signature =
        let rec module_signature items seen = function
          | Types.Mty_signature nested -> Some nested
          | Mty_strengthen (nested, _, _) -> module_signature items seen nested
          | Mty_ident path -> modtype_path items seen path
          | Mty_alias path -> module_path items seen path
          | Mty_functor _ -> None
        and modtype_path items seen path =
          let key = "modtype:" ^ Format.asprintf "%a" Path.print path in
          if List.mem key seen then reject "module-type-cycle" "retained module-type cycle"
          else
            match path with
            | Path.Pident ident ->
                let resolve scope =
                  List.find_map
                    (function
                      | Types.Sig_modtype (candidate, declaration, _)
                        when Ident.same candidate ident ->
                          Option.bind declaration.Types.mtd_type
                            (module_signature scope (key :: seen))
                      | _ -> None)
                    scope
                in
                (match resolve items with
                | Some _ as signature -> signature
                | None -> resolve compiler_interface_root)
            | Pdot (parent, name) ->
                Option.bind (module_path items (key :: seen) parent) (fun nested ->
                    List.find_map
                      (function
                        | Types.Sig_modtype (ident, declaration, _)
                          when String.equal (Ident.name ident) name ->
                            Option.bind declaration.Types.mtd_type
                              (module_signature nested (key :: seen))
                        | _ -> None)
                      nested)
            | Papply _ | Pextra_ty _ -> None
        and module_path items seen path =
          let key = "module:" ^ Format.asprintf "%a" Path.print path in
          if List.mem key seen then reject "module-alias-cycle" "retained module alias cycle"
          else
            match path with
            | Path.Pident ident -> (
                match
                  List.find_map
                    (function
                      | Types.Sig_module (candidate, _, declaration, _, _)
                        when Ident.same candidate ident ->
                          module_signature items (key :: seen)
                            declaration.Types.md_type
                      | _ -> None)
                    items
                with
                | Some _ as signature -> signature
                | None ->
                    if String.equal (Ident.name ident) unit_name then
                      Some compiler_interface_root
                    else
                      imported_interfaces
                      |> List.find_map (fun artifact ->
                             if
                               String.equal (Ident.name ident)
                                 (Compilation_unit.Name.to_string
                                    artifact.artifact_interface.cmi_name)
                             then
                               Some
                                 (Subst.Lazy.force_signature
                                    artifact.artifact_interface.cmi_sign)
                             else None))
            | Pdot (parent, name) ->
                Option.bind (module_path items (key :: seen) parent) (fun nested ->
                    List.find_map
                      (function
                        | Types.Sig_module (ident, _, declaration, _, _)
                          when String.equal (Ident.name ident) name ->
                            module_signature nested (key :: seen)
                              declaration.Types.md_type
                        | _ -> None)
                      nested)
            | Papply _ | Pextra_ty _ -> None
        in
        let rec canonical_module_path items seen path =
          let key = Format.asprintf "%a" Path.print path in
          if List.mem key seen then
            reject "module-alias-cycle" "retained module alias cycle"
          else
            match path with
            | Path.Pident ident -> (
                match
                  List.find_map
                    (function
                      | Types.Sig_module (candidate, _, declaration, _, _)
                        when Ident.same candidate ident -> (
                          match declaration.Types.md_type with
                          | Types.Mty_alias target -> Some target
                          | Mty_ident _ | Mty_signature _ | Mty_functor _
                          | Mty_strengthen _ -> None)
                      | _ -> None)
                    items
                with
                | Some target ->
                    canonical_module_path items (key :: seen) target
                | None -> path)
            | Pdot (parent, name) ->
                let canonical_parent =
                  canonical_module_path items (key :: seen) parent
                in
                (match module_path items [] canonical_parent with
                | None -> Path.Pdot (canonical_parent, name)
                | Some nested -> (
                    match
                      List.find_map
                        (function
                          | Types.Sig_module
                              (ident, _, declaration, _, _)
                            when String.equal (Ident.name ident) name -> (
                              match declaration.Types.md_type with
                              | Types.Mty_alias target -> Some target
                              | Mty_ident _ | Mty_signature _ | Mty_functor _
                              | Mty_strengthen _ -> None)
                          | _ -> None)
                        nested
                    with
                    | Some target ->
                        canonical_module_path items (key :: seen) target
                    | None -> Path.Pdot (canonical_parent, name)))
            | Papply _ | Pextra_ty _ -> path
        in
        let child_path parent ident =
          match parent with
          | None -> Path.Pident ident
          | Some parent -> Path.Pdot (parent, Ident.name ident)
        in
        let rec collect parent items =
          List.concat_map
            (function
              | Types.Sig_value (ident, description, _) -> (
                  match retained_kind description with
                  | None -> []
                  | Some kind ->
                      let uid = compiler_uid description.Types.val_uid in
                      let compiler_path = child_path parent ident in
                      let printed_path = Path.name compiler_path in
                      let local_canonical_path =
                        unit_name ^ "." ^ printed_path
                      in
                      let canonical_paths =
                        member_origins
                        |> List.filter_map
                             (fun (candidate_uid, candidate_kind,
                                   canonical_path, interface_receipt,
                                   dependency_receipt) ->
                               if
                                 String.equal uid candidate_uid
                                 && kind = candidate_kind
                                 &&
                                 (String.equal canonical_path printed_path
                                 || String.equal canonical_path
                                      local_canonical_path)
                               then
                                 Some
                                   ( canonical_path,
                                     interface_receipt,
                                     dependency_receipt )
                               else None)
                        |> List.sort_uniq compare
                      in
                      List.map
                        (fun
                          (canonical_path, interface_receipt,
                           dependency_receipt) ->
                          ( compiler_path,
                            uid,
                            kind,
                            canonical_path,
                            interface_receipt,
                            dependency_receipt ))
                        canonical_paths)
              | Types.Sig_module (ident, _, declaration, _, _) -> (
                  match module_signature items [] declaration.Types.md_type with
                  | Some nested ->
                      let module_path =
                        match declaration.Types.md_type with
                        | Types.Mty_alias target ->
                            canonical_module_path items [] target
                        | Mty_ident _ | Mty_signature _ | Mty_functor _
                        | Mty_strengthen _ -> child_path parent ident
                      in
                      collect (Some module_path) nested
                  | None -> [])
              | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
              | Types.Sig_class _ | Types.Sig_class_type _ -> [])
            items
        in
        collect root signature
      in
      let member_paths =
        List.fold_left
          (fun paths artifact ->
            let root =
              Path.Pident
                (Ident.create_persistent
                   (Compilation_unit.Name.to_string
                      artifact.artifact_interface.cmi_name))
            in
            List.rev_append
              (compiler_paths (Some root)
                 (Subst.Lazy.force_signature
                    artifact.artifact_interface.cmi_sign))
              paths)
          [] imported_interfaces
        |> ref
      in
      let witness prefix (include_ : Typedtree.include_description) =
        match strict_broadcast_attribute member_name include_.Typedtree.incl_attributes with
        | None -> None
        | Some payload ->
            if not (String.starts_with ~prefix:"v1|" payload) then
              reject "witness-version"
                "unknown typed retained broadcast witness version"
            else
              let fields =
                String.sub payload 3 (String.length payload - 3)
                |> decode_broadcast_frames
              in
              let group_name, ordinal =
                match fields with
                | [ group_name; ordinal ] -> (group_name, int_of_string ordinal)
                | [] | [ _ ] | _ :: _ :: _ :: _ ->
                    reject "witness-frame"
                      "malformed typed retained broadcast witness"
              in
              let expression =
                match include_.incl_mod.mty_desc with
                | Typedtree.Tmty_typeof
                    {
                      mod_desc =
                        Tmod_structure
                          {
                            str_items =
                              [
                                {
                                  str_desc =
                                    Tstr_value
                                      ( Asttypes.Nonrecursive,
                                        [
                                          {
                                            vb_pat = { pat_desc = Tpat_any; _ };
                                            vb_expr = expression;
                                            _;
                                          };
                                        ] );
                                  _;
                                };
                              ];
                            _;
                          };
                      _;
                    } ->
                    expression
                | Tmty_ident _ | Tmty_signature _ | Tmty_functor _
                | Tmty_with _ | Tmty_alias _ | Tmty_strengthen _
                | Tmty_typeof _ ->
                    reject "witness-shape"
                      "typed retained broadcast witness shape is invalid"
              in
              let path, source_path, description =
                match expression.Typedtree.exp_desc with
                | Texp_ident (path, source, description, _, _) ->
                    ( path,
                      String.concat "." (Longident.flatten source.txt),
                      description )
                | _ ->
                    reject "witness-target-shape"
                      "typed retained broadcast witness is not a value path"
              in
              let normalized =
                try
                  with_authenticated_interfaces imported_interfaces (fun () ->
                    let environment =
                      Envaux.env_of_only_summary ~allow_missing_modules:true
                        expression.exp_env
                    in
                    Env.normalize_value_path (Some expression.exp_loc)
                      environment path)
                with Env.Error _ ->
                  [%log.debug "rejected unnormalized typed retained broadcast path"
                    ~provider:(Delator.Field.string unit_name)
                    ~stage:(Delator.Field.string "compiler-normalization")
                    ~route:(Delator.Field.string "typed-signature")
                    ~decision:(Delator.Field.string "rejected")
                    ~reason_class:
                      (Delator.Field.string "compiler-normalization-failure")];
                  reject "compiler-normalization-failure"
                    "typed retained broadcast target cannot be compiler-normalized"
              in
              let member_kind = target_kind description in
              let member_uid = compiler_uid description.Types.val_uid in
              let member_canonical_path =
                match normalized with
                | Path.Pident _ | Pdot _ ->
                    let candidates =
                      !member_paths
                      |> List.filter_map
                         (fun
                           (compiler_path, uid, kind, canonical_path,
                            interface_receipt, dependency_receipt) ->
                           if String.equal uid member_uid && kind = member_kind
                           then
                             Some
                               ( Path.same compiler_path normalized,
                                 canonical_path,
                                 interface_receipt,
                                 dependency_receipt )
                           else None)
                      |> List.sort_uniq compare
                    in
                    let exact =
                      candidates
                      |> List.filter_map (fun (same, path, interface, dependency) ->
                             if same then Some (path, interface, dependency)
                             else None)
                      |> List.sort_uniq compare
                    and by_uid =
                      candidates
                      |> List.map (fun (_, path, interface, dependency) ->
                             (path, interface, dependency))
                      |> List.sort_uniq compare
                    in
                    [%log.debug "reconciled typed retained broadcast identity candidates"
                      ~provider:(Delator.Field.string unit_name)
                      ~stage:(Delator.Field.string "compiler-lookup")
                      ~route:(Delator.Field.string "typed-signature")
                      ~correlation:
                        (Delator.Field.string
                           ("broadcast-path-"
                           ^ (Digest.string (Path.name normalized)
                             |> Digest.to_hex)))
                      ~candidate_count:
                        (Delator.Field.int (List.length candidates))
                      ~exact_count:(Delator.Field.int (List.length exact))
                      ~decision:
                        (Delator.Field.string
                           (if exact <> [] || List.length by_uid = 1 then
                              "candidate"
                            else "unresolved"))];
                    (match exact with [] -> by_uid | _ :: _ -> exact)
                    |> (function
                         | [ exact ] -> exact
                         | [] ->
                             reject "unresolved-compiler-identity"
                               "typed retained broadcast member has no exact compiler identity"
                         | _ :: _ :: _ ->
                             reject "ambiguous-compiler-identity"
                               "typed retained broadcast member compiler identity is ambiguous")
                | Papply _ | Pextra_ty _ ->
                    [%log.debug "rejected noncanonical typed retained broadcast path"
                      ~provider:(Delator.Field.string unit_name)
                      ~stage:(Delator.Field.string "compiler-normalization")
                      ~route:(Delator.Field.string "typed-signature")
                      ~decision:(Delator.Field.string "rejected")
                      ~reason_class:
                        (Delator.Field.string "noncanonical-compiler-path")];
                    reject "noncanonical-compiler-path"
                      "typed retained broadcast member has no canonical provider path"
              in
              let member_canonical_path, member_interface_receipt,
                  member_dependency_receipt =
                member_canonical_path
              in
              let member_provider_origin =
                match String.split_on_char '.' member_canonical_path with
                | provider :: _ -> provider
                | [] -> reject "provider-origin" "typed retained broadcast member has no provider origin"
              in
              let member =
                {
                  Retained_broadcast_private.member_slot = ordinal;
                  Retained_broadcast_private.member_provider_origin;
                  member_interface_receipt;
                  member_dependency_receipt;
                  Retained_broadcast_private.member_compiler_uid =
                    member_uid;
                  member_kind;
                  member_canonical_path;
                }
              in
              let group_path =
                String.concat "." (List.rev (group_name :: prefix))
              in
              let[@log_value.trace] _correlation =
                Digest.string
                  (group_path ^ ":" ^ string_of_int ordinal ^ ":"
                 ^ member.member_compiler_uid)
                |> Digest.to_hex
              in
              [%log.trace "resolved typed retained broadcast member"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "compiler-lookup")
                ~route:(Delator.Field.string "typed-signature")
                ~member_kind:
                  (Delator.Field.string
                     (Retained_broadcast_private.kind_name member_kind))
                ~correlation:
                  (Delator.Field.string
                     ("broadcast-member-"
                     ^ (_correlation [@log_value.trace])))
                ~decision:(Delator.Field.string "accepted")];
              Some (group_path, ordinal, source_path, member)
      in
      let typed_path_key path = Format.asprintf "%a" Path.print path in
      let typed_interface_root = ref None in
      let typed_unique reason signatures =
        match signatures with
        | [] -> None
        | [ signature ] -> Some signature
        | _ :: _ :: _ ->
            reject reason "typed retained module metadata route is ambiguous"
      in
      let direct_compiler_broadcasts signature =
        signature
        |> List.filter_map (function
             | Types.Sig_value (ident, description, _) ->
                 Option.map
                   (fun kind ->
                     ( Ident.name ident,
                       compiler_uid description.Types.val_uid,
                       kind ))
                   (retained_kind description)
             | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_module _
             | Types.Sig_modtype _ | Types.Sig_class _
             | Types.Sig_class_type _ ->
                 None)
        |> List.sort compare
      in
      let direct_typed_broadcasts signature =
        signature.Typedtree.sig_items
        |> List.filter_map (fun item ->
               match item.Typedtree.sig_desc with
               | Tsig_value value ->
                   Option.map
                     (fun kind ->
                       ( Ident.name value.Typedtree.val_id,
                         compiler_uid value.val_val.Types.val_uid,
                         kind ))
                     (retained_kind value.val_val)
               | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _
               | Tsig_exception _ | Tsig_module _ | Tsig_modsubst _
               | Tsig_recmodule _ | Tsig_modtype _ | Tsig_modtypesubst _
               | Tsig_open _ | Tsig_include _ | Tsig_class _
               | Tsig_class_type _ | Tsig_attribute _ ->
                   None)
        |> List.sort compare
      in
      let compiler_signature module_type =
        let rec immediate = function
          | Types.Mty_signature signature -> Some signature
          | Mty_strengthen (nested, _, _) -> immediate nested
          | Mty_ident _ | Mty_alias _ | Mty_functor _ -> None
        in
        match immediate module_type.Typedtree.mty_type with
        | Some _ as signature -> signature
        | None -> (
            try immediate (Mtype.scrape module_type.mty_env module_type.mty_type)
            with Env.Error _ -> None)
      in
      let validate_compiler_projection ~stage:_stage module_type signature =
        let compiler = compiler_signature module_type in
        let matches =
          match compiler with
          | Some compiler ->
              direct_compiler_broadcasts compiler
              = direct_typed_broadcasts signature
          | None -> false
        in
        [%log.debug "reconciled retained module type with compiler structure"
          ~provider:(Delator.Field.string unit_name)
          ~stage:(Delator.Field.string _stage)
          ~route:(Delator.Field.string "compiler-mty-type")
          ~typed_count:
            (Delator.Field.int (List.length (direct_typed_broadcasts signature)))
          ~compiler_count:
            (Delator.Field.int
               (Option.fold ~none:0
                  ~some:(fun signature ->
                    List.length (direct_compiler_broadcasts signature))
                  compiler))
          ~decision:
            (Delator.Field.string (if matches then "accepted" else "rejected"))
          ~reason_class:
            (Delator.Field.string
               (if matches then "exact-compiler-structure"
                else "compiler-structure-mismatch"))];
        if not matches then
          reject "compiler-module-type-structure"
            "retained module metadata differs from its compiler module type"
      in
      let rec typed_module_signature owner seen module_type =
        match module_type.Typedtree.mty_desc with
        | Tmty_signature nested -> Some nested
        | Tmty_strengthen (nested, _, _) ->
            Option.map
              (fun signature ->
                validate_compiler_projection ~stage:"module-strengthening"
                  module_type signature;
                signature)
              (typed_module_signature owner seen nested)
        | Tmty_ident (path, _) -> typed_modtype_path owner seen path
        | Tmty_alias (path, _) -> typed_module_path owner seen path
        | Tmty_typeof expression -> typed_module_expression owner seen expression
        | Tmty_with (nested, constraints) ->
            let type_only =
              List.for_all
                (fun (_, _, constraint_) ->
                  match constraint_ with
                  | Typedtree.Twith_type _ | Twith_typesubst _ -> true
                  | Twith_module _ | Twith_modtype _ | Twith_modsubst _
                  | Twith_modtypesubst _ ->
                      false)
                constraints
            in
            if not type_only then (
              [%log.debug "rejected retained non-type module constraint"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "module-with-constraint")
                ~route:(Delator.Field.string "compiler-mty-type")
                ~constraint_count:(Delator.Field.int (List.length constraints))
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "non-type-constraint")];
              None)
            else
              Option.map
                (fun signature ->
                  validate_compiler_projection ~stage:"module-with-type"
                    module_type signature;
                  signature)
                (typed_module_signature owner seen nested)
        | Tmty_functor _ ->
            [%log.debug "rejected retained functor module metadata route"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "module-metadata-routing")
              ~route:(Delator.Field.string "compiler-mty-type")
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "functor-boundary")];
            None
      and typed_modtype_path owner seen path =
        let key = "modtype:" ^ typed_path_key path in
        if List.mem key seen then
          reject "module-type-cycle" "typed retained module-type metadata cycle"
        else
          match path with
          | Path.Pident ident ->
              let resolve scope =
                scope.Typedtree.sig_items
                |> List.filter_map (fun item ->
                       match item.Typedtree.sig_desc with
                       | Tsig_modtype declaration
                         when Ident.same declaration.mtd_id ident ->
                           Option.bind declaration.mtd_type (fun nested ->
                               typed_module_signature scope (key :: seen)
                                 nested)
                       | _ -> None)
                |> typed_unique "module-type-collision"
              in
              (match resolve owner with
              | Some _ as signature -> signature
              | None -> Option.bind !typed_interface_root resolve)
          | Pdot (parent, name) ->
              Option.bind (typed_module_path owner (key :: seen) parent)
                (fun nested ->
                  nested.Typedtree.sig_items
                  |> List.filter_map (fun item ->
                         match item.Typedtree.sig_desc with
                         | Tsig_modtype declaration
                           when String.equal
                                  (Ident.name declaration.mtd_id)
                                  name ->
                             Option.bind declaration.mtd_type (fun module_type ->
                                 typed_module_signature nested (key :: seen)
                                   module_type)
                         | _ -> None)
                  |> typed_unique "qualified-module-type-collision")
          | Papply _ | Pextra_ty _ -> None
      and typed_module_path owner seen path =
        let key = "module:" ^ typed_path_key path in
        if List.mem key seen then
          reject "module-alias-cycle" "typed retained module alias metadata cycle"
        else
          match path with
          | Path.Pident ident ->
              let local =
                owner.Typedtree.sig_items
                |> List.filter_map (fun item ->
                       match item.Typedtree.sig_desc with
                       | Tsig_module declaration -> (
                           match declaration.md_id with
                           | Some candidate when Ident.same candidate ident ->
                               typed_module_signature owner (key :: seen)
                                 declaration.md_type
                           | None | Some _ -> None)
                       | _ -> None)
              and persistent =
                if String.equal (Ident.name ident) unit_name then
                  Option.to_list !typed_interface_root
                else []
              in
              typed_unique "module-alias-collision" (local @ persistent)
          | Pdot (parent, name) ->
              Option.bind (typed_module_path owner (key :: seen) parent)
                (fun nested ->
                  nested.Typedtree.sig_items
                  |> List.filter_map (fun item ->
                         match item.Typedtree.sig_desc with
                         | Tsig_module declaration -> (
                             match declaration.md_id with
                             | Some ident when String.equal (Ident.name ident) name ->
                                 typed_module_signature nested (key :: seen)
                                   declaration.md_type
                             | None | Some _ -> None)
                         | _ -> None)
                  |> typed_unique "qualified-module-collision")
          | Papply _ | Pextra_ty _ -> None
      and typed_module_expression owner seen expression =
        match expression.Typedtree.mod_desc with
        | Tmod_ident (path, _) -> typed_module_path owner seen path
        | Tmod_constraint (nested, _, _, _) ->
            typed_module_expression owner seen nested
        | Tmod_structure _ | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _
        | Tmod_unpack _ -> None
      in
      let rec type_value_paths prefix items =
        List.concat_map
          (function
            | Types.Sig_value (ident, _, _) ->
                [ String.concat "." (List.rev (Ident.name ident :: prefix)) ]
            | Types.Sig_module (ident, _, declaration, _, _) -> (
                match declaration.Types.md_type with
                | Types.Mty_signature nested
                | Mty_strengthen (Types.Mty_signature nested, _, _) ->
                    type_value_paths (Ident.name ident :: prefix) nested
                | Mty_ident _ | Mty_alias _ | Mty_functor _ | Mty_strengthen _ ->
                    [])
            | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
            | Types.Sig_class _ | Types.Sig_class_type _ -> [])
          items
      in
      let rec typed_value_paths prefix signature =
        List.concat_map
          (fun item ->
            match item.Typedtree.sig_desc with
            | Tsig_value value ->
                [
                  String.concat "."
                    (List.rev (Ident.name value.Typedtree.val_id :: prefix));
                ]
            | Tsig_include (include_, _) ->
                type_value_paths prefix include_.Typedtree.incl_type
            | Tsig_module declaration -> (
                match
                  ( declaration.md_id,
                    typed_module_signature signature [] declaration.md_type )
                with
                | _, _ when (match declaration.md_type.mty_desc with Tmty_alias _ -> true | _ -> false) -> []
                | Some ident, Some nested ->
                    typed_value_paths (Ident.name ident :: prefix) nested
                | _ -> [])
            | Tsig_recmodule declarations ->
                List.concat_map
                  (fun (declaration : Typedtree.module_declaration) ->
                    match
                      ( declaration.md_id,
                        typed_module_signature signature [] declaration.md_type )
                    with
                    | Some ident, Some nested ->
                        typed_value_paths (Ident.name ident :: prefix) nested
                    | _ -> [])
                  declarations
            | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _ | Tsig_exception _
            | Tsig_modsubst _ | Tsig_modtype _ | Tsig_modtypesubst _
            | Tsig_open _ | Tsig_class _ | Tsig_class_type _
            | Tsig_attribute _ -> [])
          signature.Typedtree.sig_items
      in
      let rec typed_broadcast_values prefix signature =
        List.concat_map
          (fun item ->
            match item.Typedtree.sig_desc with
            | Tsig_value value -> (
                match retained_kind value.Typedtree.val_val with
                | None -> []
                | Some kind ->
                    [
                      ( String.concat "."
                          (List.rev
                             (Ident.name value.Typedtree.val_id :: prefix)),
                        compiler_uid value.Typedtree.val_val.Types.val_uid,
                        kind );
                    ])
            | Tsig_module declaration -> (
                match
                  ( declaration.md_name.txt,
                    typed_module_signature signature [] declaration.md_type )
                with
                | _, _ when (match declaration.md_type.mty_desc with Tmty_alias _ -> true | _ -> false) -> []
                | Some name, Some nested ->
                    typed_broadcast_values (name :: prefix) nested
                | _ -> [])
            | Tsig_recmodule declarations ->
                List.concat_map
                  (fun (declaration : Typedtree.module_declaration) ->
                    match
                      ( declaration.md_name.txt,
                        typed_module_signature signature [] declaration.md_type )
                    with
                    | Some name, Some nested ->
                        typed_broadcast_values (name :: prefix) nested
                    | _ -> [])
                  declarations
            | Tsig_include _ | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _
            | Tsig_exception _ | Tsig_modsubst _ | Tsig_modtype _
            | Tsig_modtypesubst _ | Tsig_open _ | Tsig_class _
            | Tsig_class_type _ | Tsig_attribute _ -> [])
          signature.Typedtree.sig_items
      in
      let rec collect prefix parent signature =
        member_paths :=
          List.rev_append
            (compiler_paths parent signature.Typedtree.sig_type)
            !member_paths;
        List.concat_map
          (fun item ->
            match item.Typedtree.sig_desc with
            | Tsig_include (include_, _) ->
                Option.to_list (witness prefix include_)
            | Tsig_module declaration -> (
                match
                  ( declaration.md_id,
                    declaration.md_name.txt,
                    typed_module_signature signature [] declaration.md_type )
                with
                | Some ident, Some name, Some nested ->
                    let parent =
                      match parent with
                      | None -> Path.Pident ident
                      | Some parent -> Path.Pdot (parent, Ident.name ident)
                    in
                    collect (name :: prefix) (Some parent) nested
                | _, _, _ -> [])
            | Tsig_recmodule declarations ->
                List.concat_map
                  (fun (declaration : Typedtree.module_declaration) ->
                    match
                      ( declaration.md_id,
                        declaration.md_name.txt,
                        typed_module_signature signature [] declaration.md_type )
                    with
                    | Some ident, Some name, Some nested ->
                        let parent =
                          match parent with
                          | None -> Path.Pident ident
                          | Some parent ->
                              Path.Pdot (parent, Ident.name ident)
                        in
                        collect (name :: prefix) (Some parent) nested
                    | _, _, _ -> [])
                  declarations
            | Tsig_value _ | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _
            | Tsig_exception _ | Tsig_modsubst _ | Tsig_modtype _
            | Tsig_modtypesubst _ | Tsig_open _
            | Tsig_class _ | Tsig_class_type _ | Tsig_attribute _ -> [])
          signature.Typedtree.sig_items
      in
      typed_interface_root := Some signature;
      let namespace = typed_value_paths [] signature in
      let expected_values =
        interface_syntax
        |> List.map (fun syntax ->
               ( syntax.broadcast_path,
                 syntax.broadcast_uid,
                 syntax.broadcast_kind ))
        |> List.sort compare
      and typed_values =
        typed_broadcast_values [] signature |> List.sort compare
      in
      if typed_values <> expected_values then
        reject "complete-direct-identity"
          "retained broadcast CMI and CMTI declaration identities differ";
      interface_syntax
      |> List.iter (fun syntax ->
             if
               List.length
                 (List.filter (String.equal syntax.broadcast_path) namespace)
               <> 1
             then
               reject "namespace-collision"
                 "retained broadcast value collides with an included value");
      let members = collect [] None signature in
      [%log.debug "reconstructed retained broadcast interface scope"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "interface-scope")
        ~route:(Delator.Field.string "authenticated-cmti-typed-signature")
        ~set_cardinality:(Delator.Field.int (List.length members))
        ~decision:(Delator.Field.string "accepted")];
      members
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let released_interface_broadcast_metadata syntax =
  List.fold_left
    (fun (declarations, groups) member ->
      match member.broadcast_kind with
      | Retained_broadcast_private.Declaration ->
          [%log.trace "projected broadcast interface declaration metadata"
            ~member_kind:(Delator.Field.string "declaration")
            ~correlation:
              (Delator.Field.string
                 (Digest.to_hex (Digest.string member.broadcast_path)))
            ~stage:(Delator.Field.string "released-metadata-projection")
            ~decision:(Delator.Field.string "retained")];
          (member.broadcast_path :: declarations, groups)
      | Retained_broadcast_private.Group ->
          [%log.trace "projected broadcast interface group metadata"
            ~member_kind:(Delator.Field.string "group")
            ~correlation:
              (Delator.Field.string
                 (Digest.to_hex (Digest.string member.broadcast_path)))
            ~targets:
              (Delator.Field.int (List.length member.broadcast_member_paths))
            ~stage:(Delator.Field.string "released-metadata-projection")
            ~decision:(Delator.Field.string "retained")];
          ( declarations,
            {
              group_path = member.broadcast_path;
              group_targets = member.broadcast_member_paths;
            }
            :: groups ))
    ([], []) syntax
  |> fun (declarations, groups) ->
  ( List.sort String.compare declarations,
    List.sort
      (fun left right -> String.compare left.group_path right.group_path)
      groups )

exception Symbolic_marker_failure of string

let symbolic_marker_failure reason = raise (Symbolic_marker_failure reason)

let symbolic_interface_attribute attributes =
  let name = "verocaml.internal.symbolic.interface.v1" in
  let matching =
    List.filter
      (fun attribute -> String.equal attribute.Parsetree.attr_name.txt name)
      attributes
  in
  match matching with
  | [] -> None
  | _ :: _ :: _ -> symbolic_marker_failure "duplicate symbolic interface marker"
  | [ attribute ] ->
      if
        not
          (attribute.attr_loc.Location.loc_ghost
          && attribute.attr_name.loc.loc_ghost)
      then symbolic_marker_failure "non-ghost symbolic interface marker"
      else
        let payload =
          match attribute.attr_payload with
          | PStr
              [
                {
                  pstr_desc =
                    Pstr_eval
                      ( {
                          pexp_desc =
                            Pexp_constant (Pconst_string (value, _, None));
                          _;
                        },
                        [] );
                  _;
                };
              ] ->
              value
          | PStr _ | PSig _ | PTyp _ | PPat _ ->
              symbolic_marker_failure "malformed symbolic interface marker"
        in
        (match String.split_on_char '|' payload with
        | [ "v1"; marker; payload_name; start_; stop; type_digest ] -> (
            match (int_of_string_opt start_, int_of_string_opt stop) with
            | Some start_, Some stop
              when payload_name <> "" && start_ >= 0 && stop >= start_
                   && String.length type_digest = 32
                   && String.for_all
                        (function
                          | '0' .. '9' | 'a' .. 'f' -> true
                          | _ -> false)
                        type_digest ->
                let framed tag fields =
                  let field value =
                    Printf.sprintf "%d:%s" (String.length value) value
                  in
                  tag ^ String.concat "" (List.map field fields)
                in
                let expected =
                  "symbolic."
                  ^ Digest.to_hex
                      (Digest.string
                         (framed "symbolic-interface-v1"
                            [
                              payload_name;
                              string_of_int start_;
                              string_of_int stop;
                              type_digest;
                            ]))
                in
                if String.equal marker expected then
                  Some (payload_name, marker, type_digest)
                else symbolic_marker_failure "stale symbolic interface marker"
            | Some _, Some _ | None, _ | _, None ->
                symbolic_marker_failure "malformed symbolic interface marker")
        | _ -> symbolic_marker_failure "malformed symbolic interface marker")

let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid

let exact_empty_internal_marker name attributes =
  let relevant =
    List.filter
      (fun attribute ->
        String.equal attribute.Parsetree.attr_name.txt name
        || String.equal attribute.attr_name.txt
             (if String.equal name logical_sort_marker then
                "verocaml.logical_sort"
              else "verocaml.integer_literal"))
      attributes
  in
  match relevant with
  | [] -> Ok false
  | [ attribute ]
    when String.equal attribute.attr_name.txt name
         && attribute.attr_loc.loc_ghost
         && attribute.attr_name.loc.loc_ghost
         && attribute.attr_payload = Parsetree.PStr [] ->
      Ok true
  | _ -> Error "logical-sort marker is malformed or unauthenticated"

let local_implementation_type_uid uid =
  match uid with
  | Types.Uid.Item { comp_unit; id; from = _ } ->
      let identity = Printf.sprintf "%s.%d" comp_unit id in
      [%log.trace "correlated local interface type UID with implementation UID"
        ~stage:(Delator.Field.string "symbolic-interface-typed-abi")
        ~route:(Delator.Field.string "compiler-local-type-identity")
        ~compiler_identity_class:
          (Delator.Field.string "local-implementation-type")
        ~correlation:
          (Delator.Field.string
             (Digest.to_hex (Digest.string identity)))
        ~decision:(Delator.Field.string "correlated")];
      identity
  | Compilation_unit _ | Internal | Predef _ | Unboxed_version _ ->
      compiler_uid uid

let implementation_export_uid ~unit_name ~environment ~namespace shape path =
  let shape_component shape kind name =
    match (Shape.strip_head_aliases shape).Shape.desc with
    | Shape.Struct components ->
        Shape.Item.Map.find_opt (Shape.Item.make name kind) components
    | Shape.Var _ | Abs _ | App _ | Alias _ | Leaf | Proj _ | Comp_unit _
    | Error _ | Constr _ | Tuple _ | Unboxed_tuple _ | Predef _ | Arrow
    | Poly_variant _ | Mu _ | Rec_var _ | Variant _ | Variant_unboxed _
    | Record _ | Mutrec _ | Proj_decl _ ->
        None
  in
  let reduced_uid shape =
    let rec resolved = function
      | Shape_reduce.Resolved uid -> Some (compiler_uid uid)
      | Resolved_alias (_, nested) -> resolved nested
      | Unresolved _ | Approximated _ | Internal_error_missing_uid -> None
    in
    resolved
      (Shape_reduce.local_reduce_for_uid environment shape)
  in
  let components = String.split_on_char '.' path in
  let components =
    match components with
    | root :: rest when String.equal root unit_name -> rest
    | _ -> components
  in
  let rec descend shape = function
    | [] -> None
    | [ name ] ->
        Option.bind
          (shape_component shape namespace name)
          reduced_uid
    | name :: rest ->
        Option.bind
          (shape_component shape Shape.Sig_component_kind.Module name)
          (fun nested -> descend nested rest)
  in
  descend shape components

let implementation_value_uid implementation path =
  Option.bind implementation.metadata.Cmt_format.cmt_impl_shape
    (fun shape ->
      implementation_export_uid ~unit_name:implementation.unit_name
        ~environment:implementation.structure.str_final_env
        ~namespace:Shape.Sig_component_kind.Value shape path)

let interface_value_uid_correlates implementation ~path ~interface_uid
    ~implementation_uid =
  let rec module_signature items seen = function
    | Types.Mty_signature signature -> Some signature
    | Mty_strengthen (nested, _, _) -> module_signature items seen nested
    | Mty_ident (Path.Pident ident) ->
        if List.exists (Ident.same ident) seen then None
        else
          List.find_map
            (function
              | Types.Sig_modtype (candidate, declaration, _)
                when Ident.same candidate ident ->
                  Option.bind declaration.Types.mtd_type
                    (module_signature items (ident :: seen))
              | _ -> None)
            items
    | Mty_ident _ | Mty_functor _ | Mty_alias _ -> None
  in
  let candidates =
    match implementation.embedded_interface_metadata with
    | None -> []
    | Some interface ->
        let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
        let unit_name = implementation.unit_name in
        let rec collect prefix inherited_module_types items =
          let module_types =
            List.fold_left
              (fun collected -> function
                | Types.Sig_modtype (ident, declaration, Types.Exported) ->
                    Option.fold ~none:collected
                      ~some:(fun nested -> (ident, nested) :: collected)
                      declaration.Types.mtd_type
                | _ -> collected)
              inherited_module_types items
          in
          let qualify name =
            String.concat "." (unit_name :: List.rev (name :: prefix))
          in
          List.concat_map
            (function
              | Types.Sig_value (ident, description, Types.Exported) ->
                  let path = qualify (Ident.name ident) in
                  [ ( path,
                      compiler_uid description.Types.val_uid,
                      implementation_value_uid implementation path ) ]
              | Types.Sig_module
                  (ident, _, declaration, _, Types.Exported) -> (
                  match
                    module_signature items [] declaration.Types.md_type
                  with
                  | Some nested ->
                      collect (Ident.name ident :: prefix) module_types nested
                  | None -> [])
              | Types.Sig_value (_, _, Types.Hidden)
              | Types.Sig_module (_, _, _, _, Types.Hidden)
              | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
              | Types.Sig_class _ | Types.Sig_class_type _ ->
                  [])
            items
        in
        collect [] [] root
  in
  let exact =
    List.filter
      (fun (candidate_path, candidate_interface_uid,
            candidate_implementation_uid) ->
        String.equal path candidate_path
        && String.equal interface_uid candidate_interface_uid
        && Option.equal String.equal (Some implementation_uid)
             candidate_implementation_uid)
      candidates
  in
  let accepted = List.length exact = 1 in
  let[@log_value.trace] candidate_identities =
    candidates
    |> List.filteri (fun index _ -> index < 16)
    |> List.map
         (fun (candidate_path, candidate_interface_uid,
               candidate_implementation_uid) ->
           Delator.Field.map
             [ ("path", Delator.Field.string candidate_path);
               ("interface_uid", Delator.Field.string candidate_interface_uid);
               ( "implementation_uid",
                 Delator.Field.string
                   (Option.value ~default:"<unresolved>"
                      candidate_implementation_uid) ) ])
  in
  [%log.trace "correlated logical value interface and implementation UIDs"
    ~provider:(Delator.Field.string implementation.unit_name)
    ~stage:(Delator.Field.string "logical-value-uid-correlation")
    ~route:(Delator.Field.string path)
    ~expected_interface_uid:(Delator.Field.string interface_uid)
    ~expected_implementation_uid:(Delator.Field.string implementation_uid)
    ~candidate_identities:
      (Delator.Field.seq
         ~dropped:(Int.max 0 (List.length candidates - 16))
         (candidate_identities [@log_value.trace]))
    ~candidate_count:(Delator.Field.int (List.length candidates))
    ~exact_count:(Delator.Field.int (List.length exact))
    ~decision:
      (Delator.Field.string (if accepted then "correlated" else "rejected"))];
  accepted

let rec signature_module_type items seen = function
  | Types.Mty_signature signature -> Some signature
  | Mty_strengthen (nested, _, _) -> signature_module_type items seen nested
  | Mty_ident (Path.Pident ident) ->
      if List.exists (Ident.same ident) seen then None
      else
        List.find_map
          (function
            | Types.Sig_modtype (candidate, declaration, _)
              when Ident.same candidate ident ->
                Option.bind declaration.Types.mtd_type
                  (signature_module_type items (ident :: seen))
            | _ -> None)
          items
  | Mty_ident _ | Mty_functor _ | Mty_alias _ -> None

let interface_logical_sorts ~issuer interface =
  let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
  let unit_name =
    Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
  in
  let fail reason =
    raise (Broadcast_artifact_failure { provider = unit_name; reason })
  in
  let rec unwrap typ =
    match Types.get_desc typ with
    | Types.Tpoly (body, []) | Tlink body | Tsubst (body, _) -> unwrap body
    | _ -> typ
  in
  let direct_alias_to path declaration =
    declaration.Types.type_params = []
    && declaration.type_private = Asttypes.Public
    &&
    match declaration.type_manifest with
    | Some manifest -> (
        match Types.get_desc (unwrap manifest) with
        | Types.Tconstr (candidate, [], _) -> Path.same candidate path
        | Tvar _ | Tunivar _ | Tarrow _ | Ttuple _ | Tunboxed_tuple _
        | Tconstr _ | Tobject _ | Tfield _ | Tnil | Tlink _ | Tsubst _
        | Tvariant _ | Tpoly _ | Tpackage _ | Tquote _ | Tsplice _
        | Tof_kind _ ->
            false)
    | None -> false
  in
  let direct_integer_literal_signature type_ident description =
    match Types.get_desc (unwrap description.Types.val_type) with
    | Types.Tarrow ((Types.Nolabel, _, _), domain, range, _) -> (
        match (Types.get_desc (unwrap domain), Types.get_desc (unwrap range)) with
        | ( Tconstr (domain_path, [], _),
            Tconstr (range_path, [], _) ) ->
            Path.same domain_path Predef.path_string
            && Path.same range_path (Path.Pident type_ident)
        | _ -> false)
    | _ -> false
  in
  let qualify prefix name =
    String.concat "." (unit_name :: prefix @ [ name ])
  in
  let rec scan prefix items =
    let marked_types =
      List.filter_map
        (function
          | Types.Sig_type (ident, declaration, _, Types.Exported) -> (
              match
                exact_empty_internal_marker logical_sort_marker
                  declaration.Types.type_attributes
              with
              | Ok true -> Some (ident, declaration)
              | Ok false -> None
              | Error reason -> fail reason)
          | _ -> None)
        items
    and marked_values =
      List.filter_map
        (function
          | Types.Sig_value (ident, description, Types.Exported) -> (
              match
                exact_empty_internal_marker integer_literal_marker
                  description.Types.val_attributes
              with
              | Ok true -> Some (ident, description)
              | Ok false -> None
              | Error reason -> fail reason)
          | _ -> None)
        items
    in
    let local =
      match (marked_types, marked_values) with
      | [], [] -> []
      | [ (type_ident, declaration) ], [ (value_ident, description) ] ->
          if not (direct_alias_to Predef.path_int declaration) then
            fail "logical-sort declaration is not a direct compiler int alias";
          if not (direct_integer_literal_signature type_ident description) then
            fail
              "logical-sort integer literal constructor does not have type string -> logical-sort";
          let manifest_uid =
            let environment =
              Predef.build_initial_env
                (Env.add_type ~check:false)
                (Env.add_extension ~check:false ~rebind:false)
                Env.empty
            in
            try
              compiler_uid
                (Env.find_type Predef.path_int environment).Types.type_uid
            with Not_found | Env.Error _ ->
              fail "compiler int type identity is unavailable"
          in
          let descriptor =
            match
              Logical_sort_private.create ~provider_origin:unit_name
                ~type_path:(qualify prefix (Ident.name type_ident))
                ~type_uid:(compiler_uid declaration.Types.type_uid)
                ~manifest_path:"int" ~manifest_uid
                ~integer_literal_path:
                  (qualify prefix (Ident.name value_ident))
                ~integer_literal_uid:
                  (compiler_uid description.Types.val_uid)
                ~issuer
            with
            | Ok descriptor -> descriptor
            | Error reason -> fail reason
          in
          [%log.debug "decoded authenticated logical-sort interface descriptor"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "logical-sort-interface")
            ~route:(Delator.Field.string "typed-cmi-signature")
            ~descriptor_count:(Delator.Field.int 1)
            ~decision:(Delator.Field.string "accepted")];
          [ descriptor ]
      | [], _ :: _ | _ :: _, [] ->
          fail "logical-sort type and integer literal constructor must be declared together"
      | _ :: _ :: _, _ | _, _ :: _ :: _ ->
          fail "logical-sort declaration is ambiguous within one module"
    in
    let nested =
      List.concat_map
        (function
          | Types.Sig_module
              (ident, _, declaration, _, Types.Exported) -> (
              match signature_module_type items [] declaration.Types.md_type with
              | Some nested -> scan (prefix @ [ Ident.name ident ]) nested
              | None -> [])
          | _ -> [])
        items
    in
    local @ nested
  in
  let descriptors = scan [] root |> List.sort Logical_sort_private.compare in
  if
    List.length descriptors
    <> List.length
         (List.sort_uniq Logical_sort_private.compare descriptors)
  then fail "logical-sort interface contains duplicate descriptors";
  descriptors
[@@delator.instrument] [@@delator.level debug]

type signature_type_resolution =
  | Signature_type_uid of string
  | Signature_module_alias of Path.t * string list

let signature_type_uid signature components =
  let rec module_alias_path = function
    | Types.Mty_alias path -> Some path
    | Mty_strengthen (nested, _, _) -> module_alias_path nested
    | Mty_signature _ | Mty_ident _ | Mty_functor _ -> None
  in
  let rec find items = function
    | [ name ] ->
        List.find_map
          (function
            | Types.Sig_type (ident, declaration, _, _)
              when String.equal (Ident.name ident) name ->
                Some
                  (Signature_type_uid
                     (compiler_uid declaration.Types.type_uid))
            | _ -> None)
          items
    | module_name :: rest ->
        List.find_map
          (function
            | Types.Sig_module (ident, _, declaration, _, _)
              when String.equal (Ident.name ident) module_name -> (
                [%log.trace "inspect symbolic type module path component"
                  ~stage:(Delator.Field.string "symbolic-type-identity")
                  ~module_name:(Delator.Field.string module_name)
                  ~remaining_path:
                    (Delator.Field.string (String.concat "." rest))
                  ~module_type_class:
                    (Delator.Field.string
                       (match declaration.Types.md_type with
                       | Types.Mty_signature _ -> "signature"
                       | Mty_strengthen _ -> "strengthen"
                       | Mty_ident _ -> "ident"
                       | Mty_alias _ -> "alias"
                       | Mty_functor _ -> "functor"))];
                match module_alias_path declaration.Types.md_type with
                | Some alias -> Some (Signature_module_alias (alias, rest))
                | None ->
                    Option.bind
                      (signature_module_type items [] declaration.Types.md_type)
                      (fun nested -> find nested rest))
            | _ -> None)
          items
    | [] -> None
  in
  find signature components

let persistent_type_uid ~load_paths:(load_paths [@delator.skip])
    ~(interface : Cmi_format.cmi_infos_lazy) (path [@delator.skip]) =
  let rec components path_components = function
    | Path.Pident ident -> Some (Ident.name ident, path_components)
    | Pdot (parent, name) -> components (name :: path_components) parent
    | Papply _ | Pextra_ty _ -> None
  in
  let append_components path path_components =
    List.fold_left
      (fun parent name -> Path.Pdot (parent, name))
      path path_components
  in
  let rec resolve seen authorities path =
    match components [] path with
    | None | Some (_, []) -> Ok None
    | Some (unit_name, type_path) ->
        let resolution_key = unit_name ^ "." ^ String.concat "." type_path in
        if List.mem resolution_key seen then (
          [%log.warn "rejected symbolic type module-alias cycle"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "symbolic-type-identity")
            ~route:(Delator.Field.string "authenticated-cmi-import")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "module-alias-cycle")];
          Error "symbolic type identity contains a module-alias cycle")
        else
          let imported_name = Compilation_unit.Name.of_string unit_name in
          let imports =
            authorities
            |> List.concat_map (fun authority ->
                   Array.to_list authority.Cmi_format.cmi_crcs)
            |> List.filter (fun imported ->
                   Compilation_unit.Name.equal (Import_info.name imported)
                     imported_name)
          in
          let import_crcs =
            imports |> List.filter_map Import_info.crc
            |> List.sort_uniq Digest.compare
          in
          match (imports, import_crcs) with
          | [], _ ->
              [%log.warn "rejected symbolic type identity without a compiler import"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "symbolic-type-identity")
                ~route:(Delator.Field.string "authenticated-cmi-import")
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "missing-import")];
              Error
                ("referenced type provider " ^ unit_name
               ^ " is not authenticated by the current interface imports")
          | _ :: _, [] ->
              [%log.warn "rejected symbolic type identity without an import CRC"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "symbolic-type-identity")
                ~route:(Delator.Field.string "authenticated-cmi-import")
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "missing-import-crc")];
              Error
                ("cannot authenticate imported type provider " ^ unit_name
               ^ " because its compiler import has no CRC")
          | _ :: _, _ :: _ :: _ ->
              [%log.warn "rejected ambiguous compiler imports for symbolic type identity"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "symbolic-type-identity")
                ~route:(Delator.Field.string "authenticated-cmi-import")
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:
                  (Delator.Field.string "duplicate-import-identity")];
              Error
                ("multiple compiler import identities claim symbolic type provider "
               ^ unit_name)
          | _ :: _, [ expected_crc ] -> (
              try
                match
                  find_imported_interface_artifact ~load_paths ~unit_name
                    ~expected_crc:(Digest.to_hex expected_crc) ()
                with
                | None ->
                    [%log.warn "rejected symbolic type identity without an exact imported CMI"
                      ~provider:(Delator.Field.string unit_name)
                      ~stage:(Delator.Field.string "symbolic-type-identity")
                      ~route:(Delator.Field.string "authenticated-cmi-import")
                      ~decision:(Delator.Field.string "rejected")
                      ~reason_class:
                        (Delator.Field.string "missing-exact-import")];
                    Error
                      ("cannot find the exact compiler-imported interface for type provider "
                     ^ unit_name)
                | Some artifact -> (
                    match
                      signature_type_uid
                        (Subst.Lazy.force_signature
                           artifact.artifact_interface.Cmi_format.cmi_sign)
                        type_path
                    with
                    | None ->
                        [%log.warn "rejected symbolic type path absent from its exact imported CMI"
                          ~provider:(Delator.Field.string unit_name)
                          ~stage:(Delator.Field.string "symbolic-type-identity")
                          ~route:
                            (Delator.Field.string "authenticated-cmi-import")
                          ~decision:(Delator.Field.string "rejected")
                          ~reason_class:
                            (Delator.Field.string "missing-imported-type")
                          ~type_path:
                            (Delator.Field.string
                               (String.concat "." type_path))];
                        Error
                          ("the exact compiler-imported interface for " ^ unit_name
                         ^ " does not contain the referenced type")
                    | Some (Signature_type_uid uid) ->
                        [%log.debug "resolved symbolic type identity from an exact imported CMI"
                          ~provider:(Delator.Field.string unit_name)
                          ~stage:(Delator.Field.string "symbolic-type-identity")
                          ~route:
                            (Delator.Field.string "authenticated-cmi-import")
                          ~decision:(Delator.Field.string "accepted")];
                        Ok (Some uid)
                    | Some (Signature_module_alias (alias, remaining)) ->
                        let target = append_components alias remaining in
                        [%log.debug "follow symbolic type module alias"
                          ~provider:(Delator.Field.string unit_name)
                          ~stage:(Delator.Field.string "symbolic-type-identity")
                          ~route:
                            (Delator.Field.string "authenticated-cmi-import")
                          ~alias_target:
                            (Delator.Field.string (Path.name target))
                          ~decision:(Delator.Field.string "follow")];
                        resolve (resolution_key :: seen)
                          (artifact.artifact_interface :: authorities)
                          target)
              with Failure _ ->
                [%log.warn "rejected conflicting exact imported CMIs for symbolic type identity"
                  ~provider:(Delator.Field.string unit_name)
                  ~stage:(Delator.Field.string "symbolic-type-identity")
                  ~route:(Delator.Field.string "authenticated-cmi-import")
                  ~decision:(Delator.Field.string "rejected")
                  ~reason_class:
                    (Delator.Field.string "conflicting-exact-import")];
                Error
                  ("conflicting exact compiler-imported interfaces provide type identity for "
                 ^ unit_name))
  in
  resolve [] [ interface ] path
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let predef_type_uid path =
  let environment =
    Predef.build_initial_env
      (Env.add_type ~check:false)
      (Env.add_extension ~check:false ~rebind:false)
      Env.empty
  in
  try Some (compiler_uid (Env.find_type path environment).Types.type_uid)
  with Not_found | Env.Error _ -> None

let symbolic_argument_label = function
  | Types.Nolabel -> ""
  | Labelled name -> "~" ^ name
  | Optional name -> "?" ^ name
  | Position name -> "@" ^ name

let symbolic_interface_abi ~canonical_path ~value_uid ~constructor_uid
    ~logical_sorts ~normalize typ =
  let rec quantified variables typ =
    let typ = normalize typ in
    match Types.get_desc typ with
    | Types.Tpoly (body, bound) -> quantified (variables @ bound) body
    | Tlink replacement | Tsubst (replacement, _) ->
        quantified variables replacement
    | _ -> (variables, typ)
  in
  let explicit_variables, body = quantified [] typ in
  let rec collect_variables seen typ =
    let typ = normalize typ in
    let add variable seen =
      let id = Types.get_id variable in
      if List.exists (fun candidate -> Types.get_id candidate = id) seen then
        seen
      else seen @ [ variable ]
    in
    match Types.get_desc typ with
    | Types.Tvar _ | Tunivar _ -> add typ seen
    | Tarrow (_, domain, range, _) ->
        collect_variables (collect_variables seen domain) range
    | Ttuple components | Tunboxed_tuple components ->
        List.fold_left
          (fun seen (_, component) -> collect_variables seen component)
          seen components
    | Tconstr (_, arguments, _) ->
        List.fold_left collect_variables seen arguments
    | Tpoly (nested, variables) ->
        let seen = List.fold_left (fun seen variable -> add variable seen) seen variables in
        collect_variables seen nested
    | Tlink replacement | Tsubst (replacement, _) ->
        collect_variables seen replacement
    | Tobject _ | Tfield _ | Tnil | Tvariant _ | Tpackage _ | Tquote _
    | Tsplice _ | Tof_kind _ ->
        seen
  in
  let variables = collect_variables explicit_variables body in
  let owner = Parametric_type.owner ~index:0 ~name:"symbolic-interface" in
  let binders = Parametric_type.binders owner (List.length variables) in
  let variable_binders =
    List.map2 (fun variable binder -> (Types.get_id variable, binder)) variables
      binders
  in
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let rec lower active typ =
    let typ = normalize typ in
    let id = Types.get_id typ in
    match Types.get_desc typ with
    | Types.Tvar _ | Tunivar _ -> (
        match List.assoc_opt id variable_binders with
        | Some binder -> Ok (Parametric_type.Parameter binder)
        | None -> Error "symbolic interface ABI has an unbound type parameter")
    | _ when List.mem id active ->
        Error "symbolic interface ABI contains a cyclic type expression"
    | Tpoly (body, []) | Tlink body | Tsubst (body, _) ->
        lower (id :: active) body
    | Tpoly (_, _ :: _) ->
        Error "symbolic interface ABI contains nested quantification"
    | Tconstr (path, [], _) when Path.same path Predef.path_unit ->
        Ok Parametric_type.Unit
    | Tconstr (path, [], _) when Path.same path Predef.path_bool ->
        Ok Parametric_type.Bool
    | Tconstr (path, [], _) when Path.same path Predef.path_int ->
        Ok Parametric_type.Int
    | Tconstr (path, arguments, _) ->
        let* constructor_uid =
          match constructor_uid path with
          | Some uid -> Ok uid
          | None -> Error "symbolic interface ABI type identity is unresolved"
        in
        let path_name = Path.name path in
        let logical_sort_matches =
          List.filter
            (fun descriptor ->
              String.equal descriptor.Logical_sort_private.type_path path_name
              || String.equal descriptor.type_uid constructor_uid)
            logical_sorts
        in
        let* logical_sort =
          match logical_sort_matches with
          | [] -> Ok None
          | [ _descriptor ] when arguments = [] ->
              [%log.trace "resolved symbolic interface logical-sort type"
                ~provider:
                  (Delator.Field.string
                     (_descriptor [@log_value.trace]).provider_origin)
                ~stage:(Delator.Field.string "symbolic-interface-typed-abi")
                ~route:
                  (Delator.Field.string
                     (if
                        String.equal
                          (_descriptor [@log_value.trace]).type_path path_name
                      then
                        "canonical-path"
                      else "compiler-uid"))
                ~role:(Delator.Field.string "mathematical-int")
                ~decision:(Delator.Field.string "accepted")];
              Ok (Some Parametric_type.Mathematical_int)
          | [ _ ] ->
              [%log.warn "rejected applied symbolic interface logical sort"
                ~stage:(Delator.Field.string "symbolic-interface-typed-abi")
                ~route:(Delator.Field.string "authenticated-logical-sort")
                ~argument_count:(Delator.Field.int (List.length arguments))
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:
                  (Delator.Field.string "logical-sort-type-application")];
              Error "symbolic interface logical sort has type arguments"
          | _ :: _ :: _ ->
              [%log.warn "rejected ambiguous symbolic interface logical sort"
                ~stage:(Delator.Field.string "symbolic-interface-typed-abi")
                ~route:(Delator.Field.string "authenticated-logical-sort")
                ~candidate_count:
                  (Delator.Field.int (List.length logical_sort_matches))
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "ambiguous-logical-sort")];
              Error "symbolic interface logical-sort identity is ambiguous"
        in
        (match logical_sort with
        | Some typ -> Ok typ
        | None ->
        let rec lower_arguments lowered = function
          | [] -> Ok (List.rev lowered)
          | argument :: rest ->
              let* argument = lower [] argument in
              lower_arguments (argument :: lowered) rest
        in
        let* arguments = lower_arguments [] arguments in
        Ok
          (Parametric_type.Application
             ( {
                 constructor_path = Path.name path;
                 constructor_identity = "compiler-uid:" ^ constructor_uid;
               },
               arguments )))
    | Ttuple components | Tunboxed_tuple components ->
        let rec lower_components lowered = function
          | [] -> Ok (List.rev lowered)
          | (label, component) :: rest ->
              let* component = lower [] component in
              lower_components ((label, component) :: lowered) rest
        in
        Result.map
          (fun components -> Parametric_type.Tuple components)
          (lower_components [] components)
    | Tarrow ((label, _, _), domain, range, _) -> (
        match label with
        | Types.Optional _ | Position _ ->
            [%log.warn "rejected unsupported symbolic interface callback label"
              ~stage:(Delator.Field.string "symbolic-interface-typed-abi")
              ~route:(Delator.Field.string "canonical-spec-function")
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "unsupported-arrow-label")];
            Error "symbolic interface ABI contains an unsupported callback label"
        | Nolabel | Labelled _ ->
            let label =
              match label with
              | Types.Nolabel -> None
              | Labelled name -> Some name
              | Optional _ | Position _ -> assert false
            in
            let* domain = lower (id :: active) domain in
            let* range = lower (id :: active) range in
            [%log.trace "lowered symbolic interface callback type"
              ~stage:(Delator.Field.string "symbolic-interface-typed-abi")
              ~route:(Delator.Field.string "canonical-spec-function")
              ~label_class:
                (Delator.Field.string
                   (Option.fold ~none:"unlabelled" ~some:(fun _ -> "labelled")
                      label))
              ~decision:(Delator.Field.string "accepted")];
            Ok (Parametric_type.spec_function ~label ~domain ~range))
    | Tobject _ | Tfield _ | Tnil | Tvariant _ | Tpackage _ | Tquote _
    | Tsplice _ | Tof_kind _ ->
        Error "symbolic interface ABI contains an unsupported type"
  in
  let rec parameters reversed typ =
    let typ = normalize typ in
    match Types.get_desc typ with
    | Types.Tarrow ((label, _, _), argument, result, _) ->
        parameters ((symbolic_argument_label label, argument) :: reversed) result
    | Tpoly (body, []) | Tlink body | Tsubst (body, _) ->
        parameters reversed body
    | _ -> (List.rev reversed, typ)
  in
  let parameters, result = parameters [] body in
  let rec lower_parameters labels types = function
    | [] -> Ok (List.rev labels, List.rev types)
    | (label, typ) :: rest ->
        let* typ = lower [] typ in
        lower_parameters (label :: labels) (typ :: types) rest
  in
  let* parameter_labels, parameter_types = lower_parameters [] [] parameters in
  let* result_type = lower [] result in
  Symbolic_application_private.declaration_abi_material ~canonical_path
    ~value_uid ~type_binders:binders ~parameter_labels ~parameter_types
    ~result_type

let interface_symbolic_declarations ~load_paths interface =
  let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
  let unit_name =
    Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
  in
  let fail reason =
    raise (Symbolic_artifact_failure { provider = unit_name; reason })
  in
  let rec local_type_uids prefix items =
    List.concat_map
      (function
        | Types.Sig_type (ident, declaration, _, _) ->
            let path =
              match prefix with
              | None -> Path.Pident ident
              | Some parent -> Path.Pdot (parent, Ident.name ident)
            in
            [
              ( path,
                local_implementation_type_uid declaration.Types.type_uid );
            ]
        | Types.Sig_module (ident, _, declaration, _, Types.Exported) -> (
            match signature_module_type items [] declaration.Types.md_type with
            | None -> []
            | Some nested ->
                let path =
                  match prefix with
                  | None -> Path.Pident ident
                  | Some parent -> Path.Pdot (parent, Ident.name ident)
                in
                local_type_uids (Some path) nested)
        | Types.Sig_value _ | Types.Sig_module (_, _, _, _, Types.Hidden)
        | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
        | Types.Sig_class_type _ ->
            [])
      items
  in
  let local_type_uids = local_type_uids None root in
  let typing_environment =
    let initial =
      Predef.build_initial_env
        (Env.add_type ~check:false)
        (Env.add_extension ~check:false ~rebind:false)
        Env.empty
    in
    Env.add_signature root initial
  in
  let normalize typ =
    try Ctype.expand_head typing_environment typ with Env.Error _ -> typ
  in
  let logical_sorts =
    lazy
      (let descriptors_for interface =
         let issuer =
           match interface_family_issuers interface with
           | [ issuer ] -> issuer
           | [] | _ :: _ :: _ -> ""
         in
         interface_logical_sorts ~issuer interface
       in
       let interface_identity interface =
         let unit_name =
           Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
         in
         let self_crc =
           Array.to_list interface.Cmi_format.cmi_crcs
           |> List.find_map (fun imported ->
                  if
                    Compilation_unit.Name.equal (Import_info.name imported)
                      interface.Cmi_format.cmi_name
                  then Option.map Digest.to_hex (Import_info.crc imported)
                  else None)
           |> Option.value ~default:"missing-self-crc"
         in
         unit_name ^ "@" ^ self_crc
       in
       let rec transitive_descriptors seen interface =
         let identity = interface_identity interface in
         if List.mem identity seen then (seen, [])
         else
           let seen = identity :: seen in
           authenticated_import_interfaces ~load_paths interface
           |> List.fold_left
                (fun (seen, descriptors) artifact ->
                  let seen, nested =
                    transitive_descriptors seen artifact.artifact_interface
                  in
                  (seen, descriptors @ nested))
                (seen, descriptors_for interface)
       in
       let _, descriptors = transitive_descriptors [] interface in
       let[@log_value.trace] local = descriptors_for interface in
       let[@log_value.trace] imported =
         List.filter
           (fun descriptor ->
             not
               (List.exists
                  (fun local_descriptor ->
                    Logical_sort_private.compare descriptor local_descriptor = 0)
                  (local [@log_value.trace])))
           descriptors
       in
       let descriptors =
         List.sort_uniq Logical_sort_private.compare descriptors
       in
       [%log.trace "assembled symbolic interface logical-sort environment"
         ~provider:(Delator.Field.string unit_name)
         ~stage:(Delator.Field.string "symbolic-interface-typed-abi")
         ~route:(Delator.Field.string "authenticated-cmi-imports")
         ~local_descriptor_count:
           (Delator.Field.int (List.length (local [@log_value.trace])))
         ~imported_descriptor_count:
           (Delator.Field.int (List.length (imported [@log_value.trace])))
         ~descriptor_count:(Delator.Field.int (List.length descriptors))
         ~decision:(Delator.Field.string "assembled")];
       descriptors)
  in
  let constructor_uid path =
    match
      List.find_map
        (fun (candidate, uid) -> if Path.same candidate path then Some uid else None)
        local_type_uids
    with
    | Some _ as uid -> uid
    | None -> (
        match predef_type_uid path with
        | Some _ as uid -> uid
        | None -> (
            match persistent_type_uid ~load_paths ~interface path with
            | Ok uid -> uid
            | Error reason -> fail reason))
  in
  let rec signature prefix items =
    let module_types =
      List.filter_map
        (function
          | Types.Sig_modtype (ident, declaration, _) ->
              Option.map (fun typ -> (ident, typ)) declaration.Types.mtd_type
          | _ -> None)
        items
    in
    let rec module_signature seen = function
      | Types.Mty_signature nested -> Some nested
      | Mty_strengthen (nested, _, _) -> module_signature seen nested
      | Mty_ident (Path.Pident ident) -> (
          if List.exists (Ident.same ident) seen then None
          else
            match
              List.find_opt
                (fun (candidate, _) -> Ident.same candidate ident)
                module_types
            with
            | Some (_, nested) -> module_signature (ident :: seen) nested
            | None -> None)
      | Mty_ident _ | Mty_functor _ | Mty_alias _ -> None
    in
    List.concat_map
      (function
        | Types.Sig_value (ident, description, Types.Exported) -> (
            let marker =
              try
                symbolic_interface_attribute description.Types.val_attributes
              with Symbolic_marker_failure reason -> fail reason
            in
            match marker with
            | None -> []
            | Some (payload_name, symbolic_marker, symbolic_type_digest) ->
                let name = Ident.name ident in
                if not (String.equal name payload_name) then
                  fail "symbolic interface marker name mismatch"
                else
                  let relative =
                    String.concat "." (List.rev (name :: prefix))
                  in
                  let symbolic_path = unit_name ^ "." ^ relative in
                  let symbolic_uid = compiler_uid description.Types.val_uid in
                  let symbolic_typed_abi =
                    match
                      symbolic_interface_abi ~canonical_path:symbolic_path
                        ~value_uid:symbolic_uid ~constructor_uid
                        ~logical_sorts:(Lazy.force logical_sorts)
                        ~normalize
                        description.Types.val_type
                    with
                    | Ok material -> material
                    | Error _ ->
                        fail
                          "symbolic interface typed ABI is malformed or unsupported"
                  in
                  [
                    {
                      symbolic_path;
                      symbolic_uid;
                      symbolic_marker;
                      symbolic_type_digest;
                      symbolic_typed_abi;
                    };
                  ])
        | Types.Sig_module
            (ident, _, declaration, _, Types.Exported) -> (
            match module_signature [] declaration.Types.md_type with
            | Some nested -> signature (Ident.name ident :: prefix) nested
            | None -> [])
        | Types.Sig_value (_, _, Types.Hidden)
        | Types.Sig_module (_, _, _, _, Types.Hidden)
        | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
        | Types.Sig_class _ | Types.Sig_class_type _ ->
            [])
      items
  in
  let declarations = signature [] root in
  let paths = List.map (fun item -> item.symbolic_path) declarations in
  if List.length paths <> List.length (List.sort_uniq String.compare paths) then
    fail "duplicate symbolic interface path"
  else declarations

let interface_logical_values ~load_paths ~symbolic_declarations interface =
  let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
  let unit_name =
    Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
  in
  let fail reason =
    raise (Symbolic_artifact_failure { provider = unit_name; reason })
  in
  let attributes name attributes =
    let matching =
      List.filter
      (fun attribute -> String.equal attribute.Parsetree.attr_name.txt name)
      attributes
    in
    if
      List.exists
        (fun attribute -> attribute.Parsetree.attr_payload <> Parsetree.PStr [])
        matching
    then fail ("logical value marker " ^ name ^ " has a nonempty payload");
    matching
  in
  let rec local_type_uids prefix items =
    List.concat_map
      (function
        | Types.Sig_type (ident, declaration, _, _) ->
            let path =
              match prefix with
              | None -> Path.Pident ident
              | Some parent -> Path.Pdot (parent, Ident.name ident)
            in
            [ (path, local_implementation_type_uid declaration.Types.type_uid) ]
        | Types.Sig_module (ident, _, declaration, _, Types.Exported) -> (
            match signature_module_type items [] declaration.Types.md_type with
            | None -> []
            | Some nested ->
                let path =
                  match prefix with
                  | None -> Path.Pident ident
                  | Some parent -> Path.Pdot (parent, Ident.name ident)
                in
                local_type_uids (Some path) nested)
        | Types.Sig_value _ | Types.Sig_module (_, _, _, _, Types.Hidden)
        | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
        | Types.Sig_class_type _ -> [])
      items
  in
  let local_type_uids = local_type_uids None root in
  let constructor_uid path =
    match List.find_map
            (fun (candidate, uid) -> if Path.same candidate path then Some uid else None)
            local_type_uids with
    | Some _ as uid -> uid
    | None -> (
        match predef_type_uid path with
        | Some _ as uid -> uid
        | None -> (
            match persistent_type_uid ~load_paths ~interface path with
            | Ok uid -> uid
            | Error _ -> None))
  in
  let initial =
    Predef.build_initial_env
      (Env.add_type ~check:false)
      (Env.add_extension ~check:false ~rebind:false)
      Env.empty
  in
  let typing_environment = Env.add_signature root initial in
  let normalize typ =
    try Ctype.expand_head typing_environment typ with Env.Error _ -> typ
  in
  let rec logical_sort_closure seen interface =
    let identity =
      Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
      ^ Option.value ~default:""
          (interface_self_crc interface)
    in
    if List.mem identity seen then (seen, [])
    else
      let seen = identity :: seen in
      let issuer =
        match interface_family_issuers interface with
        | [ issuer ] -> issuer
        | [] | _ :: _ :: _ -> ""
      in
      authenticated_import_interfaces ~load_paths interface
      |> List.fold_left
           (fun (seen, sorts) artifact ->
             let seen, imported =
               logical_sort_closure seen artifact.artifact_interface
             in
             (seen, sorts @ imported))
           (seen, interface_logical_sorts ~issuer interface)
  in
  let _, logical_sorts = logical_sort_closure [] interface in
  let logical_sorts =
    List.sort_uniq Logical_sort_private.compare logical_sorts
  in
  let rec value_parameter_count count typ =
    let typ = normalize typ in
    match Types.get_desc typ with
    | Types.Tarrow (_, _, result, _) -> value_parameter_count (count + 1) result
    | Tpoly (body, _) | Tlink body | Tsubst (body, _) ->
        value_parameter_count count body
    | Tvar _ | Tunivar _ | Ttuple _ | Tunboxed_tuple _ | Tconstr _
    | Tobject _ | Tfield _ | Tnil | Tvariant _ | Tpackage _
    | Tquote _ | Tsplice _ | Tof_kind _ ->
        count
  in
  let typed_abi path uid typ =
    symbolic_interface_abi ~canonical_path:path ~value_uid:uid
      ~constructor_uid ~logical_sorts ~normalize typ
  in
  let rec module_signature module_types seen = function
    | Types.Mty_signature signature -> Some signature
    | Mty_strengthen (nested, _, _) -> module_signature module_types seen nested
    | Mty_ident (Path.Pident ident) ->
        if List.exists (Ident.same ident) seen then None
        else
          List.find_map
            (fun (candidate, nested) ->
              if Ident.same candidate ident then
                module_signature module_types (ident :: seen) nested
              else None)
            module_types
    | Mty_ident _ | Mty_functor _ | Mty_alias _ -> None
  in
  let rec collect prefix inherited_module_types items =
    let module_types =
      List.fold_left
        (fun collected -> function
          | Types.Sig_modtype (ident, declaration, Types.Exported) ->
              Option.fold ~none:collected
                ~some:(fun nested -> (ident, nested) :: collected)
                declaration.Types.mtd_type
          | _ -> collected)
        inherited_module_types items
    in
    let qualify name =
      let relative = String.concat "." (List.rev (name :: prefix)) in
      unit_name ^ "." ^ relative
    in
    List.concat_map
      (function
        | Types.Sig_value (ident, description, Types.Exported) ->
            let path = qualify (Ident.name ident) in
            let uid = compiler_uid description.Types.val_uid in
            let symbolic =
              List.filter
                (fun declaration ->
                  String.equal declaration.symbolic_path path
                  && String.equal declaration.symbolic_uid uid)
                symbolic_declarations
            in
            let specs = attributes "verocaml.spec" description.val_attributes
            and opaques = attributes "verocaml.opaque" description.val_attributes
            and revealed = attributes "verocaml.revealed" description.val_attributes in
            if List.length specs > 1 || List.length opaques > 1
               || List.length revealed > 1
               || (opaques <> [] && revealed <> [])
            then fail "logical value has duplicate or conflicting visibility markers";
            if symbolic = [] && specs = [] && (opaques <> [] || revealed <> []) then
              fail "logical value visibility marker is not attached to a specification";
            let parameter_count = value_parameter_count 0 description.val_type in
            (match (symbolic, specs, parameter_count) with
            | [ receipt ], [], 0 ->
                if opaques <> [] || revealed <> [] then
                  fail "symbolic logical value carries defined visibility";
                let logical_value_class =
                  Retained_interface_authority_private.Symbolic_value
                and logical_value_visibility =
                  Retained_interface_authority_private.Symbolic_opaque
                in
                let logical_value_descriptor_receipt =
                  Retained_interface_authority_private.logical_value_receipt
                    ~path ~uid ~marker:receipt.symbolic_marker
                    ~typed_abi:receipt.symbolic_typed_abi logical_value_class
                    logical_value_visibility
                in
                [ { Retained_interface_authority_private.logical_value_path = path;
                    logical_value_uid = uid;
                    logical_value_marker = receipt.symbolic_marker;
                    logical_value_typed_abi = receipt.symbolic_typed_abi;
                    logical_value_class;
                    logical_value_visibility;
                    logical_value_descriptor_receipt } ]
            | [], [ spec ], 0 ->
                let logical_value_class =
                  Retained_interface_authority_private.Defined_value
                and logical_value_visibility =
                  if revealed = [] then
                    Retained_interface_authority_private.Defined_opaque
                  else Defined_revealed
                in
                let logical_value_typed_abi =
                  match typed_abi path uid description.val_type with
                  | Ok abi -> abi
                  | Error _ -> fail "defined logical value ABI is unsupported"
                in
                let marker =
                  Digest.string
                    (String.concat "\000"
                       [ "defined-logical-value-marker-v1"; path; uid;
                         logical_value_typed_abi;
                         string_of_int spec.attr_loc.loc_start.pos_cnum ])
                  |> Digest.to_hex
                in
                let logical_value_descriptor_receipt =
                  Retained_interface_authority_private.logical_value_receipt
                    ~path ~uid ~marker ~typed_abi:logical_value_typed_abi
                    logical_value_class logical_value_visibility
                in
                [ { Retained_interface_authority_private.logical_value_path = path;
                    logical_value_uid = uid; logical_value_marker = marker;
                    logical_value_typed_abi; logical_value_class;
                    logical_value_visibility;
                    logical_value_descriptor_receipt } ]
            | [], [], _ | [ _ ], [], _ | [], [ _ ], _ -> []
            | [], _ :: _ :: _, _ | _ :: _ :: _, _, _ | [ _ ], _ :: _, _ ->
                fail "logical value semantic collection is ambiguous")
        | Types.Sig_module (ident, _, declaration, _, Types.Exported) -> (
            match module_signature module_types [] declaration.Types.md_type with
            | Some nested -> collect (Ident.name ident :: prefix) module_types nested
            | None -> [])
        | Types.Sig_value (_, _, Types.Hidden)
        | Types.Sig_module (_, _, _, _, Types.Hidden)
        | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
        | Types.Sig_class _ | Types.Sig_class_type _ -> [])
      items
  in
  let values = collect [] [] root in
  [%log.debug "classified retained logical-value interface"
    ~provider:(Delator.Field.string unit_name)
    ~stage:(Delator.Field.string "logical-value-interface")
    ~route:(Delator.Field.string "compiler-signature")
    ~value_count:(Delator.Field.int (List.length values))
    ~decision:(Delator.Field.string "classified")];
  values
[@@delator.instrument] [@@delator.level debug]

type numeric_type_entry = {
  numeric_type_path : string;
  numeric_type_uid : string;
  numeric_type_declaration : Types.type_declaration;
  numeric_type_owner : Cmi_format.cmi_infos_lazy;
  numeric_type_owner_artifact : authenticated_interface_artifact;
  numeric_type_owner_unit : string;
  numeric_type_owner_cmi_full_key : string;
  numeric_type_owner_cmi_checked_digest : string;
  numeric_type_import_routes : string list;
}

type numeric_value_entry = {
  numeric_value_path : string;
  numeric_value_uid : string;
  numeric_value_description : Types.value_description;
  numeric_value_owner : Cmi_format.cmi_infos_lazy;
  numeric_value_owner_artifact : authenticated_interface_artifact;
  numeric_value_owner_unit : string;
  numeric_value_owner_cmi_full_key : string;
  numeric_value_owner_cmi_checked_digest : string;
  numeric_value_import_routes : string list;
}

type numeric_typed_role_resolution = {
  numeric_typed_callable_path : string;
  numeric_typed_callable_uid : string;
  numeric_typed_source : Numeric_source_claim_private.role;
  numeric_typed_source_claim : string;
  numeric_typed_carrier_path : string;
  numeric_typed_carrier_uid : string;
  numeric_typed_semantics_path : string;
  numeric_typed_semantics_uid : string;
}

type numeric_implementation_value_slot = {
  numeric_implementation_value_path : string;
  numeric_implementation_value_uid : string;
  numeric_implementation_value_environment : Env.t;
  numeric_implementation_value_attributes : Parsetree.attributes;
  numeric_implementation_value_location : Location.t;
}

type numeric_implementation_type_slot = {
  numeric_implementation_type_path : string;
  numeric_implementation_type_uid : string;
  numeric_implementation_type_environment : Env.t;
  numeric_implementation_type_attributes : Parsetree.attributes;
  numeric_implementation_type_location : Location.t;
}

let maximum_numeric_provenance_nodes = 10_000
let maximum_numeric_provenance_edges = 100_000
let maximum_numeric_provenance_bytes = 8 * 1024 * 1024
let maximum_numeric_load_paths = 4_096
let maximum_numeric_load_path_bytes = 1024 * 1024
let maximum_numeric_artifact_probes = 100_000
let maximum_numeric_artifact_probe_bytes = 64 * 1024 * 1024

let validate_numeric_load_path_sequences ~provider path_sequences =
  let reject count reason = Error (provider ^ ": " ^ reason, count) in
  let rec scan_paths count total paths =
    match paths () with
    | Seq.Nil -> Ok (count, total)
    | Seq.Cons (path, rest) ->
        if count >= maximum_numeric_load_paths then
          reject count "numeric load-path inventory exceeds its record bound"
        else if String.length path > maximum_numeric_load_path_bytes - total then
          reject count "numeric load-path inventory exceeds its byte bound"
        else scan_paths (count + 1) (total + String.length path) rest
  in
  let rec scan_sequences count total = function
    | [] -> Ok (count, total)
    | paths :: rest -> (
        match scan_paths count total paths with
        | Ok (count, total) -> scan_sequences count total rest
        | Error _ as error -> error)
  in
  let result =
    scan_sequences 0 0 path_sequences
  in
  (match result with
  | Ok ((path_count [@log_value.trace]), _) ->
      [%log.trace "accepted bounded numeric load-path inventory"
        ~provider:(Delator.Field.string provider)
        ~stage:(Delator.Field.string "numeric-load-path-preflight")
        ~path_count:(Delator.Field.int (path_count [@log_value.trace]))
        ~decision:(Delator.Field.string "accepted")]
  | Error ((reason [@log_value.warn]), (path_count [@log_value.warn])) ->
      [%log.warn "rejected excessive numeric load-path inventory"
        ~provider:(Delator.Field.string provider)
        ~stage:(Delator.Field.string "numeric-load-path-preflight")
        ~path_count:(Delator.Field.int (path_count [@log_value.warn]))
        ~reason_class:(Delator.Field.string (reason [@log_value.warn]))
        ~decision:(Delator.Field.string "rejected")]);
  match result with Ok _ -> Ok () | Error (reason, _) -> Error reason
[@@delator.instrument] [@@delator.level trace]

let validate_numeric_load_path_inventory ~provider paths =
  validate_numeric_load_path_sequences ~provider [ List.to_seq paths ]

let interface_has_numeric_markers interface =
  let rec has items =
    List.exists
      (function
        | Types.Sig_type (_, declaration, _, _) ->
            List.exists
              (fun attribute ->
                String.equal attribute.Parsetree.attr_name.txt
                  Numeric_source_claim_private.carrier_marker)
              declaration.Types.type_attributes
        | Types.Sig_value (_, description, _) ->
            List.exists
              (fun attribute ->
                String.equal attribute.Parsetree.attr_name.txt
                  Numeric_source_claim_private.role_marker)
              description.Types.val_attributes
        | Types.Sig_module (_, _, declaration, _, Types.Exported) -> (
            match signature_module_type items [] declaration.Types.md_type with
            | Some nested -> has nested
            | None -> false)
        | Types.Sig_module (_, _, _, _, Types.Hidden) | Types.Sig_typext _
        | Types.Sig_modtype _ | Types.Sig_class _ | Types.Sig_class_type _ ->
            false)
      items
  in
  has (Subst.Lazy.force_signature interface.Cmi_format.cmi_sign)

let interface_numeric_claims ~load_paths ~root_cmi_receipt ?typed_interface
    ?implementation_shape ?structure interface =
  let unit_name =
    Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
  in
  let fail ?location reason =
    [%log.warn "rejected numeric source claim reconstruction"
      ~provider:(Delator.Field.string unit_name)
      ~stage:(Delator.Field.string "numeric-source-cmi-reconstruction")
      ~route:(Delator.Field.string "exact-compiler-signature")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string reason)];
    raise (Numeric_claim_failure { provider = unit_name; reason; location })
  in
  if not (interface_has_numeric_markers interface) then (
    [%log.trace "skipped numeric dependency provenance for nonnumeric provider"
      ~provider:(Delator.Field.string unit_name)
      ~stage:(Delator.Field.string "numeric-source-cmi-reconstruction")
      ~route:(Delator.Field.string "marker-prescan")
      ~decision:(Delator.Field.string "absent")];
    { numeric_carriers = []; numeric_roles = []; numeric_provenance_nodes = 0;
      numeric_provenance_edges = 0; numeric_provenance_bytes = 0 })
  else
  let load_path_count = List.length load_paths in
  let load_path_bytes =
    List.fold_left
      (fun total path ->
        if
          String.length path > maximum_numeric_load_path_bytes - total
        then fail "numeric load-path inventory exceeds its byte bound"
        else total + String.length path)
      0 load_paths
  in
  if
    load_path_count > maximum_numeric_load_paths
    || load_path_bytes > maximum_numeric_load_path_bytes
  then fail "numeric load-path inventory exceeds its bounded size";
  let cmi_full_key artifact =
    let imports =
      imports_of_array artifact.artifact_interface.Cmi_format.cmi_crcs
      |> Array.to_list
    in
    let import_units = List.map (fun (imported : import) -> imported.unit_name) imports in
    if
      List.length import_units
      <> List.length (List.sort_uniq String.compare import_units)
    then fail "numeric owner CMI contains duplicate import units";
    Numeric_interface_claim_private.owner_cmi_full_key
      ~unit_name:(Compilation_unit.Name.to_string artifact.artifact_interface.Cmi_format.cmi_name)
      ~self_crc:artifact.artifact_self_crc ~content_receipt:artifact.artifact_content_digest
      ~imports:(List.map (fun (imported : import) -> imported.unit_name, imported.crc) imports)
  in
  let root_artifact =
    let self_crc =
      match interface_self_crc interface with
      | Some self_crc -> self_crc
      | None -> fail "numeric source owner CMI lacks an exact self CRC"
    in
    { artifact_filename = ""; artifact_interface = interface;
      artifact_content_digest = root_cmi_receipt;
      artifact_self_crc = self_crc }
  in
  let graph_nodes = Hashtbl.create 32
  and graph_edges = Hashtbl.create 32
  and canonical_routes = Hashtbl.create 32
  and node_count = ref 0
  and edge_count = ref 0
  and graph_bytes = ref 0
  and import_record_count = ref 0
  and import_record_bytes = ref 0
  and artifact_probe_count = ref 0
  and artifact_probe_bytes = ref 0 in
  let reserve ~kind bytes =
    let count_exceeded =
      match kind with
      | `Node -> !node_count >= maximum_numeric_provenance_nodes
      | `Edge -> !edge_count >= maximum_numeric_provenance_edges
      | `Route -> false
    in
    if count_exceeded || bytes > maximum_numeric_provenance_bytes - !graph_bytes
    then (
      [%log.warn "rejected excessive numeric dependency provenance"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "numeric-source-cmi-reconstruction")
        ~route:(Delator.Field.string "canonical-import-graph")
        ~node_count:(Delator.Field.int !node_count)
        ~edge_count:(Delator.Field.int !edge_count)
        ~provenance_bytes:(Delator.Field.int !graph_bytes)
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:
          (Delator.Field.string
             (if count_exceeded then "record-count-bound" else "byte-bound"))];
      fail "numeric source claim import graph exceeds its bounded provenance budget"
    );
    graph_bytes := !graph_bytes + bytes;
    match kind with
    | `Node -> incr node_count
    | `Edge -> incr edge_count
    | `Route -> ()
  in
  let reserve_import_inventory artifact =
    let raw = artifact.artifact_interface.Cmi_format.cmi_crcs in
    let raw_count = Array.length raw in
    if
      raw_count > maximum_numeric_provenance_edges - !import_record_count
    then fail "numeric raw import vector exceeds its record bound";
    Array.iter
      (fun imported ->
        let bytes =
          String.length
            (Compilation_unit.Name.to_string (Import_info.name imported))
          + Option.fold ~none:0 ~some:(fun _ -> 32) (Import_info.crc imported)
          + 16
        in
        if
          bytes > maximum_numeric_provenance_bytes - !import_record_bytes
        then fail "numeric raw import vector exceeds its byte bound";
        import_record_bytes := !import_record_bytes + bytes)
      raw;
    import_record_count := !import_record_count + raw_count;
    let probes_per_import = max 1 (2 * load_path_count) in
    if
      raw_count
      > (maximum_numeric_artifact_probes - !artifact_probe_count)
        / probes_per_import
    then fail "numeric import path probes exceed their record bound";
    artifact_probe_count :=
      !artifact_probe_count + (raw_count * probes_per_import)
  in
  let admit_candidate filename =
    let bytes = (Unix.stat filename).Unix.st_size in
    if
      bytes < 0
      || bytes > maximum_numeric_artifact_probe_bytes - !artifact_probe_bytes
    then Error "numeric imported artifact reads exceed their byte bound"
    else (
      artifact_probe_bytes := !artifact_probe_bytes + bytes;
      Ok ())
  in
  let rec collect_graph parent_route parent_route_bytes artifact =
    let parent_key = cmi_full_key artifact in
    if not (Hashtbl.mem graph_nodes parent_key) then (
      reserve ~kind:`Node (String.length parent_key);
      let route_bytes = parent_route_bytes + String.length parent_key + 32 in
      reserve ~kind:`Route route_bytes;
      let route = parent_route @ [ parent_key ] in
      Hashtbl.add graph_nodes parent_key artifact;
      Hashtbl.add canonical_routes parent_key route;
      reserve_import_inventory artifact;
      let children =
        authenticated_import_interfaces ~admit_candidate ~load_paths
          artifact.artifact_interface
        |> List.map (fun child -> (cmi_full_key child, child))
        |> List.sort (fun (left, _) (right, _) -> String.compare left right)
      in
      let child_keys =
        children |> List.map fst |> List.sort_uniq String.compare
      in
      List.iter
        (fun child_key ->
          reserve ~kind:`Edge
            (String.length parent_key + String.length child_key))
        child_keys;
      Hashtbl.add graph_edges parent_key child_keys;
      List.iter
        (fun (_, child) -> collect_graph route route_bytes child)
        children)
  in
  collect_graph [] 0 root_artifact;
  let interfaces =
    Hashtbl.to_seq graph_nodes |> List.of_seq
    |> List.map (fun (full_key, artifact) ->
           let route =
             match Hashtbl.find_opt canonical_routes full_key with
             | Some route -> Numeric_receipt_private.list route
             | None -> fail "numeric import graph contains an unreachable owner"
           in
           (artifact, [ route ]))
    |> List.sort (fun (left, _) (right, _) ->
           String.compare (cmi_full_key left) (cmi_full_key right))
  in
  [%log.debug "constructed bounded canonical numeric dependency provenance"
    ~provider:(Delator.Field.string unit_name)
    ~stage:(Delator.Field.string "numeric-source-cmi-reconstruction")
    ~route:(Delator.Field.string "canonical-import-graph")
    ~node_count:(Delator.Field.int !node_count)
    ~edge_count:(Delator.Field.int !edge_count)
    ~provenance_bytes:(Delator.Field.int !graph_bytes)
    ~import_record_count:(Delator.Field.int !import_record_count)
    ~artifact_probe_count:(Delator.Field.int !artifact_probe_count)
    ~artifact_probe_bytes:(Delator.Field.int !artifact_probe_bytes)
    ~decision:(Delator.Field.string "accepted")];
  let module_signature items declaration =
    signature_module_type items [] declaration.Types.md_type
  in
  let collect_entries (owner_artifact, import_routes) =
    let owner = owner_artifact.artifact_interface in
    let owner_cmi_full_key = cmi_full_key owner_artifact in
    let owner_cmi_checked_digest =
      Numeric_receipt_private.digest
        ~domain:"verocaml.numeric-owner-cmi-full-key.v1"
        owner_cmi_full_key
    in
    let owner_name =
      Compilation_unit.Name.to_string owner.Cmi_format.cmi_name
    in
    let root = Subst.Lazy.force_signature owner.Cmi_format.cmi_sign in
    let qualify prefix name =
      String.concat "." (owner_name :: prefix @ [ name ])
    in
    let rec collect prefix items =
      List.fold_left
        (fun (types, values) -> function
          | Types.Sig_type (ident, declaration, _, Types.Exported) ->
              ( { numeric_type_path = qualify prefix (Ident.name ident);
                  numeric_type_uid = compiler_uid declaration.Types.type_uid;
                  numeric_type_declaration = declaration;
                  numeric_type_owner = owner;
                  numeric_type_owner_artifact = owner_artifact;
                  numeric_type_owner_unit = owner_name;
                  numeric_type_owner_cmi_full_key = owner_cmi_full_key;
                  numeric_type_owner_cmi_checked_digest =
                    owner_cmi_checked_digest;
                  numeric_type_import_routes = import_routes }
                :: types,
                values )
          | Types.Sig_value (ident, description, Types.Exported) ->
              ( types,
                { numeric_value_path = qualify prefix (Ident.name ident);
                  numeric_value_uid = compiler_uid description.Types.val_uid;
                  numeric_value_description = description;
                  numeric_value_owner = owner;
                  numeric_value_owner_artifact = owner_artifact;
                  numeric_value_owner_unit = owner_name;
                  numeric_value_owner_cmi_full_key = owner_cmi_full_key;
                  numeric_value_owner_cmi_checked_digest =
                    owner_cmi_checked_digest;
                  numeric_value_import_routes = import_routes }
                :: values )
          | Types.Sig_module (ident, _, declaration, _, Types.Exported) -> (
              match module_signature items declaration with
              | None -> (types, values)
              | Some nested ->
                  let nested_types, nested_values =
                    collect (prefix @ [ Ident.name ident ]) nested
                  in
                  (nested_types @ types, nested_values @ values))
          | Types.Sig_type (_, _, _, Types.Hidden)
          | Types.Sig_value (_, _, Types.Hidden)
          | Types.Sig_module (_, _, _, _, Types.Hidden)
          | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
          | Types.Sig_class_type _ ->
              (types, values))
        ([], []) items
    in
    collect [] root
  in
  let type_entries, value_entries =
    List.fold_left
      (fun (types, values) routed_owner ->
        let owner_types, owner_values = collect_entries routed_owner in
        (owner_types @ types, owner_values @ values))
      ([], []) interfaces
  in
  let exact_markers marker attributes =
    List.filter
      (fun attribute ->
        String.equal attribute.Parsetree.attr_name.txt marker)
      attributes
  in
  let parse_one marker parse attributes =
    match exact_markers marker attributes with
    | [] -> None
    | [ attribute ] -> (
        match parse attribute with
        | Ok claim -> Some (claim, attribute.Parsetree.attr_loc)
        | Error reason -> fail ~location:attribute.attr_loc reason)
    | duplicate :: _ :: _ ->
        fail ~location:duplicate.Parsetree.attr_loc
          "duplicate numeric source claim marker"
  in
  let uid_owner = function
    | Types.Uid.Item { comp_unit; _ } -> Some comp_unit
    | Compilation_unit compilation_unit -> Some compilation_unit
    | Internal | Predef _ | Unboxed_version _ -> None
  in
  let original_type_entries uid =
    let candidates =
      List.filter (fun entry -> String.equal entry.numeric_type_uid uid)
        type_entries
    in
    match candidates with
    | [] | [ _ ] -> candidates
    | _ ->
        List.filter
          (fun entry ->
            uid_owner entry.numeric_type_declaration.Types.type_uid
            = Some entry.numeric_type_owner_unit)
          candidates
  and original_value_entries uid =
    let candidates =
      List.filter (fun entry -> String.equal entry.numeric_value_uid uid)
        value_entries
    in
    match candidates with
    | [] | [ _ ] -> candidates
    | _ ->
        List.filter
          (fun entry ->
            uid_owner entry.numeric_value_description.Types.val_uid
            = Some entry.numeric_value_owner_unit)
          candidates
  in
  let source_longident ~location path =
    match Longident.unflatten (String.split_on_char '.' path) with
    | Some path -> path
    | None -> fail ~location "numeric source reference has no compiler path"
  in
  let resolution_artifacts = List.map fst interfaces
  in
  let resolve_paths_in_environment environment location ~carrier_reference
      ~semantics_reference =
    with_authenticated_interfaces resolution_artifacts (fun () ->
        try
          let environment =
            Envaux.env_of_only_summary ~allow_missing_modules:false environment
          in
          let exact_module_member
              ~member_class:(member_class [@log_value.trace]) reference entries
              projection =
            match reference with
            | Longident.Ldot (module_reference, member_name) ->
                let module_path, _ =
                  Env.find_module_by_name_lazy module_reference environment
                in
                let module_path =
                  Env.normalize_module_path (Some location) environment
                    module_path
                in
                let canonical_path = Path.name module_path ^ "." ^ member_name in
                let matches =
                  List.filter
                    (fun entry -> String.equal (projection entry) canonical_path)
                    entries
                in
                (match matches with
                | [ entry ] ->
                    [%log.trace "resolved numeric reference through exact compiler module member"
                      ~provider:(Delator.Field.string unit_name)
                      ~stage:
                        (Delator.Field.string
                           "numeric-typed-reference-resolution")
                      ~route:
                        (Delator.Field.string "normalized-module-cmi-member")
                      ~member_class:
                        (Delator.Field.string
                           (member_class [@log_value.trace]))
                      ~decision:(Delator.Field.string "resolved")];
                    (Path.Pdot (module_path, member_name), entry)
                | [] | _ :: _ :: _ -> raise Not_found)
            | Lident _ | Lapply _ -> raise Not_found
          in
          let carrier_path, carrier =
            let reference = source_longident ~location carrier_reference in
            try
              Env.find_type_by_name reference environment
            with Not_found | Env.Error _ | Failure _ ->
              (try
                 let path, entry =
                   exact_module_member
                     ~member_class:("carrier" [@log_value.trace]) reference
                     type_entries (fun entry -> entry.numeric_type_path)
                 in
                 (path, entry.numeric_type_declaration)
               with Not_found | Env.Error _ | Failure _ ->
                 fail ~location
                   "numeric role carrier reference is not compiler-resolved in its lexical environment")
          in
          let semantics_path, semantics, _ =
            let reference = source_longident ~location semantics_reference in
            try
              let path, description =
                Env.find_value_by_name reference environment
              in
              (path, description, ())
            with Not_found | Env.Error _ | Failure _ ->
              (try
                 let path, entry =
                   exact_module_member
                     ~member_class:("semantics" [@log_value.trace]) reference
                     value_entries (fun entry -> entry.numeric_value_path)
                 in
                 (path, entry.numeric_value_description, ())
               with Not_found | Env.Error _ | Failure _ ->
                 fail ~location
                   "numeric role semantics reference is not compiler-resolved in its lexical environment")
          in
          let carrier_path =
            Env.normalize_type_path (Some location) environment carrier_path
          and semantics_path =
            Env.normalize_value_path (Some location) environment semantics_path
          in
          [%log.trace "resolved numeric role references in compiler environment"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "numeric-typed-reference-resolution")
            ~carrier_path:(Delator.Field.string (Path.name carrier_path))
            ~carrier_uid:
              (Delator.Field.string (compiler_uid carrier.Types.type_uid))
            ~semantics_path:(Delator.Field.string (Path.name semantics_path))
            ~semantics_uid:
              (Delator.Field.string (compiler_uid semantics.Types.val_uid))
            ~decision:(Delator.Field.string "resolved")];
          ( carrier_path,
            compiler_uid carrier.Types.type_uid,
            semantics_path,
            compiler_uid semantics.Types.val_uid )
        with Env.Error _ | Failure _ ->
          fail ~location
            "numeric role reference normalization failed in its compiler lexical environment")
  in
  let resolve_in_environment environment location source_claim =
    resolve_paths_in_environment environment location
      ~carrier_reference:source_claim.Numeric_source_claim_private.carrier_path
      ~semantics_reference:source_claim.semantics_path
  in
  let rec nested_structure expression =
    match expression.Typedtree.mod_desc with
    | Tmod_structure nested -> Some nested
    | Tmod_constraint (nested, _, _, _) -> nested_structure nested
    | Tmod_ident _ | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _
    | Tmod_unpack _ -> None
  in
  let rec typed_role_resolutions prefix structure =
    List.concat_map
      (fun item ->
        match item.Typedtree.str_desc with
        | Tstr_value (_, bindings) ->
            List.filter_map
              (fun binding ->
                match
                  parse_one Numeric_source_claim_private.role_marker
                    Numeric_source_claim_private.parse_role
                    binding.Typedtree.vb_attributes
                with
                | None -> None
                | Some (source_claim, location) ->
                    let identifiers =
                      Typedtree.pat_bound_idents binding.Typedtree.vb_pat
                    in
                    let identifier =
                      match identifiers with
                      | [ identifier ] -> identifier
                      | [] | _ :: _ :: _ ->
                          fail ~location
                            "numeric role implementation is not one compiler value slot"
                    in
                    let carrier_path, carrier_uid, semantics_path, semantics_uid =
                      resolve_in_environment item.str_env location source_claim
                    in
                    Some
                      { numeric_typed_callable_path =
                          String.concat "."
                            (unit_name :: prefix @ [ Ident.name identifier ]);
                        numeric_typed_callable_uid =
                          (Typedtree.pat_bound_idents_full binding.vb_pat
                          |> List.find (fun (candidate, _, _, _, _) ->
                                 Ident.same candidate identifier)
                          |> fun (_, _, _, uid, _) -> compiler_uid uid);
                        numeric_typed_source = source_claim;
                        numeric_typed_source_claim =
                          Numeric_source_claim_private.role_material source_claim;
                        numeric_typed_carrier_path = Path.name carrier_path;
                        numeric_typed_carrier_uid = carrier_uid;
                        numeric_typed_semantics_path = Path.name semantics_path;
                        numeric_typed_semantics_uid = semantics_uid })
              bindings
        | Tstr_module binding -> (
            match (binding.Typedtree.mb_name.txt, nested_structure binding.mb_expr) with
            | Some name, Some nested ->
                typed_role_resolutions (prefix @ [ name ]) nested
            | None, Some nested -> typed_role_resolutions prefix nested
            | _, None -> [])
        | Tstr_recmodule bindings ->
            List.concat_map
              (fun binding ->
                match
                  (binding.Typedtree.mb_name.txt, nested_structure binding.mb_expr)
                with
                | Some name, Some nested ->
                    typed_role_resolutions (prefix @ [ name ]) nested
                | None, Some nested -> typed_role_resolutions prefix nested
                | _, None -> [])
              bindings
        | Tstr_eval _ | Tstr_primitive _ | Tstr_type _ | Tstr_typext _
        | Tstr_exception _ | Tstr_modtype _ | Tstr_open _ | Tstr_class _
        | Tstr_class_type _ | Tstr_include _ | Tstr_attribute _ -> [])
      structure.Typedtree.str_items
  in
  let rec implementation_slots prefix structure =
    List.fold_left
      (fun (type_slots, value_slots) item ->
        match item.Typedtree.str_desc with
        | Tstr_value (_, bindings) ->
            let values =
              bindings
              |> List.concat_map (fun binding ->
                     Typedtree.pat_bound_idents binding.Typedtree.vb_pat
                     |> List.map (fun identifier ->
                            { numeric_implementation_value_path =
                                String.concat "."
                                  (unit_name :: prefix
                                  @ [ Ident.name identifier ]);
                              numeric_implementation_value_uid =
                                compiler_uid
                                  (Typedtree.pat_bound_idents_full
                                     binding.Typedtree.vb_pat
                                  |> List.find (fun (candidate, _, _, _, _) ->
                                         Ident.same candidate identifier)
                                  |> fun (_, _, _, uid, _) -> uid);
                              numeric_implementation_value_environment =
                                item.str_env;
                              numeric_implementation_value_attributes =
                                binding.vb_attributes;
                              numeric_implementation_value_location =
                                binding.vb_loc }))
            in
            (type_slots, values @ value_slots)
        | Tstr_primitive description ->
            let slot =
              { numeric_implementation_value_path =
                  String.concat "."
                    (unit_name :: prefix @ [ description.val_name.txt ]);
                numeric_implementation_value_uid =
                  compiler_uid description.val_val.Types.val_uid;
                numeric_implementation_value_environment = item.str_env;
                numeric_implementation_value_attributes =
                  description.val_attributes;
                numeric_implementation_value_location = description.val_loc }
            in
            (type_slots, slot :: value_slots)
        | Tstr_type (_, declarations) ->
            let slots =
              List.map
                (fun (declaration : Typedtree.type_declaration) ->
                  { numeric_implementation_type_path =
                      String.concat "."
                        (unit_name :: prefix @ [ declaration.typ_name.txt ]);
                    numeric_implementation_type_uid =
                      compiler_uid declaration.typ_type.Types.type_uid;
                    numeric_implementation_type_environment = item.str_env;
                    numeric_implementation_type_attributes = declaration.typ_attributes;
                    numeric_implementation_type_location = declaration.typ_loc })
                declarations
            in
            (slots @ type_slots, value_slots)
        | Tstr_module binding -> (
            match (binding.Typedtree.mb_name.txt, nested_structure binding.mb_expr) with
            | Some name, Some nested ->
                let nested_types, nested_values =
                  implementation_slots (prefix @ [ name ]) nested
                in
                (nested_types @ type_slots, nested_values @ value_slots)
            | None, Some nested ->
                let nested_types, nested_values =
                  implementation_slots prefix nested
                in
                (nested_types @ type_slots, nested_values @ value_slots)
            | _, None -> (type_slots, value_slots))
        | Tstr_recmodule bindings ->
            List.fold_left
              (fun (types, values) binding ->
                match
                  (binding.Typedtree.mb_name.txt, nested_structure binding.mb_expr)
                with
                | Some name, Some nested ->
                    let nested_types, nested_values =
                      implementation_slots (prefix @ [ name ]) nested
                    in
                    (nested_types @ types, nested_values @ values)
                | None, Some nested ->
                    let nested_types, nested_values =
                      implementation_slots prefix nested
                    in
                    (nested_types @ types, nested_values @ values)
                | _, None -> (types, values))
              (type_slots, value_slots) bindings
        | Tstr_eval _ | Tstr_typext _ | Tstr_exception _ | Tstr_modtype _
        | Tstr_open _ | Tstr_class _ | Tstr_class_type _ | Tstr_include _
        | Tstr_attribute _ ->
            (type_slots, value_slots))
      ([], []) structure.Typedtree.str_items
  in
  let rec nested_signature module_type =
    match module_type.Typedtree.mty_desc with
    | Tmty_signature nested -> Some nested
    | Tmty_strengthen (nested, _, _) | Tmty_with (nested, _) ->
        nested_signature nested
    | Tmty_ident _ | Tmty_functor _ | Tmty_typeof _ | Tmty_alias _ -> None
  in
  let typed_interface_for_owner ~location
      (artifact : authenticated_interface_artifact) =
    let owner_unit =
      Compilation_unit.Name.to_string artifact.artifact_interface.cmi_name
    in
    if String.equal owner_unit unit_name then
      match typed_interface with
      | Some signature -> signature
      | None ->
          fail ~location
            "included numeric role owner has no exact typed interface witness"
    else
      let adjacent =
        if artifact.artifact_filename = "" then []
        else
          [ Filename.remove_extension artifact.artifact_filename ^ ".cmti" ]
      in
      match
        discover_authenticated_typed_interface ~unit_name:owner_unit
          ~interface:artifact.artifact_interface ~adjacent ~load_paths ()
      with
      | Ok (_, typed) ->
          [%log.trace "selected original numeric role CMTI witness"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "numeric-included-role-reconstruction")
            ~route:(Delator.Field.string "authenticated-original-owner-cmti")
            ~owner_unit:(Delator.Field.string owner_unit)
            ~decision:(Delator.Field.string "accepted")];
          typed.typed_signature
      | Error reason -> fail ~location reason
  in
  let typed_type_declaration ~location (entry : numeric_type_entry) =
    let signature =
      typed_interface_for_owner ~location entry.numeric_type_owner_artifact
    in
    let rec declarations signature =
      List.concat_map
        (fun item ->
          match item.Typedtree.sig_desc with
          | Tsig_type (_, values) | Tsig_typesubst values ->
              values
              |> List.filter_map (fun declaration ->
                     if
                       String.equal
                         (compiler_uid
                            declaration.Typedtree.typ_type.Types.type_uid)
                         entry.numeric_type_uid
                     then Some (declaration, item.sig_env, signature.Typedtree.sig_final_env)
                     else None)
          | Tsig_module declaration -> (
              match nested_signature declaration.md_type with
              | Some nested -> declarations nested
              | None -> [])
          | Tsig_recmodule values ->
              List.concat_map
                (fun declaration ->
                  match nested_signature declaration.Typedtree.md_type with
                  | Some nested -> declarations nested
                  | None -> [])
                values
          | Tsig_value _ | Tsig_typext _ | Tsig_exception _ | Tsig_modsubst _
          | Tsig_modtype _ | Tsig_modtypesubst _ | Tsig_open _
          | Tsig_include _ | Tsig_class _ | Tsig_class_type _
          | Tsig_attribute _ ->
              [])
        signature.Typedtree.sig_items
    in
    match declarations signature with
      | [ match_ ] -> match_
      | [] ->
          fail ~location
            "numeric carrier has no exact typed owner declaration witness"
      | _ :: _ :: _ ->
          fail ~location
            "numeric carrier has ambiguous typed owner declaration witnesses"
  in
  let compiler_type_representation ~location (entry : numeric_type_entry) =
    let declaration, _, environment = typed_type_declaration ~location entry in
    with_authenticated_interfaces resolution_artifacts (fun () ->
        match Numeric_carrier_layout_private.reconstruct ~environment declaration with
        | Error reason -> fail ~location reason
        | Ok facts ->
            let representation =
              match facts.representation with
              | Numeric_carrier_layout_private.Immediate -> Artifact_immediate
              | Value -> Artifact_boxed
            in
            (facts.compiler_jkind_abi, representation))
  in
  let resolve_original_role ~location ~source_claim callable =
    let original_signature =
      typed_interface_for_owner ~location
        callable.numeric_value_owner_artifact
    in
    let expected_source =
      Numeric_source_claim_private.role_material source_claim
    in
    let rec find signature =
      List.concat_map
        (fun item ->
          match item.Typedtree.sig_desc with
          | Tsig_value description
            when String.equal
                   (compiler_uid description.Typedtree.val_val.Types.val_uid)
                   callable.numeric_value_uid -> (
              match
                parse_one Numeric_source_claim_private.role_marker
                  Numeric_source_claim_private.parse_role
                  description.Typedtree.val_attributes
              with
              | Some (original_source, original_location)
                when String.equal
                       (Numeric_source_claim_private.role_material
                          original_source)
                       expected_source ->
                  let carrier_path, carrier_uid, semantics_path, semantics_uid =
                    resolve_in_environment item.sig_env original_location
                      original_source
                  in
                  [ (Path.name carrier_path, carrier_uid,
                     Path.name semantics_path, semantics_uid) ]
              | Some _ ->
                  fail ~location
                    "included numeric role source claim differs from its original declaration"
              | None ->
                  fail ~location
                    "included numeric role original declaration lacks its typed source claim")
          | Tsig_module declaration -> (
              match nested_signature declaration.md_type with
              | Some nested -> find nested
              | None -> [])
          | Tsig_recmodule declarations ->
              List.concat_map
                (fun declaration ->
                  match nested_signature declaration.Typedtree.md_type with
                  | Some nested -> find nested
                  | None -> [])
                declarations
          | Tsig_value _ | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _
          | Tsig_exception _ | Tsig_modsubst _ | Tsig_modtype _
          | Tsig_modtypesubst _ | Tsig_open _ | Tsig_include _
          | Tsig_class _ | Tsig_class_type _ | Tsig_attribute _ ->
              [])
        signature.Typedtree.sig_items
    in
    match find original_signature with
    | [ resolution ] ->
        [%log.debug "correlated included numeric role with original typed declaration"
          ~provider:(Delator.Field.string unit_name)
          ~stage:
            (Delator.Field.string "numeric-included-role-reconstruction")
          ~route:(Delator.Field.string "original-declaration-environment")
          ~owner_unit:
            (Delator.Field.string callable.numeric_value_owner_unit)
          ~decision:(Delator.Field.string "accepted")];
        resolution
    | [] ->
        fail ~location
          "included numeric role has no exact original typed declaration witness"
    | _ :: _ :: _ ->
        fail ~location
          "included numeric role has ambiguous original typed declaration witnesses"
  in
  let rec included_role_resolutions output_prefix items =
    List.concat_map
      (function
        | Types.Sig_value (ident, description, Types.Exported) -> (
            match
              parse_one Numeric_source_claim_private.role_marker
                Numeric_source_claim_private.parse_role
                description.Types.val_attributes
            with
            | None -> []
            | Some (source_claim, location) ->
                let callable_uid = compiler_uid description.Types.val_uid in
                let callable =
                  match original_value_entries callable_uid with
                  | [ callable ] -> callable
                  | [] | _ :: _ :: _ ->
                      fail ~location
                        "included numeric role has no unique original callable owner"
                in
                let carrier_path, carrier_uid, semantics_path, semantics_uid =
                  resolve_original_role ~location ~source_claim callable
                in
                [ { numeric_typed_callable_path =
                      String.concat "."
                        (unit_name :: output_prefix @ [ Ident.name ident ]);
                    numeric_typed_callable_uid = callable_uid;
                    numeric_typed_source = source_claim;
                    numeric_typed_source_claim =
                      Numeric_source_claim_private.role_material source_claim;
                    numeric_typed_carrier_path = carrier_path;
                    numeric_typed_carrier_uid = carrier_uid;
                    numeric_typed_semantics_path = semantics_path;
                    numeric_typed_semantics_uid = semantics_uid } ])
        | Types.Sig_module (ident, _, declaration, _, Types.Exported) -> (
            match signature_module_type items [] declaration.Types.md_type with
            | Some nested ->
                included_role_resolutions
                  (output_prefix @ [ Ident.name ident ]) nested
            | None -> [])
        | Types.Sig_type _ | Types.Sig_value (_, _, Types.Hidden)
        | Types.Sig_module (_, _, _, _, Types.Hidden) | Types.Sig_typext _
        | Types.Sig_modtype _ | Types.Sig_class _ | Types.Sig_class_type _ ->
            [])
      items
  in
  let rec interface_role_resolutions prefix signature =
    List.concat_map
      (fun item ->
        match item.Typedtree.sig_desc with
        | Tsig_value description -> (
            match
              parse_one Numeric_source_claim_private.role_marker
                Numeric_source_claim_private.parse_role
                description.Typedtree.val_attributes
            with
            | None -> []
            | Some (source_claim, location) ->
                let carrier_path, carrier_uid, semantics_path, semantics_uid =
                  resolve_in_environment item.sig_env location source_claim
                in
                [ { numeric_typed_callable_path =
                      String.concat "."
                        (unit_name :: prefix @ [ description.val_name.txt ]);
                    numeric_typed_callable_uid =
                      compiler_uid description.val_val.Types.val_uid;
                    numeric_typed_source = source_claim;
                    numeric_typed_source_claim =
                      Numeric_source_claim_private.role_material source_claim;
                    numeric_typed_carrier_path = Path.name carrier_path;
                    numeric_typed_carrier_uid = carrier_uid;
                    numeric_typed_semantics_path = Path.name semantics_path;
                    numeric_typed_semantics_uid = semantics_uid } ])
        | Tsig_module declaration -> (
            match (declaration.Typedtree.md_name.txt, nested_signature declaration.md_type) with
            | Some name, Some nested ->
                interface_role_resolutions (prefix @ [ name ]) nested
            | None, Some nested -> interface_role_resolutions prefix nested
            | _, None -> [])
        | Tsig_recmodule declarations ->
            List.concat_map
              (fun declaration ->
                match
                  (declaration.Typedtree.md_name.txt,
                   nested_signature declaration.md_type)
                with
                | Some name, Some nested ->
                    interface_role_resolutions (prefix @ [ name ]) nested
                | None, Some nested -> interface_role_resolutions prefix nested
                | _, None -> [])
              declarations
        | Tsig_include (included, _) ->
            included_role_resolutions prefix included.incl_type
        | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _ | Tsig_exception _
        | Tsig_modsubst _ | Tsig_modtype _ | Tsig_modtypesubst _ | Tsig_open _
        | Tsig_class _ | Tsig_class_type _ | Tsig_attribute _ ->
            [])
      signature.Typedtree.sig_items
  in
  let implementation_marker_resolutions =
    Option.fold ~none:[] ~some:(typed_role_resolutions []) structure
  in
  let implementation_type_slots, implementation_value_slots =
    Option.fold ~none:([], []) ~some:(implementation_slots []) structure
  in
  let exact_by_uid ~identity_class uid entries projection =
    match List.filter (fun entry -> String.equal (projection entry) uid) entries with
    | [ entry ] -> Some entry
    | [] -> None
    | _ :: _ :: _ -> fail ("ambiguous " ^ identity_class ^ " compiler UID")
  in
  let exact_by_path ~identity_class path entries projection =
    match List.filter (fun entry -> String.equal (projection entry) path) entries with
    | [ entry ] -> entry
    | [] -> fail ("missing " ^ identity_class ^ " compiler slot")
    | _ :: _ :: _ ->
        fail ("ambiguous same-spelling " ^ identity_class ^ " compiler slots")
  in
  let exported_implementation_uid ~is_type path =
    match (structure, implementation_shape) with
    | Some structure, Some shape ->
        with_authenticated_interfaces resolution_artifacts (fun () ->
            let namespace =
              if is_type then Shape.Sig_component_kind.Type
              else Shape.Sig_component_kind.Value
            in
            match
              implementation_export_uid ~unit_name
                ~environment:structure.Typedtree.str_final_env ~namespace shape path
            with
            | Some uid -> uid
            | None -> fail "numeric reference has no exact implementation export")
    | _ -> fail "numeric reference has no implementation export witness"
  in
  let require_exported_implementation_uid ~is_type path uid =
    let exported_uid = exported_implementation_uid ~is_type path in
    if not (String.equal uid exported_uid) then (
      [%log.warn "rejected numeric reference to a hidden implementation binding"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "numeric-role-cmt-cmti-parity")
        ~path:(Delator.Field.string path)
        ~referenced_uid:(Delator.Field.string uid)
        ~exported_uid:(Delator.Field.string exported_uid)
        ~decision:(Delator.Field.string "rejected")];
      fail "numeric metadata refers to an earlier binding that is hidden by a later declaration")
    else
      [%log.trace "correlated numeric reference with the final compiler binding"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "numeric-role-cmt-cmti-parity")
        ~path:(Delator.Field.string path)
        ~exported_uid:(Delator.Field.string exported_uid)
        ~decision:(Delator.Field.string "accepted")]
  in
  let canonical_type_entry path uid =
      match original_type_entries uid with
      | [entry] -> entry
      | _ :: _ :: _ -> fail "ambiguous original numeric type UID"
      | [] ->
          (match
             List.filter
               (fun slot ->
                 String.equal slot.numeric_implementation_type_uid uid)
               implementation_type_slots
           with
          | [ slot ] ->
              require_exported_implementation_uid ~is_type:true
                slot.numeric_implementation_type_path uid;
              exact_by_path ~identity_class:"numeric carrier"
                slot.numeric_implementation_type_path type_entries
                (fun entry -> entry.numeric_type_path)
          | [] ->
              require_exported_implementation_uid ~is_type:true path uid;
              exact_by_path ~identity_class:"numeric carrier" path type_entries
                (fun entry -> entry.numeric_type_path)
          | _ :: _ :: _ -> fail "ambiguous implementation carrier UID")
  in
  let canonical_type_identity path uid =
    let entry = canonical_type_entry path uid in
    Numeric_receipt_private.encode
      ~schema:"verocaml.numeric-parity-type-slot.v1"
      [ entry.numeric_type_owner_cmi_full_key; entry.numeric_type_uid ]
  in
  let canonical_value_identity path uid =
    let entry =
      match
        exact_by_uid ~identity_class:"numeric semantics" uid value_entries
          (fun entry -> entry.numeric_value_uid)
      with
      | Some entry -> entry
      | None ->
          (match
             List.filter
               (fun slot ->
                 String.equal slot.numeric_implementation_value_uid uid)
               implementation_value_slots
           with
          | [ slot ] ->
              require_exported_implementation_uid ~is_type:false
                slot.numeric_implementation_value_path uid;
              exact_by_path ~identity_class:"numeric semantics"
                slot.numeric_implementation_value_path value_entries
                (fun entry -> entry.numeric_value_path)
          | [] ->
              require_exported_implementation_uid ~is_type:false path uid;
              exact_by_path ~identity_class:"numeric semantics" path
                value_entries (fun entry -> entry.numeric_value_path)
          | _ :: _ :: _ -> fail "ambiguous implementation semantics UID")
    in
    Numeric_receipt_private.encode
      ~schema:"verocaml.numeric-parity-value-slot.v1"
      [ entry.numeric_value_owner_cmi_full_key; entry.numeric_value_uid ]
  in
  let typed_role_resolutions =
    match typed_interface with
    | None -> implementation_marker_resolutions
    | Some signature ->
        let interface_resolutions = interface_role_resolutions [] signature in
        if Option.is_none structure then interface_resolutions
        else (
        List.iter
          (fun implementation ->
            require_exported_implementation_uid ~is_type:false
              implementation.numeric_typed_callable_path
              implementation.numeric_typed_callable_uid;
            match
              List.find_opt
                (fun interface ->
                  String.equal interface.numeric_typed_callable_path
                    implementation.numeric_typed_callable_path)
                interface_resolutions
            with
            | None ->
                fail
                  "numeric role implementation marker has no typed interface slot"
            | Some interface
              when String.equal interface.numeric_typed_source_claim
                     implementation.numeric_typed_source_claim ->
                ()
            | Some _ ->
                fail
                  "numeric role implementation metadata differs from its typed interface")
          implementation_marker_resolutions;
        let implementation_witnesses =
          interface_resolutions
          |> List.filter_map (fun interface ->
                 let marker_witnesses =
                   List.filter
                     (fun implementation ->
                       String.equal implementation.numeric_typed_callable_path
                         interface.numeric_typed_callable_path)
                     implementation_marker_resolutions
                 in
                 let slots =
                   List.filter
                     (fun slot ->
                       String.equal slot.numeric_implementation_value_path
                         interface.numeric_typed_callable_path
                       && String.equal slot.numeric_implementation_value_uid
                            (exported_implementation_uid ~is_type:false
                               interface.numeric_typed_callable_path))
                     implementation_value_slots
                 in
                 match marker_witnesses with
                 | [ witness ] -> Some witness
                 | _ :: _ :: _ ->
                     fail
                       "numeric role has ambiguous implementation marker witnesses"
                 | [] ->
                 match slots with
                 | [ slot ] ->
                     (match
                        parse_one Numeric_source_claim_private.role_marker
                          Numeric_source_claim_private.parse_role
                          slot.numeric_implementation_value_attributes
                      with
                     | Some (source, _)
                       when not
                              (String.equal
                                 (Numeric_source_claim_private.role_material
                                    source)
                                 interface.numeric_typed_source_claim) ->
                         fail
                           "numeric role implementation metadata differs from its typed interface"
                     | None ->
                         [%log.trace "resolved interface-only numeric metadata"
                           ~provider:(Delator.Field.string unit_name)
                           ~stage:(Delator.Field.string "numeric-role-cmt-cmti-parity")
                           ~callable:(Delator.Field.string interface.numeric_typed_callable_path)
                           ~route:(Delator.Field.string "cmti-reference-and-exact-implementation-export")
                           ~decision:(Delator.Field.string "accepted")];
                         None
                     | Some _ ->
                     let carrier_path, carrier_uid, semantics_path,
                         semantics_uid =
                       resolve_in_environment
                         slot.numeric_implementation_value_environment
                         slot.numeric_implementation_value_location
                         interface.numeric_typed_source
                     in
                     Some
                       { interface with
                         numeric_typed_carrier_path = Path.name carrier_path;
                         numeric_typed_carrier_uid = carrier_uid;
                         numeric_typed_semantics_path = Path.name semantics_path;
                         numeric_typed_semantics_uid = semantics_uid })
                 | [] -> (
                     match original_value_entries interface.numeric_typed_callable_uid with
                     | [ original ]
                       when not
                              (String.equal original.numeric_value_owner_unit
                                 unit_name) ->
                         None
                     | [] | [ _ ] | _ :: _ :: _ ->
                         fail
                           "numeric role typed interface has no exact implementation callable slot")
                 | _ :: _ :: _ ->
                     fail
                       "numeric role typed interface has ambiguous implementation callable slots")
        in
        List.iter2
          (fun interface implementation ->
            let interface_carrier =
              canonical_type_identity interface.numeric_typed_carrier_path
                interface.numeric_typed_carrier_uid
            and implementation_carrier =
              canonical_type_identity implementation.numeric_typed_carrier_path
                implementation.numeric_typed_carrier_uid
            and interface_semantics =
              canonical_value_identity interface.numeric_typed_semantics_path
                interface.numeric_typed_semantics_uid
            and implementation_semantics =
              canonical_value_identity
                implementation.numeric_typed_semantics_path
                implementation.numeric_typed_semantics_uid
            in
            if
              not
                (String.equal interface_carrier implementation_carrier
                && String.equal interface_semantics implementation_semantics)
            then
              fail
                "numeric role implementation and interface resolve different exact compiler slots";
            [%log.trace "correlated numeric role implementation and interface slots"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "numeric-role-cmt-cmti-parity")
              ~route:(Delator.Field.string "full-owner-and-compiler-slot")
              ~decision:(Delator.Field.string "accepted")])
          (List.filter
             (fun interface ->
               List.exists
                 (fun implementation ->
                   String.equal implementation.numeric_typed_callable_path
                     interface.numeric_typed_callable_path)
                 implementation_witnesses)
             interface_resolutions)
          implementation_witnesses;
        interface_resolutions)
  in
  let local_type_uids owner =
    let root = Subst.Lazy.force_signature owner.Cmi_format.cmi_sign in
    let rec collect parent items =
      List.concat_map
        (function
          | Types.Sig_type (ident, declaration, _, _) ->
              let direct = Path.Pident ident in
              let qualified =
                match parent with
                | None -> direct
                | Some parent -> Path.Pdot (parent, Ident.name ident)
              in
              let uid = compiler_uid declaration.Types.type_uid in
              if Path.same direct qualified then [ (direct, uid) ]
              else [ (direct, uid); (qualified, uid) ]
          | Types.Sig_module (ident, _, declaration, _, Types.Exported) -> (
              match module_signature items declaration with
              | None -> []
              | Some nested ->
                  let parent =
                    match parent with
                    | None -> Path.Pident ident
                    | Some parent -> Path.Pdot (parent, Ident.name ident)
                  in
                  collect (Some parent) nested)
          | Types.Sig_value _
          | Types.Sig_module (_, _, _, _, Types.Hidden)
          | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
          | Types.Sig_class_type _ ->
              [])
        items
    in
    collect None root
  in
  let abi_context owner =
    let local_type_uids = local_type_uids owner in
    let normalize typ = typ in
    let root_signature =
      Subst.Lazy.force_signature owner.Cmi_format.cmi_sign
    in
    let append_components path components =
      List.fold_left (fun parent name -> Path.Pdot (parent, name)) path
        components
    in
    let rec signature_uid seen path =
      let key = Path.name path in
      if List.mem key seen then None
      else
        let from_resolution = function
          | Some (Signature_type_uid uid) -> Some uid
          | Some (Signature_module_alias (alias, remaining)) ->
              signature_uid (key :: seen) (append_components alias remaining)
          | None -> (
              match persistent_type_uid ~load_paths ~interface:owner path with
              | Ok uid -> uid
              | Error _ -> None)
        in
        match Path.flatten path with
        | `Contains_apply -> None
        | `Ok (root_ident, components)
          when Ident.is_global_or_predef root_ident
               && String.equal (Ident.name root_ident)
                    (Compilation_unit.Name.to_string owner.Cmi_format.cmi_name) ->
            from_resolution (signature_type_uid root_signature components)
        | `Ok (root_ident, []) ->
            root_signature
            |> List.find_map (function
                 | Types.Sig_type (ident, declaration, _, _)
                   when Ident.same ident root_ident ->
                     Some (compiler_uid declaration.Types.type_uid)
                 | _ -> None)
        | `Ok (root_ident, components) ->
            let exact_local_module =
              root_signature
              |> List.find_map (function
                   | Types.Sig_module (ident, _, declaration, _, _)
                     when Ident.same ident root_ident ->
                       Some declaration.Types.md_type
                   | _ -> None)
            in
            (match exact_local_module with
            | Some module_type -> (
                match module_type with
                | Types.Mty_alias alias ->
                    signature_uid (key :: seen)
                      (append_components alias components)
                | Mty_signature nested ->
                    from_resolution (signature_type_uid nested components)
                | Mty_strengthen (Mty_signature nested, _, _) ->
                    from_resolution (signature_type_uid nested components)
                | Mty_strengthen _ | Mty_ident _ | Mty_functor _ -> None)
            | None -> from_resolution None)
    in
    let constructor_uid path =
      match
        List.find_map
          (fun (candidate, uid) ->
            if Path.same candidate path then Some uid else None)
          local_type_uids
      with
      | Some _ as uid -> uid
      | None -> (
          match signature_uid [] path with
          | Some _ as uid -> uid
          | None ->
          match predef_type_uid path with
          | Some _ as uid -> uid
          | None -> (
              match persistent_type_uid ~load_paths ~interface:owner path with
              | Ok uid -> uid
              | Error _ -> None))
    in
    let rec sorts seen candidate =
      let identity =
        Compilation_unit.Name.to_string candidate.Cmi_format.cmi_name
        ^ "@"
        ^ Option.value ~default:"missing-self-crc"
            (interface_self_crc candidate)
      in
      if List.mem identity seen then (seen, [])
      else
        let seen = identity :: seen in
        let issuer =
          match interface_family_issuers candidate with
          | [ issuer ] -> issuer
          | [] | _ :: _ :: _ -> ""
        in
        authenticated_import_interfaces ~load_paths candidate
        |> List.fold_left
             (fun (seen, collected) artifact ->
               let seen, imported = sorts seen artifact.artifact_interface in
               (seen, collected @ imported))
             (seen, interface_logical_sorts ~issuer candidate)
    in
    let _, logical_sorts = sorts [] owner in
    ( constructor_uid,
      List.sort_uniq Logical_sort_private.compare logical_sorts,
      normalize )
  in
  let value_abi ~location entry =
    let constructor_uid, logical_sorts, normalize =
      abi_context entry.numeric_value_owner
    in
    match
      symbolic_interface_abi ~canonical_path:entry.numeric_value_path
        ~value_uid:entry.numeric_value_uid ~constructor_uid ~logical_sorts
        ~normalize entry.numeric_value_description.Types.val_type
    with
    | Ok abi -> abi
    | Error _ ->
        fail ~location "numeric callable has unsupported compiler ABI"
  in
  let value_mode_abi ~location entry =
    let prefix = entry.numeric_value_owner_unit ^ "." in
    let relative =
      if String.starts_with ~prefix entry.numeric_value_path then
        String.sub entry.numeric_value_path (String.length prefix)
          (String.length entry.numeric_value_path - String.length prefix)
      else
        fail ~location "numeric callable owner path is inconsistent"
    in
    match
      List.assoc_opt ("value:" ^ relative)
        (interface_mode_signatures entry.numeric_value_owner)
    with
    | Some (Some mode_abi) ->
        if mode_abi = "" then
          fail ~location "numeric callable has no exact compiler mode ABI"
        else mode_abi
    | Some None | None ->
        fail ~location "numeric callable has no exact compiler mode ABI"
  in
  let variance_material variance =
    let flag flag = string_of_bool (Types.Variance.mem flag variance) in
    Numeric_receipt_private.encode ~schema:"verocaml.type-variance.v1"
      [ flag Types.Variance.May_pos; flag Types.Variance.May_neg;
        flag Types.Variance.May_weak; flag Types.Variance.Inj;
        flag Types.Variance.Pos; flag Types.Variance.Neg;
        flag Types.Variance.Inv ]
  in
  let binder_abi declaration =
    Numeric_receipt_private.encode ~schema:"verocaml.numeric-type-binders.v1"
      [ string_of_int declaration.Types.type_arity;
        Numeric_receipt_private.list
          (List.map variance_material declaration.type_variance);
        Numeric_receipt_private.list
          (List.map
             (fun mode -> string_of_int (Types.Separability.rank mode))
             declaration.type_separability) ]
  in
  let carrier_abi entry =
    let declaration = entry.numeric_type_declaration in
    let constructor_uid, logical_sorts, normalize =
      abi_context entry.numeric_type_owner
    in
    let manifest =
      match declaration.Types.type_manifest with
      | None -> "opaque"
      | Some typ -> (
          match
            symbolic_interface_abi
              ~canonical_path:(entry.numeric_type_path ^ "#manifest")
              ~value_uid:entry.numeric_type_uid ~constructor_uid
              ~logical_sorts ~normalize typ
          with
          | Ok abi -> abi
          | Error _ -> fail "numeric carrier manifest has unsupported compiler ABI")
    in
    let kind =
      match declaration.Types.type_kind with
      | Types.Type_abstract _ -> "abstract"
      | Types.Type_record _ | Types.Type_record_unboxed_product _
      | Types.Type_variant _ | Types.Type_open ->
          fail "numeric carrier declaration has unsupported aggregate representation"
    and privacy =
      match declaration.Types.type_private with
      | Asttypes.Public -> "public"
      | Asttypes.Private -> "private"
    in
    Numeric_receipt_private.encode
      ~schema:"verocaml.numeric-type-constructor-abi.v1"
      [ entry.numeric_type_path; entry.numeric_type_uid;
        string_of_int declaration.type_arity; kind; privacy; manifest;
        string_of_bool declaration.type_is_newtype;
        string_of_bool declaration.type_unboxed_default ]
  in
  let resolve_numeric_base environment location path =
    with_authenticated_interfaces resolution_artifacts (fun () ->
      try
        let environment = Envaux.env_of_only_summary ~allow_missing_modules:false environment in
        let reference = source_longident ~location path in
        let resolved, declaration =
          try Env.find_type_by_name reference environment
          with Not_found | Env.Error _ | Failure _ ->
            match reference with
            | Longident.Ldot (prefix, name) ->
                let parent, _ = Env.find_module_by_name_lazy prefix environment in
                let parent = Env.normalize_module_path (Some location) environment parent in
                let path = Path.name parent ^ "." ^ name in
                let entry = exact_by_path ~identity_class:"numeric base" path type_entries
                  (fun entry -> entry.numeric_type_path) in
                Path.Pdot (parent, name), entry.numeric_type_declaration
            | _ -> raise Not_found in
        canonical_type_entry (Path.name resolved) (compiler_uid declaration.Types.type_uid)
      with Not_found | Env.Error _ | Failure _ ->
        fail ~location "The numeric base must name an accessible logical type declaration.")
  in
  let carrier_base ~location entry source =
    let declaration, lexical_environment, _ = typed_type_declaration ~location entry in
    let source_material = Numeric_source_claim_private.carrier_material source in
    (match parse_one Numeric_source_claim_private.carrier_marker Numeric_source_claim_private.parse_carrier declaration.typ_attributes with
    | Some (typed_source, _) when String.equal source_material (Numeric_source_claim_private.carrier_material typed_source) -> ()
    | _ -> fail ~location "Numeric carrier metadata does not match its original typed declaration.");
    let resolve environment location source =
      Option.map (resolve_numeric_base environment location) source.Numeric_source_claim_private.base_path in
    let base = resolve lexical_environment declaration.typ_loc source in
    if String.equal entry.numeric_type_owner_unit unit_name then begin
      match List.filter (fun slot -> String.equal slot.numeric_implementation_type_path entry.numeric_type_path) implementation_type_slots with
      | [slot] ->
          require_exported_implementation_uid ~is_type:true slot.numeric_implementation_type_path slot.numeric_implementation_type_uid;
          (match parse_one Numeric_source_claim_private.carrier_marker Numeric_source_claim_private.parse_carrier slot.numeric_implementation_type_attributes with
          | None ->
              [%log.trace "retained interface-only numeric base reference"
                ~carrier:(Delator.Field.string entry.numeric_type_path)
                ~route:(Delator.Field.string "original-cmti-lexical-environment")]
          | Some (implementation_source, _) ->
              if not (String.equal source_material (Numeric_source_claim_private.carrier_material implementation_source)) then
                fail ~location "Numeric carrier metadata differs between its implementation and interface.";
              let implementation_base = resolve slot.numeric_implementation_type_environment slot.numeric_implementation_type_location implementation_source in
              let identity = Option.map (fun entry -> entry.numeric_type_owner_cmi_full_key, entry.numeric_type_uid) in
              if identity base <> identity implementation_base then
                fail ~location "The numeric base resolves to different types in the implementation and interface.")
      | [] -> ()
      | _ -> fail ~location "The numeric carrier has ambiguous implementation declarations."
    end;
    Option.map (fun entry ->
      let owner = Numeric_interface_claim_private.owner
        ~owner_unit:entry.numeric_type_owner_unit ~owner_cmi_full_key:entry.numeric_type_owner_cmi_full_key
        ~owner_cmi_checked_digest:entry.numeric_type_owner_cmi_checked_digest ~import_routes:entry.numeric_type_import_routes in
      let base = Result.bind owner (fun base_owner -> Numeric_interface_claim_private.base_reference
        ~type_path:entry.numeric_type_path ~type_uid:entry.numeric_type_uid ~base_owner) in
      match base with
      | Error reason -> fail ~location reason
      | Ok reference ->
          [%log.debug "resolved numeric base type reference"
            ~type_uid:(Delator.Field.string reference.type_uid)
            ~owner:(Delator.Field.string reference.base_owner.owner_unit)
            ~authority:(Delator.Field.string "compiler-type-reference-not-logical-admission")];
          reference) base
  in
  let semantic_attributes description =
    List.filter
      (fun attribute ->
        List.mem attribute.Parsetree.attr_name.txt
          [ "verocaml.spec"; "verocaml.proof";
            "verocaml.external_specification";
            "verocaml.internal.symbolic.interface.v1" ])
      description.Types.val_attributes
  in
  let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
  let rec scan prefix items =
    List.fold_left
      (fun (carriers, roles) -> function
        | Types.Sig_type (_ident, declaration, _, Types.Exported) ->
            (match
               exact_markers Numeric_source_claim_private.role_marker
                 declaration.Types.type_attributes
             with
            | [] -> ()
            | marker :: _ ->
                fail ~location:marker.Parsetree.attr_loc
                  "numeric semantic-role marker is attached to a type");
            (match
               parse_one Numeric_source_claim_private.carrier_marker
                 Numeric_source_claim_private.parse_carrier
                 declaration.Types.type_attributes
             with
            | None -> (carriers, roles)
            | Some (numeric_carrier_source, claim_location) ->
                let uid = compiler_uid declaration.Types.type_uid in
                let entry =
                  match original_type_entries uid with
                  | [ entry ] -> entry
                  | [] | _ :: _ :: _ ->
                      fail ~location:claim_location
                        "numeric carrier marker is not bound to one exact type slot"
                in
                let numeric_carrier_compiler_jkind_abi,
                    numeric_carrier_compiler_representation =
                  compiler_type_representation ~location:claim_location entry
                in
                let receipt =
                  { numeric_carrier_source;
                    numeric_carrier_base = carrier_base ~location:claim_location entry numeric_carrier_source;
                    numeric_carrier_path = entry.numeric_type_path;
                    numeric_carrier_uid = uid;
                    numeric_carrier_owner_unit = entry.numeric_type_owner_unit;
                    numeric_carrier_owner_cmi_full_key =
                      entry.numeric_type_owner_cmi_full_key;
                    numeric_carrier_owner_cmi_checked_digest =
                      entry.numeric_type_owner_cmi_checked_digest;
                    numeric_carrier_import_routes =
                      entry.numeric_type_import_routes;
                    numeric_carrier_constructor_abi =
                      (try carrier_abi entry
                       with Numeric_claim_failure failure ->
                         raise
                           (Numeric_claim_failure
                              { failure with location = Some claim_location }));
                    numeric_carrier_binder_abi =
                      binder_abi entry.numeric_type_declaration;
                    numeric_carrier_compiler_jkind_abi;
                    numeric_carrier_compiler_representation }
                in
                [%log.trace "reconstructed numeric carrier compiler identity"
                  ~provider:(Delator.Field.string unit_name)
                  ~stage:
                    (Delator.Field.string "numeric-source-cmi-reconstruction")
                  ~route:(Delator.Field.string "exact-type-slot")
                  ~type_arity:(Delator.Field.int declaration.type_arity)
                  ~decision:(Delator.Field.string "reconstructed")];
                (receipt :: carriers, roles))
        | Types.Sig_value (ident, description, Types.Exported) ->
            (match
               exact_markers Numeric_source_claim_private.carrier_marker
                 description.Types.val_attributes
             with
            | [] -> ()
            | marker :: _ ->
                fail ~location:marker.Parsetree.attr_loc
                  "numeric carrier marker is attached to a value");
            (match
               parse_one Numeric_source_claim_private.role_marker
                 Numeric_source_claim_private.parse_role
                 description.Types.val_attributes
             with
            | None -> (carriers, roles)
            | Some (numeric_role_source, claim_location) ->
                let path =
                  String.concat "."
                    (unit_name :: prefix @ [ Ident.name ident ])
                and uid = compiler_uid description.Types.val_uid in
                let callable =
                  match original_value_entries uid with
                  | [ entry ] -> entry
                  | [] | _ :: _ :: _ ->
                      fail ~location:claim_location
                        "numeric role marker is not bound to one exact value slot"
                in
                let typed_resolution =
                  let source_material =
                    Numeric_source_claim_private.role_material numeric_role_source
                  in
                  match
                    List.filter
                      (fun resolution ->
                        String.equal resolution.numeric_typed_callable_path path
                        && String.equal resolution.numeric_typed_source_claim
                             source_material)
                      typed_role_resolutions
                  with
                  | [ resolution ] -> resolution
                  | [] ->
                      fail ~location:claim_location
                        "numeric role has no exact typed implementation reference witness"
                  | _ :: _ :: _ ->
                      fail ~location:claim_location
                        "numeric role typed implementation reference witness is ambiguous"
                in
                let carrier =
                  match
                    original_type_entries
                      typed_resolution.numeric_typed_carrier_uid
                  with
                  | [ entry ] -> entry
                  | [] ->
                      fail ~location:claim_location
                        "numeric role carrier typed identity is absent from authenticated CMIs"
                  | _ :: _ :: _ ->
                      fail ~location:claim_location
                        "numeric role carrier typed identity is ambiguous"
                in
                (match
                   exact_markers Numeric_source_claim_private.carrier_marker
                     carrier.numeric_type_declaration.Types.type_attributes
                 with
                | [ marker ] -> (
                    match Numeric_source_claim_private.parse_carrier marker with
                    | Ok _ -> ()
                    | Error reason ->
                        fail ~location:claim_location
                          ("numeric role carrier declaration is malformed: "
                         ^ reason))
                | [] ->
                    fail ~location:claim_location
                      "numeric role carrier path does not name a declared numeric carrier"
                | _ :: _ :: _ ->
                    fail ~location:claim_location
                      "numeric role carrier declaration is ambiguous");
                let semantics =
                  match
                    original_value_entries
                      typed_resolution.numeric_typed_semantics_uid
                  with
                  | [ entry ] -> entry
                  | [] ->
                      fail ~location:claim_location
                        "numeric role semantics typed identity is absent from authenticated CMIs"
                  | _ :: _ :: _ ->
                      fail ~location:claim_location
                        "numeric role semantics typed identity is ambiguous"
                in
                if semantic_attributes semantics.numeric_value_description = [] then
                  fail ~location:claim_location
                    "numeric role semantics path does not name a typed specification, proof, axiom, external specification, or symbolic declaration";
                let checked_value_abi (identity_class [@log_value.warn]) entry =
                  try value_abi ~location:claim_location entry
                  with Numeric_claim_failure failure ->
                    [%log.warn "rejected numeric role callable ABI reconstruction"
                      ~provider:(Delator.Field.string unit_name)
                      ~stage:
                        (Delator.Field.string "numeric-source-cmi-reconstruction")
                      ~identity_class:
                        (Delator.Field.string
                           (identity_class [@log_value.warn]))
                      ~role_identity:
                        (Delator.Field.string numeric_role_source.role_identity)
                      ~decision:(Delator.Field.string "rejected")
                      ~reason_class:(Delator.Field.string "unsupported-typed-abi")];
                    raise (Numeric_claim_failure failure)
                in
                let receipt =
                  { numeric_role_callable_shape =
                      (let constructor_uid, logical_sorts, _ = abi_context callable.numeric_value_owner in
                       Numeric_callable_domain_private.shape ~constructor_uid ~logical_sorts
                         ~carrier_uid:carrier.numeric_type_uid
                         ~carrier:carrier.numeric_type_declaration callable.numeric_value_description.Types.val_type);
                    numeric_role_callable_domain =
                      (let constructor_uid, logical_sorts, _ = abi_context callable.numeric_value_owner in
                       Numeric_callable_domain_private.classify ~constructor_uid ~logical_sorts
                         ~carrier_uid:carrier.numeric_type_uid
                         ~carrier:carrier.numeric_type_declaration callable.numeric_value_description.Types.val_type);
                    numeric_role_source;
                    numeric_role_callable_path = callable.numeric_value_path;
                    numeric_role_callable_uid = uid;
                    numeric_role_callable_abi =
                      checked_value_abi
                        ("operation" [@log_value.warn]) callable;
                    numeric_role_callable_mode_abi =
                      value_mode_abi ~location:claim_location callable;
                    numeric_role_callable_owner_unit =
                      callable.numeric_value_owner_unit;
                    numeric_role_callable_owner_cmi_full_key =
                      callable.numeric_value_owner_cmi_full_key;
                    numeric_role_callable_owner_cmi_checked_digest =
                      callable.numeric_value_owner_cmi_checked_digest;
                    numeric_role_callable_import_routes =
                      callable.numeric_value_import_routes;
                    numeric_role_semantics_path = semantics.numeric_value_path;
                    numeric_role_semantics_uid = semantics.numeric_value_uid;
                    numeric_role_semantics_abi =
                      checked_value_abi
                        ("semantics" [@log_value.warn]) semantics;
                    numeric_role_semantics_mode_abi =
                      value_mode_abi ~location:claim_location semantics;
                    numeric_role_semantics_owner_unit =
                      semantics.numeric_value_owner_unit;
                    numeric_role_semantics_owner_cmi_full_key =
                      semantics.numeric_value_owner_cmi_full_key;
                    numeric_role_semantics_owner_cmi_checked_digest =
                      semantics.numeric_value_owner_cmi_checked_digest;
                    numeric_role_semantics_import_routes =
                      semantics.numeric_value_import_routes;
                    numeric_role_carrier_uid = carrier.numeric_type_uid;
                    numeric_role_carrier_owner_unit =
                      carrier.numeric_type_owner_unit;
                    numeric_role_carrier_owner_cmi_full_key =
                      carrier.numeric_type_owner_cmi_full_key;
                    numeric_role_carrier_owner_cmi_checked_digest =
                      carrier.numeric_type_owner_cmi_checked_digest;
                    numeric_role_carrier_import_routes =
                      carrier.numeric_type_import_routes }
                in
                [%log.trace "reconstructed numeric role compiler identities"
                  ~provider:(Delator.Field.string unit_name)
                  ~stage:
                    (Delator.Field.string "numeric-source-cmi-reconstruction")
                  ~route:
                    (Delator.Field.string "exact-callable-and-semantics-slots")
                  ~decision:(Delator.Field.string "reconstructed")];
                (carriers, receipt :: roles))
        | Types.Sig_module (ident, _, declaration, _, Types.Exported) -> (
            match module_signature items declaration with
            | None -> (carriers, roles)
            | Some nested ->
                let nested_carriers, nested_roles =
                  scan (prefix @ [ Ident.name ident ]) nested
                in
                (nested_carriers @ carriers, nested_roles @ roles))
        | Types.Sig_type (_, _, _, Types.Hidden)
        | Types.Sig_value (_, _, Types.Hidden)
        | Types.Sig_module (_, _, _, _, Types.Hidden)
        | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
        | Types.Sig_class_type _ ->
            (carriers, roles))
      ([], []) items
  in
  let numeric_carriers, numeric_roles = scan [] root in
  let numeric_carriers =
    List.sort
      (fun left right ->
        compare
          (left.numeric_carrier_uid, left.numeric_carrier_constructor_abi)
          (right.numeric_carrier_uid, right.numeric_carrier_constructor_abi))
      numeric_carriers
  and numeric_roles =
    List.sort
      (fun left right ->
        compare
          (left.numeric_role_callable_uid, left.numeric_role_semantics_uid,
           left.numeric_role_callable_abi)
          (right.numeric_role_callable_uid, right.numeric_role_semantics_uid,
           right.numeric_role_callable_abi))
      numeric_roles
  in
  let duplicate projection values =
    let keys = List.map projection values in
    List.length keys <> List.length (List.sort_uniq compare keys)
  in
  if duplicate (fun value -> value.numeric_carrier_uid) numeric_carriers then
    fail "duplicate numeric carrier compiler identity"
  else if duplicate (fun value -> value.numeric_role_callable_uid) numeric_roles
  then fail "duplicate numeric role callable compiler identity"
  else (
    [%log.info "completed numeric source claim compiler reconstruction"
      ~provider:(Delator.Field.string unit_name)
      ~stage:(Delator.Field.string "numeric-source-cmi-reconstruction")
      ~route:(Delator.Field.string "exact-compiler-signature")
      ~carrier_count:(Delator.Field.int (List.length numeric_carriers))
      ~role_count:(Delator.Field.int (List.length numeric_roles))
      ~authority:(Delator.Field.string "claim-only-not-completion-authority")
      ~decision:(Delator.Field.string "completed")];
    { numeric_carriers; numeric_roles;
      numeric_provenance_nodes = !node_count;
      numeric_provenance_edges = !edge_count;
      numeric_provenance_bytes = !graph_bytes })
[@@delator.instrument] [@@delator.level info]

let retained_numeric_claims (claims : interface_numeric_claims) =
  let fail reason =
    raise
      (Numeric_claim_failure
         { provider = "retained-numeric-interface"; reason; location = None })
  in
  let owner ~owner_unit ~owner_cmi_full_key ~owner_cmi_checked_digest
      ~import_routes =
    match
      Numeric_interface_claim_private.owner ~owner_unit ~owner_cmi_full_key
        ~owner_cmi_checked_digest ~import_routes
    with
    | Ok owner -> owner
    | Error reason -> fail reason
  in
  let carrier_reconstruction_claims =
    claims.numeric_carriers
    |> List.map (fun carrier ->
           let owner =
             owner ~owner_unit:carrier.numeric_carrier_owner_unit
               ~owner_cmi_full_key:carrier.numeric_carrier_owner_cmi_full_key
               ~owner_cmi_checked_digest:
                 carrier.numeric_carrier_owner_cmi_checked_digest
               ~import_routes:carrier.numeric_carrier_import_routes
           in
           match
             Numeric_interface_claim_private.carrier_with_base
               ~base_reference:carrier.numeric_carrier_base
               ~source_claim:
                 (Numeric_source_claim_private.carrier_material
                    carrier.numeric_carrier_source)
               ~carrier_path:carrier.numeric_carrier_path
               ~carrier_uid:carrier.numeric_carrier_uid ~owner
               ~constructor_abi:carrier.numeric_carrier_constructor_abi
               ~binder_abi:carrier.numeric_carrier_binder_abi
               ~compiler_jkind_abi:carrier.numeric_carrier_compiler_jkind_abi
               ~compiler_representation:
                 (numeric_artifact_representation_name
                    carrier.numeric_carrier_compiler_representation)
           with
           | Ok claim -> claim.transport_key
           | Error reason -> fail reason)
    |> List.sort String.compare
  in
  let role_reconstruction_claims =
    claims.numeric_roles
    |> List.map (fun role ->
           let callable_owner =
             owner ~owner_unit:role.numeric_role_callable_owner_unit
               ~owner_cmi_full_key:
                 role.numeric_role_callable_owner_cmi_full_key
               ~owner_cmi_checked_digest:
                 role.numeric_role_callable_owner_cmi_checked_digest
               ~import_routes:role.numeric_role_callable_import_routes
           and semantics_owner =
             owner ~owner_unit:role.numeric_role_semantics_owner_unit
               ~owner_cmi_full_key:
                 role.numeric_role_semantics_owner_cmi_full_key
               ~owner_cmi_checked_digest:
                 role.numeric_role_semantics_owner_cmi_checked_digest
               ~import_routes:role.numeric_role_semantics_import_routes
           and carrier_owner =
             owner ~owner_unit:role.numeric_role_carrier_owner_unit
               ~owner_cmi_full_key:
                 role.numeric_role_carrier_owner_cmi_full_key
               ~owner_cmi_checked_digest:
                 role.numeric_role_carrier_owner_cmi_checked_digest
               ~import_routes:role.numeric_role_carrier_import_routes
           in
           match
             Numeric_interface_claim_private.role
               ~source_claim:
                 (Numeric_source_claim_private.role_material
                    role.numeric_role_source)
               ~callable_path:role.numeric_role_callable_path
               ~callable_uid:role.numeric_role_callable_uid
               ~callable_type_abi:role.numeric_role_callable_abi
               ~callable_mode_abi:role.numeric_role_callable_mode_abi
               ~callable_owner
               ~semantics_path:role.numeric_role_semantics_path
               ~semantics_uid:role.numeric_role_semantics_uid
               ~semantics_type_abi:role.numeric_role_semantics_abi
               ~semantics_mode_abi:role.numeric_role_semantics_mode_abi
               ~semantics_owner ~carrier_uid:role.numeric_role_carrier_uid
               ~carrier_owner
           with
           | Ok claim -> claim.transport_key
           | Error reason -> fail reason)
    |> List.sort String.compare
  in
  let payload_bytes =
    List.fold_left (fun total claim -> total + String.length claim) 0
      (carrier_reconstruction_claims @ role_reconstruction_claims)
  in
  if payload_bytes > maximum_numeric_provenance_bytes then (
    [%log.warn "rejected oversized retained numeric claim payload"
      ~stage:(Delator.Field.string "numeric-retained-inventory")
      ~route:(Delator.Field.string "pre-emission-size-gate")
      ~payload_bytes:(Delator.Field.int payload_bytes)
      ~carrier_count:
        (Delator.Field.int (List.length carrier_reconstruction_claims))
      ~role_count:(Delator.Field.int (List.length role_reconstruction_claims))
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "retained-payload-byte-bound")];
    fail "retained numeric reconstruction inventory exceeds its byte bound"
  );
  [%log.info "constructed retained numeric reconstruction inventory"
    ~stage:(Delator.Field.string "numeric-retained-inventory")
    ~route:(Delator.Field.string "compiler-reconstructed")
    ~carrier_count:
      (Delator.Field.int (List.length carrier_reconstruction_claims))
    ~role_count:(Delator.Field.int (List.length role_reconstruction_claims))
    ~authority:(Delator.Field.string "claim-only-not-completion-authority")
    ~decision:(Delator.Field.string "constructed")];
  { Retained_interface_authority_private.carrier_reconstruction_claims;
    role_reconstruction_claims }
[@@delator.instrument] [@@delator.level info]

let embedded_interface_metadata ~load_paths = function
  | None ->
      ( false,
        None,
        None,
        0,
        [||],
        [],
        [],
        [],
        [],
        [],
        [],
        [],
        [] )
  | Some interface ->
      let implementation_unit_name =
        match interface.Cmi_format.cmi_kind with
        | Cmi_format.Normal { cmi_impl; _ } ->
            Some (Compilation_unit.name_as_string cmi_impl)
        | Cmi_format.Parameter -> None
      in
      let interface_broadcast_syntax =
        try interface_broadcast_syntax ~load_paths interface
        with Failure reason ->
          raise
            (Broadcast_artifact_failure
               {
                 provider =
                   Compilation_unit.Name.to_string interface.Cmi_format.cmi_name;
                 reason;
               })
      in
      let interface_broadcast_declarations, interface_broadcast_groups =
        released_interface_broadcast_metadata interface_broadcast_syntax
      in
      ( true,
        Some
          (Compilation_unit.Name.to_string interface.Cmi_format.cmi_name),
        implementation_unit_name,
        List.length interface.Cmi_format.cmi_params,
        imports_of_array interface.Cmi_format.cmi_crcs,
        interface_family_markers interface,
        interface_family_issuers interface,
        interface_mode_signatures interface,
        interface_finite_signatures interface,
        interface_broadcast_declarations,
        interface_broadcast_groups,
        interface_symbolic_declarations ~load_paths interface,
        interface_broadcast_syntax )

let adjacent_interface filename =
  let basename =
    try Filename.chop_extension filename with Invalid_argument _ -> filename
  in
  let filename = basename ^ ".cmi" in
  if Sys.file_exists filename then
    let interface, receipt = read_stable_cmi filename in
    Some (filename, interface, receipt)
  else None

let implementation_interface_digest info =
  match info.Cmt_format.cmt_interface_digest with
  | Some digest -> Some digest
  | None ->
      let self = Compilation_unit.name info.Cmt_format.cmt_modname in
      let matching =
        Array.to_list info.Cmt_format.cmt_imports
        |> List.filter (fun imported ->
               Compilation_unit.Name.equal (Import_info.name imported) self)
      in
      (match matching with [ imported ] -> Import_info.crc imported | _ -> None)

let interface_digest_matches info interface =
  match implementation_interface_digest info with
  | None -> false
  | Some expected ->
      let matching =
        Array.to_list interface.Cmi_format.cmi_crcs
        |> List.filter (fun imported ->
               Compilation_unit.Name.equal
                 (Import_info.name imported)
                 interface.Cmi_format.cmi_name)
      in
      (match matching with
      | [ imported ] -> Import_info.crc imported = Some expected
      | _ -> false)

let explicit_interface_identity_matches info interface =
  let implementation_name =
    Compilation_unit.name_as_string info.Cmt_format.cmt_modname
  in
  let interface_name =
    Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
  in
  let interface_implementation_name =
    match interface.Cmi_format.cmi_kind with
    | Cmi_format.Normal { cmi_impl; _ } ->
        Some (Compilation_unit.name_as_string cmi_impl)
    | Cmi_format.Parameter -> None
  in
  String.equal implementation_name interface_name
  && interface_implementation_name = Some implementation_name

let explicit_interface_unsupported interface =
  if interface.Cmi_format.cmi_params <> [] then
    Some (Diagnostic.Unsupported_input Explicit_cmi_parameters)
  else
    match interface.Cmi_format.cmi_kind with
    | Cmi_format.Normal { cmi_arg_for = Some _; _ } ->
        Some (Unsupported_input Explicit_cmi_argument_for)
    | Normal { cmi_arg_for = None; _ } | Parameter -> None

exception Unsupported_interface_metadata of Diagnostic.classification

type lexical_path = {
  absolute : bool;
  components : string list;
}

let lexical_path path =
  let absolute = not (Filename.is_relative path) in
  let separator = Filename.dir_sep.[0] in
  let components =
    String.split_on_char separator path
    |> List.fold_left
         (fun components -> function
           | "" | "." -> components
           | ".." -> (
               match components with
               | component :: rest when not (String.equal component "..") ->
                   rest
               | _ when absolute -> components
               | _ -> ".." :: components)
           | component -> component :: components)
         []
    |> List.rev
  in
  { absolute; components }

let lexical_path_string path =
  let body = String.concat Filename.dir_sep path.components in
  if path.absolute then Filename.dir_sep ^ body
  else if String.equal body "" then "."
  else body

let append_lexical base relative =
  lexical_path
    (Filename.concat (lexical_path_string base) (lexical_path_string relative))

let path_from_build_directory build_directory path =
  let path = lexical_path path in
  if path.absolute then path else append_lexical build_directory path

let rec drop_common left right =
  match (left, right) with
  | left :: lefts, right :: rights when String.equal left right ->
      drop_common lefts rights
  | _ -> (left, right)

let path_relative_to ~from target =
  if from.absolute <> target.absolute then
    raise
      (Failure
         (Printf.sprintf "relocated load-path roots differ: from=%s target=%s"
            (lexical_path_string from) (lexical_path_string target)))
  else
    let remaining_from, remaining_target =
      drop_common from.components target.components
    in
    {
      absolute = false;
      components =
        List.init (List.length remaining_from) (Fun.const "..")
        @ remaining_target;
    }

let path_has_prefix ~prefix path =
  prefix.absolute = path.absolute
  &&
  let rec loop prefix path =
    match (prefix, path) with
    | [], _ -> true
    | prefix :: prefixes, path :: paths when String.equal prefix path ->
        loop prefixes paths
    | _ -> false
  in
  loop prefix.components path.components

let relocated_paths ~filename ~build_directory ~compiler_arguments load_path =
  let absolute_filename =
    if Filename.is_relative filename then Filename.concat (Sys.getcwd ()) filename
    else filename
  in
  let artifact_directory = lexical_path (Filename.dirname absolute_filename) in
  let build_directory = lexical_path build_directory in
  let output_directories =
    argument_after "-o" compiler_arguments
    |> List.map Filename.dirname |> List.sort_uniq String.compare
  in
  let output_directory =
    match output_directories with
    | [] -> build_directory
    | [ output ] -> path_from_build_directory build_directory output
    | _ -> raise (Failure "ambiguous compiler output directories")
  in
  let relocate original =
    if original = output_directory then artifact_directory
    else if path_has_prefix ~prefix:build_directory original then
      append_lexical artifact_directory
        (path_relative_to ~from:output_directory original)
    else original
  in
  let resolve path =
    path_from_build_directory build_directory path |> relocate
    |> lexical_path_string
  in
  ( relocate build_directory |> lexical_path_string,
    List.map resolve load_path.Load_path.visible,
    List.map resolve load_path.Load_path.hidden )

let broadcast_witness_receipt ~provider_origin ~interface_digest ~compiler_uid
    ~canonical_path ~dependency_receipt source_members =
  let material source =
    [
      string_of_int source.Retained_broadcast_private.member_slot;
      source.Retained_broadcast_private.member_provider_origin;
      source.member_interface_receipt;
      source.member_dependency_receipt;
      source.Retained_broadcast_private.member_compiler_uid;
      Retained_broadcast_private.kind_name source.member_kind;
      source.member_canonical_path;
    ]
    |> List.map (fun field ->
           Printf.sprintf "%d:%s" (String.length field) field)
    |> String.concat ""
  in
  let members = source_members |> List.map material |> List.sort String.compare in
  [
    provider_origin;
    interface_digest;
    compiler_uid;
    canonical_path;
    dependency_receipt;
  ]
  @ members
  |> List.map (fun field -> Printf.sprintf "%d:%s" (String.length field) field)
  |> String.concat "" |> Digest.string |> Digest.to_hex

let implementation_imported_group_witnesses ~unit_name ~imports ~load_paths
    interface structure =
  let artifacts = authenticated_import_interfaces ~load_paths interface in
  let marker_path path =
    Array.exists
      (fun (import : import) ->
        String.equal import.unit_name "Vero_ghost"
        && import.crc = Some Trusted_imports.ghost_crc)
      imports
    &&
    match Path.flatten path with
    | `Ok (root, [ component ]) ->
        Ident.is_global_or_predef root
        && String.equal (Ident.name root) "Vero_ghost"
        && String.equal component "marker"
    | `Ok _ | `Contains_apply -> false
  in
  let imported_identity expression path description =
    if marker_path path then None
    else
      let normalized =
        with_authenticated_interfaces artifacts (fun () ->
            let environment =
              Envaux.env_of_only_summary ~allow_missing_modules:true
                expression.Typedtree.exp_env
            in
            Env.normalize_value_path (Some expression.exp_loc) environment path)
      in
      match Path.flatten normalized with
      | `Ok (root, _) when Ident.is_global_or_predef root ->
          let provider = Ident.name root in
          if String.equal provider unit_name then None
          else
            Some
              ( Format.asprintf "%a" Types.Uid.print description.Types.val_uid,
                Path.name normalized )
      | `Ok _ | `Contains_apply -> None
  in
  let group_binding prefix binding =
    let payloads =
      List.filter_map
        (fun attribute ->
          if
            String.equal attribute.Parsetree.attr_name.txt
              "verocaml.internal.broadcast.carrier.v1"
          then
            Option.map (String.split_on_char '|')
              (string_payload_attribute attribute.attr_name.txt [ attribute ])
          else None)
        binding.Typedtree.vb_attributes
    in
    match payloads with
    | [ [ "group"; _id; name; _start; _stop ] ] ->
        let witnesses = ref [] in
        let default = Tast_iterator.default_iterator in
        let iterator =
          {
            default with
            expr =
              (fun self expression ->
                (match expression.Typedtree.exp_desc with
                | Texp_ident (path, _, description, _, _) ->
                    Option.iter
                      (fun identity -> witnesses := identity :: !witnesses)
                      (imported_identity expression path description)
                | _ -> ());
                default.expr self expression);
          }
        in
        iterator.expr iterator binding.vb_expr;
        Some
          (String.concat "." (prefix @ [ name ]),
           List.sort_uniq compare !witnesses)
    | [] -> None
    | [ _ ] | _ :: _ :: _ ->
        raise (Failure "retained broadcast implementation group receipt is malformed")
  in
  let rec collect prefix structure =
    List.concat_map
      (fun item ->
        match item.Typedtree.str_desc with
        | Tstr_value (_, bindings) ->
            List.filter_map (group_binding prefix) bindings
        | Tstr_module (binding : Typedtree.module_binding) -> (
            match (binding.mb_name.txt, binding.mb_expr.mod_desc) with
            | Some name, Tmod_structure nested -> collect (prefix @ [ name ]) nested
            | None, _ | Some _, _ -> [])
        | Tstr_recmodule bindings ->
            List.concat_map
              (fun (binding : Typedtree.module_binding) ->
                match (binding.mb_name.txt, binding.mb_expr.mod_desc) with
                | Some name, Tmod_structure nested ->
                    collect (prefix @ [ name ]) nested
                | None, _ | Some _, _ -> [])
              bindings
        | Tstr_eval _ | Tstr_primitive _ | Tstr_type _ | Tstr_typext _
        | Tstr_exception _ | Tstr_modtype _ | Tstr_open _ | Tstr_class _
        | Tstr_class_type _ | Tstr_include _ | Tstr_attribute _ -> [])
      structure.Typedtree.str_items
  in
  collect [] structure

let authenticate_implementation_imported_witnesses ~unit_name ~imports ~load_paths
    interface structure interface_broadcasts =
  let groups =
    implementation_imported_group_witnesses ~unit_name ~imports ~load_paths
      interface structure
  in
  List.iter
    (fun (member : Retained_broadcast_private.interface_member) ->
      if member.identity.kind = Retained_broadcast_private.Group then
        let prefix = unit_name ^ "." in
        let relative =
          String.sub member.identity.canonical_path (String.length prefix)
            (String.length member.identity.canonical_path - String.length prefix)
        in
        let expected =
          member.source_members
          |> List.filter_map
               (fun (source : Retained_broadcast_private.source_reference) ->
                 if String.starts_with ~prefix source.member_canonical_path then
                   None
                 else
                   Some
                     ( source.member_compiler_uid,
                       source.member_canonical_path ))
          |> List.sort_uniq compare
        in
        let actual = Option.value ~default:[] (List.assoc_opt relative groups) in
        if actual <> expected then (
          [%log.debug "rejected retained implementation/CMTI imported witness map"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "typed-witness")
            ~route:(Delator.Field.string "cmt-cmti-witness-map")
            ~expected_count:(Delator.Field.int (List.length expected))
            ~witness_count:(Delator.Field.int (List.length actual))
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "complete-map-receipt")];
          raise
            (Failure
               "retained broadcast implementation and typed interface witness maps differ")))
    interface_broadcasts

let artifact_io_warning ~route:_route =
  [%log.warn "compiler artifact system read issue"
    ~route:(Delator.Field.string _route)
    ~stage:(Delator.Field.string "artifact-load")
    ~decision:(Delator.Field.string "unavailable")
    ~reason_class:(Delator.Field.string "artifact-io")]

type retained_authority_mode = Import_authority | Emit_authority

let cmti_identity info =
  let annotation_kind =
    match info.Cmt_format.cmt_annots with
    | Cmt_format.Interface _ -> "interface"
    | Implementation _ -> "implementation"
    | Partial_implementation _ -> "partial-implementation"
    | Partial_interface _ -> "partial-interface"
    | Packed _ -> "packed"
  in
  [
    Config.cmt_magic_number;
    annotation_kind;
    Compilation_unit.name_as_string info.Cmt_format.cmt_modname;
    Option.value ~default:""
      (Option.map Digest.to_hex info.Cmt_format.cmt_interface_digest);
    import_receipt (imports info);
  ]
  |> List.map (fun value -> Printf.sprintf "%d:%s" (String.length value) value)
  |> String.concat "" |> Digest.string |> Digest.to_hex

let cmi_identity interface =
  let self =
    match interface_self_crc interface with
    | Some receipt -> receipt
    | None -> raise (Failure "retained interface CMI identity is incomplete")
  in
  [
    Config.cmi_magic_number;
    Compilation_unit.Name.to_string interface.Cmi_format.cmi_name;
    self;
    import_receipt (imports_of_array interface.Cmi_format.cmi_crcs);
  ]
  |> List.map (fun value -> Printf.sprintf "%d:%s" (String.length value) value)
  |> String.concat "" |> Digest.string |> Digest.to_hex

type authority_validation = {
  validated : (string, string) Hashtbl.t;
  mutable visiting : string list;
}

let authority_validation () = { validated = Hashtbl.create 17; visiting = [] }

let artifact_candidates ~extension ~unit_name ~adjacent ~load_paths =
  let basenames =
    [
      String.uncapitalize_ascii unit_name ^ extension;
      unit_name ^ extension;
    ]
  in
  adjacent
  :: List.concat_map
       (fun directory ->
         List.map (fun basename -> Filename.concat directory basename) basenames)
       load_paths
  |> stable_unique String.equal |> List.filter Sys.file_exists

let authority_candidates ~unit_name ~cmi_filename ~load_paths =
  artifact_candidates
    ~extension:Retained_interface_authority_private.extension ~unit_name
    ~adjacent:
      (Filename.remove_extension cmi_filename
      ^ Retained_interface_authority_private.extension)
    ~load_paths

let coalesce_authority_candidates ~unit_name:(_unit_name [@delator.field Fun.id]) candidates =
  let decoded =
    List.map
      (fun filename ->
        match Retained_interface_authority_private.decode (read_all filename) with
        | Ok authority -> (filename, authority)
        | Error _ ->
            raise
              (Failure
                 "retained interface dependency authority artifact is malformed"))
      candidates
  in
  match decoded with
  | [] ->
      [%log.debug "rejected retained dependency authority lookup"
        ~provider:(Delator.Field.string _unit_name)
        ~stage:(Delator.Field.string "dependency-sidecar-lookup")
        ~route:(Delator.Field.string "interface-inventory")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "missing-artifact")];
      raise (Failure "retained interface dependency authority artifact is missing")
  | (filename, authority) :: rest ->
      if
        not
          (List.for_all
             (fun (_, candidate) ->
               Retained_interface_authority_private.equal authority candidate)
             rest)
      then (
        [%log.debug "rejected conflicting retained dependency authority candidates"
          ~provider:(Delator.Field.string _unit_name)
          ~stage:(Delator.Field.string "dependency-sidecar-coalescence")
          ~route:(Delator.Field.string "interface-inventory")
          ~candidate_count:(Delator.Field.int (List.length decoded))
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "conflicting-complete-receipts")];
        raise
          (Failure "retained interface dependency authority candidates conflict"));
      (filename, authority)

let validate_authority_payload ?implementation_shape ?structure ~unit_name
    ~cmti_filename ~load_paths interface authority =
  let syntax = interface_broadcast_syntax ~load_paths interface in
  let interface_digest = Option.value ~default:"" (interface_self_crc interface) in
  let typed_interface =
    match authenticate_typed_interface ~unit_name ~interface cmti_filename with
    | Ok artifact -> artifact
    | Error reason -> raise (Failure reason)
  in
  let typed_members =
    typed_interface_broadcast_members
      ~typed_interface:typed_interface.typed_signature ~unit_name
      ~interface_digest ~load_paths ~interface ~interface_syntax:syntax
  in
  let family =
    match interface_family_markers interface with
    | [ value ] -> value
    | [] | _ :: _ :: _ ->
        raise (Failure "retained dependency has no exact artifact family")
  and route =
    match interface_family_issuers interface with
    | [ value ] -> value
    | [] | _ :: _ :: _ ->
        raise (Failure "retained dependency has no exact issuer route")
  in
  let authorities =
    Retained_interface_authority_private.broadcast_witnesses authority
  in
  let exact_authority syntax =
    let canonical_path = unit_name ^ "." ^ syntax.broadcast_path in
    match
      List.filter
        (fun member ->
          let identity = member.Retained_broadcast_private.identity in
          String.equal identity.compiler_uid syntax.broadcast_uid
          && identity.kind = syntax.broadcast_kind
          && String.equal identity.canonical_path canonical_path)
        authorities
    with
    | [ member ] -> member
    | [] | _ :: _ :: _ ->
        raise
          (Failure
             "retained dependency CMI and authority declaration sets differ")
  in
  List.iter
    (fun syntax ->
      let member = exact_authority syntax in
      let identity = member.Retained_broadcast_private.identity in
      if
        not
          (String.equal identity.provider_origin unit_name
          && String.equal identity.interface_digest interface_digest
          && String.equal identity.artifact_family family
          && String.equal identity.provider_route route)
      then
        raise
          (Failure
             "retained dependency declaration identity receipt is inconsistent");
      let expected_sources =
        typed_members
        |> List.filter_map (fun (group_path, ordinal, source_path, source) ->
               if String.equal group_path syntax.broadcast_path then
                 Some (ordinal, source_path, source)
               else None)
        |> List.sort (fun (left, _, _) (right, _, _) -> Int.compare left right)
      in
      let source_paths =
        List.map (fun (_, source_path, _) -> source_path) expected_sources
        |> List.sort String.compare
      and sources = List.map (fun (_, _, source) -> source) expected_sources in
      if
        source_paths <> syntax.broadcast_member_paths
        || List.sort compare sources
           <> List.sort compare member.Retained_broadcast_private.source_members
      then
        raise
          (Failure
             "retained dependency CMTI and authority witness sets differ");
      let witness_receipt =
        broadcast_witness_receipt ~provider_origin:unit_name
          ~interface_digest ~compiler_uid:syntax.broadcast_uid
          ~canonical_path:identity.canonical_path
          ~dependency_receipt:identity.dependency_receipt sources
      in
      if not (String.equal identity.witness_receipt witness_receipt) then
        raise (Failure "retained dependency witness receipt is stale"))
    syntax;
  if List.length syntax <> List.length authorities then
    raise (Failure "retained dependency authority has undeclared members");
  let logical_sorts = interface_logical_sorts ~issuer:route interface in
  let retained_logical_sorts =
    Retained_interface_authority_private.logical_sorts authority
  in
  if
    List.sort Logical_sort_private.compare logical_sorts
    <> List.sort Logical_sort_private.compare retained_logical_sorts
  then
    raise
      (Failure
         "retained dependency logical-sort descriptors differ from the compiler interface")
  else
    let symbolic_declarations =
      interface_symbolic_declarations ~load_paths interface
    in
    let logical_values =
      interface_logical_values ~load_paths ~symbolic_declarations interface
    and retained_logical_values =
      Retained_interface_authority_private.logical_values authority
    in
    if List.sort compare logical_values <> List.sort compare retained_logical_values
    then
      raise
        (Failure
           "retained dependency logical-value descriptors differ from the compiler interface");
    let retained_numeric_inventory =
      Retained_interface_authority_private.numeric_claims authority
    in
    let compiler_numeric_claims =
      interface_numeric_claims ~load_paths
        ~root_cmi_receipt:authority.cmi_receipt
        ~typed_interface:typed_interface.typed_signature ?implementation_shape
        ?structure interface
      |> retained_numeric_claims
    in
    let expected_numeric_claims =
      if
        compiler_numeric_claims.carrier_reconstruction_claims = []
        && compiler_numeric_claims.role_reconstruction_claims = []
      then []
      else [ compiler_numeric_claims ]
    in
    if retained_numeric_inventory <> expected_numeric_claims then (
      [%log.warn "rejected substituted retained numeric reconstruction inventory"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "numeric-retained-reconciliation")
        ~route:(Delator.Field.string "exact-cmi-cmti-vri")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "full-claim-set-mismatch")];
      raise
        (Failure
           "retained numeric reconstruction claims differ from the compiler interface")
    ) else
      [%log.info "authenticated retained numeric reconstruction inventory"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "numeric-retained-reconciliation")
        ~route:(Delator.Field.string "exact-cmi-cmti-vri")
        ~carrier_count:
          (Delator.Field.int
             (List.length
                compiler_numeric_claims.carrier_reconstruction_claims))
        ~role_count:
          (Delator.Field.int
             (List.length compiler_numeric_claims.role_reconstruction_claims))
        ~authority:(Delator.Field.string "retained-claim-authenticated")
        ~decision:(Delator.Field.string "accepted")]

let rec dependency_authority_receipt_with validation ~load_paths artifact =
  let interface = artifact.artifact_interface in
  let issuer =
    match interface_family_issuers interface with
    | [ issuer ] -> issuer
    | [] | _ :: _ :: _ -> ""
  in
  if
    interface_broadcast_syntax ~load_paths interface = []
    && interface_logical_sorts ~issuer interface = []
    &&
    let symbolic_declarations =
      interface_symbolic_declarations ~load_paths interface
    in
    interface_logical_values ~load_paths ~symbolic_declarations interface = []
    &&
    not (interface_has_numeric_markers interface)
  then None
  else
    let unit_name = Compilation_unit.Name.to_string interface.Cmi_format.cmi_name in
    let key = unit_name ^ ":" ^ artifact.artifact_content_digest in
    match Hashtbl.find_opt validation.validated key with
    | Some receipt -> Some receipt
    | None ->
        if List.mem key validation.visiting then (
          [%log.warn "rejected retained dependency authority cycle"
            ~provider:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "dependency-closure")
            ~route:(Delator.Field.string "interface-inventory")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "authority-cycle")];
          raise (Failure "retained interface dependency authority cycle"));
        validation.visiting <- key :: validation.visiting;
        Fun.protect
          ~finally:(fun () ->
            validation.visiting <-
              List.filter (fun candidate -> not (String.equal candidate key))
                validation.visiting)
          (fun () ->
            let candidates =
              authority_candidates ~unit_name
                ~cmi_filename:artifact.artifact_filename ~load_paths
            in
            let filename, authority =
              coalesce_authority_candidates ~unit_name candidates
            in
            let provider_matches =
              String.equal authority.provider_unit unit_name
              && String.equal authority.provider_origin unit_name
            and compiler_matches =
              String.equal authority.compiler_abi
                (Config.cmi_magic_number ^ Config.cmt_magic_number)
            and content_matches =
              String.equal authority.cmi_receipt
                artifact.artifact_content_digest
            and self_crc_matches =
              String.equal authority.cmi_self_crc
                (Option.value ~default:"" (interface_self_crc interface))
            and identity_matches =
              String.equal authority.cmi_identity (cmi_identity interface)
            and imports_match =
              List.sort compare authority.cmi_imports
              = (imports_of_array interface.Cmi_format.cmi_crcs
                |> Array.to_list
                |> List.map (fun (imported : import) ->
                       (imported.unit_name, imported.crc))
                |> List.sort compare)
            in
            if
              not
                (provider_matches && compiler_matches && content_matches
                && self_crc_matches && identity_matches && imports_match)
            then (
              [%log.warn "rejected retained dependency CMI authority binding"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "dependency-closure")
                ~route:(Delator.Field.string "exact-cmi-vri")
                ~provider_matches:(Delator.Field.bool provider_matches)
                ~compiler_matches:(Delator.Field.bool compiler_matches)
                ~content_matches:(Delator.Field.bool content_matches)
                ~self_crc_matches:(Delator.Field.bool self_crc_matches)
                ~identity_matches:(Delator.Field.bool identity_matches)
                ~imports_match:(Delator.Field.bool imports_match)
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "complete-cmi-binding")];
              raise
                (Failure
                   "retained interface dependency CMI or ABI receipt mismatch"));
            let cmti_filename, typed_interface =
              match
                discover_authenticated_typed_interface
                  ~required_receipt:authority.cmti_receipt ~unit_name ~interface
                  ~adjacent:
                    [ Filename.remove_extension artifact.artifact_filename
                      ^ ".cmti" ]
                  ~load_paths ()
              with
              | Error _ ->
                  [%log.warn "rejected missing retained dependency CMTI binding"
                    ~provider:(Delator.Field.string unit_name)
                    ~stage:(Delator.Field.string "dependency-closure")
                    ~route:(Delator.Field.string "stable-cmti-vri")
                    ~decision:(Delator.Field.string "rejected")
                    ~reason_class:
                      (Delator.Field.string "missing-or-stale-cmti")];
                  raise
                    (Failure
                       "retained interface dependency CMTI artifact is missing or stale")
              | Ok pair -> pair
            in
            let cmti = typed_interface.typed_metadata in
            let cmti_imports =
              imports cmti |> Array.to_list
              |> List.map (fun (imported : import) ->
                     (imported.unit_name, imported.crc))
              |> List.sort compare
            in
            let cmti_digest_matches =
              authority.cmti_interface_digest
              = Option.map Digest.to_hex cmti.Cmt_format.cmt_interface_digest
            and cmti_identity_matches =
              String.equal authority.cmti_identity (cmti_identity cmti)
            and cmti_imports_match =
              List.sort compare authority.cmti_imports = cmti_imports
            in
            if
              not
                (cmti_digest_matches && cmti_identity_matches
                && cmti_imports_match)
            then (
              [%log.warn "rejected retained dependency CMTI authority binding"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "dependency-closure")
                ~route:(Delator.Field.string "stable-cmti-vri")
                ~digest_matches:(Delator.Field.bool cmti_digest_matches)
                ~identity_matches:(Delator.Field.bool cmti_identity_matches)
                ~imports_match:(Delator.Field.bool cmti_imports_match)
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "complete-cmti-binding")];
              raise
                (Failure
                   "retained interface dependency CMTI identity or import receipt mismatch"));
            validate_authority_payload ~unit_name ~cmti_filename ~load_paths
              interface authority;
            let interface_imports =
              imports_of_array interface.Cmi_format.cmi_crcs
              |> Array.to_list
              |> List.filter (fun (imported : import) ->
                     not (String.equal imported.unit_name unit_name))
            in
            let missing_interface_edge =
              List.find_opt
                (fun (imported : import) ->
                  not
                    (List.exists
                       (fun edge ->
                         String.equal edge.Retained_interface_authority_private.dependency_unit
                           imported.unit_name
                         && edge.dependency_compiler_receipt = imported.crc)
                       authority.dependencies))
                interface_imports
            in
            if Option.is_some missing_interface_edge then
              raise
                (Failure
                   "retained interface dependency authority omits a compiler interface import");
            let dependencies =
              authority.dependencies
              |> List.map (fun edge ->
                     let dependency_unit =
                       edge.Retained_interface_authority_private.dependency_unit
                     in
                     match edge.dependency_compiler_receipt with
                     | None ->
                         raise
                           (Failure
                              "retained interface dependency has no compiler receipt")
                     | Some crc -> (
                         match
                           find_imported_interface_artifact ~load_paths
                             ~unit_name:dependency_unit ~expected_crc:crc ()
                         with
                         | None ->
                             raise
                               (Failure
                                  "retained transitive interface artifact is missing or stale")
                         | Some dependency ->
                             {
                               Retained_interface_authority_private.dependency_unit;
                               dependency_compiler_receipt = Some crc;
                               dependency_interface_receipt =
                                 dependency.artifact_content_digest;
                               dependency_authority_receipt =
                                 dependency_authority_receipt_with validation
                                   ~load_paths dependency;
                             }))
              |> List.sort compare
            in
            if
              List.length dependencies
              <> List.length (List.sort_uniq compare dependencies)
              || List.sort compare authority.dependencies <> dependencies
            then
              raise
                (Failure
                   "retained interface dependency authority closure differs");
            let receipt = artifact_content_receipt filename in
            Hashtbl.add validation.validated key receipt;
            [%log.trace "validated retained dependency authority closure"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "dependency-closure")
              ~route:(Delator.Field.string "cmi-cmti-vri")
              ~direct_dependency_count:
                (Delator.Field.int (List.length dependencies))
              ~decision:(Delator.Field.string "accepted")];
            Some receipt)

let retained_authority_record ~unit_name ~cmi_receipt ~cmti_filename
    ~cmti_receipt ~ordinary_cmi_receipts ~load_paths ~implementation_imports interface
    interface_broadcasts interface_logical_sorts interface_logical_values
    interface_numeric_claims =
  let typed_interface =
    match authenticate_typed_interface ~unit_name ~interface cmti_filename with
    | Ok artifact -> artifact
    | Error reason -> raise (Failure reason)
  in
  let cmti = typed_interface.typed_metadata
  and observed_cmti_receipt = typed_interface.typed_content_receipt in
  if not (String.equal cmti_receipt observed_cmti_receipt) then
    raise (Failure "retained interface CMTI snapshot receipt changed");
  if
    not
      (String.equal
         (Compilation_unit.name_as_string cmti.Cmt_format.cmt_modname)
         unit_name)
  then raise (Failure "retained interface CMTI provider identity mismatch");
  (match cmti.Cmt_format.cmt_annots with
  | Cmt_format.Interface _ -> ()
  | Implementation _ | Partial_implementation _ | Partial_interface _ | Packed _ ->
      raise (Failure "retained interface CMTI is not a finalized interface"));
  let cmi_imports =
    imports_of_array interface.Cmi_format.cmi_crcs |> Array.to_list
    |> List.map (fun (imported : import) -> (imported.unit_name, imported.crc))
    |> List.sort compare
  and cmti_imports =
    imports cmti |> Array.to_list
    |> List.map (fun (imported : import) -> (imported.unit_name, imported.crc))
    |> List.sort compare
  in
  if cmi_imports <> cmti_imports then (
    [%log.debug "rejected retained CMI/CMTI import-vector mismatch"
      ~provider:(Delator.Field.string unit_name)
      ~stage:(Delator.Field.string "typed-interface-binding")
      ~route:(Delator.Field.string "cmi-cmti")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "complete-import-vector")];
    raise (Failure "retained interface CMI and CMTI import vectors differ"));
  let authority_validation = authority_validation () in
  let interface_dependency_imports =
    Array.to_list (imports_of_array interface.Cmi_format.cmi_crcs)
    |> List.filter (fun (imported : import) ->
           not (String.equal imported.unit_name unit_name))
  in
  let dependency_imports =
    List.map (fun imported -> (imported, true)) interface_dependency_imports
    @ (Array.to_list implementation_imports
      |> List.filter (fun (imported : import) ->
             not (String.equal imported.unit_name unit_name))
      |> List.map (fun imported -> (imported, false)))
    |> List.sort (fun ((left : import), _) ((right : import), _) ->
           compare (left.unit_name, left.crc) (right.unit_name, right.crc))
    |> fun imports ->
    let rec coalesce merged = function
      | [] -> List.rev merged
      | ((imported : import), required) :: rest -> (
          match merged with
          | ((existing : import), existing_required) :: tail
            when String.equal existing.unit_name imported.unit_name
                 && existing.crc = imported.crc ->
              coalesce
                ((existing, existing_required || required) :: tail)
                rest
          | _ -> coalesce ((imported, required) :: merged) rest)
    in
    coalesce [] imports
  in
  let dependencies =
    dependency_imports
    |> List.filter_map (fun ((imported : import), interface_required) ->
           let dependency_unit = imported.unit_name in
           match imported.crc with
             | None when interface_required ->
                 raise
                   (Failure
                      "retained interface dependency has no compiler receipt")
             | None -> None
             | Some crc -> (
                 match
                   find_imported_interface_artifact ~load_paths ~unit_name:dependency_unit
                     ~expected_crc:crc ()
                  with
                 | None when interface_required ->
                     raise
                       (Failure
                          "retained interface dependency artifact is missing or stale")
                 | None -> None
                 | Some artifact ->
                     let dependency_authority_receipt =
                       try
                         dependency_authority_receipt_with authority_validation
                           ~load_paths artifact
                       with Failure (reason [@log_value.warn]) as failure ->
                         [%log.warn "rejected retained dependency during authority closure"
                           ~provider:(Delator.Field.string unit_name)
                           ~stage:(Delator.Field.string "dependency-closure")
                           ~route:(Delator.Field.string "exact-cmi-cmti-vri")
                           ~dependency_unit:
                             (Delator.Field.string dependency_unit)
                           ~decision:(Delator.Field.string "rejected")
                           ~reason_class:
                             (Delator.Field.string
                                (reason [@log_value.warn]))];
                         raise failure
                     in
                     if
                       (not interface_required)
                       && Option.is_none dependency_authority_receipt
                     then None
                     else
                       Some
                         {
                           Retained_interface_authority_private.dependency_unit;
                           dependency_compiler_receipt = Some crc;
                           dependency_interface_receipt =
                             artifact.artifact_content_digest;
                           dependency_authority_receipt;
                         }))
    |> List.sort_uniq compare
  in
  {
    Retained_interface_authority_private.provider_unit = unit_name;
    provider_origin = unit_name;
    compiler_abi = Config.cmi_magic_number ^ Config.cmt_magic_number;
    cmi_receipt;
    cmi_self_crc =
      Option.value ~default:"" (interface_self_crc interface);
    cmi_imports;
    cmi_identity = cmi_identity interface;
    cmti_receipt;
    cmti_interface_digest =
      Option.map Digest.to_hex cmti.Cmt_format.cmt_interface_digest;
    cmti_imports;
    cmti_identity = cmti_identity cmti;
    ordinary_cmi_receipts = List.sort_uniq String.compare ordinary_cmi_receipts;
    dependencies;
    payloads =
      [ Retained_interface_authority_private.Broadcast_witnesses
          interface_broadcasts;
        Retained_interface_authority_private.Logical_sorts
          interface_logical_sorts;
        Retained_interface_authority_private.Logical_values
          interface_logical_values ]
      @
      let numeric = retained_numeric_claims interface_numeric_claims in
      if
        numeric.carrier_reconstruction_claims = []
        && numeric.role_reconstruction_claims = []
      then []
      else [ Retained_interface_authority_private.Numeric_claims numeric ];
  }

let discover_retained_authority ~unit_name ~cmi_filename ~load_paths
    ~explicit_filenames ~implicit_discovery ~expected =
  let basename =
    String.uncapitalize_ascii unit_name
    ^ Retained_interface_authority_private.extension
  in
  let candidates =
    (match explicit_filenames with
    | _ :: _ as filenames -> filenames
    | [] when implicit_discovery ->
        [
          Filename.remove_extension cmi_filename
          ^ Retained_interface_authority_private.extension;
        ]
        @ List.map (fun directory -> Filename.concat directory basename) load_paths
    | [] ->
        [%log.debug "required declared retained interface authority"
          ~provider:(Delator.Field.string unit_name)
          ~stage:(Delator.Field.string "sidecar-lookup")
          ~route:(Delator.Field.string "declared-interface-inventory")
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "missing-declared-authority")];
        [])
    |> stable_unique String.equal
    |> List.filter Sys.file_exists
  in
  [%log.debug "discovered retained interface authority candidates"
    ~provider:(Delator.Field.string unit_name)
    ~stage:(Delator.Field.string "sidecar-lookup")
    ~route:(Delator.Field.string "interface-inventory")
    ~candidate_count:(Delator.Field.int (List.length candidates))
    ~decision:
      (Delator.Field.string (if candidates = [] then "rejected" else "inspect"))
    ~reason_class:
      (Delator.Field.string (if candidates = [] then "missing-artifact" else "candidate-set"))];
  let decoded =
    List.map
      (fun filename ->
        match Retained_interface_authority_private.decode (read_all filename) with
        | Ok authority ->
            let[@log_value.trace] members =
              Retained_interface_authority_private.broadcast_witnesses authority
            in
            let[@log_value.trace] _witness_slots =
              List.fold_left
                (fun count member ->
                  count
                  + List.length
                      member.Retained_broadcast_private.source_members)
                0 (members [@log_value.trace])
            in
            [%log.trace "decoded retained interface authority symbols"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "sidecar-canonicalization")
              ~route:(Delator.Field.string "interface-inventory")
              ~declaration_count:
                (Delator.Field.int
                   (List.length (members [@log_value.trace])))
              ~witness_count:
                (Delator.Field.int (_witness_slots [@log_value.trace]))
              ~direct_dependency_count:
                (Delator.Field.int (List.length authority.dependencies))
              ~decision:(Delator.Field.string "accepted")];
            (filename, authority)
        | Error _ ->
            [%log.debug "rejected malformed retained interface authority"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "sidecar-validation")
              ~route:(Delator.Field.string "interface-inventory")
              ~payload_kind:(Delator.Field.string "broadcast-witnesses-v1")
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "malformed-artifact")];
            raise (Failure "retained interface authority artifact is malformed"))
      candidates
  in
  match decoded with
  | [] -> raise (Failure "retained interface authority artifact is missing")
  | (filename, first) :: rest ->
      if
        not
          (List.for_all
             (fun (_, candidate) ->
               Retained_interface_authority_private.equal first candidate)
             rest)
      then (
        [%log.debug "rejected conflicting retained interface authority candidates"
          ~provider:(Delator.Field.string unit_name)
          ~stage:(Delator.Field.string "sidecar-coalescence")
          ~route:(Delator.Field.string "interface-inventory")
          ~candidate_count:(Delator.Field.int (List.length decoded))
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "conflicting-complete-receipts")];
        raise (Failure "retained interface authority candidates conflict"));
      let expected = expected first in
      if not (Retained_interface_authority_private.equal_known first expected) then (
        [%log.debug "rejected mismatched retained interface authority"
          ~provider:(Delator.Field.string unit_name)
          ~stage:(Delator.Field.string "sidecar-validation")
          ~route:(Delator.Field.string "cmi-cmti-vri")
          ~payload_kind:(Delator.Field.string "broadcast-witnesses-v1")
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "component-receipt-mismatch")];
        raise (Failure "retained interface authority component receipt mismatch"));
      let ordinary_directories =
        Filename.dirname cmi_filename
        :: Filename.dirname (Filename.dirname cmi_filename)
        :: load_paths
        |> stable_unique String.equal
      in
      List.iter
        (fun receipt ->
          let candidates =
            artifact_candidates ~extension:".cmi" ~unit_name
              ~adjacent:(Filename.remove_extension cmi_filename ^ ".ordinary.cmi")
              ~load_paths:ordinary_directories
            |> List.filter_map (fun candidate ->
                   try
                     let interface, _ = read_stable_cmi candidate in
                     let unit_matches =
                       Compilation_unit.Name.equal interface.Cmi_format.cmi_name
                         (Compilation_unit.Name.of_string unit_name)
                     and receipt_matches = interface_self_crc interface = Some receipt
                     and family_count =
                       List.length (interface_family_markers interface)
                     and broadcast_count =
                       List.length
                         (interface_broadcast_syntax
                            ~load_paths:ordinary_directories interface)
                     in
                     [%log.trace "inspected ordinary interface authority view candidate"
                       ~provider:(Delator.Field.string unit_name)
                       ~stage:(Delator.Field.string "ordinary-view-binding")
                       ~route:(Delator.Field.string "cmi-vri")
                       ~unit_matches:(Delator.Field.bool unit_matches)
                       ~receipt_matches:(Delator.Field.bool receipt_matches)
                       ~family_count:(Delator.Field.int family_count)
                       ~broadcast_count:(Delator.Field.int broadcast_count)];
                     if
                       unit_matches && receipt_matches && family_count = 0
                       && broadcast_count = 0
                     then Some candidate
                     else None
                   with Cmi_format.Error _ | End_of_file | Sys_error _ -> None)
          in
          match candidates with
          | _ :: _ ->
              [%log.trace "validated ordinary interface authority view"
                ~provider:(Delator.Field.string unit_name)
                ~stage:(Delator.Field.string "ordinary-view-binding")
                ~route:(Delator.Field.string "cmi-vri")
                ~decision:(Delator.Field.string "accepted")]
          | [] ->
              raise
                (Failure
                   "retained authority ordinary interface view is missing or stale"))
        first.ordinary_cmi_receipts;
      [%log.info "completed retained interface authority validation"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "sidecar-validation")
        ~route:(Delator.Field.string "cmi-cmti-vri")
        ~payload_kind:(Delator.Field.string "broadcast-witnesses-v1")
        ~candidate_count:(Delator.Field.int (List.length decoded))
        ~decision:(Delator.Field.string "accepted")];
      (filename, first)

let load_internal ?int_size ?interface_info ?interface_filename
    ?interface_content_receipt ?cmti_filename
    ?(authority_filenames = []) ?(authority_mode = Import_authority)
    ?(implicit_authority_discovery = true)
    ?(ordinary_cmi_receipts = [])
    ?(inventory_load_paths = [])
    (filename [@delator.skip]) =
  match Int_bounds.check_target ?int_size () with
  | Error _ as error -> error
  | Ok () -> (
      let input_error classification =
        Error
          (Diagnostic.make classification (Diagnostic.file_span filename))
      in
      match classify_magic filename with
      | Incompatible -> input_error Incompatible_magic
      | Malformed -> input_error Malformed_input
      | Readable_abi -> (
          try
            let (embedded_interface_info, info), raw_artifact_receipt =
              stable_file_snapshot filename (fun snapshot ->
                  match Cmt_format.read snapshot with
                  | embedded_interface, Some info -> (embedded_interface, info)
                  | _, None -> raise (Failure "missing CMT metadata"))
            in
            let raw_artifact_digest =
              digest_of_content_receipt raw_artifact_receipt
            in
            let embedded_interface =
              Option.is_some embedded_interface_info
            in
            let preflight_provider =
              Compilation_unit.name_as_string info.Cmt_format.cmt_modname
            in
            (match
               validate_numeric_load_path_inventory
                 ~provider:preflight_provider inventory_load_paths
             with
            | Ok () -> ()
            | Error reason ->
                raise
                  (Numeric_claim_failure
                     { provider = preflight_provider; reason; location = None }));
            (* The compiler embeds the implementation interface only when no
               separately compiled interface constrained the unit.  A CMTI is
               optional, so its presence cannot authenticate this boundary. *)
            let explicit_interface = not embedded_interface in
            let interface_filename, interface_info, interface_content_receipt =
              match interface_info with
              | Some explicit ->
                  (interface_filename, Some explicit, interface_content_receipt)
              | None -> (
                  match
                    (adjacent_interface filename, embedded_interface_info)
                  with
                  | Some (filename, adjacent, receipt), _ ->
                      (Some filename, Some adjacent, Some receipt)
                  | None, Some embedded -> (None, Some embedded, None)
                  | None, None ->
                      raise
                        (Failure
                           "separate implementation interface has no adjacent CMI"))
            in
            (match
               validate_numeric_load_path_sequences
                 ~provider:preflight_provider
                 [
                   List.to_seq info.Cmt_format.cmt_loadpath.visible;
                   List.to_seq info.Cmt_format.cmt_loadpath.hidden;
                 ]
             with
            | Ok () -> ()
            | Error reason ->
                raise
                  (Numeric_claim_failure
                     { provider = preflight_provider; reason; location = None }));
            let build_directory, load_path_visible, load_path_hidden =
              relocated_paths ~filename
                ~build_directory:info.Cmt_format.cmt_builddir
                ~compiler_arguments:info.Cmt_format.cmt_args
                info.Cmt_format.cmt_loadpath
            in
            let resolve_interface_path path =
              if not (Filename.is_relative path) then path
              else
                let rec resolve directory =
                  let candidate = Filename.concat directory path in
                  if Sys.file_exists candidate then candidate
                  else
                    let parent = Filename.dirname directory in
                    if String.equal parent directory then path
                    else resolve parent
                in
                resolve (Filename.dirname filename)
            in
            let declared_artifact_directories =
              inventory_load_paths |> List.map resolve_interface_path
              |> stable_unique String.equal
            in
            let interface_load_paths =
              declared_artifact_directories
              @ Option.to_list (Option.map Filename.dirname interface_filename)
              @ load_path_visible @ load_path_hidden
              @ [ Config.standard_library ]
              |> List.map resolve_interface_path |> stable_unique String.equal
            in
            let interface_metadata =
              embedded_interface_metadata ~load_paths:interface_load_paths
                interface_info
            in
            let interface_symbolic_receipts =
              let _, _, _, _, _, _, _, _, _, _, _, declarations, _ =
                interface_metadata
              in
              declarations
            in
            let interface_broadcast_receipts =
              let _, _, _, _, _, _, _, _, _, _, _, _, broadcasts =
                interface_metadata
              in
              broadcasts
            in
            (match interface_info with
            | Some interface -> (
                match explicit_interface_unsupported interface with
                | Some classification ->
                    raise (Unsupported_interface_metadata classification)
                | None ->
                    if not (explicit_interface_identity_matches info interface)
                    then
                      if interface_broadcast_receipts <> [] then
                        raise
                          (Broadcast_artifact_failure
                             {
                               provider =
                                 Compilation_unit.name_as_string
                                   info.Cmt_format.cmt_modname;
                               reason =
                                 "CMT/CMI provider identity receipt mismatch";
                             })
                      else raise (Failure "explicit CMT/CMI identity mismatch"))
            | None -> raise (Failure "missing implementation interface"));
            if
              match interface_info with
              | Some interface -> not (interface_digest_matches info interface)
              | None -> false
            then (
              match (interface_broadcast_receipts, interface_symbolic_receipts) with
              | _ :: _, _ ->
                  raise
                    (Broadcast_artifact_failure
                       {
                         provider =
                           Compilation_unit.name_as_string
                             info.Cmt_format.cmt_modname;
                         reason = "CMT/CMI retained interface receipt mismatch";
                       })
              | [], declaration :: _ ->
                  raise
                    (Symbolic_artifact_failure
                       {
                         provider =
                           (match String.split_on_char '.' declaration.symbolic_path with
                           | provider :: _ -> provider
                           | [] -> "dependency");
                         reason = "CMT/CMI receipt digest mismatch";
                       })
              | [], [] -> raise (Failure "CMT/CMI interface digest mismatch"));
            let source_file = source_file filename info in
            match classify_annots ~source_file info.Cmt_format.cmt_annots with
            | Error _ as error -> error
            | Ok structure ->
                let implementation_metadata = implementation_metadata structure in
                let ( _interface_present,
                      interface_unit_name,
                      interface_implementation_unit_name,
                      interface_parameter_count,
                      interface_imports,
                      interface_family_markers,
                      interface_family_issuers,
                      interface_mode_signatures,
                      interface_finite_signatures,
                      interface_broadcast_declarations,
                      interface_broadcast_groups,
                      interface_symbolic_declarations,
                      interface_broadcast_syntax ) =
                  interface_metadata
                in
                let interface_broadcast_declarations,
                    interface_broadcast_groups, interface_broadcast_syntax =
                  if
                    implementation_metadata.implementation_family_markers
                    = [ "ordinary-v1" ]
                  then (
                    if interface_broadcast_syntax <> [] then
                      [%log.debug "erased imported retained include metadata from ordinary verification view"
                        ~provider:
                          (Delator.Field.string
                             (Compilation_unit.name_as_string
                                info.Cmt_format.cmt_modname))
                        ~stage:(Delator.Field.string "include-sanitization")
                        ~route:(Delator.Field.string "ordinary-post-typing")
                        ~payload_kind:
                          (Delator.Field.string "broadcast-witnesses-v1")
                        ~set_cardinality:
                          (Delator.Field.int
                             (List.length interface_broadcast_syntax))
                        ~decision:(Delator.Field.string "erased")
                        ~reason_class:
                          (Delator.Field.string "ordinary-authority-boundary")];
                    ([], [], []))
                  else
                    ( interface_broadcast_declarations,
                      interface_broadcast_groups,
                      interface_broadcast_syntax )
                in
                let unit_name =
                  Compilation_unit.name_as_string info.Cmt_format.cmt_modname
                in
                let imports = imports info in
                let interface_digest =
                  Option.map Digest.to_hex (implementation_interface_digest info)
                in
                let cmi_filename =
                  match interface_filename with
                  | Some filename -> filename
                  | None ->
                      raise
                        (Broadcast_artifact_failure
                           {
                             provider = unit_name;
                             reason = "has no exact CMI artifact receipt";
                           })
                in
                let typed_interface_filename =
                  Option.value ~default:(Filename.remove_extension cmi_filename ^ ".cmti")
                    cmti_filename
                in
                let typed_interface_artifact =
                  if Sys.file_exists typed_interface_filename then
                    match interface_info with
                    | Some interface -> (
                        match
                          authenticate_typed_interface ~unit_name ~interface
                            typed_interface_filename
                        with
                        | Ok artifact -> Some artifact
                        | Error reason -> raise (Failure reason))
                    | None ->
                        raise
                          (Failure
                             "typed interface has no exact compiler interface")
                  else None
                in
                let typed_interface =
                  Option.map
                    (fun artifact -> artifact.typed_signature)
                    typed_interface_artifact
                in
                let cmi_content_receipt =
                  match interface_content_receipt with
                  | Some receipt -> receipt
                  | None -> artifact_content_receipt cmi_filename
                and cmti_content_receipt =
                  Option.map
                    (fun artifact -> artifact.typed_content_receipt)
                    typed_interface_artifact
                in
                let interface_broadcasts =
                  match interface_broadcast_syntax with
                  | [] -> []
                  | _ :: _ ->
                      let fail reason =
                        raise (Broadcast_artifact_failure { provider = unit_name; reason })
                      in
                      let digest =
                        Option.value ~default:"" interface_digest
                      and family =
                        match interface_family_markers with
                        | [ family ] -> family
                        | [] | _ :: _ :: _ -> fail "has no exact retained family receipt"
                      and route =
                        match interface_family_issuers with
                        | [ route ] -> route
                        | [] | _ :: _ :: _ -> fail "has no exact retained route receipt"
                      in
                      let typed_members =
                        try
                          let interface =
                            match interface_info with
                            | Some interface -> interface
                            | None -> fail "has no retained interface receipt"
                          in
                          typed_interface_broadcast_members
                            ~typed_interface:
                              (match typed_interface with
                              | Some signature -> signature
                              | None ->
                                  fail
                                    "has no authenticated typed interface receipt")
                            ~unit_name
                            ~interface_digest:digest
                            ~load_paths:interface_load_paths ~interface
                            ~interface_syntax:interface_broadcast_syntax
                        with Failure reason -> fail reason
                      in
                      List.map
                        (fun syntax ->
                          let canonical_path =
                            unit_name ^ "." ^ syntax.broadcast_path
                          in
                          let source_members =
                            if
                              syntax.broadcast_kind
                              = Retained_broadcast_private.Declaration
                            then []
                            else
                              let matching =
                                typed_members
                                |> List.filter_map
                                     (fun
                                       (group_path, ordinal, source_path, member)
                                     ->
                                       if
                                         String.equal group_path
                                           syntax.broadcast_path
                                       then Some (ordinal, source_path, member)
                                       else None)
                                |> List.sort (fun (left, _, _) (right, _, _) ->
                                       Int.compare left right)
                              in
                              let ordinals =
                                List.map (fun (ordinal, _, _) -> ordinal) matching
                              in
                              if
                                ordinals
                                <> List.init syntax.broadcast_member_count Fun.id
                              then (
                                [%log.debug "rejected retained broadcast witness ordinals"
                                  ~provider:(Delator.Field.string unit_name)
                                  ~stage:(Delator.Field.string "typed-witness")
                                  ~route:
                                    (Delator.Field.string "cmti-typed-signature")
                                  ~witness_count:
                                    (Delator.Field.int (List.length ordinals))
                                  ~expected_count:
                                    (Delator.Field.int
                                       syntax.broadcast_member_count)
                                  ~decision:(Delator.Field.string "rejected")
                                  ~reason_class:
                                    (Delator.Field.string "witness-ordinal-set")];
                                fail
                                  "typed retained broadcast member witnesses are incomplete, duplicated, or out of range"
                              );
                              let source_paths =
                                matching
                                |> List.map (fun (_, source_path, _) -> source_path)
                                |> List.sort String.compare
                              in
                              if source_paths <> syntax.broadcast_member_paths then (
                                [%log.debug "rejected retained broadcast CMI witness-map receipt"
                                  ~provider:(Delator.Field.string unit_name)
                                  ~stage:(Delator.Field.string "typed-witness")
                                  ~route:
                                    (Delator.Field.string "cmi-cmti-witness-map")
                                  ~set_cardinality:
                                    (Delator.Field.int (List.length source_paths))
                                  ~decision:(Delator.Field.string "rejected")
                                  ~reason_class:
                                    (Delator.Field.string "complete-map-receipt")];
                                fail
                                  "retained broadcast CMI and CMTI witness maps differ"
                              );
                              let members =
                                List.map (fun (_, _, member) -> member) matching
                              in
                              let member_key member =
                                ( member.Retained_broadcast_private.member_compiler_uid,
                                  Retained_broadcast_private.kind_name
                                    member.member_kind,
                                  member.member_canonical_path )
                              in
                              let sorted =
                                List.sort
                                  (fun left right ->
                                    compare (member_key left) (member_key right))
                                  members
                              in
                              let rec duplicate = function
                                | left :: (right :: _ as rest) ->
                                    if member_key left = member_key right then true
                                    else duplicate rest
                                | [] | [ _ ] -> false
                              in
                              if duplicate sorted then (
                                [%log.debug "rejected duplicate complete retained broadcast witness"
                                  ~provider:(Delator.Field.string unit_name)
                                  ~stage:(Delator.Field.string "typed-witness")
                                  ~route:
                                    (Delator.Field.string "cmi-cmti-witness-map")
                                  ~set_cardinality:
                                    (Delator.Field.int (List.length sorted))
                                  ~decision:(Delator.Field.string "rejected")
                                  ~reason_class:
                                    (Delator.Field.string "duplicate-full-identity")];
                                fail
                                  "retained broadcast group contains duplicate typed members"
                              );
                              sorted
                          in
                          let dependency_receipt =
                            import_receipt interface_imports
                          in
                          let witness_receipt =
                            broadcast_witness_receipt ~provider_origin:unit_name
                              ~interface_digest:digest
                              ~compiler_uid:syntax.broadcast_uid ~canonical_path
                              ~dependency_receipt source_members
                          in
                          match
                            Retained_broadcast_private.make_identity
                              ~provider_origin:unit_name ~interface_digest:digest
                              ~compiler_uid:syntax.broadcast_uid
                              ~kind:syntax.broadcast_kind ~canonical_path
                              ~dependency_receipt ~witness_receipt
                              ~artifact_family:family ~provider_route:route
                          with
                          | Ok identity ->
                              {
                                Retained_broadcast_private.identity;
                                source_members;
                              }
                          | Error reason -> fail reason)
                        interface_broadcast_syntax
                in
                let interface_logical_sorts =
                  match interface_info with
                  | None -> []
                  | Some interface ->
                      let issuer =
                        match interface_family_issuers with
                        | [ issuer ] -> issuer
                        | [] | _ :: _ :: _ -> ""
                      in
                      interface_logical_sorts ~issuer interface
                in
                let interface_logical_values =
                  match interface_info with
                  | None -> []
                  | Some interface ->
                      interface_logical_values
                        ~load_paths:interface_load_paths
                        ~symbolic_declarations:interface_symbolic_declarations
                        interface
                in
                let interface_numeric_claims =
                  match interface_info with
                  | None ->
                      { numeric_carriers = []; numeric_roles = [];
                        numeric_provenance_nodes = 0;
                        numeric_provenance_edges = 0;
                        numeric_provenance_bytes = 0 }
                  | Some interface ->
                      interface_numeric_claims
                        ~load_paths:interface_load_paths
                        ~root_cmi_receipt:cmi_content_receipt
                        ?typed_interface
                        ?implementation_shape:info.Cmt_format.cmt_impl_shape
                        ~structure interface
                in
                let has_retained_authority =
                  interface_broadcast_syntax <> []
                  || interface_logical_sorts <> []
                  || interface_logical_values <> []
                  || interface_numeric_claims.numeric_carriers <> []
                  || interface_numeric_claims.numeric_roles <> []
                in
                let retained_authority_filename, retained_authority,
                    interface_broadcasts =
                  match (interface_info, has_retained_authority) with
                  | Some interface, true ->
                      let cmti_content_receipt =
                        match cmti_content_receipt with
                        | Some receipt -> receipt
                        | None ->
                            raise
                              (Broadcast_artifact_failure
                                 { provider = unit_name;
                                   reason =
                                     "has no authenticated typed interface receipt" })
                      in
                      (try
                         match authority_mode with
                      | Emit_authority ->
                          let expected =
                            retained_authority_record ~unit_name
                              ~cmi_receipt:cmi_content_receipt
                              ~cmti_filename:typed_interface_filename
                              ~cmti_receipt:cmti_content_receipt
                              ~ordinary_cmi_receipts
                              ~implementation_imports:imports
                              ~load_paths:interface_load_paths interface
                              interface_broadcasts interface_logical_sorts
                              interface_logical_values interface_numeric_claims
                          in
                          [%log.info "completed retained interface authority generation"
                            ~provider:(Delator.Field.string unit_name)
                            ~stage:(Delator.Field.string "sidecar-generation")
                            ~route:(Delator.Field.string "post-typing-emitter")
                            ~payload_kind:
                              (Delator.Field.string "broadcast-witnesses-v1")
                            ~set_cardinality:
                              (Delator.Field.int (List.length interface_broadcasts))
                            ~decision:(Delator.Field.string "accepted")];
                          (None, Some expected, interface_broadcasts)
                      | Import_authority ->
                          let authority_filename, authority =
                            discover_retained_authority ~unit_name ~cmi_filename
                              ~load_paths:interface_load_paths
                              ~explicit_filenames:authority_filenames
                              ~implicit_discovery:implicit_authority_discovery
                              ~expected:(fun authority ->
                                validate_authority_payload ~unit_name
                                  ?implementation_shape:info.Cmt_format.cmt_impl_shape
                                  ~cmti_filename:typed_interface_filename
                                  ~load_paths:interface_load_paths ~structure interface
                                  authority;
                                [%log.debug
                                  "independently recomputed direct-provider retained witnesses"
                                  ~provider:(Delator.Field.string unit_name)
                                  ~stage:
                                    (Delator.Field.string
                                       "direct-provider-witness-validation")
                                  ~route:
                                    (Delator.Field.string "exact-cmti-vri")
                                  ~set_cardinality:
                                    (Delator.Field.int
                                       (List.length interface_broadcasts))
                                  ~decision:(Delator.Field.string "accepted")];
                                retained_authority_record ~unit_name
                                  ~cmi_receipt:cmi_content_receipt
                                  ~cmti_filename:typed_interface_filename
                                  ~cmti_receipt:cmti_content_receipt
                                  ~ordinary_cmi_receipts:
                                    authority.ordinary_cmi_receipts
                                  ~implementation_imports:imports
                                  ~load_paths:interface_load_paths interface
                                  interface_broadcasts interface_logical_sorts
                                  interface_logical_values
                                  interface_numeric_claims)
                          in
                          ( Some authority_filename,
                            Some authority,
                            interface_broadcasts )
                       with Failure reason ->
                         raise
                           (Broadcast_artifact_failure
                              { provider = unit_name; reason }))
                  | None, true ->
                      raise
                        (Broadcast_artifact_failure
                           {
                             provider = unit_name;
                             reason = "has no exact retained interface";
                           })
                  | _, false -> (None, None, [])
                in
                (match (interface_info, interface_broadcasts) with
                | Some interface, _ :: _ ->
                    (try
                       authenticate_implementation_imported_witnesses ~unit_name
                         ~imports ~load_paths:interface_load_paths interface
                         structure interface_broadcasts
                     with Failure reason ->
                       raise
                         (Broadcast_artifact_failure
                            { provider = unit_name; reason }))
                | None, _ | Some _, [] -> ());
                (if interface_symbolic_declarations <> [] then
                  if not implementation_metadata.implementation_metadata_valid
                  then
                    raise
                      (Symbolic_artifact_failure
                         {
                           provider = unit_name;
                           reason = "has malformed implementation metadata";
                         })
                  else
                    let implementation_receipt =
                      implementation_metadata.implementation_family_markers
                      = [ "retained-v1" ]
                      &&
                      match
                        implementation_metadata.implementation_family_issuers
                      with
                      | [ "standalone-v1" ] | [ "ppxlib-v1" ] -> true
                      | [] | [ _ ] | _ :: _ :: _ -> false
                    and interface_receipt =
                      ((not explicit_interface)
                      && interface_family_markers = []
                      && interface_family_issuers = [])
                      || (interface_family_markers = [ "retained-v1" ]
                         && interface_family_issuers
                            = implementation_metadata.implementation_family_issuers)
                    in
                    if not (implementation_receipt && interface_receipt) then
                      raise
                        (Symbolic_artifact_failure
                           {
                             provider = unit_name;
                             reason =
                               "does not have one matching retained issuer receipt";
                           }));
                (if interface_broadcasts <> [] then
                  let implementation_receipt =
                    implementation_metadata.implementation_metadata_valid
                    && implementation_metadata.implementation_family_markers
                       = [ "retained-v1" ]
                    && implementation_metadata.implementation_family_issuers
                       = interface_family_issuers
                  and interface_receipt =
                    interface_family_markers = [ "retained-v1" ]
                    &&
                    match interface_family_issuers with
                    | [ "standalone-v1" ] | [ "ppxlib-v1" ] -> true
                    | [] | [ _ ] | _ :: _ :: _ -> false
                  in
                  if not (implementation_receipt && interface_receipt) then
                    raise
                      (Broadcast_artifact_failure
                         {
                           provider = unit_name;
                           reason =
                             "does not have one matching retained implementation and interface receipt";
                         }));
                (if interface_broadcasts <> [] then
                   [%log.info "completed retained broadcast artifact load"
                     ~provider:(Delator.Field.string unit_name)
                     ~route:(Delator.Field.string "cmt-cmi")
                     ~stage:(Delator.Field.string "artifact-load")
                     ~set_cardinality:
                       (Delator.Field.int (List.length interface_broadcasts))
                     ~decision:(Delator.Field.string "accepted")]);
                let implementation =
                  {
                    metadata = info;
                    embedded_interface_metadata = interface_info;
                    raw_artifact_digest;
                    raw_artifact_receipt;
                    filename;
                    source_file;
                    unit_name;
                    interface_digest;
                    interface_filename;
                    retained_authority_filename;
                    retained_authority;
                    retained_authority_receipt =
                      Option.map artifact_content_receipt
                        retained_authority_filename;
                    retained_authority_index =
                      Option.map Retained_interface_authority_private.index
                        retained_authority;
                    interface_view_receipts =
                      Option.value ~default:[]
                        (Option.map
                           (fun authority -> authority.Retained_interface_authority_private.ordinary_cmi_receipts)
                           retained_authority);
                    structure;
                    imports;
                    compiler_arguments =
                      Array.copy info.Cmt_format.cmt_args;
                    source_digest = info.Cmt_format.cmt_source_digest;
                    build_directory;
                    load_path_visible;
                    load_path_hidden;
                    declared_artifact_directories;
                    embedded_interface;
                    explicit_interface;
                    interface_unit_name;
                    interface_implementation_unit_name;
                    interface_parameter_count;
                    interface_imports;
                    implementation_family_markers =
                      implementation_metadata.implementation_family_markers;
                    implementation_family_issuers =
                      implementation_metadata.implementation_family_issuers;
                    ppxlib_context = implementation_metadata.ppxlib_context;
                    verification_scope_markers =
                      implementation_metadata.verification_scope_markers;
                    implementation_metadata_valid =
                      implementation_metadata.implementation_metadata_valid;
                    interface_family_markers;
                    interface_family_issuers;
                    interface_mode_signatures;
                    interface_finite_signatures;
                    interface_broadcast_declarations;
                    interface_broadcast_groups;
                    interface_symbolic_declarations;
                    interface_logical_values;
                    interface_logical_sorts;
                    interface_numeric_claims;
                    interface_broadcasts;
                    declaration_dependency_count =
                      List.length info.Cmt_format.cmt_declaration_dependencies;
                    has_implementation_shape =
                      Option.is_some info.Cmt_format.cmt_impl_shape;
                    identifier_occurrence_count =
                      Array.length info.Cmt_format.cmt_ident_occurrences;
                  }
                in
                [%log.debug "loaded compiler artifact"
                  ~filename:(Delator.Field.string filename)
                  ~unit_name:(Delator.Field.string implementation.unit_name)
                  ~imports:
                    (Delator.Field.int (Array.length implementation.imports))
                  ~visible_load_paths:
                    (Delator.Field.int
                       (List.length implementation.load_path_visible))
                  ~hidden_load_paths:
                    (Delator.Field.int
                       (List.length implementation.load_path_hidden))
                  ~declared_artifact_directory_count:
                    (Delator.Field.int
                       (List.length
                          implementation.declared_artifact_directories))
                  ~retained:
                    (Delator.Field.bool
                       (retained_preprocessing implementation))
                  ~broadcast_declarations:
                    (Delator.Field.int
                       (List.length
                          implementation.interface_broadcast_declarations))
                  ~broadcast_groups:
                    (Delator.Field.int
                       (List.length implementation.interface_broadcast_groups))
                  ~retained_broadcasts:
                    (Delator.Field.int
                       (List.length implementation.interface_broadcasts))
                  ~decision:(Delator.Field.string "accepted")];
                Ok implementation
          with
          | Numeric_claim_failure { provider; reason; location } ->
              [%log.debug "routing numeric declaration rejection"
                ~provider:(Delator.Field.string provider)
                ~stage:(Delator.Field.string "artifact-diagnostic")
                ~failure_class:
                  (Delator.Field.string "numeric-source-claim")
                ~decision:(Delator.Field.string "rejected")];
              let span =
                match location with
                | None -> Diagnostic.file_span filename
                | Some location ->
                    Diagnostic.span_of_location ~fallback_file:filename location
              in
              Error
                (Diagnostic.make
                   (Diagnostic.Invalid_numeric_declaration
                      ("Invalid numeric declaration in " ^ provider ^ ": "
                     ^ reason))
                   span)
          | Symbolic_artifact_failure { provider; reason } ->
              [%log.debug "routing typed symbolic artifact rejection"
                ~provider:(Delator.Field.string provider)
                ~stage:(Delator.Field.string "artifact-diagnostic")
                ~failure_class:
                  (Delator.Field.string "symbolic-artifact-receipt")
                ~remedy_class:
                  (Delator.Field.string "rebuild-provider-consumer")];
              input_error
                (Diagnostic.Invalid_symbolic_dependency
                   {
                     provider;
                     reason = "typed symbolic interface receipt " ^ reason;
                     remedy =
                       "rebuild the provider and consumer with the official retained VeroCaml PPX";
                   })
          | Broadcast_artifact_failure { provider; reason = _reason } ->
              let failure = broadcast_artifact_failure _reason in
              [%log.debug "routing retained broadcast artifact rejection"
                ~provider:(Delator.Field.string provider)
                ~route:(Delator.Field.string "artifact-load")
                ~stage:(Delator.Field.string "broadcast-artifact-receipt")
                ~failure_class:(Delator.Field.string "dependency-artifact")
                  ~cause_class:
                    (Delator.Field.string
                       (_broadcast_artifact_failure_name failure))
                  ~reason_class:(Delator.Field.string _reason)
                ~correlation:
                  (Delator.Field.string
                     (Digest.string
                        (provider ^ ":artifact-load:"
                       ^ _broadcast_artifact_failure_name failure)
                     |> Digest.to_hex))
                ~decision:(Delator.Field.string "rejected")
                ~remedy_class:
                  (Delator.Field.string "rebuild-provider-consumer")];
              input_error
                (Diagnostic.Invalid_broadcast_dependency
                   {
                     provider;
                     failure;
                   })
          | Unsupported_interface_metadata classification ->
              input_error classification
          | Failure (reason [@log_value.warn])
          | Invalid_argument (reason [@log_value.warn]) ->
              [%log.warn "rejected malformed compiler artifact"
                ~stage:(Delator.Field.string "artifact-load")
                ~route:(Delator.Field.string "cmt-cmi")
                ~reason:
                  (Delator.Field.string (reason [@log_value.warn]))
                ~decision:(Delator.Field.string "rejected")];
              input_error Malformed_input
          | Cmt_format.Error _ | Cmi_format.Error _ | End_of_file ->
              input_error Malformed_input
          | Sys_error _ ->
              artifact_io_warning ~route:"cmt-cmi";
              input_error Input_io_error)
      | exception End_of_file -> input_error Malformed_input
      | exception Sys_error _ ->
          artifact_io_warning ~route:"cmt";
          input_error Input_io_error)
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let load ?int_size (filename [@delator.field Fun.id]) =
  load_internal ?int_size filename
[@@delator.instrument]
[@@delator.level trace]
[@@delator.no_exn_log]

let normalization_load_paths (implementation : implementation) =
  implementation.declared_artifact_directories
  @ Option.to_list
    (Option.map Filename.dirname implementation.interface_filename)
  @ implementation.load_path_visible @ implementation.load_path_hidden
  @ [ Config.standard_library ]
  |> List.map (fun path ->
         if not (Filename.is_relative path) then path
         else
           let rec resolve directory =
             let candidate = Filename.concat directory path in
             if Sys.file_exists candidate then candidate
             else
               let parent = Filename.dirname directory in
               if String.equal parent directory then candidate
               else resolve parent
           in
           resolve (Filename.dirname implementation.filename))
  |> stable_unique String.equal

let broadcast_complete_receipt (implementation : implementation) =
  match implementation.retained_authority_index with
  | Some index -> index
  | None ->
      implementation.interface_broadcasts
      |> List.map (fun (member : Retained_broadcast_private.interface_member) ->
             member.identity.witness_receipt)
      |> List.sort String.compare |> String.concat "|" |> Digest.string
      |> Digest.to_hex

let normalize_value_path (implementation : implementation) location environment
    path =
  try
    let artifacts =
      Array.to_list implementation.imports
      |> List.filter_map (fun (imported : import) ->
             if String.equal imported.unit_name implementation.unit_name then None
             else
               Option.bind imported.crc (fun expected_crc ->
                   find_imported_interface_artifact
                     ~load_paths:(normalization_load_paths implementation)
                     ~unit_name:imported.unit_name ~expected_crc ()))
    in
    Some
      (with_authenticated_interfaces artifacts (fun () ->
           let environment =
             Envaux.env_of_only_summary ~allow_missing_modules:true environment
           in
           Env.normalize_value_path (Some location) environment path))
  with Env.Error _ | Failure _ ->
    [%log.debug "rejected compiler path normalization"
      ~provider:(Delator.Field.string implementation.unit_name)
      ~stage:(Delator.Field.string "compiler-normalization")
      ~route:(Delator.Field.string "implementation-typedtree")
      ~decision:(Delator.Field.string "rejected")
    ~reason_class:(Delator.Field.string "missing-or-ambiguous-interface")];
    None
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let load_with_interface ?int_size ?cmti ?vri ?(vri_candidates = [])
    ?(artifact_directories = [])
    ?(implicit_authority_discovery = true)
    ~cmt:(cmt [@delator.field Fun.id])
    ~cmi:(cmi [@delator.field Fun.id]) () =
  match Int_bounds.check_target ?int_size () with
  | Error _ as error -> error
  | Ok () ->
      let input_error classification =
        Error (Diagnostic.make classification (Diagnostic.file_span cmi))
      in
      (match classify_magic cmi with
      | Incompatible -> input_error Incompatible_magic
      | Malformed -> input_error Malformed_input
      | Readable_abi -> (
          try
            let interface_info, interface_content_receipt =
              read_stable_cmi cmi
            in
            match explicit_interface_unsupported interface_info with
            | Some classification -> input_error classification
            | None ->
                let authority_filenames =
                  Option.to_list vri @ vri_candidates
                  |> stable_unique String.equal
                in
                load_internal ?int_size ~interface_info
                  ~interface_filename:cmi ~interface_content_receipt
                  ?cmti_filename:cmti ~authority_filenames
                  ~implicit_authority_discovery
                  ~inventory_load_paths:artifact_directories cmt
          with
          | Cmi_format.Error _ | End_of_file | Failure _ | Invalid_argument _ ->
              input_error Malformed_input)
      | exception End_of_file -> input_error Malformed_input
      | exception Sys_error _ ->
          artifact_io_warning ~route:"cmi";
          input_error Input_io_error)
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let emit_retained_interface_authority ?int_size ?(ordinary_cmis = [])
    ?(artifact_directories = []) ~cmt ~cmi ~cmti ~output () =
  try
    let artifact_directories =
      artifact_directories
      |> List.map (fun directory ->
             if Filename.is_relative directory then
               Filename.concat (Sys.getcwd ()) directory
             else directory)
      |> List.sort_uniq String.compare
    in
    let ordinary_cmi_receipts =
    ordinary_cmis
    |> List.map (fun filename ->
           let interface, _ = read_stable_cmi filename in
           match interface_self_crc interface with
           | Some receipt -> receipt
           | None -> raise (Failure "ordinary CMI view has no compiler receipt"))
    |> List.sort_uniq String.compare
    in
    let interface_info, interface_content_receipt = read_stable_cmi cmi in
    match
      load_internal ?int_size ~interface_info
        ~interface_content_receipt
        ~interface_filename:cmi ~cmti_filename:cmti
        ~ordinary_cmi_receipts
        ~inventory_load_paths:artifact_directories
        ~authority_mode:Emit_authority cmt
    with
    | Error _ as error -> error
    | Ok implementation -> (
      match implementation.retained_authority with
      | None -> Ok ()
      | Some authority ->
          let encoded = Retained_interface_authority_private.encode authority in
          (match Retained_interface_authority_private.decode encoded with
          | Ok decoded
            when Retained_interface_authority_private.equal authority decoded ->
              ()
          | Ok _ | Error _ ->
              raise
                (Failure
                   "retained interface authority exceeds canonical decoder bounds"));
          let temporary = ref None in
          (try
             let filename =
               Filename.temp_file ~temp_dir:(Filename.dirname output)
                 (Filename.basename output ^ ".tmp-") ""
             in
             temporary := Some filename;
             let channel = open_out_bin filename in
             Fun.protect
               ~finally:(fun () -> close_out_noerr channel)
               (fun () ->
                 output_string channel encoded;
                 flush channel;
                 Unix.fsync (Unix.descr_of_out_channel channel));
             Unix.rename filename output;
             temporary := None;
             [%log.info "installed retained interface authority artifact"
               ~provider:(Delator.Field.string implementation.unit_name)
               ~stage:(Delator.Field.string "sidecar-install")
               ~route:(Delator.Field.string "post-typing-emitter")
               ~payload_kind:(Delator.Field.string "broadcast-witnesses-v1")
               ~decision:(Delator.Field.string "accepted")];
             Ok ()
           with Sys_error _ | Unix.Unix_error _ ->
             Option.iter
               (fun filename ->
                 try if Sys.file_exists filename then Sys.remove filename
                 with Sys_error _ -> ())
               !temporary;
             Error
               (Diagnostic.make Diagnostic.Input_io_error
                  (Diagnostic.file_span output))))
  with
  | Cmi_format.Error _ | End_of_file | Failure _ | Invalid_argument _ ->
      Error
        (Diagnostic.make Diagnostic.Malformed_input (Diagnostic.file_span cmi))
  | Sys_error _ | Unix.Unix_error _ ->
      Error
        (Diagnostic.make Diagnostic.Input_io_error (Diagnostic.file_span output))
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let retained_authority_matches ~vri ~cmt ~cmi =
  try
    match Retained_interface_authority_private.decode (read_all vri) with
    | Error _ -> false
    | Ok authority ->
        let information = Cmt_format.read_cmt cmt in
        String.equal authority.provider_unit
          (Compilation_unit.name_as_string information.Cmt_format.cmt_modname)
        && String.equal authority.cmi_receipt (artifact_content_receipt cmi)
  with Cmt_format.Error _ | End_of_file | Sys_error _ -> false
[@@delator.instrument]
[@@delator.level trace]
[@@delator.no_exn_log]

let erase_retained_interface_authority ~input ~output =
  let erased_values = ref 0
  and erased_modules = ref 0
  and stripped_attributes = ref 0 in
  let verification_attribute attribute =
    let name = attribute.Parsetree.attr_name.txt in
    String.starts_with ~prefix:"verocaml.internal." name
    || String.starts_with ~prefix:"verocaml." name
  in
  let authority_value attributes =
    List.exists
      (fun attribute ->
        let name = attribute.Parsetree.attr_name.txt in
        List.mem name
          [
            "verocaml.proof";
            "verocaml.spec";
            "verocaml.axiom";
            "verocaml.type_invariant";
            "verocaml.external_specification";
            "verocaml.internal.symbolic.interface.v1";
            "verocaml.internal.broadcast.interface_declaration.v1";
            "verocaml.internal.broadcast.interface_group.v1";
          ])
      attributes
  in
  let ordinary_attributes attributes =
    let ordinary =
      List.filter (fun attribute -> not (verification_attribute attribute))
        attributes
    in
    stripped_attributes :=
      !stripped_attributes + List.length attributes - List.length ordinary;
    ordinary
  in
  let rec persistent_root = function
    | Path.Pident ident -> Ident.is_global_or_predef ident
    | Pdot (parent, _) -> persistent_root parent
    | Papply _ | Pextra_ty _ -> true
  in
  let rec module_type = function
    | Types.Mty_signature signature ->
        Some (Types.Mty_signature (signature_items signature))
    | Types.Mty_functor (parameter, result) ->
        let parameter =
          match parameter with
          | Types.Unit -> Some Types.Unit
          | Types.Named (ident, argument) ->
              Option.map (fun argument -> Types.Named (ident, argument))
                (module_type argument)
        in
        Option.bind parameter (fun parameter ->
            Option.map
              (fun result -> Types.Mty_functor (parameter, result))
              (module_type result))
    | Types.Mty_strengthen (nested, path, aliasability) ->
        Option.map
          (fun nested -> Types.Mty_strengthen (nested, path, aliasability))
          (module_type nested)
    | Types.Mty_ident path when persistent_root path ->
        [%log.trace "erased persistent module-type reexport from ordinary view"
          ~stage:(Delator.Field.string "ordinary-interface-erasure")
          ~route:(Delator.Field.string "recursive-signature")
          ~member_kind:(Delator.Field.string "module-type-reference")
          ~decision:(Delator.Field.string "erased")
          ~reason_class:(Delator.Field.string "uninspectable-reexport")];
        None
    | Types.Mty_ident _ as unchanged -> Some unchanged
    | Types.Mty_alias _ ->
        [%log.trace "erased module alias reexport from ordinary view"
          ~stage:(Delator.Field.string "ordinary-interface-erasure")
          ~route:(Delator.Field.string "recursive-signature")
          ~member_kind:(Delator.Field.string "module-alias")
          ~decision:(Delator.Field.string "erased")
          ~reason_class:(Delator.Field.string "uninspectable-reexport")];
        None
  and signature_items signature =
    List.filter_map
      (function
        | Types.Sig_value (_, description, _)
          when authority_value description.val_attributes ->
            incr erased_values;
            None
        | Types.Sig_value (ident, description, visibility) ->
            Some
              (Types.Sig_value
                 ( ident,
                   {
                     description with
                     Types.val_attributes =
                       ordinary_attributes description.val_attributes;
                   },
                   visibility ))
        | Types.Sig_type (ident, declaration, recursion, visibility) ->
            Some
              (Types.Sig_type
                 ( ident,
                   {
                     declaration with
                     Types.type_attributes =
                       ordinary_attributes declaration.type_attributes;
                   },
                   recursion,
                   visibility ))
        | Types.Sig_module (ident, presence, declaration, recursion, visibility) ->
            Option.map
              (fun md_type ->
                Types.Sig_module
                  ( ident,
                    presence,
                    {
                      declaration with
                      Types.md_type;
                      md_attributes =
                        ordinary_attributes declaration.md_attributes;
                    },
                    recursion,
                    visibility ))
              (module_type declaration.md_type)
            |> (function
                 | Some _ as retained -> retained
                 | None ->
                     incr erased_modules;
                     None)
        | Types.Sig_modtype (ident, declaration, visibility) ->
            Some
              (Types.Sig_modtype
                 ( ident,
                   {
                     declaration with
                     Types.mtd_type = Option.bind declaration.mtd_type module_type;
                     mtd_attributes = ordinary_attributes declaration.mtd_attributes;
                   },
                   visibility ))
        | Types.Sig_typext (ident, declaration, status, visibility) ->
            Some
              (Types.Sig_typext
                 ( ident,
                   {
                     declaration with
                     Types.ext_attributes =
                       ordinary_attributes declaration.ext_attributes;
                   },
                   status,
                   visibility ))
        | Types.Sig_class (ident, declaration, recursion, visibility) ->
            Some
              (Types.Sig_class
                 ( ident,
                   {
                     declaration with
                     Types.cty_attributes =
                       ordinary_attributes declaration.cty_attributes;
                   },
                   recursion,
                   visibility ))
        | Types.Sig_class_type (ident, declaration, recursion, visibility) ->
            Some
              (Types.Sig_class_type
                 ( ident,
                   {
                     declaration with
                     Types.clty_attributes =
                       ordinary_attributes declaration.clty_attributes;
                   },
                   recursion,
                   visibility )))
      signature
  in
  try
    let interface, _ = read_stable_cmi input in
    let signature =
      Subst.Lazy.force_signature interface.Cmi_format.cmi_sign
      |> signature_items |> Subst.Lazy.of_signature
    in
    let temporary =
      Filename.temp_file ~temp_dir:(Filename.dirname output)
        (Filename.basename output ^ ".tmp-") ""
    in
    Fun.protect
      ~finally:(fun () ->
        try if Sys.file_exists temporary then Sys.remove temporary
        with Sys_error _ -> ())
      (fun () ->
        let channel = open_out_bin temporary in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () ->
            ignore
              (Cmi_format.output_cmi temporary channel
                 {
                   interface with
                   Cmi_format.cmi_sign = signature;
                   cmi_crcs =
                     Array.to_list interface.cmi_crcs
                     |> List.filter (fun imported ->
                            not
                              (Compilation_unit.Name.equal
                                 (Import_info.name imported)
                                 interface.cmi_name))
                     |> Array.of_list;
                 });
            flush channel;
            Unix.fsync (Unix.descr_of_out_channel channel));
        Unix.rename temporary output);
    [%log.info "installed ordinary erased interface view"
      ~stage:(Delator.Field.string "ordinary-interface-erasure")
      ~route:(Delator.Field.string "package-artifact-family")
      ~erased_value_count:(Delator.Field.int !erased_values)
      ~erased_module_count:(Delator.Field.int !erased_modules)
      ~stripped_attribute_count:(Delator.Field.int !stripped_attributes)
      ~decision:(Delator.Field.string "accepted")];
    Ok ()
  with
  | Cmi_format.Error _ | End_of_file | Invalid_argument _ ->
      Error
        (Diagnostic.make Diagnostic.Malformed_input (Diagnostic.file_span input))
  | Sys_error _ | Unix.Unix_error _ ->
      Error
        (Diagnostic.make Diagnostic.Input_io_error (Diagnostic.file_span output))
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let implementation_type_logical_sorts (implementation [@delator.skip]) ~implementation_uid =
  let matches = match implementation.embedded_interface_metadata, implementation.metadata.Cmt_format.cmt_impl_shape with
    | Some interface, Some shape ->
        let signature = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
        List.filter (fun descriptor ->
          String.equal descriptor.Logical_sort_private.provider_origin implementation.unit_name
          && (let components = String.split_on_char '.' descriptor.type_path in
              match components with
              | root :: components when String.equal root implementation.unit_name ->
                  (match signature_type_uid signature components with
                  | Some (Signature_type_uid uid) when String.equal uid descriptor.type_uid ->
                      implementation_export_uid ~unit_name:implementation.unit_name
                        ~environment:implementation.structure.str_final_env
                        ~namespace:Shape.Sig_component_kind.Type shape descriptor.type_path
                      = Some implementation_uid
                  | _ -> false)
              | _ -> false)) implementation.interface_logical_sorts
        |> List.sort_uniq Logical_sort_private.compare
    | _ -> [] in
  [%log.trace "correlated local logical type through its exact compiler export"
    ~provider:(Delator.Field.string implementation.unit_name)
    ~implementation_uid:(Delator.Field.string implementation_uid)
    ~matching_receipts:(Delator.Field.int (List.length matches))
    ~identity_basis:(Delator.Field.string "source-uid-shape-export-cmi-slot")];
  matches
[@@delator.instrument] [@@delator.level trace]
