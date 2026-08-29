open Typedtree

type request_site =
  | Binding
  | Expression
  | Pattern
  | Formal
  | Result
  | Field
  | Component_type

type request = {
  site : request_site;
  index : int option;
  mode : Sst.instance_mode;
  subject_span : Diagnostic.span;
  annotation_span : Diagnostic.span;
}

type sealed_identity = {
  source_file : string;
  cmt_file : string;
  unit_name : string;
  interface_digest : string;
  source_digest : string;
  signature_snapshot : string;
  explicit_interface : bool;
  interface_mode_signatures : (string * string option) list;
  interface_finite_signatures : (string * string option) list;
}

type registration = {
  program : Sst.program;
  structure : Typedtree.structure;
  requests : request list;
  family_markers : string list;
  raw_snapshot : string;
  mutable sealed : sealed_identity option;
}

type subject =
  | Binding_subject of Sst.function_id * Sst.binding
  | Expression_subject of Sst.function_id * Sst.expression
  | Pattern_subject of Sst.function_id * Sst.pattern
  | Field_subject of Sst.field_definition
  | Formal_subject of Sst.function_definition * int * Sst.parameter
  | Result_subject of Sst.function_definition

type descriptor = {
  issuer : unit ref;
  authentication_index : int;
  registration : registration option;
  snapshot : string;
  identity : string Lazy.t;
  subject : subject;
  mode : Sst.instance_mode;
  explicit : bool;
  annotation_span : Diagnostic.span option;
  defaulting_context : string;
  typ : Sst.typ;
  signature_snapshot : string;
}

module Descriptor_identity = struct
  type t = descriptor

  let equal left right = left == right
  let hash descriptor = descriptor.authentication_index
end

module Descriptor_table = Hashtbl.Make (Descriptor_identity)

type environment = {
  issuer : unit ref;
  program : Sst.program;
  registration : registration option;
  authenticated : bool;
  snapshot : string;
  signature_snapshot : string;
  mutable descriptors : descriptor list;
  issued_descriptors : unit Descriptor_table.t;
  mutable next_descriptor_index : int;
  mutable tracked_formal_authority : (Sst.function_id * int) list;
  mutable forgetting_traces : string list;
  mutable standalone_proof_binding_traces : string list;
}

type error = {
  function_id : Sst.function_id option;
  span : Diagnostic.span;
  message : string;
}

let process_issuer = ref ()
let registrations : registration list ref = ref []
let ghost_formal_flow_count = ref 0

type builtin_local_assertion_mode = {
  builtin_mode_program : Sst.program;
  builtin_mode_function : Sst.function_id;
  builtin_mode_expression_span : Diagnostic.span;
  builtin_mode_predicate_span : Diagnostic.span;
  builtin_mode_ordinal : int;
  builtin_mode_direct_exec : bool;
}

let builtin_local_assertion_modes : builtin_local_assertion_mode list ref =
  ref []

let builtin_local_assertion_mode_observations :
    (Sst.function_id * int * Sst.instance_mode * Sst.instance_mode) list ref =
  ref []

let same_span (left : Diagnostic.span) (right : Diagnostic.span) =
  String.equal left.file right.file
  && left.start_pos = right.start_pos
  && left.end_pos = right.end_pos

let register_builtin_local_assertion ~(program : Sst.program)
    ~(definition : Sst.function_definition)
    ~(expression : Sst.expression) ~(predicate : Sst.expression) ~direct_exec =
  let ordinal =
    match expression.Sst.expression_desc with
    | Sst.Local_assert { assertion_ordinal; _ } -> assertion_ordinal
    | _ -> invalid_arg "builtin local assertion mode source is not an assertion"
  in
  if
    not
      (List.exists
         (fun registered ->
           registered.builtin_mode_program == program
           && registered.builtin_mode_function = definition.function_id
           && same_span registered.builtin_mode_expression_span
                expression.span
           && registered.builtin_mode_ordinal = ordinal)
         !builtin_local_assertion_modes)
  then
    builtin_local_assertion_modes :=
      {
        builtin_mode_program = program;
        builtin_mode_function = definition.function_id;
        builtin_mode_expression_span = expression.span;
        builtin_mode_predicate_span = predicate.span;
        builtin_mode_ordinal = ordinal;
        builtin_mode_direct_exec = direct_exec;
      }
      :: !builtin_local_assertion_modes
  else ()

let direct_exec_builtin_local_assertion program
    (definition : Sst.function_definition) (expression : Sst.expression)
    (predicate : Sst.expression) =
  List.exists
    (fun registered ->
      registered.builtin_mode_program == program
      && registered.builtin_mode_function = definition.function_id
      && same_span registered.builtin_mode_expression_span
           expression.span
      && same_span registered.builtin_mode_predicate_span
           predicate.span
      &&
      match expression.Sst.expression_desc with
      | Sst.Local_assert { assertion_ordinal; _ } ->
          registered.builtin_mode_ordinal = assertion_ordinal
          && registered.builtin_mode_direct_exec
      | _ -> false)
    !builtin_local_assertion_modes

let mode_name = function
  | Sst.Exec_instance -> "Exec"
  | Sst.Tracked_instance -> "Tracked"
  | Sst.Ghost_instance -> "Ghost"

let site_name = function
  | Binding -> "binding"
  | Expression -> "expression"
  | Pattern -> "pattern"
  | Formal -> "formal"
  | Result -> "result"
  | Field -> "field"
  | Component_type -> "component-type"

let site_of_name = function
  | "binding" -> Some Binding
  | "expression" -> Some Expression
  | "pattern" -> Some Pattern
  | "formal" -> Some Formal
  | "result" -> Some Result
  | "field" -> Some Field
  | "component-type" -> Some Component_type
  | _ -> None

let mode_of_name = function
  | "tracked" -> Some Sst.Tracked_instance
  | "ghost" -> Some Sst.Ghost_instance
  | _ -> None

let same_callable_span (left : Diagnostic.span) (right : Diagnostic.span) =
  let position_le (left : Diagnostic.position) (right : Diagnostic.position) =
    left.line < right.line
    ||
    (left.line = right.line && left.column <= right.column)
  in
  let contains (outer : Diagnostic.span) (inner : Diagnostic.span) =
    position_le outer.start_pos inner.start_pos
    && position_le inner.end_pos outer.end_pos
  in
  String.equal left.file right.file
  && (contains left right || contains right left)

let span source_file location =
  Diagnostic.span_of_location ~fallback_file:source_file location

let empty_payload = function Parsetree.PStr [] -> true | _ -> false

let request_of_attribute ~source_file ~subject_span attribute =
  let parts = String.split_on_char '.' attribute.Parsetree.attr_name.txt in
  match parts with
  | [ "verocaml"; "internal"; "instance_mode"; site; mode ] -> (
      match (site_of_name site, mode_of_name mode) with
      | Some site, Some mode
        when attribute.attr_loc.Location.loc_ghost
             && attribute.attr_name.loc.loc_ghost
             && empty_payload attribute.attr_payload ->
          Some
            {
              site;
              index = None;
              mode;
              subject_span;
              annotation_span = span source_file attribute.attr_loc;
            }
      | _ -> None)
  | [ "verocaml"; "internal"; "instance_mode"; "formal"; index; mode ] -> (
      match (int_of_string_opt index, mode_of_name mode) with
      | Some index, Some mode
        when index >= 0
             && attribute.attr_loc.Location.loc_ghost
             && attribute.attr_name.loc.loc_ghost
             && empty_payload attribute.attr_payload ->
          Some
            {
              site = Formal;
              index = Some index;
              mode;
              subject_span;
              annotation_span = span source_file attribute.attr_loc;
            }
      | _ -> None)
  | _ -> None

let family_of_attribute attribute =
  let prefix = "verocaml.internal.artifact_family." in
  if
    attribute.Parsetree.attr_loc.Location.loc_ghost
    && String.starts_with ~prefix attribute.attr_name.txt
  then
    Some
      (String.sub attribute.attr_name.txt (String.length prefix)
         (String.length attribute.attr_name.txt - String.length prefix))
  else None

let collect_metadata ~source_file structure =
  let requests = ref [] in
  let families = ref [] in
  let add_attributes site subject_span attributes =
    List.iter
      (fun attribute ->
        Option.iter
          (fun request ->
            if request.site = site then requests := request :: !requests)
          (request_of_attribute ~source_file ~subject_span attribute);
        Option.iter
          (fun family -> families := family :: !families)
          (family_of_attribute attribute))
      attributes
  in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      value_binding =
        (fun self binding ->
          let binding_span = span source_file binding.vb_pat.pat_loc in
          add_attributes Binding binding_span binding.vb_attributes;
          add_attributes Result (span source_file binding.vb_loc)
            binding.vb_expr.exp_attributes;
          List.iter
            (fun attribute ->
              Option.iter
                (fun family -> families := family :: !families)
                (family_of_attribute attribute))
            binding.vb_attributes;
          default.value_binding self binding);
      expr =
        (fun self expression ->
          add_attributes Expression (span source_file expression.exp_loc)
            expression.exp_attributes;
          List.iter
            (fun attribute ->
              match
                request_of_attribute ~source_file
                  ~subject_span:(span source_file expression.exp_loc)
                  attribute
              with
              | Some ({ site = Formal; index = Some _; _ } as request) ->
                  requests := request :: !requests
              | Some _ | None -> ())
            expression.exp_attributes;
          default.expr self expression);
      pat =
        (fun (type k) self (pattern : k general_pattern) ->
          let subject_span = span source_file pattern.pat_loc in
          add_attributes Pattern subject_span pattern.pat_attributes;
          add_attributes Formal subject_span pattern.pat_attributes;
          default.pat self pattern);
      type_declaration =
        (fun self declaration ->
          List.iter
            (fun attribute ->
              Option.iter
                (fun family -> families := family :: !families)
                (family_of_attribute attribute))
            declaration.typ_attributes;
          (match declaration.typ_kind with
          | Ttype_record labels | Ttype_record_unboxed_product labels ->
              List.iter
                (fun label ->
                  add_attributes Field (span source_file label.ld_loc)
                    label.ld_attributes)
                labels
          | Ttype_variant constructors ->
              List.iter
                (fun constructor ->
                  match constructor.cd_args with
                  | Cstr_record labels ->
                      List.iter
                        (fun label ->
                          add_attributes Field (span source_file label.ld_loc)
                            label.ld_attributes)
                        labels
                  | Cstr_tuple arguments ->
                      List.iter
                        (fun argument ->
                          add_attributes Component_type
                            (span source_file argument.ca_loc)
                            argument.ca_type.ctyp_attributes)
                        arguments)
                constructors
          | Ttype_abstract | Ttype_open -> ());
          default.type_declaration self declaration);
    }
  in
  iterator.structure iterator structure;
  (List.rev !requests, List.sort_uniq String.compare !families)

let prepare ~structure ~program =
  let source_file =
    match program.Sst.functions with
    | definition :: _ -> definition.span.file
    | [] -> (
        match program.types with
        | definition :: _ -> definition.span.file
        | [] -> "<semantic-sst>")
  in
  let requests, family_markers = collect_metadata ~source_file structure in
  let registration =
    {
      program;
      structure;
      requests;
      family_markers;
      raw_snapshot = Sst.to_string program;
      sealed = None;
    }
  in
  registrations :=
    registration
    :: List.filter
         (fun (existing : registration) -> existing.program != program)
         !registrations

let find_registration program =
  List.find_opt
    (fun (registration : registration) -> registration.program == program)
    !registrations

let has_sealed_registration program =
  match find_registration program with
  | Some { sealed = Some _; raw_snapshot; _ } ->
      String.equal raw_snapshot (Sst.to_string program)
  | Some _ | None -> false

let ppx_is_retained arguments =
  let rec loop = function
    | [] | [ _ ] -> false
    | "-ppx" :: command :: rest ->
        String.split_on_char ' ' command
        |> List.exists (String.equal "--keep-ghost")
        || loop rest
    | _ :: rest -> loop rest
  in
  loop (Array.to_list arguments)

let digest value = Digest.string value |> Digest.to_hex

let span_string (span : Diagnostic.span) =
  Printf.sprintf "%s:%d:%d-%d:%d" span.file span.start_pos.line
    span.start_pos.column span.end_pos.line span.end_pos.column

let signature_snapshot registration identity =
  let requests =
    registration.requests
    |> List.map (fun request ->
           Printf.sprintf "%s:%s:%s"
             (site_name request.site
             ^ Option.fold ~none:""
                 ~some:(fun index -> Printf.sprintf "#%d" index)
                 request.index)
             (mode_name request.mode)
             (span_string request.annotation_span))
    |> String.concat "|"
  in
  digest
    (String.concat "\000"
       [
         identity.unit_name;
         identity.interface_digest;
         identity.source_digest;
         registration.raw_snapshot;
         requests;
         (identity.interface_finite_signatures
         |> List.map (fun (path, signature) ->
                path ^ "=" ^ Option.value ~default:"<missing>" signature)
         |> String.concat "|");
       ])

let seal implementation program =
  let fail ?function_id span message = Error { function_id; span; message } in
  let fallback = Diagnostic.file_span implementation.Cmt_input.source_file in
  match find_registration program with
  | None ->
      fail fallback
        "instance-mode authority has no exact Typedtree lowering registration"
  | Some registration
    when registration.structure != implementation.Cmt_input.structure ->
      fail fallback
        "instance-mode authority rejected a copied Typedtree/program pairing"
  | Some registration
    when not (String.equal registration.raw_snapshot (Sst.to_string program)) ->
      fail fallback "instance-mode authority rejected a changed SST snapshot"
  | Some registration
    when
      (not (ppx_is_retained implementation.compiler_arguments))
      && registration.requests = []
      && registration.family_markers = [] ->
      (* Legacy/default-only CMTs may still pass through ordinary semantic
         verification. They remain deliberately unsealed, so no
         mode-sensitive or invariant-authority query can authenticate them. *)
      Ok ()
  | Some _registration
    when not (ppx_is_retained implementation.compiler_arguments) ->
      fail fallback
        "instance-mode authority requires the retained artifact family"
  | Some registration
    when registration.family_markers <> [ "retained-v1" ] ->
      fail fallback
        "retained CMT does not carry one exact retained-family marker"
  | Some _registration
    when
      (implementation.explicit_interface
      || implementation.interface_family_markers <> [])
      && implementation.interface_family_markers <> [ "retained-v1" ] ->
      fail fallback
        "retained CMT is not paired with one exact retained interface family"
  | Some registration -> (
      match
        (implementation.interface_digest, implementation.source_digest)
      with
      | Some interface_digest, Some source_digest ->
          let provisional =
            {
              source_file = implementation.source_file;
              cmt_file = implementation.filename;
              unit_name = implementation.unit_name;
              interface_digest;
              source_digest;
              signature_snapshot = "";
              explicit_interface = implementation.explicit_interface;
              interface_mode_signatures =
                implementation.interface_mode_signatures;
              interface_finite_signatures =
                implementation.interface_finite_signatures;
            }
          in
          registration.sealed <-
            Some
              {
                provisional with
                signature_snapshot =
                  signature_snapshot registration provisional;
              };
          Ok ()
      | None, _ | _, None ->
          fail fallback
            "instance-mode authority requires exact interface and source \
             digests")

let request ?index registration site subject_span =
  match registration with
  | None -> None
  | Some registration ->
      List.find_opt
        (fun request ->
          request.site = site
          && request.index = index
          &&
          (match index with
          | Some _ -> same_callable_span request.subject_span subject_span
          | None -> same_span request.subject_span subject_span))
        registration.requests

let descriptor_binding_matches (environment : environment)
    (descriptor : descriptor) =
  environment.issuer == process_issuer
  && descriptor.issuer == process_issuer
  && descriptor.registration == environment.registration
  &&
  (match environment.registration with
  | Some registration -> registration.program == environment.program
  | None -> true)
  && String.equal descriptor.snapshot environment.snapshot
  && String.equal descriptor.signature_snapshot environment.signature_snapshot

let authenticated_descriptor (environment : environment)
    (descriptor : descriptor) =
  descriptor_binding_matches environment descriptor
  && Descriptor_table.mem environment.issued_descriptors descriptor

let add_descriptor (environment : environment) (descriptor : descriptor) =
  if not (descriptor_binding_matches environment descriptor) then
    invalid_arg "unauthenticated instance-mode descriptor";
  Descriptor_table.add environment.issued_descriptors descriptor ();
  environment.descriptors <- descriptor :: environment.descriptors

module For_testing = struct
  let reset_ghost_formal_flow_count () = ghost_formal_flow_count := 0
  let ghost_formal_flow_count () = !ghost_formal_flow_count

  let reset_builtin_local_assertion_mode_observations () =
    builtin_local_assertion_mode_observations := []

  let builtin_local_assertion_mode_observations () =
    List.rev !builtin_local_assertion_mode_observations

  let copied_descriptor_rejected environment =
    match environment.descriptors with
    | [] -> true
    | descriptor :: _ ->
        let copied = { descriptor with snapshot = descriptor.snapshot } in
        not (authenticated_descriptor environment copied)
end

let issue (environment : environment) ~identity ~subject ~mode ~request
    ~defaulting_context ~typ =
  let request : request option = request in
  let descriptor =
    {
      issuer = process_issuer;
      authentication_index = environment.next_descriptor_index;
      registration = environment.registration;
      snapshot = environment.snapshot;
      identity;
      subject;
      mode;
      explicit = Option.is_some request;
      annotation_span =
        Option.map
          (fun (request : request) -> request.annotation_span)
          request;
      defaulting_context;
      typ;
      signature_snapshot = environment.signature_snapshot;
    }
  in
  environment.next_descriptor_index <- environment.next_descriptor_index + 1;
  add_descriptor environment descriptor;
  descriptor

let error ?function_id span message = Error { function_id; span; message }

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let rec iter_result action = function
  | [] -> Ok ()
  | value :: rest ->
      let* () = action value in
      iter_result action rest

let field_key (field : Sst.field_definition) =
  let owner =
    match field.field_id.field_owner with
    | Sst.Record_owner owner ->
        Printf.sprintf "record:%s#%d" owner.type_name owner.type_index
    | Sst.Constructor_owner constructor ->
        Printf.sprintf "constructor:%s#%d:%d"
          constructor.constructor_type.type_name
          constructor.constructor_type.type_index constructor.constructor_index
  in
  Printf.sprintf "%s:%d:%s" owner field.field_id.field_index
    field.field_id.field_name

let find_field_definition program field_id =
  let fields =
    List.concat_map
      (fun definition ->
        match definition.Sst.type_kind with
        | Sst.Record_definition fields -> fields
        | Sst.Variant_definition constructors ->
            List.concat_map
              (fun constructor -> constructor.Sst.constructor_fields)
              constructors)
      program.Sst.types
  in
  List.find_opt (fun field -> field.Sst.field_id = field_id) fields

let find_descriptor environment matches =
  List.find_opt
    (fun descriptor ->
      authenticated_descriptor environment descriptor
      && matches descriptor.subject)
    environment.descriptors

let descriptor_for_binding environment function_id binding =
  find_descriptor environment (function
    | Binding_subject (candidate, subject) ->
        candidate = function_id && subject = binding
    | _ -> false)

let descriptor_for_expression environment function_id expression =
  find_descriptor environment (function
    | Expression_subject (candidate, subject) ->
        candidate = function_id && subject == expression
    | _ -> false)

let descriptor_for_pattern environment function_id pattern =
  find_descriptor environment (function
    | Pattern_subject (candidate, subject) ->
        candidate = function_id && subject == pattern
    | _ -> false)

let descriptor_for_field environment field =
  find_descriptor environment (function
    | Field_subject subject -> subject = field
    | _ -> false)

let descriptor_for_formal environment definition index =
  find_descriptor environment (function
    | Formal_subject (candidate, candidate_index, _) ->
        candidate == definition && candidate_index = index
    | _ -> false)

let descriptor_for_result environment definition =
  find_descriptor environment (function
    | Result_subject candidate -> candidate == definition
    | _ -> false)

let require_descriptor message = function
  | Some descriptor -> descriptor
  | None -> invalid_arg message

let binding_mode environment function_id binding =
  match descriptor_for_binding environment function_id binding with
  | Some descriptor -> descriptor.mode
  | None ->
      invalid_arg
        (Printf.sprintf
           "instance-mode binding descriptor is absent (function=%s:%d \
            binding=%s:%d)"
           function_id.Sst.function_name function_id.function_index
           binding.Sst.name binding.id)

let expression_mode environment function_id expression =
  (require_descriptor "instance-mode expression descriptor is absent"
     (descriptor_for_expression environment function_id expression))
    .mode

let expression_explicit environment function_id expression =
  (require_descriptor "instance-mode expression descriptor is absent"
     (descriptor_for_expression environment function_id expression))
    .explicit

let pattern_mode environment function_id pattern =
  (require_descriptor "instance-mode pattern descriptor is absent"
     (descriptor_for_pattern environment function_id pattern))
    .mode

let pattern_explicit environment function_id pattern =
  (require_descriptor "instance-mode pattern descriptor is absent"
     (descriptor_for_pattern environment function_id pattern))
    .explicit

let field_mode environment field =
  (require_descriptor "instance-mode field descriptor is absent"
     (descriptor_for_field environment field))
    .mode

let field_explicit environment field =
  (require_descriptor "instance-mode field descriptor is absent"
     (descriptor_for_field environment field))
    .explicit

let formal_mode environment definition index _parameter =
  (require_descriptor "instance-mode formal descriptor is absent"
     (descriptor_for_formal environment definition index))
    .mode

let formal_has_closed_authority environment definition index =
  List.exists
    (fun (function_id, candidate_index) ->
      function_id = definition.Sst.function_id && candidate_index = index)
    environment.tracked_formal_authority

let result_mode environment definition =
  (require_descriptor "instance-mode result descriptor is absent"
     (descriptor_for_result environment definition))
    .mode

let callable_result_mode environment definition =
  match definition.Sst.body with
  | Sst.External_specification (Sst.Imported_unverified_target _) ->
      Sst.Exec_instance
  | Sst.Checked_exec _ | Sst.Spec_definition _
  | Sst.Recursive_spec_definition _ | Sst.Proof_body _
  | Sst.External_specification _ | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
      result_mode environment definition

let rec pattern_bindings pattern =
  match pattern.Sst.pattern_desc with
  | Sst.Bind binding -> [ binding ]
  | Sst.Owned_tree_cursor_pattern cursor -> [ cursor.cursor_binding ]
  | Sst.Tuple_pattern components ->
      List.concat_map (fun (_, pattern) -> pattern_bindings pattern) components
  | Sst.Record_pattern fields ->
      List.concat_map (fun (_, pattern) -> pattern_bindings pattern) fields
  | Sst.Constructor_pattern (_, patterns) ->
      List.concat_map pattern_bindings patterns
  | Sst.Or_pattern (left, right) ->
      pattern_bindings left @ pattern_bindings right
  | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _
  | Sst.Unit_pattern ->
      []

let erased = function
  | Sst.Ghost_instance | Sst.Tracked_instance -> true
  | Sst.Exec_instance -> false

let default_for_definition (definition : Sst.function_definition) =
  match definition.mode with
  | Sst.Exec -> Sst.Exec_instance
  | Sst.Spec | Sst.Proof -> Sst.Ghost_instance

let invariant_bearing_type program type_id =
  List.exists
    (fun definition ->
      match definition.Sst.representation with
      | Sst.Abstract_with_evidence
          (Sst.Proposed_same_cmt_abstraction evidence
          | Sst.Authenticated_same_cmt_abstraction evidence) ->
          (evidence.abstract_signature_type = type_id
          || evidence.hidden_implementation_type = type_id)
          && List.exists
               (fun operation ->
                 operation.Sst.public_role = Sst.Abstract_invariant)
               evidence.public_surface
      | Sst.Revealed
      | Sst.Abstract_with_evidence (Sst.Incomplete_abstraction_evidence _) ->
          false)
    program.Sst.types

let validate program =
  let program_snapshot = Sst.to_string program in
  let registration = find_registration program in
  let authenticated, signature_snapshot =
    match registration with
    | Some { sealed = Some identity; raw_snapshot; _ }
      when String.equal raw_snapshot program_snapshot ->
        (true, identity.signature_snapshot)
    | Some _ | None -> (false, "raw-or-unsealed")
  in
  let environment =
    {
      issuer = process_issuer;
      program;
      registration;
      authenticated;
      snapshot = digest program_snapshot;
      signature_snapshot;
      descriptors = [];
      issued_descriptors = Descriptor_table.create 256;
      next_descriptor_index = 0;
      tracked_formal_authority = [];
      forgetting_traces = [];
      standalone_proof_binding_traces = [];
    }
  in
  let request ?index site span = request ?index registration site span in
  let issue_field (field : Sst.field_definition) =
    let selected =
      match request Field field.Sst.span with
      | Some request -> Some request
      | None -> request Component_type field.span
    in
    let mode =
      Option.fold ~none:Sst.Exec_instance
        ~some:(fun (request : request) -> request.mode)
        selected
    in
    ignore
      (issue environment ~identity:(lazy ("field:" ^ field_key field))
         ~subject:(Field_subject field) ~mode ~request:selected
         ~defaulting_context:"aggregate-field-default" ~typ:field.field_type)
  in
  List.iter
    (fun definition ->
      match definition.Sst.type_kind with
      | Sst.Record_definition fields -> List.iter issue_field fields
      | Sst.Variant_definition constructors ->
          List.iter
            (fun constructor ->
              List.iter issue_field constructor.Sst.constructor_fields)
            constructors)
    program.Sst.types;
  let find_definition function_id =
    List.find_opt
      (fun definition -> definition.Sst.function_id = function_id)
      program.functions
  in
  let fail definition span message =
    error ~function_id:definition.Sst.function_id span message
  in
  let rec issue_pattern (definition : Sst.function_definition) expected
      (pattern : Sst.pattern) =
    let selected = request Pattern pattern.Sst.span in
    let mode =
      Option.fold ~none:expected
        ~some:(fun (request : request) -> request.mode)
        selected
    in
    ignore
      (issue environment
         ~identity:
           (lazy
             (Printf.sprintf "pattern:%d:%s"
                definition.Sst.function_id.function_index
                (span_string pattern.span)))
         ~subject:(Pattern_subject (definition.function_id, pattern))
         ~mode ~request:selected ~defaulting_context:"pattern-flow"
         ~typ:pattern.typ);
    let validate_field_pattern field nested nested_mode =
      let declared = field_mode environment field in
      if erased declared then
        if
          pattern_explicit environment definition.function_id nested
          && nested_mode = declared
        then Ok ()
        else
          fail definition nested.span
            (Printf.sprintf
               "erased field pattern requires one explicit matching %s \
                annotation"
               (mode_name declared))
      else if
        pattern_explicit environment definition.function_id nested
        && erased nested_mode
      then
        fail definition nested.span
          "runtime field pattern cannot carry an erased mode annotation"
      else Ok ()
    in
    let* () =
      match pattern.pattern_desc with
    | Sst.Bind binding | Sst.Owned_tree_cursor_pattern { cursor_binding = binding; _ } ->
        let binding_request =
          request Binding binding.span
        in
        let binding_mode =
          Option.fold ~none:mode
            ~some:(fun (request : request) -> request.mode)
            binding_request
        in
        ignore
          (issue environment
             ~identity:
               (lazy
                 (Printf.sprintf "binding:%d:%d"
                    definition.function_id.function_index binding.id))
             ~subject:(Binding_subject (definition.function_id, binding))
             ~mode:binding_mode ~request:binding_request
             ~defaulting_context:"binding-flow" ~typ:binding.typ);
        Ok ()
    | Sst.Tuple_pattern components ->
        iter_result
          (fun (_, nested) ->
            let* _ = issue_pattern definition mode nested in
            Ok ())
          components
    | Sst.Record_pattern fields ->
        iter_result
          (fun (field_id, (nested : Sst.pattern)) ->
            match find_field_definition program field_id with
            | None ->
                fail definition nested.span
                  "record pattern names an unknown field"
            | Some field ->
                let declared = field_mode environment field in
                let expected =
                  if
                    mode = Sst.Ghost_instance
                    && declared = Sst.Exec_instance
                  then Sst.Ghost_instance
                  else declared
                in
                let* nested_mode =
                  issue_pattern definition expected nested
                in
                validate_field_pattern field nested nested_mode)
          fields
    | Sst.Constructor_pattern (constructor, patterns) ->
        let fields =
          match
            List.find_opt
              (fun type_definition ->
                type_definition.Sst.type_id = constructor.constructor_type)
              program.types
          with
          | Some { type_kind = Sst.Variant_definition constructors; _ } ->
              Option.value ~default:[]
                (List.find_map
                   (fun candidate ->
                     if candidate.Sst.constructor_id = constructor then
                       Some candidate.constructor_fields
                     else None)
                   constructors)
          | _ -> []
        in
        let aggregate_argument =
          match patterns with
          | [ { Sst.pattern_desc = Sst.Tuple_pattern _; _ } ] ->
              None
          | [ nested ] when List.length fields > 1 ->
              Some nested
          | _ -> None
        in
        let patterns =
          match patterns with
          | [ { Sst.pattern_desc = Sst.Tuple_pattern components; _ } ]
            when List.length fields > 1 ->
              List.map snd components
          | patterns -> patterns
        in
        if Option.is_some aggregate_argument then
          let* _ =
            issue_pattern definition mode (Option.get aggregate_argument)
          in
          Ok ()
        else if List.length fields <> List.length patterns then
          fail definition pattern.span
            (Printf.sprintf
               "constructor pattern mode arity differs from its declaration \
                (fields=%d patterns=%d)"
               (List.length fields) (List.length patterns))
        else
          List.combine fields patterns
          |> iter_result (fun (field, (nested : Sst.pattern)) ->
                 let declared = field_mode environment field in
                 let expected =
                   if
                     mode = Sst.Ghost_instance
                     && declared = Sst.Exec_instance
                   then Sst.Ghost_instance
                   else declared
                 in
                 let* nested_mode =
                   issue_pattern definition expected nested
                 in
                 validate_field_pattern field nested nested_mode)
    | Sst.Or_pattern (left, right) ->
        let* _ = issue_pattern definition mode left in
        let* _ = issue_pattern definition mode right in
        Ok ()
    | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _
    | Sst.Unit_pattern ->
        Ok ()
    in
    Ok mode
  in
  let rec issue_expression ?(forgetting = false)
      (definition : Sst.function_definition) default expected
      (expression : Sst.expression) =
    let selected = request Expression expression.Sst.span in
    let standalone_nonrecursive_proof =
      environment.authenticated
      && definition.mode = Sst.Proof
      && not definition.recursive
      &&
      match definition.body with
      | Sst.Proof_body
          { provenance = Sst.Authenticated_typedtree _; _ } ->
          true
      | Sst.Proof_body { provenance = Sst.Raw_semantic_body _; _ }
      | Sst.Checked_exec _ | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ->
          false
    in
    let bare_tracked_binding =
      if
        standalone_nonrecursive_proof && selected = None && not forgetting
      then
        match expression.expression_desc with
        | Sst.Variable { binding; _ } -> (
            match
              descriptor_for_binding environment definition.function_id
                binding
            with
            | Some descriptor when descriptor.mode = Sst.Tracked_instance ->
                Some (binding, descriptor)
            | Some _ | None -> None)
        | _ -> None
      else None
    in
    let expected_use_context = Option.value ~default expected in
    let inferred =
      match bare_tracked_binding with
      | Some _ -> (
          match expected_use_context with
          | Sst.Ghost_instance -> Sst.Ghost_instance
          | Sst.Tracked_instance -> Sst.Tracked_instance
          | Sst.Exec_instance -> Sst.Tracked_instance)
      | None -> (
          match expression.expression_desc with
      | Sst.Variable { binding; _ } ->
          Option.fold ~none:default
            ~some:(fun descriptor -> descriptor.mode)
            (descriptor_for_binding environment definition.function_id binding)
      | Sst.Field_read { field; _ } ->
          let field = Option.get (find_field_definition program field) in
          if default = Sst.Ghost_instance
             && field_mode environment field = Sst.Exec_instance
          then Sst.Ghost_instance
          else field_mode environment field
      | Sst.Field_write { field; _ } ->
          let field = Option.get (find_field_definition program field) in
          field_mode environment field
      | Sst.Direct_call { callee; _ } ->
          Option.fold ~none:default
            ~some:(fun callee -> callable_result_mode environment callee)
            (find_definition callee)
      | _ -> Option.value ~default expected)
    in
    let mode =
      Option.fold ~none:inferred
        ~some:(fun (request : request) -> request.mode)
        selected
    in
    ignore
      (issue environment
         ~identity:
           (lazy
             (Printf.sprintf "expression:%d:%s"
                definition.function_id.function_index
                (span_string expression.span)))
         ~subject:(Expression_subject (definition.function_id, expression))
         ~mode ~request:selected ~defaulting_context:(mode_name default)
         ~typ:expression.typ);
    let () =
      match bare_tracked_binding with
      | Some (binding, descriptor)
        when
          (expected_use_context = Sst.Ghost_instance
          && mode = Sst.Ghost_instance)
          || (expected_use_context = Sst.Tracked_instance
             && mode = Sst.Tracked_instance) ->
          let outcome, edges =
            match mode with
            | Sst.Ghost_instance -> ("observe-as-Ghost", 1)
            | Sst.Tracked_instance -> ("preserve-exact-Tracked", 0)
            | Sst.Exec_instance -> assert false
          in
          let trace =
            Printf.sprintf
              "standalone-proof-binding source=%s binding=%s binding-id=%d \
               caller=%s#%d caller-mode=Proof recursive=false \
               enclosing-body=Proof_body authenticated=true expected=%s \
               incoming=Tracked outcome=%s original-binding=%s edges=%d \
               synthetic-ghost=0 synthetic-tracked=0 \
               unrelated-authority=0"
              expression.span.file (Lazy.force descriptor.identity) binding.id
              definition.function_id.function_name
              definition.function_id.function_index
              (mode_name expected_use_context) outcome
              (Lazy.force descriptor.identity)
              edges
          in
          environment.standalone_proof_binding_traces <-
            trace :: environment.standalone_proof_binding_traces;
          (match Sys.getenv_opt "VEROCAML_TEST_INSTANCE_MODE_TRACE" with
          | Some "1" -> prerr_endline trace
          | Some _ | None -> ())
      | Some _ | None -> ()
    in
    let child ?expected child =
      issue_expression ~forgetting definition default expected child
    in
    let exact expected actual span site =
      if expected = actual then Ok ()
      else
        fail definition span
          (Printf.sprintf "%s mode mismatch: expected %s but found %s" site
             (mode_name expected) (mode_name actual))
    in
    let require_erased_annotation field component component_mode site =
      let field_mode = field_mode environment field in
      if erased field_mode then
        if
          (not
             (match site with
             | `Expression -> expression_explicit environment definition.function_id component
             | `Pattern -> false))
          || component_mode <> field_mode
        then
          fail definition component.Sst.span
            (Printf.sprintf
               "erased field/component requires one explicit matching %s \
                annotation"
               (mode_name field_mode))
        else Ok ()
      else if
        expression_explicit environment definition.function_id component
        && erased component_mode
      then
        fail definition component.span
          "runtime field/component cannot carry an erased mode annotation"
      else Ok ()
    in
    let rec pure_erased nested =
      match nested.Sst.expression_desc with
      | Sst.Direct_call { callee; call_form = Sst.Exec_call; _ } -> (
          match find_definition callee with
          | Some callee when erased (callable_result_mode environment callee) ->
              iter_result pure_erased
                (Sst_callback_private.expression_children nested)
          | Some _ | None ->
              fail definition nested.span
                "erased expression contains an executable call")
      | Sst.Mutable_write _ | Sst.Let_mutable _ | Sst.Mutable_read _
      | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _ ->
          fail definition nested.span
            "erased expression contains executable state or mutation"
      | Sst.Field_write _ when nested != expression ->
          fail definition nested.span
            "nested erased field updates are unsupported"
      | _ ->
          let rec loop = function
            | [] -> Ok ()
            | child :: rest ->
                let* () = pure_erased child in
                loop rest
          in
          loop (Sst_callback_private.expression_children nested)
    in
    let validate_children () =
      match expression.expression_desc with
      | Sst.Direct_call _
        when Spec_function_sst_private.lambda expression <> None ->
          Option.get
            (Spec_function_sst_private.validate_instance_lambda
               ~issue_pattern:(fun pattern ->
                 Result.map ignore
                   (issue_pattern definition Sst.Ghost_instance pattern))
               ~issue_expression:(fun expression ->
                 Result.map ignore
                   (child ~expected:Sst.Ghost_instance expression))
               expression)
      | Sst.Variable { binding; _ } ->
          let source = binding_mode environment definition.function_id binding in
          let exact_bare_tracked =
            match bare_tracked_binding with
            | Some (candidate, descriptor) ->
                candidate = binding
                && descriptor.mode = Sst.Tracked_instance
                &&
                ((mode = Sst.Ghost_instance
                 && expected_use_context = Sst.Ghost_instance)
                || (mode = Sst.Tracked_instance
                   && expected_use_context = Sst.Tracked_instance))
            | None -> false
          in
          if mode = source then
            if
              erased source
              &&
              (default = Sst.Exec_instance
              || source = Sst.Tracked_instance)
              && selected = None && not forgetting
              && not exact_bare_tracked
            then
              fail definition expression.span
                (Printf.sprintf
                   "%s variable use requires one explicit matching annotation"
                   (mode_name source))
            else Ok ()
          else if
            source = Sst.Exec_instance
            && mode = Sst.Tracked_instance
            && selected <> None
          then Ok ()
          else if
            mode = Sst.Ghost_instance
            && (selected <> None || exact_bare_tracked)
          then Ok ()
          else
            fail definition expression.span
              (Printf.sprintf "variable mode laundering from %s to %s"
                 (mode_name source) (mode_name mode))
      | Sst.Tuple_value components ->
          List.fold_left
            (fun result (_, component) ->
              let* () = result in
              let* component_mode = child ~expected:mode component in
              exact mode component_mode component.span "tuple component")
            (Ok ()) components
      | Sst.Record_value { fields; _ } ->
          List.fold_left
            (fun result (field_id, component) ->
              let* () = result in
              let field = Option.get (find_field_definition program field_id) in
              let declared = field_mode environment field in
              let expected =
                if
                  default = Sst.Ghost_instance
                  && mode = Sst.Ghost_instance
                  && declared = Sst.Exec_instance
                then Sst.Ghost_instance
                else declared
              in
              let* component_mode =
                child ~expected component
              in
              require_erased_annotation field component component_mode `Expression)
            (Ok ()) fields
      | Sst.Constructor_value { constructor; arguments } ->
          let fields =
            match
              List.find_opt
                (fun definition ->
                  definition.Sst.type_id = constructor.constructor_type)
                program.types
            with
            | Some { type_kind = Sst.Variant_definition constructors; _ } ->
                Option.value ~default:[]
                  (List.find_map
                     (fun candidate ->
                       if candidate.Sst.constructor_id = constructor then
                         Some candidate.constructor_fields
                       else None)
                     constructors)
            | _ -> []
          in
          if List.length fields = List.length arguments then
            List.fold_left2
              (fun result field component ->
                let* () = result in
                let declared = field_mode environment field in
                let expected =
                  if
                    default = Sst.Ghost_instance
                    && mode = Sst.Ghost_instance
                    && declared = Sst.Exec_instance
                  then Sst.Ghost_instance
                  else declared
                in
                let* component_mode =
                  child ~expected component
                in
                require_erased_annotation field component component_mode
                  `Expression)
              (Ok ()) fields arguments
          else
            (* Built-in constructors have no aggregate declaration in the
               program.  Their components inherit the enclosing expression
               mode; source aggregate arity has already been authenticated by
               canonical SST validation. *)
            List.fold_left
              (fun result component ->
                let* () = result in
                let* component_mode = child ~expected:mode component in
                exact mode component_mode component.span
                  "constructor component")
              (Ok ()) arguments
      | Sst.Field_read { record; field } ->
          let* _ = child record in
          let field = Option.get (find_field_definition program field) in
          if erased (field_mode environment field) then
            if
              selected = None || mode <> field_mode environment field
            then
              fail definition expression.span
                "erased projection requires one exact matching annotation"
            else Ok ()
          else Ok ()
      | Sst.Field_write { provenance; field; value; _ } ->
          let field = Option.get (find_field_definition program field) in
          let declared = field_mode environment field in
          let* value_mode =
            child ~expected:declared value
          in
          if not (erased declared) then
            if selected <> None || erased mode then
              fail definition expression.span
                "runtime field update cannot carry an erased mode annotation"
            else exact declared value_mode value.span "field update value"
          else if
            selected = None || mode <> declared || value_mode <> declared
          then
            fail definition expression.span
              "only a whole exact-mode Ghost/Tracked erased-field update is \
               admitted"
          else
            let owner =
              match field.field_id.field_owner with
              | Sst.Record_owner owner -> owner
              | Sst.Constructor_owner constructor ->
                  constructor.constructor_type
            in
            if invariant_bearing_type program owner
               || invariant_bearing_type program
                    (match provenance.root.typ with
                    | Sst.Aggregate type_id -> type_id
                    | _ -> owner)
            then
              fail definition expression.span
                "invariant-bearing erased-field updates are not supported"
            else pure_erased value
      | Sst.Let (bindings, body) ->
          let rec bindings_loop = function
            | [] ->
                let* body_mode = child ~expected:mode body in
                exact mode body_mode body.span "let result"
            | (pattern, value) :: rest ->
                let binding_request =
                  match pattern_bindings pattern with
                  | [ binding ] -> request Binding binding.span
                  | _ -> None
                in
                let expected =
                  Option.fold ~none:default
                    ~some:(fun (request : request) -> request.mode)
                    binding_request
                in
                let* value_mode = child ~expected value in
                let* pattern_mode =
                  issue_pattern definition value_mode pattern
                in
                let* () =
                  exact pattern_mode value_mode value.span "let binding"
                in
                bindings_loop rest
          in
          bindings_loop bindings
      | Sst.Sequence (first, second) ->
          let* _ = child first in
          let* second_mode = child ~expected:mode second in
          exact mode second_mode second.span "sequence result"
      | Sst.If (condition, consequent, alternative) ->
          let* condition_mode = child condition in
          let* () =
            if default = Sst.Exec_instance && erased condition_mode then
              fail definition condition.span
                "erased value cannot affect executable control flow"
            else Ok ()
          in
          let* left_mode = child ~expected:mode consequent in
          let* () = exact mode left_mode consequent.span "branch result" in
          Option.fold ~none:(Ok ())
            ~some:(fun right ->
              let* right_mode = child ~expected:mode right in
              exact mode right_mode right.span "branch result")
            alternative
      | Sst.Match (scrutinee, cases) ->
          let* scrutinee_mode = child scrutinee in
          let* () =
            if default = Sst.Exec_instance && erased scrutinee_mode then
              fail definition scrutinee.span
                "erased value cannot affect executable match control"
            else Ok ()
          in
          List.fold_left
            (fun result (case : Sst.case) ->
              let* () = result in
              let* _ =
                issue_pattern definition scrutinee_mode case.case_pattern
              in
              let* () =
                match case.case_guard with
                | None -> Ok ()
                | Some guard ->
                    let* _ = child guard in
                    Ok ()
              in
              let* body_mode = child ~expected:mode case.case_body in
              exact mode body_mode case.case_body.span "match result")
            (Ok ()) cases
      | Sst.Direct_call { callee; arguments; type_arguments; _ } -> (
          match find_definition callee with
          | None -> Ok ()
          | Some
              {
                Sst.body =
                  Sst.External_specification
                    (Sst.Imported_unverified_target _);
                _;
              } ->
              let* () =
                List.fold_left
                  (fun result argument ->
                    let* () = result in
                    match argument with
                    | Sst.Value_argument { value = actual; _ } ->
                        let* actual_mode =
                          issue_expression definition default
                            (Some Sst.Exec_instance) actual
                        in
                        exact Sst.Exec_instance actual_mode actual.span
                          "imported external actual"
                    | Sst.Callback_argument _ ->
                        fail definition expression.span
                          "imported external callback actuals are unsupported")
                  (Ok ()) arguments
              in
              exact mode Sst.Exec_instance expression.span
                "imported external result"
          | Some callee ->
              let callback_substitutions =
                if
                  List.length callee.type_binders
                  = List.length type_arguments
                then List.combine callee.type_binders type_arguments
                else []
              in
              let callee_result = result_mode environment callee in
              let* () =
                if
                  default = Sst.Exec_instance
                  && erased callee_result
                  && selected = None
                then
                  fail definition expression.span
                    "erased executable call result requires one explicit \
                     matching annotation"
                else Ok ()
              in
              let* value_edges =
                match
                  Sst_callback_private.split_direct_arguments
                    ~substitutions:callback_substitutions callee.parameters
                    arguments
                with
                | Ok edges -> Ok edges
                | Error message -> fail definition expression.span message
              in
              let* () =
                List.fold_left
                  (fun result (formal, (_, actual)) ->
                    let* () = result in
                    let formal_index =
                      let rec find index = function
                        | [] -> 0
                        | Sst.Value_parameter candidate :: rest ->
                            if candidate == formal then index
                            else find (index + 1) rest
                        | Sst.Callback_parameter _ :: rest ->
                            find (index + 1) rest
                      in
                      find 0 callee.parameters
                    in
                    let formal_descriptor =
                      match
                        descriptor_for_formal environment callee formal_index
                      with
                      | Some descriptor -> descriptor
                      | None ->
                          invalid_arg
                            "instance-mode formal descriptor is absent"
                    in
                    let expected = formal_descriptor.mode in
                    let forget_at_boundary =
                      formal_descriptor.mode = Sst.Ghost_instance
                      && not formal_descriptor.explicit
                      &&
                      (callee.mode = Sst.Spec || callee.mode = Sst.Proof)
                    in
                    let* actual_mode =
                      if forget_at_boundary then
                        issue_expression ~forgetting:true definition default
                          None actual
                      else
                        issue_expression definition default (Some expected)
                          actual
                    in
                    if forget_at_boundary then
                      let* () =
                        pure_erased actual
                      in
                      let* authenticated_mode =
                        match
                          descriptor_for_expression environment
                            definition.function_id actual
                        with
                        | Some descriptor when descriptor.mode = actual_mode ->
                            Ok descriptor.mode
                        | Some _ | None ->
                          fail definition actual.span
                            "Ghost-formal forgetting requires one authenticated \
                             incoming actual mode"
                      in
                      let ghost_before, tracked_before =
                        List.fold_left
                          (fun (ghost, tracked) descriptor ->
                            match descriptor.mode with
                            | Sst.Ghost_instance -> (ghost + 1, tracked)
                            | Sst.Tracked_instance -> (ghost, tracked + 1)
                            | Sst.Exec_instance -> (ghost, tracked))
                          (0, 0) environment.descriptors
                      in
                      incr ghost_formal_flow_count;
                      let ghost_after, tracked_after = (ghost_before, tracked_before) in
                      let trace =
                        Printf.sprintf
                          "forgetting-edge source=%s caller=%s#%d callee=%s#%d \
                          callee-mode=%s recursive=%b formal=%d incoming=%s \
                           authenticated=true formal-mode=Ghost \
                           boundary-result-mode=Ghost callee-result-mode=%s \
                           edges=1 synthetic-ghost=%d synthetic-tracked=%d"
                          actual.span.file
                          definition.function_id.function_name
                          definition.function_id.function_index
                          callee.function_id.function_name
                          callee.function_id.function_index
                          (match callee.mode with
                          | Sst.Spec -> "Spec"
                          | Sst.Proof -> "Proof"
                          | Sst.Exec -> "Exec")
                          callee.recursive formal_index
                          (mode_name authenticated_mode)
                          (mode_name callee_result)
                          (ghost_after - ghost_before)
                          (tracked_after - tracked_before)
                      in
                      environment.forgetting_traces <-
                        trace :: environment.forgetting_traces;
                      (match
                         Sys.getenv_opt "VEROCAML_TEST_INSTANCE_MODE_TRACE"
                       with
                      | Some "1" -> prerr_endline trace
                      | Some _ | None -> ());
                      Ok ()
                    else exact expected actual_mode actual.span "call actual")
                  (Ok ()) value_edges
              in
              exact callee_result mode expression.span "call result")
      | Sst.Proof_region body ->
          let* body_mode =
            issue_expression definition Sst.Ghost_instance
              (Some Sst.Ghost_instance) body
          in
          exact Sst.Ghost_instance body_mode body.span "proof-region payload"
      | Sst.Local_assert { predicate; _ } ->
          let assertion_mode =
            if
              direct_exec_builtin_local_assertion environment.program definition
                expression predicate
            then Sst.Exec_instance
            else Sst.Ghost_instance
          in
          let* () =
            exact assertion_mode mode expression.span
              "local-assert statement"
          in
          let* predicate_mode =
            issue_expression definition assertion_mode
              (Some assertion_mode) predicate
          in
          let* () =
            exact assertion_mode predicate_mode predicate.span
              "local-assert predicate"
          in
          let assertion_ordinal =
            match expression.expression_desc with
            | Sst.Local_assert { assertion_ordinal; _ } ->
                assertion_ordinal
            | _ -> assert false
          in
          builtin_local_assertion_mode_observations :=
            ( definition.function_id,
              assertion_ordinal,
              assertion_mode,
              predicate_mode )
            :: !builtin_local_assertion_mode_observations;
          Ok ()
      | Sst.Forall quantifier | Sst.Exists quantifier ->
          Quantifier_validation_private.validate_instance_mode
            {
              issue_binder =
                (fun ~identity binder ->
                  ignore
                    (issue environment ~identity:(lazy identity)
                       ~subject:
                         (Binding_subject (definition.function_id, binder))
                       ~mode:Sst.Ghost_instance ~request:None
                       ~defaulting_context:"logical-quantifier-binder"
                       ~typ:binder.typ));
              issue_ghost_expression =
                issue_expression definition Sst.Ghost_instance
                  (Some Sst.Ghost_instance);
              exact_ghost =
                (fun actual nested site ->
                  exact Sst.Ghost_instance actual nested.Sst.span site);
            }
            ~function_index:definition.function_id.function_index ~expression
            ~mode quantifier
      | Sst.Use_type_invariant { value; _ } ->
          let* value_mode = child value in
          if value_mode = Sst.Ghost_instance then
            fail definition value.span
              "use_type_invariant requires an exact Exec or Tracked instance"
          else if not authenticated then
            fail definition value.span
              "use_type_invariant requires authenticated instance-mode \
               authority"
          else Ok ()
      | _ ->
          let rec children = function
            | [] -> Ok ()
            | nested :: rest ->
                let* _ = child nested in
                children rest
          in
          children (Sst_callback_private.expression_children expression)
    in
    let* () = validate_children () in
    let* () =
      if mode = Sst.Tracked_instance && selected = None then
        match expression.expression_desc with
        | Sst.Variable _ when forgetting || Option.is_some bare_tracked_binding ->
            Ok ()
        | Sst.Let _ | Sst.Sequence _ | Sst.If _ | Sst.Match _
        | Sst.Proof_region _ ->
            Ok ()
        | _ ->
            fail definition expression.span
              "Tracked construction, preservation, use, or return requires \
               one explicit [@tracked] annotation"
      else Ok ()
    in
    if erased mode then
      let* () = pure_erased expression in
      Ok mode
    else Ok mode
  in
  let issue_signature definition =
    let default = default_for_definition definition in
    let unauthenticated_boundary =
      match definition.Sst.body with
      | Sst.External_specification _
      | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _ ->
          true
      | Sst.Checked_exec _ | Sst.Spec_definition _ | Sst.Proof_body _
      | Sst.Recursive_spec_definition _ | Sst.Symbolic_declaration _ ->
          false
    in
    let rec formals index = function
      | [] -> Ok ()
      | Sst.Callback_parameter _ :: rest ->
          formals (index + 1) rest
      | Sst.Value_parameter parameter :: rest ->
          let selected =
            request ~index Formal definition.span
          in
          let mode =
            Option.fold ~none:default
              ~some:(fun (request : request) -> request.mode)
              selected
          in
          let* () =
            match (definition.mode, definition.recursive, selected, mode) with
            | _, _, Some _, _ when unauthenticated_boundary ->
                fail definition parameter.pattern.span
                  "external and trusted signatures cannot carry instance modes"
            | Sst.Spec, _, Some _, Sst.Tracked_instance ->
                fail definition parameter.pattern.span
                  "specification formals cannot be Tracked"
            | Sst.Proof, true, Some _, _ ->
                fail definition parameter.pattern.span
                  "recursive proof formals reject mode annotations"
            | _ -> Ok ()
          in
          ignore
            (issue environment
               ~identity:
                 (lazy
                   (Printf.sprintf "formal:%d:%d"
                      definition.function_id.function_index index))
               ~subject:
                 (Formal_subject
                    (definition, index, Sst.Value_parameter parameter))
               ~mode ~request:selected ~defaulting_context:(mode_name default)
               ~typ:parameter.pattern.typ);
          let* _ = issue_pattern definition mode parameter.pattern in
          let* _ =
            match parameter.optional_default with
            | None -> Ok mode
            | Some optional_default ->
                issue_pattern definition mode optional_default.optional_pattern
          in
          formals (index + 1) rest
    in
    let* () = formals 0 definition.parameters in
    let selected = request Result definition.span in
    let mode =
      Option.fold ~none:default
        ~some:(fun (request : request) -> request.mode)
        selected
    in
    let* () =
      match (definition.mode, definition.recursive, selected, mode) with
      | _, _, Some _, _ when unauthenticated_boundary ->
          fail definition definition.span
            "external and trusted signatures cannot carry instance modes"
      | Sst.Spec, _, Some _, Sst.Tracked_instance ->
          fail definition definition.span
            "specification results cannot be Tracked"
      | Sst.Proof, true, Some _, _ ->
          fail definition definition.span
            "recursive proof results reject mode annotations"
      | _ -> Ok ()
    in
    ignore
      (issue environment
         ~identity:
           (lazy
             (Printf.sprintf "result:%d"
                definition.function_id.function_index))
         ~subject:(Result_subject definition) ~mode ~request:selected
         ~defaulting_context:(mode_name default) ~typ:definition.result_type);
    Ok ()
  in
  let rec signatures = function
    | [] -> Ok ()
    | definition :: rest ->
        let* () =
          match definition.Sst.body with
          | Sst.External_specification
              (Sst.Imported_unverified_target _) ->
              Ok ()
          | Sst.Checked_exec _ | Sst.Spec_definition _
          | Sst.Recursive_spec_definition _ | Sst.Proof_body _
          | Sst.External_specification _
          | Sst.Trusted_external_spec_target _
          | Sst.Trusted_external_body _
          | Sst.Symbolic_declaration _ ->
              issue_signature definition
        in
        signatures rest
  in
  let* () = signatures program.functions in
  let requested_spelling descriptor =
    if descriptor.explicit then
      String.lowercase_ascii (mode_name descriptor.mode)
    else "default"
  in
  let callable_signature (definition : Sst.function_definition) =
    let formals =
      List.mapi
        (fun index _parameter ->
          match descriptor_for_formal environment definition index with
          | Some descriptor ->
              Printf.sprintf "%d=%s" index
                (requested_spelling descriptor)
          | None -> assert false)
        definition.parameters
    in
    let result =
      match descriptor_for_result environment definition with
      | Some descriptor ->
          Printf.sprintf "%d=%s" (List.length definition.parameters)
            (requested_spelling descriptor)
      | None -> assert false
    in
    String.concat ";" (formals @ [ result ])
  in
  let field_spelling field =
    match descriptor_for_field environment field with
    | Some descriptor -> requested_spelling descriptor
    | None -> assert false
  in
  let type_signature (definition : Sst.type_definition) =
    match definition.type_kind with
    | Sst.Record_definition fields ->
        List.map
          (fun field ->
            Printf.sprintf "record:%s=%s" field.Sst.field_id.field_name
              (field_spelling field))
          fields
        |> String.concat ";"
    | Sst.Variant_definition constructors ->
        List.concat_map
          (fun constructor ->
            List.map
              (fun field ->
                let component =
                  if
                    String.length field.Sst.field_id.field_name > 0
                    && field.field_id.field_name.[0] = '$'
                  then
                    String.sub field.field_id.field_name 1
                      (String.length field.field_id.field_name - 1)
                  else field.field_id.field_name
                in
                Printf.sprintf "variant:%s:%s=%s"
                  constructor.Sst.constructor_id.constructor_name component
                  (field_spelling field))
              constructor.constructor_fields)
          constructors
        |> String.concat ";"
  in
  let actual_interface_signature key =
    if String.starts_with ~prefix:"value:" key then
      let name =
        String.sub key 6 (String.length key - 6)
      in
      List.find_opt
        (fun definition ->
          String.equal definition.Sst.function_id.function_name name)
        program.functions
      |> Option.map callable_signature
    else if String.starts_with ~prefix:"type:" key then
      let name =
        String.sub key 5 (String.length key - 5)
      in
      List.find_opt
        (fun definition ->
          String.equal definition.Sst.type_id.type_name name)
        program.types
      |> Option.map type_signature
    else None
  in
  let* () =
    match registration with
    | Some
        {
          sealed =
            Some
              {
                explicit_interface = true;
                interface_mode_signatures;
                _;
              };
          _;
        } ->
        List.fold_left
          (fun result (key, expected) ->
            let* () = result in
            match (expected, actual_interface_signature key) with
            | Some expected, Some actual when String.equal expected actual ->
                Ok ()
            | Some _, Some _ ->
                Error
                  {
                    function_id = None;
                    span =
                      (match program.functions with
                      | definition :: _ -> definition.span
                      | [] -> Diagnostic.file_span "<semantic-sst>");
                    message =
                      "retained implementation modes do not match the exact \
                       retained interface mode signature for "
                      ^ key;
                  }
            | None, Some _ ->
                Error
                  {
                    function_id = None;
                    span =
                      (match program.functions with
                      | definition :: _ -> definition.span
                      | [] -> Diagnostic.file_span "<semantic-sst>");
                    message =
                      "retained interface is missing an authenticated mode \
                       signature for "
                      ^ key;
                  }
            | Some _, None | None, None -> Ok ())
          (Ok ()) interface_mode_signatures
    | Some _ | None -> Ok ()
  in
  let body_expression definition =
    match definition.Sst.body with
    | Sst.Checked_exec { body; _ }
    | Sst.Spec_definition body
    | Sst.Proof_body { body; _ }
    | Sst.Recursive_spec_definition { body; _ } ->
        Some body.expression
    | Sst.External_specification _
    | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _
    | Sst.Symbolic_declaration _ ->
        None
  in
  let issue_callback_captures definition =
    match Sst_callback_private.authenticated_captures program definition with
    | Error (span, message) -> fail definition span message
    | Ok captures ->
        List.iter
          (fun
            ( (capture : Callback_certificate_private.capture),
              binding ) ->
            ignore
              (issue environment
                 ~identity:
                   (lazy
                     (Printf.sprintf "callback-capture:%d:%d"
                        definition.Sst.function_id.function_index
                        capture.binding_id))
                 ~subject:
                   (Binding_subject (definition.function_id, binding))
                 ~mode:Sst.Exec_instance ~request:None
                 ~defaulting_context:
                   "authenticated-immutable-callback-capture"
                 ~typ:capture.typ))
          captures;
        Ok ()
  in
  let rec bodies = function
    | [] -> Ok ()
    | definition :: rest -> (
        match body_expression definition with
        | None -> bodies rest
        | Some body ->
            let default = default_for_definition definition in
            let* () = issue_callback_captures definition in
            let* () =
              let rec defaults = function
                | [] -> Ok ()
                | Sst.Callback_parameter _ :: rest -> defaults rest
                | Sst.Value_parameter parameter :: rest -> (
                    match parameter.optional_default with
                    | None -> defaults rest
                    | Some optional_default ->
                        let expected =
                          pattern_mode environment definition.function_id
                            optional_default.optional_pattern
                        in
                        let* mode =
                          issue_expression definition expected (Some expected)
                            optional_default.optional_expression
                        in
                        if mode = expected then defaults rest
                        else
                          fail definition optional_default.optional_expression.span
                            "optional default mode differs from its payload")
              in
              defaults definition.parameters
            in
            let* body_mode =
              issue_expression definition default
                (Some (result_mode environment definition))
                body
            in
            let* () =
              if body_mode = result_mode environment definition then Ok ()
              else
                fail definition body.span
                  (Printf.sprintf "return mode mismatch: expected %s, found %s"
                     (mode_name (result_mode environment definition))
                     (mode_name body_mode))
            in
            bodies rest)
  in
  (* Validate callers before earlier declarations when possible.  This makes
     exact call-boundary forgetting observable before a callee-local authority
     attack rejects, without changing the closed signature environment. *)
  let* () = bodies (List.rev program.functions) in
  (* This slice has no source-level visibility proof strong enough to certify
     that an otherwise top-level tracked proof formal is private to checked
     same-CMT callers.  Keep the sealed authority set empty rather than turn a
     mode annotation or one observed call into an entry-value certificate. *)
  environment.tracked_formal_authority <- [];
  Ok environment

let authenticated environment = environment.authenticated
let snapshot_digest environment = environment.snapshot

let to_string environment =
  let authority =
    match environment.registration with
    | Some { sealed = Some identity; _ } ->
        Printf.sprintf
          "authority=retained unit=%s source=%s cmt=%s interface=%s"
          identity.unit_name identity.source_file identity.cmt_file
          identity.interface_digest
    | Some _ -> "authority=unsealed"
    | None -> "authority=raw"
  in
  let descriptors =
    environment.descriptors
    |> List.sort (fun left right ->
           String.compare
             (Lazy.force left.identity)
             (Lazy.force right.identity))
    |> List.map (fun descriptor ->
           Printf.sprintf
             "%s mode=%s explicit=%b annotation=%s default=%s type=%s \
              snapshot=%s"
             (Lazy.force descriptor.identity) (mode_name descriptor.mode)
             descriptor.explicit
             (Option.fold ~none:"default" ~some:span_string
                descriptor.annotation_span)
             descriptor.defaulting_context
             (Sst.string_of_type descriptor.typ)
             descriptor.signature_snapshot)
  in
  authority
  :: (descriptors @ List.rev environment.forgetting_traces
     @ List.rev environment.standalone_proof_binding_traces)
  |> String.concat "\n"
