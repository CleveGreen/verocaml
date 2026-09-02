open Typedtree

type request = {
  ordinal : int;
  subject_span : Diagnostic.span;
  annotation_span : Diagnostic.span;
}

type sealed_identity = {
  unit_name : string;
  interface_digest : string;
  source_digest : string;
  explicit_interface : bool;
  interface_finite_signatures : (string * string option) list;
}

type registration = {
  program : Sst.program;
  structure : Typedtree.structure;
  requests : request list;
  raw_snapshot : string;
  mutable sealed : sealed_identity option;
}

type descriptor = {
  issuer : unit ref;
  registration : registration;
  function_id : Sst.function_id;
  ordinal : int;
  requirement_digest : string;
}

type environment = {
  issuer : unit ref;
  program : Sst.program;
  registration : registration option;
  descriptors : descriptor list;
}

let process_issuer = ref ()
let registrations : registration list ref = ref []

let span source_file location =
  Diagnostic.span_of_location ~fallback_file:source_file location

let empty_payload = function Parsetree.PStr [] -> true | _ -> false

let finite_signature_payload attributes =
  let matching =
    List.filter
      (fun attribute ->
        String.equal attribute.Parsetree.attr_name.txt
          "verocaml.internal.finite_signature")
      attributes
  in
  match matching with
  | [] -> Error "finite signature metadata is absent"
  | _ :: _ :: _ -> Error "finite signature metadata is duplicated"
  | [ attribute ] ->
      if
        not
          (attribute.attr_loc.Location.loc_ghost
          && attribute.attr_name.loc.loc_ghost)
      then Error "finite signature metadata is not ghost-located"
      else
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
            ]
          when String.starts_with ~prefix:"v1|" value ->
            Ok value
        | _ -> Error "finite signature metadata payload is malformed"

let authenticate_constrained_signature ~implementation ~interface =
  match
    ( finite_signature_payload implementation,
      finite_signature_payload interface )
  with
  | Ok implementation, Ok interface when String.equal implementation interface ->
      Ok ()
  | Ok _, Ok _ ->
      Error
        "same-CMT constrained implementation finite formals do not match the authenticated public signature"
  | ( Error "finite signature metadata is absent",
      Error "finite signature metadata is absent" ) ->
      Ok ()
  | Error message, _ | _, Error message -> Error message

let request_of_attribute ~source_file ~subject_span attribute =
  let prefix = "verocaml.internal.finite_formal." in
  if
    attribute.Parsetree.attr_loc.Location.loc_ghost
    && attribute.attr_name.loc.loc_ghost
    && String.starts_with ~prefix attribute.attr_name.txt
    && empty_payload attribute.attr_payload
  then
    let suffix =
      String.sub attribute.attr_name.txt (String.length prefix)
        (String.length attribute.attr_name.txt - String.length prefix)
    in
    match int_of_string_opt suffix with
    | Some ordinal when ordinal >= 0 ->
        Some
          {
            ordinal;
            subject_span;
            annotation_span = span source_file attribute.attr_loc;
          }
    | Some _ | None -> None
  else None

let collect_requests ~source_file structure =
  let requests = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          List.iter
            (fun attribute ->
              Option.iter
                (fun request -> requests := request :: !requests)
                (request_of_attribute ~source_file
                   ~subject_span:(span source_file expression.exp_loc)
                   attribute))
            expression.exp_attributes;
          default.expr self expression);
    }
  in
  iterator.structure iterator structure;
  List.rev !requests

let requires_authentication (implementation : Cmt_input.implementation) =
  List.exists (fun (_, signature) -> Option.is_some signature)
    implementation.interface_finite_signatures
  || collect_requests ~source_file:implementation.source_file
       implementation.structure
     <> []

let source_file program =
  match program.Sst.functions with
  | definition :: _ -> definition.span.file
  | [] -> (
      match program.types with
      | definition :: _ -> definition.span.file
      | [] -> "<semantic-sst>")

let prepare ~structure ~program =
  let registration =
    {
      program;
      structure;
      requests = collect_requests ~source_file:(source_file program) structure;
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

let seal
    (implementation [@delator.skip])
    (program [@delator.skip]) =
  match find_registration program with
  | None ->
      [%log.warn "rejected finite-formal authority seal"
        ~stage:(Delator.Field.string "finite-formal-seal")
        ~artifact_family:
          (Delator.Field.string
             (if Cmt_input.ordinary_ppx_artifact implementation then "ordinary"
              else if Cmt_input.retained_ppx_artifact implementation then
                "retained"
              else if implementation.implementation_family_markers = [] then
                "unmarked"
              else "unsupported"))
        ~request_count:(Delator.Field.int 0)
        ~family_marker_count:
          (Delator.Field.int
             (List.length implementation.implementation_family_markers))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "missing-registration")];
      Error "finite-formal authority has no exact Typedtree lowering registration"
  | Some registration
    when registration.structure != implementation.Cmt_input.structure ->
      [%log.warn "rejected finite-formal authority seal"
        ~stage:(Delator.Field.string "finite-formal-seal")
        ~artifact_family:
          (Delator.Field.string
             (if Cmt_input.ordinary_ppx_artifact implementation then "ordinary"
              else if Cmt_input.retained_ppx_artifact implementation then
                "retained"
              else if implementation.implementation_family_markers = [] then
                "unmarked"
              else "unsupported"))
        ~request_count:(Delator.Field.int (List.length registration.requests))
        ~family_marker_count:
          (Delator.Field.int
             (List.length implementation.implementation_family_markers))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "typedtree-program-pairing")];
      Error "finite-formal authority rejected a copied Typedtree/program pairing"
  | Some registration
    when not (String.equal registration.raw_snapshot (Sst.to_string program)) ->
      [%log.warn "rejected finite-formal authority seal"
        ~stage:(Delator.Field.string "finite-formal-seal")
        ~artifact_family:
          (Delator.Field.string
             (if Cmt_input.ordinary_ppx_artifact implementation then "ordinary"
              else if Cmt_input.retained_ppx_artifact implementation then
                "retained"
              else if implementation.implementation_family_markers = [] then
                "unmarked"
              else "unsupported"))
        ~request_count:(Delator.Field.int (List.length registration.requests))
        ~family_marker_count:
          (Delator.Field.int
             (List.length implementation.implementation_family_markers))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "sst-snapshot")];
      Error "finite-formal authority rejected a changed SST snapshot"
  | Some registration
    when
      registration.requests = []
      &&
      (Cmt_input.ordinary_ppx_artifact implementation
      || ((not (Cmt_input.retained_ppx_artifact implementation))
         && implementation.implementation_family_markers = [])) ->
      [%log.debug "accepted finite-formal authority seal without requirements"
        ~stage:(Delator.Field.string "finite-formal-seal")
        ~artifact_family:
          (Delator.Field.string
             (if Cmt_input.ordinary_ppx_artifact implementation then "ordinary"
              else if Cmt_input.retained_ppx_artifact implementation then
                "retained"
              else if implementation.implementation_family_markers = [] then
                "unmarked"
              else "unsupported"))
        ~request_count:(Delator.Field.int 0)
        ~decision:(Delator.Field.string "accepted")
        ~reason_class:(Delator.Field.string "no-finite-requirements")];
      Ok ()
  | Some _registration when not (Cmt_input.retained_ppx_artifact implementation) ->
      [%log.warn "rejected finite-formal authority seal"
        ~stage:(Delator.Field.string "finite-formal-seal")
        ~artifact_family:
          (Delator.Field.string
             (if Cmt_input.ordinary_ppx_artifact implementation then "ordinary"
              else if implementation.implementation_family_markers = [] then
                "unmarked"
              else "unsupported"))
        ~request_count:(Delator.Field.int (List.length _registration.requests))
        ~family_marker_count:
          (Delator.Field.int
             (List.length implementation.implementation_family_markers))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "artifact-family")];
      Error "finite-formal authority requires the retained artifact family"
  | Some registration -> (
      match
        (implementation.Cmt_input.interface_digest, implementation.source_digest)
      with
      | Some interface_digest, Some source_digest ->
          registration.sealed <-
            Some
              {
                unit_name = implementation.unit_name;
                interface_digest;
                source_digest;
                explicit_interface = implementation.explicit_interface;
                interface_finite_signatures =
                  implementation.interface_finite_signatures;
              };
          [%log.debug "completed finite-formal authority seal"
            ~stage:(Delator.Field.string "finite-formal-seal")
            ~artifact_family:
              (Delator.Field.string
                 (if Cmt_input.retained_ppx_artifact implementation then
                    "retained"
                  else "unsupported"))
            ~request_count:(Delator.Field.int (List.length registration.requests))
            ~interface_signature_count:
              (Delator.Field.int
                 (List.length implementation.interface_finite_signatures))
            ~explicit_interface:
              (Delator.Field.bool implementation.explicit_interface)
            ~decision:(Delator.Field.string "accepted")
            ~reason_class:(Delator.Field.string "authenticated-identity")];
          Ok ()
      | None, _ | _, None ->
          [%log.warn "rejected finite-formal authority seal"
            ~stage:(Delator.Field.string "finite-formal-seal")
            ~artifact_family:
              (Delator.Field.string
                 (if Cmt_input.retained_ppx_artifact implementation then
                    "retained"
                  else "unsupported"))
            ~request_count:
              (Delator.Field.int (List.length registration.requests))
            ~family_marker_count:
              (Delator.Field.int
                 (List.length implementation.implementation_family_markers))
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "missing-identity-digest")];
          Error
            "finite-formal authority requires exact interface and source digests")
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let position_le (left : Diagnostic.position) (right : Diagnostic.position) =
  left.line < right.line
  || (left.line = right.line && left.column <= right.column)

let span_contains (outer : Diagnostic.span) (inner : Diagnostic.span) =
  String.equal outer.file inner.file
  && position_le outer.start_pos inner.start_pos
  && position_le inner.end_pos outer.end_pos

let digest value = Digest.string value |> Digest.to_hex

let vector_for_definition requests (definition : Sst.function_definition) =
  "v1|"
  ^ (List.mapi
    (fun ordinal (parameter : Sst.parameter) ->
      let finite =
        List.exists
          (fun (request : request) ->
            request.ordinal = ordinal
            && span_contains definition.span request.subject_span)
          requests
      in
      let parameter = Sst.require_value_parameter parameter in
      let label =
        match parameter.label with
        | None -> "-"
        | Some label when String.starts_with ~prefix:"?" label ->
            "optional:"
            ^ String.sub label 1 (String.length label - 1)
        | Some label -> "label:" ^ label
      in
      Printf.sprintf "%d:%s=%s" ordinal label
        (if finite then "finite" else "default"))
       definition.parameters
    |> String.concat ";")

let validate_interface registration identity =
  if not identity.explicit_interface then Ok ()
  else
  let actual key =
    if String.starts_with ~prefix:"value:" key then
      let name = String.sub key 6 (String.length key - 6) in
      List.find_opt
        (fun definition ->
          String.equal definition.Sst.function_id.function_name name)
        registration.program.functions
      |> Option.map (vector_for_definition registration.requests)
    else None
  in
  List.fold_left
    (fun result (key, expected) ->
      match result with
      | Error _ -> result
      | Ok () -> (
          match (expected, actual key) with
          | Some expected, Some actual when String.equal expected actual -> Ok ()
          | Some _, Some _ ->
              Error
                ("retained implementation finite formals do not match the exact \
                  retained interface finite signature for "
                ^ key)
          | None, Some actual when actual <> "" ->
              Error
                ("retained interface is missing an authenticated finite signature for "
                ^ key)
          | Some _, None | None, None | None, Some _ -> Ok ()))
    (Ok ()) identity.interface_finite_signatures

let validate program =
  match find_registration program with
  | None ->
      Ok
        {
          issuer = process_issuer;
          program;
          registration = None;
          descriptors = [];
        }
  | Some registration
    when not (String.equal registration.raw_snapshot (Sst.to_string program)) ->
      Error "finite-formal metadata rejected a changed SST snapshot"
  | Some { sealed = None; requests = []; _ } as registration ->
      let registration = Option.get registration in
      Ok
        {
          issuer = process_issuer;
          program;
          registration = Some registration;
          descriptors = [];
        }
  | Some { sealed = None; _ } ->
      Error "finite-formal metadata is not sealed to an authenticated CMT"
  | Some registration ->
      let identity = Option.get registration.sealed in
      (match validate_interface registration identity with
      | Error _ as error -> error
      | Ok () ->
          let missing = ref false in
          let descriptors =
            List.concat_map
              (fun (request : request) ->
                let definitions =
                  List.filter
                    (fun (definition : Sst.function_definition) ->
                      request.ordinal
                      < List.length definition.Sst.parameters
                      && span_contains definition.span request.subject_span)
                    program.functions
                in
                if definitions = [] then missing := true;
                List.map
                  (fun (definition : Sst.function_definition) ->
                    {
                      issuer = process_issuer;
                      registration;
                      function_id = definition.function_id;
                      ordinal = request.ordinal;
                      requirement_digest =
                        digest
                          (String.concat "\000"
                             [
                               identity.unit_name;
                               identity.interface_digest;
                               identity.source_digest;
                               definition.function_id.function_name;
                               string_of_int
                                 definition.function_id.function_index;
                               vector_for_definition registration.requests
                                 definition;
                               Printf.sprintf "%s:%d:%d"
                                 request.annotation_span.file
                                 request.annotation_span.start_pos.line
                                 request.annotation_span.start_pos.column;
                             ]);
                    })
                  definitions)
              registration.requests
          in
          if !missing then
            Error "finite-formal marker does not name an exact callable formal"
          else
            Ok
              {
                issuer = process_issuer;
                program;
                registration = Some registration;
                descriptors;
              })

let find environment function_id ~ordinal =
  if
    environment.issuer != process_issuer
    ||
    (match environment.registration with
    | Some registration -> registration.program != environment.program
    | None -> environment.descriptors <> [])
  then None
  else
    List.find_opt
      (fun (descriptor : descriptor) ->
        descriptor.issuer == process_issuer
        &&
        (match environment.registration with
        | Some registration -> descriptor.registration == registration
        | None -> false)
        && descriptor.function_id = function_id
        && descriptor.ordinal = ordinal)
      environment.descriptors

let ordinal descriptor = descriptor.ordinal
let requirement_digest descriptor = descriptor.requirement_digest

let dump environment =
  environment.descriptors
  |> List.sort (fun (left : descriptor) (right : descriptor) ->
         match Int.compare left.function_id.function_index right.function_id.function_index with
         | 0 -> Int.compare left.ordinal right.ordinal
         | order -> order)
  |> List.map (fun (descriptor : descriptor) ->
         Printf.sprintf "%s#%d formal=%d digest=%s"
           descriptor.function_id.function_name
           descriptor.function_id.function_index descriptor.ordinal
           descriptor.requirement_digest)
  |> String.concat "\n"
