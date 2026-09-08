type origin_kind = Formal | Top_level | Local

(* This is deliberately a tiny immutable value.  It contains a domain tag and
   a digest of compiler-owned metadata; a Cmt_input.implementation never
   crosses the SST/VIR boundary. *)
type compilation_identity = {
  compilation_digest : string;
}

type capture = {
  binding_id : int;
  binding_uid : string;
  typ : Parametric_type.t;
  compiler_mode : string;
  instance_mode : string;
  frozen_value_identity : string;
}

type origin = {
  issuer : unit ref;
  origin_identity : unit ref;
  kind : origin_kind;
  compilation_identity : compilation_identity;
  callable_identity : string;
  shape : Callback_shape_private.t;
  compiler_mode : string;
  contract_identity : string;
  captures : capture list;
}

type t = {
  issuer : unit ref;
  certificate_identity : unit ref;
  origin : origin;
  caller_identity : string option;
  call_edge_identity : string option;
  mutable session : unit ref option;
}

let issuer = ref ()

let identity_material certificate =
  let node = Numeric_receipt_private.encode in
  let option = function None -> node ~schema:"none" [] | Some s -> node ~schema:"some" [s] in
  let origin = certificate.origin in
  node ~schema:"verocaml.callback-semantic-identity.v1"
    [(match origin.kind with Formal -> "formal" | Top_level -> "top-level" | Local -> "local");
     origin.compilation_identity.compilation_digest; origin.callable_identity;
     Numeric_receipt_private.list (List.map (fun (label, typ) ->
       node ~schema:"endpoint"
         [option (Callback_shape_private.label_to_option label); Parametric_type.structural_identity_material typ])
       (Callback_shape_private.endpoints origin.shape));
     Parametric_type.structural_identity_material (Callback_shape_private.result origin.shape);
     origin.compiler_mode; origin.contract_identity;
     Numeric_receipt_private.list (List.map (fun capture -> node ~schema:"capture"
       [string_of_int capture.binding_id; capture.binding_uid;
        Parametric_type.structural_identity_material capture.typ; capture.compiler_mode;
        capture.instance_mode; capture.frozen_value_identity]) origin.captures);
     option certificate.caller_identity; option certificate.call_edge_identity]

let digest fields =
  Digest.string (String.concat "\000" fields) |> Digest.to_hex

let kind_identity = function
  | Formal -> "formal"
  | Top_level -> "top-level"
  | Local -> "local"

let compilation_domain = "Cmt-v1"

let option_identity = function None -> "-" | Some value -> value

let import_identity (import : Cmt_input.import) =
  String.concat "\001" [ import.unit_name; option_identity import.crc ]

let compare_import (left : Cmt_input.import) (right : Cmt_input.import) =
  let by_unit = String.compare left.unit_name right.unit_name in
  if by_unit <> 0 then by_unit
  else Option.compare String.compare left.crc right.crc

let cmt_compilation_identity (implementation : Cmt_input.implementation) =
  match (implementation.source_digest, implementation.interface_digest) with
  | Some source_digest, Some interface_digest
    when implementation.unit_name <> "" && implementation.has_implementation_shape ->
      Ok
        {
          compilation_digest =
            digest
              [
                compilation_domain;
                implementation.unit_name;
                Digest.to_hex source_digest;
                interface_digest;
                implementation.raw_artifact_digest;
                Array.to_list implementation.imports
                |> List.sort compare_import |> List.map import_identity
                |> String.concat "\002";
              ];
        }
  | None, _ -> Error "callback CMT authority has no compiler source digest"
  | _, None -> Error "callback CMT authority has no compiler interface digest"
  | Some _, Some _ ->
      Error "callback CMT authority is not an implementation compilation"

let caller_identity ~function_index ~function_name =
  digest
    [
      "callback-caller-v1";
      string_of_int function_index;
      function_name;
    ]

let edge_identity ~caller ~start_line ~start_column ~end_line ~end_column =
  digest
    [
      "callback-edge-v1";
      caller;
      string_of_int start_line;
      string_of_int start_column;
      string_of_int end_line;
      string_of_int end_column;
    ]

let same_compilation left right =
  String.equal left.compilation_digest right.compilation_digest

let compilation_identity_string identity =
  compilation_domain ^ ":" ^ identity.compilation_digest

let capture_identity capture =
  String.concat "\001"
    [
      string_of_int capture.binding_id;
      capture.binding_uid;
      Parametric_type.to_string capture.typ;
      capture.compiler_mode;
      capture.instance_mode;
      capture.frozen_value_identity;
    ]

let relation_identity origin =
  digest
    [
      kind_identity origin.kind;
      compilation_identity_string origin.compilation_identity;
      origin.callable_identity;
      origin.compiler_mode;
      origin.contract_identity;
      Callback_shape_private.to_string origin.shape;
      String.concat "\002" (List.map capture_identity origin.captures);
    ]

let certificate origin ~caller_identity ~call_edge_identity =
  {
    issuer;
    certificate_identity = ref ();
    origin;
    caller_identity;
    call_edge_identity;
    session = None;
  }

let issue_formal ~compilation_identity ~callable_identity ~shape =
  let origin =
    {
      issuer;
      origin_identity = ref ();
      kind = Formal;
      compilation_identity;
      callable_identity;
      shape;
      compiler_mode = "compiler-formal";
      contract_identity = "abstract-callback-relation";
      captures = [];
    }
  in
  certificate origin ~caller_identity:None ~call_edge_identity:None

let unique_capture_ids captures =
  let ids = List.map (fun capture -> capture.binding_id) captures in
  List.length ids = List.length (List.sort_uniq Int.compare ids)

let complete_capture capture =
  not (String.equal capture.binding_uid "")
  && not (String.equal capture.compiler_mode "")
  && not (String.equal capture.instance_mode "")
  && not (String.equal capture.frozen_value_identity "")

let issue_origin ~kind ~compilation_identity ~callable_identity ~shape
    ~compiler_mode ~contract_identity ~captures ~pure ~total ~complete =
  if kind = Formal then Error "a callback origin cannot be a formal relation"
  else if List.exists (String.equal "") [ callable_identity; compiler_mode; contract_identity ]
  then Error "callback certificate identity evidence is incomplete"
  else if not (pure && total && complete) then
    Error "callback purity, totality, or completion evidence is absent"
  else if
    (not (unique_capture_ids captures))
    || not (List.for_all complete_capture captures)
  then Error "callback capture evidence is incomplete or ambiguous"
  else
    Ok
      {
        issuer;
        origin_identity = ref ();
        kind;
        compilation_identity;
        callable_identity;
        shape;
        compiler_mode;
        contract_identity;
        captures;
      }

let seal_call_edge origin ~caller_identity ~call_edge_identity =
  certificate origin ~caller_identity:(Some caller_identity)
    ~call_edge_identity:(Some call_edge_identity)

let authentic certificate =
  certificate.issuer == issuer && certificate.origin.issuer == issuer

let bind_session certificate ~compilation_identity ~session =
  if not (authentic certificate) then Error "forged callback certificate"
  else if not (same_compilation compilation_identity certificate.origin.compilation_identity) then
    Error "callback certificate compilation identity mismatch"
  else
    match certificate.session with
    | None ->
        certificate.session <- Some session;
        Ok ()
    | Some existing when existing == session -> Ok ()
    | Some _ -> Error "callback certificate was replayed across sessions"

let authenticate certificate ~shape ~compilation_identity ~session
    ~caller_identity ~call_edge_identity =
  if not (authentic certificate) then Error "forged callback certificate"
  else if not (Callback_shape_private.same_identity shape certificate.origin.shape) then
    Error "callback certificate shape identity mismatch"
  else if not (same_compilation compilation_identity certificate.origin.compilation_identity) then
    Error "callback certificate compilation identity mismatch"
  else if
    match certificate.session with
    | Some actual -> actual != session
    | None -> true
  then Error "callback certificate session identity mismatch"
  else if caller_identity <> certificate.caller_identity then
    Error "callback certificate caller mismatch"
  else if call_edge_identity <> certificate.call_edge_identity then
    Error "callback certificate call-edge mismatch"
  else Ok ()

let authenticate_shape certificate shape =
  if not (authentic certificate) then Error "forged callback certificate"
  else if Callback_shape_private.same_identity shape certificate.origin.shape then Ok ()
  else Error "callback certificate shape identity mismatch"

let kind certificate = certificate.origin.kind
let callable_identity certificate = certificate.origin.callable_identity
let relation_identity certificate = relation_identity certificate.origin
let call_edge_identity certificate = certificate.call_edge_identity
let captures certificate = certificate.origin.captures
let same_identity left right = left.certificate_identity == right.certificate_identity
let same_origin left right = left.origin.origin_identity == right.origin.origin_identity

let describe certificate =
  let kind = kind_identity certificate.origin.kind in
  Printf.sprintf "%s:%s:%s:mode=%s:caller=%s" kind
    certificate.origin.callable_identity (relation_identity certificate)
    certificate.origin.compiler_mode
    (Option.value ~default:"-" certificate.caller_identity)
