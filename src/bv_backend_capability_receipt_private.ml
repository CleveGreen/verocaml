type id = string

type receipt = {
  schema : string;
  issuer_authority : string;
  build_compatibility : string;
  ocaml_abi : string;
  ocaml_max_int : Z.t;
  z3_package : string;
  c_unsigned_width : int;
  c_unsigned_maximum : Z.t;
  maximum_bv_width : int;
  full_key : string;
  checked_id : id;
}

type artifact_binding = { full_key : string }
type reference = { encoded : string }
type capability = { token : unit ref }
type worker_receipt : value mod portable = {
  maximum_bv_width : int;
  full_key : string;
  checked_id : string;
  checked_id_hex : string;
  abi_supports_ceiling : bool;
  artifact_binding_key : string;
}

external c_unsigned_facts : unit -> int * string
  = "verocaml_bv_c_unsigned_facts"

let schema = "verocaml.bv-backend-capability.v1"
let maximum_bv_width = Bv_backend_capability_generated.maximum_bv_width
let capability_token = ref ()
let capability () = { token = capability_token }
let reference_magic = "VBVREF01"
let reference_maximum_bytes = 512

let raw_digest32 ~domain value =
  Digest.string (domain ^ "\0000\000" ^ value)
  ^ Digest.string (domain ^ "\0001\000" ^ value)

let lower_hex raw =
  let digits = "0123456789abcdef" in
  String.init (2 * String.length raw) (fun index ->
      let byte = Char.code raw.[index / 2] in
      if index mod 2 = 0 then digits.[byte lsr 4]
      else digits.[byte land 0xf])

let id_to_hex = lower_hex

let build_compatibility () =
  Numeric_receipt_private.encode
    ~schema:Bv_backend_capability_generated.build_input_schema
    [ Config.version; Config.target; Config.architecture; Config.model;
      Config.system; Config.ccomp_type; Config.ext_obj; Config.ext_lib ]

let ocaml_abi () =
  Numeric_receipt_private.encode ~schema:"verocaml.bv-ocaml-abi.v1"
    [ Sys.ocaml_version; Config.cmi_magic_number; Config.cmt_magic_number;
      string_of_int Sys.word_size; string_of_int max_int ]

let z3_package () =
  Numeric_receipt_private.encode ~schema:"verocaml.bv-z3-package.v1"
    [ Bv_backend_capability_generated.z3_package_version;
      Bv_backend_capability_generated.z3_portability_patch_sha256;
      string_of_int Z3.Version.major; string_of_int Z3.Version.minor;
      string_of_int Z3.Version.build; Z3.Version.full_version ]

let derive () =
  let c_unsigned_width, c_unsigned_maximum_text = c_unsigned_facts () in
  let c_unsigned_maximum = Z.of_string c_unsigned_maximum_text in
  let issuer_authority = Bv_backend_capability_generated.issuer_authority in
  let build_compatibility = build_compatibility () in
  let ocaml_abi = ocaml_abi () in
  let z3_package = z3_package () in
  let full_key =
    Numeric_receipt_private.encode ~schema
      [ "1"; issuer_authority; build_compatibility; ocaml_abi;
        string_of_int max_int; z3_package; string_of_int c_unsigned_width;
        c_unsigned_maximum_text; string_of_int maximum_bv_width ]
  in
  let checked_id =
    raw_digest32 ~domain:"verocaml.bv-backend-capability.id.v1" full_key
  in
  ( { schema; issuer_authority; build_compatibility; ocaml_abi;
      ocaml_max_int = Z.of_int max_int; z3_package; c_unsigned_width;
      c_unsigned_maximum; maximum_bv_width; full_key; checked_id },
    None )

let sealed = lazy (derive ())

(** Portable workers compare references against this immutable value derived
    only from the current build; transported fields never replace it. *)
let worker_sealed =
  let receipt, artifact_binding = derive () in
  { maximum_bv_width = receipt.maximum_bv_width;
    full_key = receipt.full_key;
    checked_id = receipt.checked_id;
    checked_id_hex = lower_hex receipt.checked_id;
    abi_supports_ceiling =
      receipt.ocaml_max_int >= Z.of_int receipt.maximum_bv_width
      && receipt.c_unsigned_maximum >= Z.of_int receipt.maximum_bv_width
      && receipt.c_unsigned_width > 0;
    artifact_binding_key =
      (match artifact_binding with
      | None -> "\000"
      | Some binding -> "\001" ^ binding.full_key)
  }

let authenticate (capability [@delator.skip]) =
  let result =
    if capability.token != capability_token then
      Error "BV backend capability has no live build issuer"
    else
      let receipt, artifact_binding = Lazy.force sealed in
      if receipt.maximum_bv_width <> maximum_bv_width then
        Error "BV backend capability ceiling mismatch"
      else if receipt.ocaml_max_int < Z.of_int maximum_bv_width then
        Error "pinned OCaml ABI cannot represent the BV width ceiling"
      else if receipt.c_unsigned_maximum < Z.of_int maximum_bv_width then
        Error "pinned Z3 C ABI cannot represent the BV width ceiling"
      else if receipt.c_unsigned_width <= 0 then
        Error "pinned Z3 C unsigned width is invalid"
      else if String.length receipt.checked_id <> 32 then
        Error "BV backend capability ID is not 32 bytes"
      else
        let expected =
          raw_digest32 ~domain:"verocaml.bv-backend-capability.id.v1"
            receipt.full_key
        in
        if String.equal expected receipt.checked_id then
          Ok (receipt, artifact_binding)
        else Error "BV backend capability digest mismatch"
  in
  (match result with
  | Ok ((receipt [@log_value.trace]), _) ->
      [%log.trace "authenticated sealed BV backend capability"
        ~stage:(Delator.Field.string "bv-backend-capability")
        ~schema:(Delator.Field.string (receipt [@log_value.trace]).schema)
        ~maximum_width:
          (Delator.Field.int (receipt [@log_value.trace]).maximum_bv_width)
        ~inner_key_bytes:
          (Delator.Field.int
             (String.length (receipt [@log_value.trace]).full_key))
        ~c_unsigned_width:
          (Delator.Field.int (receipt [@log_value.trace]).c_unsigned_width)
        ~capability_id:
          (Delator.Field.string
             (id_to_hex (receipt [@log_value.trace]).checked_id))
        ~outer_artifact_binding:(Delator.Field.string "absent-unfinalized")
        ~authority:(Delator.Field.string "official-build")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.error]) ->
      [%log.error "rejected BV backend capability"
        ~stage:(Delator.Field.string "bv-backend-capability")
        ~reason_class:(Delator.Field.string (reason [@log_value.error]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let authenticate_profile capability
    (profile : Build_target_profile_private.profile) =
  let result =
    match authenticate capability with
    | Error _ as error -> error
    | Ok _ ->
        let build_capability = Build_target_profile_private.capability () in
        (match
           Build_target_profile_private.decode_profile build_capability
             profile.Build_target_profile_private.full_key
         with
        | Ok authenticated
          when Build_target_profile_private.equal_profile authenticated profile ->
            Ok ()
        | Ok _ | Error _ -> Error "logical BV profile is not issued by this build")
  in
  (match result with
  | Ok () ->
      [%log.trace "authenticated exact sealed logical BV profile"
        ~stage:(Delator.Field.string "bv-profile-capability-agreement")
        ~profile:(Delator.Field.string profile.full_key)
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.error]) ->
      [%log.error "rejected BV backend and logical profile disagreement"
        ~stage:(Delator.Field.string "bv-profile-capability-agreement")
        ~reason_class:(Delator.Field.string (reason [@log_value.error]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let authenticate_instance capability
    (instance : Build_target_profile_private.instance) =
  let result =
    match authenticate capability with
    | Error _ as error -> error
    | Ok _ ->
        let build_capability = Build_target_profile_private.capability () in
        (match
           Build_target_profile_private.decode_instance build_capability
             instance.Build_target_profile_private.full_key
         with
        | Ok authenticated
          when Build_target_profile_private.equal_instance authenticated
                 instance ->
            Ok ()
        | Ok _ | Error _ ->
            Error "logical BV target instance is not issued by this build")
  in
  (match result with
  | Ok () ->
      [%log.trace "authenticated exact sealed target instance for BV"
        ~stage:(Delator.Field.string "bv-instance-capability-agreement")
        ~target:(Delator.Field.string instance.full_key)
        ~machine_width:(Delator.Field.int instance.target_claim.width)
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.error]) ->
      [%log.error "rejected BV backend and target instance disagreement"
        ~stage:(Delator.Field.string "bv-instance-capability-agreement")
        ~reason_class:(Delator.Field.string (reason [@log_value.error]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let encoded_binding = function
  | None -> "\000"
  | Some binding -> "\001" ^ binding.full_key

let encode_parts (receipt : receipt) artifact_binding =
  reference_magic ^ receipt.full_key ^ receipt.checked_id
  ^ encoded_binding artifact_binding

let reference capability =
  match authenticate capability with
  | Error _ as error -> error
  | Ok (receipt, artifact_binding) ->
      let encoded = encode_parts receipt artifact_binding in
      if String.length encoded > reference_maximum_bytes then
        Error "BV backend capability reference exceeds 512 bytes"
      else Ok { encoded }

let encode_reference reference = reference.encoded

let decode_reference capability encoded =
  let result =
    if String.length encoded > reference_maximum_bytes then
      Error "BV backend capability reference exceeds 512 bytes"
    else
      match authenticate capability with
      | Error _ as error -> error
      | Ok (receipt, artifact_binding) ->
          let binding = encoded_binding artifact_binding in
          let magic_bytes = String.length reference_magic in
          let key_bytes = String.length receipt.full_key in
          let id_bytes = String.length receipt.checked_id in
          let expected_bytes = magic_bytes + key_bytes + id_bytes + String.length binding in
          if String.length encoded <> expected_bytes then
            Error "BV backend capability reference has the wrong fixed size"
          else
            let transported_magic = String.sub encoded 0 magic_bytes in
            let transported_key = String.sub encoded magic_bytes key_bytes in
            let transported_id =
              String.sub encoded (magic_bytes + key_bytes) id_bytes
            in
            let transported_binding =
              String.sub encoded (magic_bytes + key_bytes + id_bytes)
                (String.length binding)
            in
            if not (String.equal transported_magic reference_magic) then
              Error "BV backend capability reference magic mismatch"
            else if not (String.equal transported_key receipt.full_key) then
              Error "BV backend capability full key differs from the sealed build"
            else if not (String.equal transported_id receipt.checked_id) then
              Error "BV backend capability ID differs from its sealed full key"
            else if not (String.equal transported_binding binding) then
              Error "BV backend capability artifact binding differs from the sealed build"
            else Ok { encoded = encode_parts receipt artifact_binding }
  in
  (match result with
  | Ok _ ->
      [%log.trace "validated private BV capability transport reference"
        ~stage:(Delator.Field.string "bv-capability-reference")
        ~encoded_bytes:(Delator.Field.int (String.length encoded))
        ~authority:(Delator.Field.string "local-sealed-build")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.error]) ->
      [%log.error "rejected private BV capability transport reference"
        ~stage:(Delator.Field.string "bv-capability-reference")
        ~encoded_bytes:(Delator.Field.int (String.length encoded))
        ~reason_class:(Delator.Field.string (reason [@log_value.error]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let decode_reference_for_worker @ portable = fun encoded ->
  if String.length encoded > reference_maximum_bytes then
    Error "detached BV backend capability reference exceeds 512 bytes"
  else
    let receipt = worker_sealed in
    let binding = receipt.artifact_binding_key in
    let magic_bytes = String.length reference_magic in
    let key_bytes = String.length receipt.full_key in
    let id_bytes = String.length receipt.checked_id in
    let expected_bytes =
      magic_bytes + key_bytes + id_bytes + String.length binding
    in
    if receipt.maximum_bv_width <> maximum_bv_width then
      Error "detached BV backend capability ceiling mismatch"
    else if not receipt.abi_supports_ceiling then
      Error "detached pinned ABI cannot represent the BV width ceiling"
    else if String.length receipt.checked_id <> 32 then
      Error "detached BV backend capability ID is not 32 bytes"
    else if String.length encoded <> expected_bytes then
      Error "detached BV backend capability reference has the wrong fixed size"
    else
      let transported_magic = String.sub encoded 0 magic_bytes in
      let transported_key = String.sub encoded magic_bytes key_bytes in
      let transported_id =
        String.sub encoded (magic_bytes + key_bytes) id_bytes
      in
      let transported_binding =
        String.sub encoded (magic_bytes + key_bytes + id_bytes)
          (String.length binding)
      in
      if not (String.equal transported_magic reference_magic) then
        Error "detached BV backend capability reference magic mismatch"
      else if not (String.equal transported_key receipt.full_key) then
        Error "detached BV backend capability full key differs from the local build"
      else if not (String.equal transported_id receipt.checked_id) then
        Error "detached BV backend capability ID differs from its local full key"
      else if not (String.equal transported_binding binding) then
        Error "detached BV backend artifact binding differs from the local build"
      else Ok receipt

let equal_receipt (left : receipt) (right : receipt) =
  String.equal left.full_key right.full_key

let equal_artifact_binding (left : artifact_binding)
    (right : artifact_binding) =
  String.equal left.full_key right.full_key
