open Outcome_test_support

let () = ignore Numeric_receipt_prerequisites.ready
let suite_path = "test/numeric_receipts/outcome_cases.ml"
let require condition message = if not condition then failwith message
let rejected = function Error _ -> true | Ok _ -> false

let read_file filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file filename contents =
  let channel = open_out_bin filename in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let copy_file source target = write_file target (read_file source)

let run_process program arguments =
  let output = Filename.temp_file "numeric-compiler-" ".log" in
  let channel =
    Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close channel;
      if Sys.file_exists output then Sys.remove output)
    (fun () ->
      let argv = Array.of_list (program :: arguments) in
      let process =
        Unix.create_process program argv Unix.stdin channel channel
      in
      match snd (Unix.waitpid [] process) with
      | Unix.WEXITED 0 -> ()
      | WEXITED code ->
          failwith
            (Printf.sprintf "compiler fixture process exited with code %d: %s"
               code (read_file output))
      | WSIGNALED signal | WSTOPPED signal ->
          failwith
            (Printf.sprintf "compiler fixture process stopped by signal %d: %s"
               signal (read_file output)))

let ok label = function
  | Ok value -> value
  | Error reason -> failwith (label ^ ": " ^ reason)

let direct_case ~name check =
  let unit_name = name in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit unit_name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ unit_name)
           (Outcome.Function_exists unit_name))
    (fun ~environment:_ ~workspace:_ ->
      try
        check ();
        Ok
          (Outcome.observation ~status:Outcome.Verified
             ~units:[ (unit_name, Outcome.Unit_verified) ]
             ~named_facts:
               [ ("function:" ^ unit_name, Outcome.Function_exists unit_name) ]
             ()
          |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let logical_sort () =
  ok "logical sort"
    (Logical_sort_private.create ~provider_origin:"math-provider:uid"
       ~type_path:"math-provider:type-path" ~type_uid:"math-provider:type-uid"
       ~manifest_path:"int" ~manifest_uid:"compiler-predef-int:uid"
       ~integer_literal_path:"math-provider:literal-path"
       ~integer_literal_uid:"math-provider:literal-uid"
       ~issuer:"retained-interface:issuer")

let layouts () =
  let make width =
    ok "layout claim"
      (Target_profile_private.layout ~layout_class:"shared-word-layout"
         ~layout_abi:("exact-layout-abi:" ^ string_of_int width) ~width
         ~signed:true)
  in
  (make 32, make 64)

let logical_bv_permission issuer_claim =
  ok "logical BV width permission"
    (Target_profile_private.issue_logical_bv_width_permission ~issuer_claim)

let profile_claim () =
  let layout32, layout64 = layouts () in
  let profile =
    ok "profile claim"
      (Target_profile_private.issue_claim ~compiler_abi:"compiler-abi:claim"
         ~toolchain_abi:"toolchain-abi:claim"
         ~target_identity:"target-family:claim"
         ~layouts:[ layout64; layout32 ] ~supported_widths:[ 64; 32 ]
         ~logical_bv_width_permission:
           (logical_bv_permission "build-issuer:claim")
         ~issuer_claim:"build-issuer:claim")
  in
  (layout32, layout64, profile)

let base_claim () =
  let sort = logical_sort () in
  ok "base Int claim"
    (Numeric_descriptor_private.base_int_claim ~logical_sort:sort
       ~validated_digest:(Logical_sort_private.digest sort)
       ~outer_artifact_binding_key_claim:"outer-artifact-binding:claim")

let carrier_claim ?(representation = Numeric_descriptor_private.Boxed) () =
  ok "carrier claim"
    (Numeric_descriptor_private.carrier_claim
       ~type_path_claim:"nested.carrier:path-claim"
       ~type_uid_claim:"nested.carrier:uid-claim"
       ~constructor_abi_claim:"carrier:constructor-abi-claim"
       ~binder_abi_claim:"carrier:binder-abi-claim"
       ~representation_claim:representation
       ~type_argument_claims:[ "same-type:uid-claim"; "same-type:uid-claim" ]
       ~fully_instantiated_claim:true)

let callable_claim identity =
  ok "callable claim"
    (Numeric_descriptor_private.callable_claim
       ~callable_path_claim:(identity ^ ":path-claim")
       ~callable_uid_claim:(identity ^ ":uid-claim")
       ~binder_abi_claim:(identity ^ ":binder-abi-claim")
       ~parameter_abi_claims:[ "carrier:uid-claim" ]
       ~result_abi_claim:"result:uid-claim" ~total_claim:true
       ~effect_claims:[] ~exception_claims:[])

let binding_claim () =
  ok "binding claim"
    (Numeric_descriptor_private.binding_claim
       ~law_identity_claim:"law:full-key-claim"
       ~callable_claim:(callable_claim "view")
       ~semantic_role:
         (Numeric_descriptor_private.Operation
            { role_schema = "provider-neutral-role.v1";
              role_identity = "operation:identity" })
       ~int_semantics:
         (Numeric_descriptor_private.Int_relation
            { relation_full_key_claim = "int-relation:full-key-claim" })
       ~semantic_authority_claim:
         (Numeric_descriptor_private.Trusted_axiom_claim
            { trust_receipt_claim = "law-trust:claim" })
       ~implementation_refinement_claim:
         Numeric_descriptor_private.Unknown_refinement_claim
       ~visibility:Numeric_descriptor_private.Opaque_body
       ~reveal_permission:Numeric_descriptor_private.Reveal_forbidden
       ~inline_permission:Numeric_descriptor_private.Inline_forbidden)

let descriptor_claim () =
  let _, layout64, profile_claim = profile_claim () in
  let target_claim =
    ok "target claim"
      (Target_profile_private.instantiate_claim profile_claim ~layout:layout64)
  in
  let value =
    ok "descriptor claim"
      (Numeric_descriptor_private.issue_claim ~base_int_claim:(base_claim ())
         ~provider_unit_claim:"provider-unit:claim"
         ~provider_origin_claim:"provider-origin:claim"
         ~provider_interface_receipt_claim:"provider-interface:claim"
         ~direct_import_provenance_claim:"direct-import:claim"
         ~carrier_claim:(carrier_claim ()) ~profile_claim ~target_claim
         ~binding_claims:[ binding_claim () ] ~issuer_claim:"issuer:claim")
  in
  (profile_claim, value)

let strict_canonical_grammar () =
  let encoded =
    Numeric_receipt_private.encode ~schema:"strict.v1" [ "a"; "bc" ]
  in
  require
    (ok "decode"
       (Numeric_receipt_private.decode ~schema:"strict.v1" ~field_count:2
          encoded)
    = [ "a"; "bc" ])
    "canonical record did not round trip";
  require
    (rejected
       (Numeric_receipt_private.decode ~schema:"strict.v1" ~field_count:(-1)
          encoded))
    "negative field count was admitted";
  require
    (rejected
       (Numeric_receipt_private.decode ~schema:"strict.v1" ~field_count:100_001
          encoded))
    "excessive field count was admitted";
  require
    (rejected
       (Numeric_receipt_private.decode ~schema:"strict.v1" ~field_count:2
          ("09:strict.v1" ^ "1:a2:bc")))
    "noncanonical length was admitted";
  let repeated = [ "same"; "same"; "different" ] in
  require
    (ok "list decode"
       (Numeric_receipt_private.decode_list
          (Numeric_receipt_private.list repeated))
    = repeated)
    "ordered repeated vector was changed"

let profile_claim_round_trip () =
  let layout32, layout64, profile = profile_claim () in
  let target32 =
    ok "target32"
      (Target_profile_private.instantiate_claim profile ~layout:layout32)
  and target64 =
    ok "target64"
      (Target_profile_private.instantiate_claim profile ~layout:layout64)
  in
  require
    (not (String.equal target32.full_key target64.full_key))
    "complete target keys collapsed by shared class spelling";
  let decoded =
    ok "profile decode"
      (Target_profile_private.decode_claim profile.full_key)
  in
  require (Target_profile_private.equal_claim profile decoded)
    "profile claim full key did not round trip";
  require
    (rejected
       (Target_profile_private.issue_claim ~compiler_abi:"compiler"
          ~toolchain_abi:"toolchain" ~target_identity:"target"
          ~layouts:[ layout32 ] ~supported_widths:[ 32; 64 ]
          ~logical_bv_width_permission:(logical_bv_permission "issuer")
          ~issuer_claim:"issuer"))
    "incomplete supported-width layout facts were admitted"

let build_target_profile_authority () =
  let contexts_before = (Z3_bridge.counters ()).contexts_created in
  let capability = Build_target_profile_private.capability () in
  let profile =
    ok "authenticated build target profile"
      (Build_target_profile_private.authenticate_profile capability)
  and instances =
    ok "authenticated build target instances"
      (Build_target_profile_private.authenticate_instances capability)
  in
  let repeated =
    ok "repeated authenticated build target profile"
      (Build_target_profile_private.authenticate_profile capability)
  in
  require
    (Build_target_profile_private.equal_profile profile repeated)
    "build-produced profile identity changed across authentication";
  require
    (profile.profile_claim.supported_widths = [ 32; 64 ]
    && List.length profile.profile_claim.layouts = 2
    && List.length instances = 2)
    "build-produced profile did not seal exact 32/64 coverage";
  let widths =
    List.map
      (fun (instance : Build_target_profile_private.instance) ->
        instance.target_claim.width)
      instances
    |> List.sort Int.compare
  and layout_classes =
    List.map
      (fun (instance : Build_target_profile_private.instance) ->
        instance.target_claim.layout_class)
      instances
  in
  require (widths = [ 32; 64 ])
    "build-produced instances did not expand deterministically by width";
  require
    (match layout_classes with
    | [ left; right ] -> String.equal left right
    | [] | [ _ ] | _ :: _ :: _ -> false)
    "equal layout classes with different complete layout identities were collapsed";
  require
    (List.map
       (fun (instance : Build_target_profile_private.instance) ->
         instance.full_key)
       instances
     = List.sort String.compare
         (List.map
            (fun (instance : Build_target_profile_private.instance) ->
              instance.full_key)
            instances))
    "build-produced target instances were not ordered by complete full key";
  require
    (List.for_all
       (fun (instance : Build_target_profile_private.instance) ->
         Build_target_profile_private.equal_instance instance
           (ok "authenticated serialized build target instance"
              (Build_target_profile_private.decode_instance capability
                 instance.full_key)))
       instances)
    "build-produced target instance did not authenticate by exact full key";
  require
    (Build_target_profile_private.equal_profile profile
       (ok "authenticated serialized build target profile"
          (Build_target_profile_private.decode_profile capability
             profile.full_key)))
    "build-produced target profile did not authenticate by exact full key";
  require
    (rejected
       (Build_target_profile_private.decode_profile capability
          (profile.full_key ^ "0:"))
    && rejected
         (Build_target_profile_private.decode_instance capability
            ((List.hd instances).full_key ^ "0:")))
    "substituted build target profile or instance was accepted";
  let layout32, layout64 = layouts () in
  let layout16 =
    ok "general 16-bit target layout"
      (Target_profile_private.layout ~layout_class:"other-layout"
         ~layout_abi:"other-layout-16" ~width:16 ~signed:false)
  in
  require
    (Result.is_ok
       (Target_profile_private.issue_claim ~compiler_abi:"other-compiler"
          ~toolchain_abi:"other-toolchain" ~target_identity:"other-target"
          ~layouts:[ layout16 ] ~supported_widths:[ 16 ]
          ~logical_bv_width_permission:
            (logical_bv_permission "other-build-claim")
          ~issuer_claim:"other-build-claim"))
    "general target-profile claim layer imposed the current build's 32/64 matrix";
  let source_forged_profile =
    ok "source-forged pure profile claim"
      (Target_profile_private.issue_claim ~compiler_abi:"source-compiler"
         ~toolchain_abi:"source-toolchain" ~target_identity:"source-target"
         ~layouts:[ layout32; layout64 ] ~supported_widths:[ 32; 64 ]
         ~logical_bv_width_permission:(logical_bv_permission "source-issuer")
         ~issuer_claim:"source-issuer")
  in
  require
    (rejected
       (Build_target_profile_private.decode_profile capability
          source_forged_profile.full_key))
    "a pure source profile claim altered build-produced authority";
  require
    (rejected
       (Target_profile_private.layout ~layout_class:"invalid"
          ~layout_abi:"invalid" ~width:0 ~signed:true)
    && rejected
         (Target_profile_private.issue_claim ~compiler_abi:"compiler"
            ~toolchain_abi:"toolchain" ~target_identity:"target"
            ~layouts:[ layout32; layout32 ] ~supported_widths:[ 32 ]
            ~logical_bv_width_permission:(logical_bv_permission "issuer")
            ~issuer_claim:"issuer")
    && rejected
         (Target_profile_private.issue_claim ~compiler_abi:"compiler"
            ~toolchain_abi:"toolchain" ~target_identity:"target"
            ~layouts:[ layout32 ] ~supported_widths:[ 32; 64 ]
            ~logical_bv_width_permission:(logical_bv_permission "issuer")
            ~issuer_claim:"issuer"))
    "nonpositive, duplicate, or incomplete target layout facts were accepted";
  require
    ((Z3_bridge.counters ()).contexts_created = contexts_before)
    "build target profile validation created a solver context"

let bv_width_handoff_boundaries () =
  let contexts_before = (Z3_bridge.counters ()).contexts_created in
  let profile_capability = Build_target_profile_private.capability () in
  let profile =
    ok "sealed logical BV profile"
      (Build_target_profile_private.authenticate_profile profile_capability)
  and targets =
    ok "sealed target instances"
      (Build_target_profile_private.authenticate_instances profile_capability)
  in
  let backend_capability =
    Bv_backend_capability_receipt_private.capability ()
  in
  let receipt, outer_binding =
    ok "sealed BV backend capability"
      (Bv_backend_capability_receipt_private.authenticate backend_capability)
  in
  require
    (receipt.schema = "verocaml.bv-backend-capability.v1"
    && receipt.maximum_bv_width = 4096
    && receipt.c_unsigned_maximum >= Z.of_int 4096
    && String.length
         (Bv_backend_capability_receipt_private.id_to_hex receipt.checked_id)
       = 64
    && Option.is_none outer_binding)
    "sealed backend capability lost its bounded ABI facts or invented an artifact binding";
  let reference =
    ok "BV capability reference"
      (Bv_backend_capability_receipt_private.reference backend_capability)
  in
  let encoded_reference =
    Bv_backend_capability_receipt_private.encode_reference reference
  in
  let mutate index =
    let bytes = Bytes.of_string encoded_reference in
    Bytes.set bytes index
      (if Char.equal (Bytes.get bytes index) '\000' then '\001' else '\000');
    Bytes.unsafe_to_string bytes
  in
  let key_offset = String.length "VBVREF01" in
  let id_offset = key_offset + String.length receipt.full_key in
  require
    (String.length encoded_reference <= 512
    && Result.is_ok
         (Bv_backend_capability_receipt_private.decode_reference
            backend_capability encoded_reference)
    && rejected
         (Bv_backend_capability_receipt_private.decode_reference
            backend_capability
            (String.sub encoded_reference 0
               (String.length encoded_reference - 1)))
    && rejected
         (Bv_backend_capability_receipt_private.decode_reference
            backend_capability (mutate 0))
    && rejected
         (Bv_backend_capability_receipt_private.decode_reference
            backend_capability (mutate key_offset))
    && rejected
         (Bv_backend_capability_receipt_private.decode_reference
            backend_capability (mutate id_offset))
    && rejected
         (Bv_backend_capability_receipt_private.decode_reference
            backend_capability (mutate (String.length encoded_reference - 1)))
    && rejected
         (Bv_backend_capability_receipt_private.decode_reference
            backend_capability (String.make 513 'x')))
    "BV capability transport did not enforce exact local full-key agreement and its byte ceiling";
  let widths =
    List.map
      (fun width ->
        ok ("logical BV width " ^ width)
          (Bv_width.of_string ~profile backend_capability width))
      [ "1"; "8"; "128"; "4096" ]
  in
  require
    (List.map Bv_width.to_int widths = [ 1; 8; 128; 4096 ]
    && List.length targets = 2
    && List.for_all
         (fun target ->
           List.for_all
             (fun width ->
               match Bv_width.for_instance backend_capability target width with
               | Ok bound ->
                   Bv_width.target_full_key bound = Some target.full_key
                   && Bv_width.to_int bound = Bv_width.to_int width
               | Error _ -> false)
             widths)
         targets)
    "logical BV permissions were conflated with the two physical target widths";
  require
    (rejected (Bv_width.of_string ~profile backend_capability "0")
    && rejected (Bv_width.of_string ~profile backend_capability "4097")
    && rejected (Bv_width.of_string ~profile backend_capability "0001")
    && rejected (Bv_width.of_string ~profile backend_capability "12345")
    && rejected
         (Bv_width.of_z ~profile backend_capability (Z.of_int (-1))))
    "invalid or oversized native BV width crossed bounded preflight";
  let width4096 = List.nth widths 3 in
  let largest_decimal = "1" ^ String.make 1233 '0' in
  let largest =
    ok "1234-byte BV residue"
      (Bv_value.of_string ~width:width4096 largest_decimal)
  in
  require
    (String.length (Bv_value.canonical_decimal largest) = 1234
    && rejected
         (Bv_value.of_string ~width:width4096
            ("1" ^ String.make 1234 '0')))
    "BV residue parser did not enforce the 1234-byte boundary before bigint parsing";
  let limit = Z.shift_left Z.one 4096 in
  let limit_minus_one = Z.pred limit in
  let maximum_z =
    ok "maximum width-4096 BV value from Z"
      (Bv_value.of_z ~width:width4096 limit_minus_one)
  and maximum_text =
    ok "maximum width-4096 BV value from canonical decimal"
      (Bv_value.of_string ~width:width4096 (Z.to_string limit_minus_one))
  in
  require
    (Bv_value.equal maximum_z maximum_text
    && Z.equal maximum_z.unsigned_bits limit_minus_one
    && String.equal (Bv_value.canonical_decimal maximum_text)
         (Z.to_string limit_minus_one)
    && rejected (Bv_value.of_z ~width:width4096 limit)
    && rejected
         (Bv_value.of_string ~width:width4096 (Z.to_string limit)))
    "width-4096 BV value admission did not enforce the exact 2^4096 boundary";
  let mathematical_4097 = Z.shift_left Z.one 4097 in
  require (Z.numbits mathematical_4097 = 4098)
    "ordinary mathematical Int computation was constrained by native BV width";
  require
    ((Z3_bridge.counters ()).contexts_created = contexts_before)
    "BV width/profile preflight created a solver context"

let descriptor_claim_round_trip () =
  let profile, claim = descriptor_claim () in
  let decoded =
    ok "descriptor decode"
      (Numeric_descriptor_private.decode_claim
         ~expected_profile_claim:profile
         ~expected_base_int_claim:claim.base_int_claim claim.full_key)
  in
  require (Numeric_descriptor_private.equal_claim claim decoded)
    "descriptor claim full key did not round trip";
  require
    (rejected
       (Numeric_descriptor_private.decode_claim
          ~expected_profile_claim:profile
          ~expected_base_int_claim:claim.base_int_claim
          (claim.full_key ^ "0:")))
    "noncanonical descriptor claim was admitted";
  let _, layout64, profile = profile_claim () in
  let target =
    ok "target"
      (Target_profile_private.instantiate_claim profile ~layout:layout64)
  in
  require
    (rejected
       (Numeric_descriptor_private.issue_claim ~base_int_claim:(base_claim ())
          ~provider_unit_claim:"provider" ~provider_origin_claim:"origin"
          ~provider_interface_receipt_claim:"interface"
          ~direct_import_provenance_claim:"import"
          ~carrier_claim:
            (carrier_claim ~representation:Numeric_descriptor_private.Unboxed ())
          ~profile_claim:profile ~target_claim:target
          ~binding_claims:[ binding_claim () ] ~issuer_claim:"issuer"))
    "unsupported representation claim was admitted"

let retained_unknown_optional_round_trip () =
  let sort = logical_sort () in
  let owner_cmi_full_key unit_name =
    Numeric_receipt_private.encode
      ~schema:"verocaml.numeric-owner-cmi-full-key.v1"
      [ "compiler-magic"; unit_name; "self-crc"; "content-receipt";
        Numeric_receipt_private.list [] ]
  in
  let carrier_claim unit_name uid =
    let owner_cmi_full_key = owner_cmi_full_key unit_name in
    let owner =
      ok "numeric owner claim"
        (Numeric_interface_claim_private.owner ~owner_unit:unit_name
           ~owner_cmi_full_key
           ~owner_cmi_checked_digest:
             (Numeric_receipt_private.digest
                ~domain:"verocaml.numeric-owner-cmi-full-key.v1"
                owner_cmi_full_key)
           ~import_routes:
             [ Numeric_receipt_private.list [ owner_cmi_full_key ] ])
    in
    ok "numeric carrier reconstruction claim"
      (Numeric_interface_claim_private.carrier
         ~source_claim:
           (Numeric_receipt_private.encode
              ~schema:"verocaml.numeric-carrier-source-claim.v1"
              [ "profile"; "boxed";
                Numeric_receipt_private.list [ "layout" ] ])
         ~carrier_path:(unit_name ^ ".carrier") ~carrier_uid:uid ~owner
         ~constructor_abi:"constructor-abi" ~binder_abi:"binder-abi"
         ~compiler_jkind_abi:"compiler-jkind-abi"
         ~compiler_representation:"boxed")
  in
  let first_carrier = carrier_claim "Provider" "carrier-uid"
  and second_carrier = carrier_claim "Other" "other-carrier-uid" in
  let open Retained_interface_authority_private in
  let value =
    { provider_unit = "Provider"; provider_origin = "Provider:origin";
      compiler_abi = "compiler:abi"; cmi_receipt = "cmi:receipt";
      cmi_self_crc = "cmi:crc"; cmi_imports = [];
      cmi_identity = "cmi:identity"; cmti_receipt = "cmti:receipt";
      cmti_interface_digest = Some "cmti:digest"; cmti_imports = [];
      cmti_identity = "cmti:identity"; ordinary_cmi_receipts = [];
      dependencies = [];
      payloads =
        [ Broadcast_witnesses []; Logical_sorts [ sort ]; Logical_values [];
          Numeric_claims
            { carrier_reconstruction_claims =
                [ second_carrier.transport_key; first_carrier.transport_key ];
              role_reconstruction_claims = [] };
          Unknown_optional_section
            { section_name = "numeric-extension"; section_version = "99";
              section_payload = "opaque-future-record" } ] }
  in
  let encoded = encode value in
  let decoded = ok "retained decode" (decode encoded) in
  require (equal value decoded)
    "unknown optional retained section did not round trip";
  require (List.length (logical_sorts decoded) = 1)
    "unknown numeric extension disabled the base logical sort";
  require (List.length (numeric_claims decoded) = 1)
    "known optional numeric claims were dropped";
  let reversed =
    { value with
      payloads =
        List.map
          (function
            | Numeric_claims claims ->
                Numeric_claims
                  { carrier_reconstruction_claims =
                      List.rev claims.carrier_reconstruction_claims;
                    role_reconstruction_claims =
                      List.rev claims.role_reconstruction_claims }
            | payload -> payload)
          value.payloads }
  in
  require (String.equal encoded (encode reversed))
    "numeric claims were not ordered by complete canonical bytes"

let parse_interface source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "numeric_source_claim.mli";
  Parse.interface lexbuf

let retained_interface source =
  let mapper = Vero_ppx_rewriter.make [ "--keep-ghost" ] in
  mapper.Ast_mapper.signature mapper (parse_interface source)

let ordinary_interface source =
  let mapper = Vero_ppx_rewriter.make [] in
  mapper.Ast_mapper.signature mapper (parse_interface source)

let rewriter_rejects source =
  try
    ignore (retained_interface source);
    false
  with Location.Error _ -> true

let typed_source_claim_surface () =
  let source =
    {|type 'a measure
[@@verocaml.numeric_carrier
  { profile = "portable-profile";
    representation = "boxed";
    compatibility = ["word-layout"] }]

val relation : 'a measure -> int
[@@verocaml.spec]

val operation : 'a measure -> int
[@@verocaml.spec]
[@@verocaml.numeric_role
  { carrier = measure;
    role_schema = "numeric-role.v1";
    role = "view";
    semantics = relation;
    visibility = "opaque";
    reveal = false;
    inline = false }]
|}
  in
  let retained = retained_interface source in
  let carrier_claims, role_claims =
    List.fold_left
      (fun (carriers, roles) item ->
        match item.Parsetree.psig_desc with
        | Psig_type (_, declarations) ->
            let claims =
              declarations
              |> List.concat_map (fun (declaration : Parsetree.type_declaration) ->
                     List.filter
                       (fun attribute ->
                         String.equal attribute.Parsetree.attr_name.txt
                           Numeric_source_claim_private.carrier_marker)
                       declaration.ptype_attributes)
            in
            (claims @ carriers, roles)
        | Psig_value value ->
            let claims =
              List.filter
                (fun attribute ->
                  String.equal attribute.Parsetree.attr_name.txt
                    Numeric_source_claim_private.role_marker)
                value.pval_attributes
            in
            (carriers, claims @ roles)
        | _ -> (carriers, roles))
      ([], []) retained.Parsetree.psg_items
  in
  let carrier =
    match carrier_claims with
    | [ claim ] -> ok "carrier source claim" (Numeric_source_claim_private.parse_carrier claim)
    | _ -> failwith "retained PPX did not issue one carrier source claim"
  and role =
    match role_claims with
    | [ claim ] -> ok "role source claim" (Numeric_source_claim_private.parse_role claim)
    | _ -> failwith "retained PPX did not issue one role source claim"
  in
  require (carrier.profile_reference = "portable-profile")
    "carrier profile compatibility request changed";
  require (role.carrier_path = "measure")
    "carrier declaration path changed";
  require (role.semantics_path = "relation")
    "semantic declaration path was not retained as a path";
  require
    (rewriter_rejects
       {|val wrong : int
[@@verocaml.numeric_carrier
  { profile = "p"; representation = "boxed"; compatibility = [] }]
|})
    "numeric carrier was accepted on a callable";
  require
    (rewriter_rejects
       {|type wrong = int
[@@verocaml.numeric_role
  { carrier = wrong; role_schema = "v1"; role = "view";
    semantics = wrong; visibility = "opaque"; reveal = false; inline = false }]
|})
    "numeric role was accepted on a type";
  require
    (rewriter_rejects
       {|val wrong : int -> int
[@@verocaml.spec]
[@@verocaml.numeric_role
  { carrier = wrong; role_schema = "v1"; role = "view";
    semantics = "arbitrary semantic text";
    visibility = "opaque"; reveal = false; inline = false }]
|})
    "numeric semantic authority accepted arbitrary text"
  ;
  let ordinary = ordinary_interface source in
  let has_numeric_attribute =
    List.exists
      (fun item ->
        let attributes =
          match item.Parsetree.psig_desc with
          | Psig_type (_, declarations) ->
              List.concat_map
                (fun (declaration : Parsetree.type_declaration) ->
                  declaration.ptype_attributes)
                declarations
          | Psig_value value -> value.pval_attributes
          | _ -> []
        in
        List.exists
          (fun attribute ->
            String.starts_with ~prefix:"verocaml.numeric"
              attribute.Parsetree.attr_name.txt
            || String.equal attribute.attr_name.txt
                 Numeric_source_claim_private.carrier_marker
            || String.equal attribute.attr_name.txt
                 Numeric_source_claim_private.role_marker)
          attributes)
      ordinary.Parsetree.psg_items
  in
  require (not has_numeric_attribute)
    "ordinary PPX retained numeric source or internal claim markers";
  require
    (rewriter_rejects
       {|type forged
[@@verocaml.internal.numeric.carrier_source_claim.v1
  { profile = "p"; representation = "boxed"; compatibility = [] }]
|})
    "source-authored internal numeric marker was accepted"

let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid

let compiler_reconstructed_claims ~workspace =
  let artifact_directory = Filename.dirname Sys.executable_name in
  let artifact extension =
    Filename.concat artifact_directory ("numeric-source-provider" ^ extension)
  in
  let cmt = artifact ".cmt"
  and cmi = artifact ".cmi"
  and cmti = artifact ".cmti"
  and vri = Filename.concat workspace "numeric_source_provider.vri" in
  let local_artifact extension =
    Filename.concat workspace ("numeric_source_provider" ^ extension)
  in
  copy_file cmt (local_artifact ".cmt");
  copy_file cmi (local_artifact ".cmi");
  copy_file cmti (local_artifact ".cmti");
  let cmt = local_artifact ".cmt"
  and cmi = local_artifact ".cmi"
  and cmti = local_artifact ".cmti" in
  (match
     Cmt_input.emit_retained_interface_authority ~cmt ~cmi ~cmti ~output:vri
       ~artifact_directories:[ artifact_directory ] ()
   with
  | Ok () -> ()
  | Error diagnostic ->
      failwith
        ("retained numeric source fixture emission failed: "
       ^ diagnostic.Diagnostic.code));
  let retained_claims =
    match
      Retained_interface_authority_private.decode (read_file vri)
    with
    | Ok authority -> (
        match Retained_interface_authority_private.numeric_claims authority with
        | [ claims ] -> claims
        | _ ->
            failwith
              "provider VRI did not carry one numeric reconstruction inventory")
    | Error reason ->
        failwith ("provider VRI did not decode: " ^ reason)
  in
  let retained_carriers =
    List.map
      (fun encoded ->
        ok "retained carrier reconstruction"
          (Numeric_interface_claim_private.decode_carrier encoded))
      retained_claims.carrier_reconstruction_claims
  and retained_roles =
    List.map
      (fun encoded ->
        ok "retained role reconstruction"
          (Numeric_interface_claim_private.decode_role encoded))
      retained_claims.role_reconstruction_claims
  in
  let retained_carrier =
    List.find
      (fun claim ->
        claim.Numeric_interface_claim_private.carrier_path
        = "Numeric_source_provider.measure")
      retained_carriers
  and retained_nested_carrier =
    List.find
      (fun claim ->
        claim.Numeric_interface_claim_private.carrier_path
        = "Numeric_source_provider.Nested.carrier")
      retained_carriers
  and retained_role =
    List.find
      (fun claim ->
        claim.Numeric_interface_claim_private.callable_path
        = "Numeric_source_provider.operation")
      retained_roles
  in
  let implementation =
    match
      Cmt_input.load_with_interface ~cmt ~cmi ~cmti ~vri
        ~artifact_directories:[ artifact_directory ] ()
    with
    | Ok implementation -> implementation
    | Error diagnostic ->
        failwith
          ("retained numeric source fixture load failed: "
         ^ diagnostic.Diagnostic.code)
  in
  let carrier =
    List.find
      (fun claim ->
        claim.Cmt_input.numeric_carrier_path = "Numeric_source_provider.measure")
      implementation.Cmt_input.interface_numeric_claims.numeric_carriers
  and role =
    List.find
      (fun claim ->
        claim.Cmt_input.numeric_role_callable_path
        = "Numeric_source_provider.operation")
      implementation.Cmt_input.interface_numeric_claims.numeric_roles
  in
  let signature =
    Cmi_format.read_cmi_lazy cmi
    |> fun information ->
    Subst.Lazy.force_signature information.Cmi_format.cmi_sign
  in
  let type_uid, operation_uid, relation_uid =
    List.fold_left
      (fun (type_uid, operation_uid, relation_uid) -> function
        | Types.Sig_type (ident, declaration, _, Types.Exported)
          when String.equal (Ident.name ident) "measure" ->
            (Some (compiler_uid declaration.Types.type_uid), operation_uid,
             relation_uid)
        | Types.Sig_value (ident, description, Types.Exported)
          when String.equal (Ident.name ident) "operation" ->
            (type_uid, Some (compiler_uid description.Types.val_uid), relation_uid)
        | Types.Sig_value (ident, description, Types.Exported)
          when String.equal (Ident.name ident) "relation" ->
            (type_uid, operation_uid,
             Some (compiler_uid description.Types.val_uid))
        | _ -> (type_uid, operation_uid, relation_uid))
      (None, None, None) signature
  in
  require
    (Some carrier.numeric_carrier_uid = type_uid
    && Some role.numeric_role_callable_uid = operation_uid
    && Some role.numeric_role_semantics_uid = relation_uid)
    "numeric source claims did not bind exact compiler declaration UIDs";
  require
    (role.numeric_role_semantics_path
     = "Numeric_source_provider.relation"
    && role.numeric_role_source.semantics_path = "relation")
    "semantic declaration provenance and compiler identity were conflated";
  require
    (List.length retained_carriers = 2
    && List.length retained_roles = 2
    && List.exists
         (fun claim ->
           claim.Numeric_interface_claim_private.carrier_path
           = "Numeric_source_provider.Nested.carrier")
         retained_carriers
    && List.exists
         (fun claim ->
           claim.Numeric_interface_claim_private.callable_path
           = "Numeric_source_provider.Nested.operation"
           && claim.semantics_path = "Numeric_source_provider.Nested.law")
         retained_roles)
    "nested numeric declaration paths were not reconstructed exactly";
  require
    (retained_carrier.owner.owner_cmi_full_key
       = carrier.numeric_carrier_owner_cmi_full_key
    && retained_carrier.owner.owner_cmi_checked_digest
       = carrier.numeric_carrier_owner_cmi_checked_digest
    && retained_role.callable_owner.owner_cmi_full_key
       = role.numeric_role_callable_owner_cmi_full_key
    && retained_role.semantics_owner.owner_cmi_full_key
       = role.numeric_role_semantics_owner_cmi_full_key
    && retained_role.carrier_owner.owner_cmi_full_key
       = role.numeric_role_carrier_owner_cmi_full_key
    && retained_role.callable_mode_abi
       = role.numeric_role_callable_mode_abi
    && retained_role.semantics_mode_abi
       = role.numeric_role_semantics_mode_abi)
    "VRI did not preserve exact owner full keys, checked digests, routes, and structured ABIs";
  require
    (role.numeric_role_callable_uid <> role.numeric_role_semantics_uid
    && carrier.numeric_carrier_constructor_abi <> ""
    && carrier.numeric_carrier_binder_abi <> ""
    && role.numeric_role_callable_abi <> ""
    && role.numeric_role_semantics_abi <> "")
    "compiler reconstruction omitted a complete identity or typed ABI";
  let contexts_before_artifact_binding =
    (Z3_bridge.counters ()).contexts_created
  in
  require
    (rejected
       (Numeric_artifact_binding_private.correlate implementation
          retained_carrier))
    "a boxed source request over an immediate compiler Jkind was promoted";
  let artifact_binding =
    ok "numeric artifact correlation"
      (Numeric_artifact_binding_private.correlate implementation
         retained_nested_carrier)
  in
  let artifact_facts =
    ok "numeric artifact correlation facts"
      (Numeric_artifact_binding_private.facts artifact_binding)
  and repeated_artifact_facts =
    ok "repeated numeric artifact correlation facts"
      (Numeric_artifact_binding_private.facts artifact_binding)
  in
  require
    (artifact_facts.representation = Cmt_input.Artifact_immediate
    && artifact_facts.binding_full_key
       = repeated_artifact_facts.binding_full_key
    && artifact_facts.provider_artifact_full_key <> ""
    && implementation.raw_artifact_receipt <> "")
    "numeric artifact correlation omitted compiler-derived or stable artifact facts";
  let forged_carrier =
    ok "forged detached numeric carrier claim"
      (Numeric_interface_claim_private.carrier
         ~source_claim:retained_nested_carrier.source_claim
         ~carrier_path:(retained_nested_carrier.carrier_path ^ ".substituted")
         ~carrier_uid:retained_nested_carrier.carrier_uid
         ~owner:retained_nested_carrier.owner
         ~constructor_abi:retained_nested_carrier.constructor_abi
         ~binder_abi:retained_nested_carrier.binder_abi
         ~compiler_jkind_abi:retained_nested_carrier.compiler_jkind_abi
         ~compiler_representation:
           retained_nested_carrier.compiler_representation)
  in
  require
    (rejected
       (Numeric_artifact_binding_private.correlate implementation
          forged_carrier))
    "a detached numeric carrier claim minted artifact authority";
  require
    ((Z3_bridge.counters ()).contexts_created
    = contexts_before_artifact_binding)
    "numeric artifact correlation created a solver context";
  let forged_role =
    ok "forged retained role claim"
      (Numeric_interface_claim_private.role
         ~source_claim:retained_role.source_claim
         ~callable_path:retained_role.callable_path
         ~callable_uid:retained_role.callable_uid
         ~callable_type_abi:retained_role.callable_type_abi
         ~callable_mode_abi:retained_role.callable_mode_abi
         ~callable_owner:retained_role.callable_owner
         ~semantics_path:retained_role.semantics_path
         ~semantics_uid:(retained_role.semantics_uid ^ "-substituted")
         ~semantics_type_abi:retained_role.semantics_type_abi
         ~semantics_mode_abi:retained_role.semantics_mode_abi
         ~semantics_owner:retained_role.semantics_owner
         ~carrier_uid:retained_role.carrier_uid
         ~carrier_owner:retained_role.carrier_owner)
  in
  let forged_authority =
    match Retained_interface_authority_private.decode (read_file vri) with
    | Error reason -> failwith ("retained authority reload failed: " ^ reason)
    | Ok authority ->
        { authority with
          payloads =
            List.map
              (function
                | Retained_interface_authority_private.Numeric_claims claims ->
                    Retained_interface_authority_private.Numeric_claims
                      { claims with
                        role_reconstruction_claims =
                          forged_role.transport_key
                          :: List.filter
                               (fun encoded ->
                                 not
                                   (String.equal encoded
                                      retained_role.transport_key))
                               claims.role_reconstruction_claims }
                | payload -> payload)
              authority.payloads }
  in
  let forged_vri = Filename.concat workspace "forged-numeric-provider.vri" in
  write_file forged_vri
    (Retained_interface_authority_private.encode forged_authority);
  (match
     Cmt_input.load_with_interface ~cmt ~cmi ~cmti ~vri:forged_vri
       ~artifact_directories:[ workspace; artifact_directory ] ()
   with
  | Error
      { Diagnostic.classification = Invalid_broadcast_dependency _;
        _ } ->
      ()
  | Error diagnostic ->
      failwith
        ("substituted numeric VRI used the wrong rejection: "
       ^ diagnostic.Diagnostic.code)
  | Ok _ -> failwith "substituted numeric VRI claim was accepted");
  let extension_fixture extension =
    Filename.concat artifact_directory
      ("numeric-extension-provider" ^ extension)
  and extension_artifact extension =
    Filename.concat workspace ("numeric_extension_provider" ^ extension)
  in
  List.iter
    (fun extension ->
      copy_file (extension_fixture extension) (extension_artifact extension))
    [ ".cmt"; ".cmi"; ".cmti" ];
  let extension_vri =
    Filename.concat workspace "numeric_extension_provider.vri"
  in
  (match
     Cmt_input.emit_retained_interface_authority
       ~cmt:(extension_artifact ".cmt")
       ~cmi:(extension_artifact ".cmi")
       ~cmti:(extension_artifact ".cmti") ~output:extension_vri
       ~artifact_directories:[ workspace; artifact_directory ] ()
   with
  | Ok () -> ()
  | Error diagnostic ->
      failwith
        ("imported numeric extension VRI emission failed: "
       ^ diagnostic.Diagnostic.code));
  let extension =
    match
      Cmt_input.load_with_interface
        ~cmt:(extension_artifact ".cmt")
        ~cmi:(extension_artifact ".cmi")
        ~cmti:(extension_artifact ".cmti") ~vri:extension_vri
        ~artifact_directories:[ workspace; artifact_directory ] ()
    with
    | Ok implementation -> implementation
    | Error diagnostic ->
        failwith
          ("imported numeric extension VRI load failed: "
         ^ diagnostic.Diagnostic.code)
  in
  let imported_role =
    match extension.interface_numeric_claims with
    | { numeric_carriers = []; numeric_roles = [ role ]; _ } -> role
    | _ ->
        failwith
          "imported extension did not reconstruct one role without minting a carrier"
  in
  let route_lengths routes =
    List.map
      (fun route ->
        ok "imported numeric route"
          (Numeric_receipt_private.decode_list route)
        |> List.length)
      routes
  in
  require
    (imported_role.numeric_role_callable_owner_unit
       = "Numeric_extension_provider"
    && imported_role.numeric_role_carrier_owner_unit
       = "Numeric_source_provider"
    && imported_role.numeric_role_semantics_owner_unit
       = "Numeric_source_provider"
    && List.exists (( <= ) 2)
         (route_lengths imported_role.numeric_role_carrier_import_routes)
    && List.exists (( <= ) 2)
         (route_lengths imported_role.numeric_role_semantics_import_routes))
    "imported carrier/semantics owner or transitive route was not preserved"

let compiler_reconstructed_claim_case =
  let name = "compiler-reconstructed-source-claims" in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ name)
           (Outcome.Function_exists name))
    (fun ~environment:_ ~workspace ->
      try
        compiler_reconstructed_claims ~workspace;
        Ok
          (Outcome.observation ~status:Outcome.Verified
             ~units:[ (name, Outcome.Unit_verified) ]
             ~named_facts:
               [ ("function:" ^ name, Outcome.Function_exists name) ]
             ()
          |> Outcome.project)
      with Failure message ->
        Error (Failure.make Failure.Runner_internal message))

let nonsemantic_reference_rejection () =
  let artifact_directory = Filename.dirname Sys.executable_name in
  let artifact extension =
    Filename.concat artifact_directory
      ("numeric-bad-semantics-provider" ^ extension)
  in
  match
    Cmt_input.load_with_interface ~cmt:(artifact ".cmt")
      ~cmi:(artifact ".cmi") ~cmti:(artifact ".cmti")
      ~artifact_directories:[ artifact_directory ] ()
  with
  | Error
      { Diagnostic.classification = Invalid_numeric_declaration _;
        _ } ->
      ()
  | Error diagnostic ->
      failwith
        ("nonsemantic numeric relation used the wrong rejection: "
       ^ diagnostic.Diagnostic.code)
  | Ok _ ->
      failwith "nonsemantic numeric relation was accepted as semantic authority"

let fixture_artifact basename extension =
  Filename.concat (Filename.dirname Sys.executable_name) (basename ^ extension)

let emit_and_load_numeric_fixture ~workspace basename =
  let cmt = fixture_artifact basename ".cmt"
  and cmi = fixture_artifact basename ".cmi"
  and cmti = fixture_artifact basename ".cmti" in
  let vri = Filename.concat workspace (basename ^ ".vri") in
  let artifact_directories =
    [ workspace; Filename.dirname Sys.executable_name ]
  in
  (match
     Cmt_input.emit_retained_interface_authority ~cmt ~cmi ~cmti ~output:vri
       ~artifact_directories ()
   with
  | Ok () -> ()
  | Error diagnostic ->
      failwith (basename ^ " VRI emission failed: " ^ diagnostic.Diagnostic.code));
  let implementation =
    match
      Cmt_input.load_with_interface ~cmt ~cmi ~cmti ~vri
        ~artifact_directories ()
    with
    | Ok implementation -> implementation
    | Error diagnostic ->
        failwith (basename ^ " VRI load failed: " ^ diagnostic.Diagnostic.code)
  in
  (implementation, vri)

let install_numeric_authority ~workspace ~unit_basename source =
  copy_file source (Filename.concat workspace (unit_basename ^ ".vri"))

let typed_reference_resolution ~workspace =
  let mli_only, _ =
    emit_and_load_numeric_fixture ~workspace "numeric-mli-only-provider"
  in
  require
    (List.length mli_only.interface_numeric_claims.numeric_carriers = 1
    && List.length mli_only.interface_numeric_claims.numeric_roles = 1)
    "an mli-only numeric role did not obtain a typed CMTI witness";
  let _, source_vri =
    emit_and_load_numeric_fixture ~workspace "numeric-source-provider"
  in
  install_numeric_authority ~workspace
    ~unit_basename:"numeric_source_provider" source_vri;
  let _, reexport_vri =
    emit_and_load_numeric_fixture ~workspace "numeric-direct-reexport"
  in
  let claim_identities filename =
    let authority =
      ok "reexport retained authority decode"
        (Retained_interface_authority_private.decode (read_file filename))
    in
    let claims =
      match Retained_interface_authority_private.numeric_claims authority with
      | [ claims ] -> claims
      | [] | _ :: _ :: _ ->
          failwith "reexport retained authority has ambiguous numeric inventory"
    in
    let carriers =
      List.map
        (fun encoded ->
          (ok "reexport carrier transport"
             (Numeric_interface_claim_private.decode_carrier encoded))
            .claim_key)
        claims.carrier_reconstruction_claims
    and roles =
      List.map
        (fun encoded ->
          (ok "reexport role transport"
             (Numeric_interface_claim_private.decode_role encoded))
            .claim_key)
        claims.role_reconstruction_claims
    in
    (List.sort String.compare carriers, List.sort String.compare roles)
  in
  require
    (claim_identities source_vri = claim_identities reexport_vri)
    "direct reexport changed original numeric carrier or role full identity";
  let _, extension_vri =
    emit_and_load_numeric_fixture ~workspace "numeric-extension-provider"
  in
  install_numeric_authority ~workspace
    ~unit_basename:"numeric_extension_provider" extension_vri;
  let _, external_reexport_vri =
    emit_and_load_numeric_fixture ~workspace
      "numeric-external-direct-reexport"
  in
  let _, extension_roles = claim_identities extension_vri
  and external_carriers, external_roles =
    claim_identities external_reexport_vri
  in
  require
    (external_carriers = [] && external_roles = extension_roles)
    "direct reexport reinterpreted an original role's external references";
  let _, collision_vri =
    emit_and_load_numeric_fixture ~workspace "numeric-collision-dependency"
  in
  install_numeric_authority ~workspace
    ~unit_basename:"numeric_collision_dependency" collision_vri;
  let resolution, _ =
    emit_and_load_numeric_fixture ~workspace "numeric-resolution-provider"
  in
  let roles = resolution.interface_numeric_claims.numeric_roles in
  let role identity =
    match
      List.filter
        (fun role ->
          role.Cmt_input.numeric_role_source.role_identity = identity)
        roles
    with
    | [ role ] -> role
    | [] | _ :: _ :: _ ->
        failwith ("missing or duplicate resolved numeric role " ^ identity)
  in
  let local = role "local-collision-view"
  and imported = role "alias-import-view"
  and opened = role "opened-import-view"
  and enclosing = role "enclosing-view" in
  require
    (local.numeric_role_carrier_owner_unit = "Numeric_resolution_provider"
    && imported.numeric_role_carrier_owner_unit
       = "Numeric_collision_dependency"
    && opened.numeric_role_carrier_owner_unit = "Numeric_source_provider"
    && enclosing.numeric_role_carrier_owner_unit
       = "Numeric_resolution_provider")
    "compiler lexical/alias/open resolution selected a spelling-colliding owner";
  require
    (List.for_all
       (fun role ->
         List.length role.Cmt_input.numeric_role_carrier_import_routes = 1
         && List.length role.numeric_role_semantics_import_routes = 1)
       roles)
    "resolved numeric owners did not carry one canonical bounded route"

let typed_reference_mismatch_rejection () =
  let basename = "numeric-mismatch-provider" in
  let cmt = fixture_artifact basename ".cmt"
  and cmi = fixture_artifact basename ".cmi"
  and cmti = fixture_artifact basename ".cmti" in
  let contexts_before = (Z3_bridge.counters ()).contexts_created in
  let output = Filename.temp_file "numeric-mismatch" ".vri" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists output then Sys.remove output)
    (fun () ->
      match
        Cmt_input.emit_retained_interface_authority ~cmt ~cmi ~cmti ~output
          ~artifact_directories:[ Filename.dirname Sys.executable_name ] ()
      with
      | Error
          { Diagnostic.classification = Invalid_numeric_declaration _; _ } -> ()
      | Error diagnostic ->
          failwith
            ("mismatched ml/mli numeric metadata used the wrong rejection: "
           ^ diagnostic.Diagnostic.code)
      | Ok () -> failwith "mismatched ml/mli numeric metadata was accepted");
  require
    ((Z3_bridge.counters ()).contexts_created = contexts_before)
    "numeric metadata mismatch created a solver context"

let typed_reference_shadow_rejection () =
  let basename = "numeric-shadow-provider" in
  let cmt = fixture_artifact basename ".cmt"
  and cmi = fixture_artifact basename ".cmi"
  and cmti = fixture_artifact basename ".cmti" in
  let contexts_before = (Z3_bridge.counters ()).contexts_created in
  let output = Filename.temp_file "numeric-shadow" ".vri" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists output then Sys.remove output)
    (fun () ->
      match
        Cmt_input.emit_retained_interface_authority ~cmt ~cmi ~cmti ~output
          ~artifact_directories:[ Filename.dirname Sys.executable_name ] ()
      with
      | Error
          { Diagnostic.classification = Invalid_numeric_declaration _; _ } -> ()
      | Error diagnostic ->
          failwith
            ("same-spelling numeric shadow used the wrong rejection: "
           ^ diagnostic.Diagnostic.code)
      | Ok () ->
          failwith
            "implementation/interface same-spelling numeric shadow was accepted");
  require
    ((Z3_bridge.counters ()).contexts_created = contexts_before)
    "same-spelling numeric shadow rejection created a solver context"

let exported_binding_case ~name ~nested ~accepted definitions =
  Suite.case ~name
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace ->
      try
        let stem = Filename.concat workspace "export_binding_provider" in
        let ppx =
          Filename.concat (Filename.dirname Sys.executable_name)
            "../../ppx/vero_ppx.exe"
          |> Unix.realpath
        and ghost =
          Filename.concat (Filename.dirname Sys.executable_name)
            "../../runtime/.vero_ghost.objs/byte"
          |> Unix.realpath
        and compiler = Filename.concat Config.bindir "ocamlc" in
        let role =
          {|[@@verocaml.numeric_role
  { carrier = carrier; role_schema = "numeric-role.v1";
    role = "view"; semantics = law; visibility = "opaque";
    reveal = false; inline = false }]
|}
        in
        let signature =
          {|type carrier = int
[@@verocaml.numeric_carrier
  { profile = "export-profile"; representation = "immediate";
    compatibility = [] }]
val law : carrier -> int [@@verocaml.spec]
val operation : carrier -> int
|} ^ role
        and implementation =
          "type carrier = int\n" ^ definitions role
        in
        write_file (stem ^ ".mli")
          (if nested then "module Nested : sig\n" ^ signature ^ "end\n"
           else signature);
        write_file (stem ^ ".ml")
          ("[@@@warning \"-32\"]\n"
          ^ (if nested then "module Nested = struct\n" ^ implementation ^ "end\n"
             else implementation));
        run_process compiler
          [ "-c"; "-bin-annot"; "-ppx"; ppx ^ " --keep-ghost";
            "-o"; stem ^ ".cmi"; stem ^ ".mli" ];
        run_process compiler
          [ "-c"; "-bin-annot"; "-I"; workspace; "-I"; ghost; "-ppx";
            ppx ^ " --keep-ghost"; "-o"; stem ^ ".cmo"; stem ^ ".ml" ];
        let contexts_before = (Z3_bridge.counters ()).contexts_created in
        let result =
          Cmt_input.emit_retained_interface_authority
            ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi")
            ~cmti:(stem ^ ".cmti") ~output:(stem ^ ".vri")
            ~artifact_directories:[ workspace ] ()
        in
        (match accepted, result with
        | true, Ok () ->
            let provider =
              match Cmt_input.load_with_interface
                ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi")
                ~cmti:(stem ^ ".cmti") ~artifact_directories:[ workspace ] ()
              with
              | Ok provider -> provider
              | Error diagnostic -> failwith diagnostic.Diagnostic.code
            in
            let role =
              match provider.interface_numeric_claims.numeric_roles with
              | [ role ] -> role
              | _ -> failwith "export binding fixture lost its numeric role"
            in
            let exported_path =
              "Export_binding_provider." ^ (if nested then "Nested." else "")
              ^ "law"
            in
            require (String.equal role.numeric_role_semantics_path exported_path)
              "accepted role was not bound to the exported law";
            let interface = Cmi_format.read_cmi_lazy (stem ^ ".cmi") in
            let rec exported_law_uid items =
              List.find_map
                (function
                  | Types.Sig_value (ident, description, Types.Exported)
                    when String.equal (Ident.name ident) "law" ->
                      Some (Format.asprintf "%a" Types.Uid.print description.val_uid)
                  | Types.Sig_module (_, _, { md_type = Mty_signature items; _ }, _, Types.Exported) ->
                      exported_law_uid items
                  | _ -> None)
                items
            in
            require
              (exported_law_uid (Subst.Lazy.force_signature interface.cmi_sign)
               = Some role.numeric_role_semantics_uid)
              "retained role substituted the exported law's compiler identity"
        | false, Error
            { Diagnostic.classification = Invalid_numeric_declaration _; _ } ->
            require (not (Sys.file_exists (stem ^ ".vri")))
              "hidden binding rejection still wrote retained authority"
        | true, Error diagnostic ->
            failwith ("valid final binding rejected: " ^ diagnostic.Diagnostic.code)
        | false, Error diagnostic ->
            failwith ("hidden binding used the wrong rejection: " ^ diagnostic.Diagnostic.code)
        | false, Ok () -> failwith "hidden predecessor supplied exported numeric metadata");
        require ((Z3_bridge.counters ()).contexts_created = contexts_before)
          "numeric export correlation created a solver context";
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let old_law = "let law x = x [@@verocaml.spec]\n"
let exported_law = "let law x = x + 1 [@@verocaml.spec]\n"
let numeric_operation role = "let operation x = law x\n" ^ role

let carrier_layout_case ~name ~interface ~implementation ~request
    ?(following_declarations = "") expected =
  Suite.case ~name
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace ->
      try
        let stem = Filename.concat workspace "layout_provider" in
        let ppx =
          Filename.concat (Filename.dirname Sys.executable_name)
            "../../ppx/vero_ppx.exe" |> Unix.realpath
        in
        let compiler = Filename.concat Config.bindir "ocamlc" in
        write_file (stem ^ ".mli")
          (interface ^ Printf.sprintf
             {|[@@verocaml.numeric_carrier
  { profile = "layout-profile"; representation = %S; compatibility = [] }]
|} request ^ following_declarations);
        write_file (stem ^ ".ml") implementation;
        run_process compiler
          [ "-c"; "-bin-annot"; "-ppx"; ppx ^ " --keep-ghost";
            "-o"; stem ^ ".cmi"; stem ^ ".mli" ];
        run_process compiler
          [ "-c"; "-bin-annot"; "-I"; workspace; "-ppx";
            ppx ^ " --keep-ghost"; "-o"; stem ^ ".cmo"; stem ^ ".ml" ];
        let result =
          Cmt_input.emit_retained_interface_authority
            ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
            ~output:(stem ^ ".vri") ~artifact_directories:[ workspace ] ()
        in
        (match expected, result with
        | None, Error { Diagnostic.classification = Invalid_numeric_declaration _; _ } ->
            require (not (Sys.file_exists (stem ^ ".vri")))
              "unsupported carrier layout emitted retained authority"
        | None, Ok () -> failwith "unsupported carrier layout was accepted"
        | _, Error diagnostic -> failwith diagnostic.Diagnostic.code
        | Some expected, Ok () ->
            let provider =
              match Cmt_input.load_with_interface
                ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
                ~artifact_directories:[ workspace ] ()
              with Ok provider -> provider | Error diagnostic -> failwith diagnostic.Diagnostic.code
            in
            let inventory = Cmt_input.retained_numeric_claims provider.interface_numeric_claims in
            let carrier =
              match inventory.carrier_reconstruction_claims with
              | [ encoded ] -> ok "numeric layout carrier decode"
                  (Numeric_interface_claim_private.decode_carrier encoded)
              | _ -> failwith "layout fixture did not retain its carrier"
            in
            let binding = ok "numeric layout artifact binding"
                (Numeric_artifact_binding_private.correlate provider carrier) in
            let facts = ok "numeric layout binding facts"
                (Numeric_artifact_binding_private.facts binding) in
            require (facts.representation = expected)
              "carrier representation did not follow the compiler layout");
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let cmti_semantic_substitution_rejection ~workspace =
  let source = Filename.concat workspace "hybrid_provider.mli"
  and implementation_source =
    Filename.concat workspace "hybrid_provider.ml"
  and cmi = Filename.concat workspace "hybrid_provider.cmi"
  and cmti = Filename.concat workspace "hybrid_provider.cmti"
  and cmo = Filename.concat workspace "hybrid_provider.cmo"
  and cmt = Filename.concat workspace "hybrid_provider.cmt"
  and accepted_cmi = Filename.concat workspace "accepted.cmi"
  and accepted_cmti = Filename.concat workspace "accepted.cmti"
  and substituted_cmi = Filename.concat workspace "substituted.cmi"
  and substituted_cmti = Filename.concat workspace "substituted.cmti" in
  let ppx =
    Filename.concat (Filename.dirname Sys.executable_name)
      "../../ppx/vero_ppx.exe"
    |> Unix.realpath
  and ghost =
    Filename.concat (Filename.dirname Sys.executable_name)
      "../../runtime/.vero_ghost.objs/byte"
    |> Unix.realpath
  and compiler = Filename.concat Config.bindir "ocamlc" in
  let interface opened =
    Printf.sprintf
      {|type carrier = int
[@@verocaml.numeric_carrier
  { profile = "hybrid-profile"; representation = "immediate";
    compatibility = [] }]

module A : sig
  val law : carrier -> int [@@verocaml.spec]
end
module B : sig
  val law : carrier -> int [@@verocaml.spec]
end
open %s
val operation : carrier -> int
[@@verocaml.numeric_role
  { carrier = carrier; role_schema = "numeric-role.v1";
    role = "hybrid-view"; semantics = law; visibility = "opaque";
    reveal = false; inline = false }]
|}
      opened
  in
  let compile_interface opened =
    write_file source (interface opened);
    run_process compiler
      [ "-c"; "-bin-annot"; "-ppx"; ppx ^ " --keep-ghost"; "-o"; cmi;
        source ]
  in
  compile_interface "A";
  copy_file cmi accepted_cmi;
  copy_file cmti accepted_cmti;
  compile_interface "B";
  copy_file cmi substituted_cmi;
  copy_file cmti substituted_cmti;
  require (String.equal (read_file accepted_cmi) (read_file substituted_cmi))
    "adversarial CMTI fixture did not preserve byte-identical CMI authority";
  require
    (not
       (String.equal (read_file accepted_cmti) (read_file substituted_cmti)))
    "adversarial CMTI fixture did not produce byte-distinct typed interfaces";
  copy_file accepted_cmi cmi;
  write_file implementation_source
    {|type carrier = int
[@@verocaml.numeric_carrier
  { profile = "hybrid-profile"; representation = "immediate";
    compatibility = [] }]
module A = struct
  let law value = value [@@verocaml.spec]
end
module B = struct
  let law value = value + 1 [@@verocaml.spec]
end
open A
let operation (value : carrier) = law value
[@@verocaml.numeric_role
  { carrier = carrier; role_schema = "numeric-role.v1";
    role = "hybrid-view"; semantics = law; visibility = "opaque";
    reveal = false; inline = false }]
|};
  run_process compiler
    [ "-c"; "-bin-annot"; "-I"; workspace; "-I"; ghost; "-ppx";
      ppx ^ " --keep-ghost"; "-o"; cmo; implementation_source ];
  let contexts_before = (Z3_bridge.counters ()).contexts_created in
  let accepted_vri = Filename.concat workspace "accepted.vri" in
  (match
     Cmt_input.emit_retained_interface_authority ~cmt ~cmi
       ~cmti:accepted_cmti ~output:accepted_vri
       ~artifact_directories:[ workspace ] ()
   with
  | Ok () -> ()
  | Error diagnostic ->
      failwith
        ("matching CMT/CMI/CMTI semantic fixture failed: " ^ diagnostic.code));
  let substituted_vri = Filename.concat workspace "substituted.vri" in
  (match
     Cmt_input.emit_retained_interface_authority ~cmt ~cmi
       ~cmti:substituted_cmti ~output:substituted_vri
       ~artifact_directories:[ workspace ] ()
   with
  | Error
      { Diagnostic.classification = Invalid_numeric_declaration _; _ } -> ()
  | Error diagnostic ->
      failwith
        ("substituted CMTI used the wrong rejection: " ^ diagnostic.code)
  | Ok () ->
      failwith
        "byte-distinct semantically substituted CMTI emitted valid authority");
  require
    ((Z3_bridge.counters ()).contexts_created = contexts_before)
    "CMTI semantic substitution rejection created a solver context"

let cmti_semantic_substitution_case =
  let name = "cmti-semantic-substitution-rejected" in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ name)
           (Outcome.Function_exists name))
    (fun ~environment:_ ~workspace ->
      try
        cmti_semantic_substitution_rejection ~workspace;
        Ok
          (Outcome.observation ~status:Outcome.Verified
             ~units:[ (name, Outcome.Unit_verified) ]
             ~named_facts:
               [ ("function:" ^ name, Outcome.Function_exists name) ]
             ()
          |> Outcome.project)
      with Failure message ->
        Error (Failure.make Failure.Runner_internal message))

let numeric_provenance_budget_rejection ~workspace =
  let basename = "numeric-source-provider" in
  let cmt = fixture_artifact basename ".cmt"
  and cmi = fixture_artifact basename ".cmi"
  and cmti = fixture_artifact basename ".cmti" in
  let output = Filename.concat workspace "oversized-provenance.vri" in
  let excessive_search_space =
    List.init 20_000 (fun ordinal ->
        Filename.concat workspace ("probe-" ^ string_of_int ordinal))
  in
  let contexts_before = (Z3_bridge.counters ()).contexts_created in
  (match
     Cmt_input.emit_retained_interface_authority ~cmt ~cmi ~cmti ~output
       ~artifact_directories:
         (Filename.dirname Sys.executable_name :: excessive_search_space)
       ()
   with
  | Error
      { Diagnostic.classification = Invalid_numeric_declaration _; _ } -> ()
  | Error diagnostic ->
      failwith
        ("oversized numeric provenance used the wrong rejection: "
       ^ diagnostic.code)
  | Ok () -> failwith "oversized numeric provenance search space was accepted");
  require
    ((Z3_bridge.counters ()).contexts_created = contexts_before)
    "numeric provenance budget rejection created a solver context"

let numeric_provenance_budget_case =
  let name = "numeric-provenance-budget-rejected" in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ name)
           (Outcome.Function_exists name))
    (fun ~environment:_ ~workspace ->
      try
        numeric_provenance_budget_rejection ~workspace;
        Ok
          (Outcome.observation ~status:Outcome.Verified
             ~units:[ (name, Outcome.Unit_verified) ]
             ~named_facts:
               [ ("function:" ^ name, Outcome.Function_exists name) ]
             ()
          |> Outcome.project)
      with Failure message ->
        Error (Failure.make Failure.Runner_internal message))

let bounded_numeric_provenance ~workspace =
  let _, leaf_vri =
    emit_and_load_numeric_fixture ~workspace "numeric-diamond-leaf"
  in
  install_numeric_authority ~workspace ~unit_basename:"numeric_diamond_leaf"
    leaf_vri;
  let _, left_vri =
    emit_and_load_numeric_fixture ~workspace "numeric-diamond-left"
  in
  install_numeric_authority ~workspace ~unit_basename:"numeric_diamond_left"
    left_vri;
  let _, right_vri =
    emit_and_load_numeric_fixture ~workspace "numeric-diamond-right"
  in
  install_numeric_authority ~workspace ~unit_basename:"numeric_diamond_right"
    right_vri;
  let diamond, vri =
    emit_and_load_numeric_fixture ~workspace "numeric-diamond-root"
  in
  let claims = diamond.interface_numeric_claims in
  let carrier =
    match claims.numeric_carriers with
    | [ carrier ] -> carrier
    | [] | _ :: _ :: _ -> failwith "diamond provider did not retain one carrier"
  in
  require
    (carrier.numeric_carrier_owner_unit = "Numeric_diamond_root"
    && List.length carrier.numeric_carrier_import_routes = 1)
    "diamond provenance did not retain one canonical root route";
  require
    (claims.numeric_provenance_nodes > 1
    && claims.numeric_provenance_nodes < 32
    && claims.numeric_provenance_edges < 64
    && claims.numeric_provenance_bytes < (8 * 1024 * 1024)
    && (Unix.stat vri).Unix.st_size < (16 * 1024 * 1024))
    "diamond provenance exceeded bounded graph or retained-section limits";
  require
    (Result.is_ok
       (Retained_interface_authority_private.decode (read_file vri)))
    "emitter produced a retained envelope its decoder rejected";
  let nonnumeric =
    match
      Cmt_input.load_with_interface
        ~cmt:(fixture_artifact "numeric-nonnumeric-provider" ".cmt")
        ~cmi:(fixture_artifact "numeric-nonnumeric-provider" ".cmi")
        ~cmti:(fixture_artifact "numeric-nonnumeric-provider" ".cmti")
        ~artifact_directories:[ Filename.dirname Sys.executable_name ] ()
    with
    | Ok implementation -> implementation
    | Error diagnostic ->
        failwith
          ("nonnumeric provider load failed: " ^ diagnostic.Diagnostic.code)
  in
  require
    (nonnumeric.interface_numeric_claims.numeric_provenance_nodes = 0
    && nonnumeric.interface_numeric_claims.numeric_provenance_edges = 0
    && nonnumeric.interface_numeric_claims.numeric_provenance_bytes = 0)
    "nonnumeric provider traversed numeric dependency provenance"

let typed_reference_resolution_case =
  let name = "typed-numeric-reference-resolution" in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ name)
           (Outcome.Function_exists name))
    (fun ~environment:_ ~workspace ->
      try
        typed_reference_resolution ~workspace;
        Ok
          (Outcome.observation ~status:Outcome.Verified
             ~units:[ (name, Outcome.Unit_verified) ]
             ~named_facts:
               [ ("function:" ^ name, Outcome.Function_exists name) ]
             ()
          |> Outcome.project)
      with Failure message ->
        Error (Failure.make Failure.Runner_internal message))

let bounded_numeric_provenance_case =
  let name = "bounded-numeric-provenance" in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ name)
           (Outcome.Function_exists name))
    (fun ~environment:_ ~workspace ->
      try
        bounded_numeric_provenance ~workspace;
        Ok
          (Outcome.observation ~status:Outcome.Verified
             ~units:[ (name, Outcome.Unit_verified) ]
             ~named_facts:
               [ ("function:" ^ name, Outcome.Function_exists name) ]
             ()
          |> Outcome.project)
      with Failure message ->
        Error (Failure.make Failure.Runner_internal message))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    (Numeric_semantics_cases.cases ~write_file ~run_process @
    Numeric_base_int_cases.cases ~write_file ~run_process @
    Numeric_ghost_law_cases.cases ~write_file ~run_process @
    Numeric_registry_cases.cases ~write_file ~run_process @
    Numeric_signed_law_cases.cases ~write_file ~run_process @
    Numeric_target_set_cases.cases ~write_file ~run_process @
    Numeric_relation_cases.cases ~write_file ~run_process @
    Numeric_bv_source_cases.cases ~write_file ~run_process @
    Numeric_ecosystem_cases.cases ~write_file @
    Numeric_original_import_cases.cases ~write_file ~run_process @
    Numeric_extension_cases.cases ~write_file ~run_process @
    Numeric_law_cases.cases ~write_file ~run_process @
    [ direct_case ~name:"strict-canonical-grammar" strict_canonical_grammar;
      direct_case ~name:"profile-claim-full-key-round-trip"
        profile_claim_round_trip;
      direct_case ~name:"build-target-profile-authority"
        build_target_profile_authority;
      direct_case ~name:"bv-width-handoff-boundaries"
        bv_width_handoff_boundaries;
      direct_case ~name:"descriptor-claim-full-key-round-trip"
        descriptor_claim_round_trip;
      direct_case ~name:"retained-unknown-optional-round-trip"
        retained_unknown_optional_round_trip;
      direct_case ~name:"typed-source-claim-surface"
        typed_source_claim_surface;
      compiler_reconstructed_claim_case;
      direct_case ~name:"nonsemantic-reference-rejected"
        nonsemantic_reference_rejection;
      typed_reference_resolution_case;
      direct_case ~name:"typed-reference-mismatch-rejected"
        typed_reference_mismatch_rejection;
      direct_case ~name:"typed-reference-shadow-rejected"
        typed_reference_shadow_rejection;
      exported_binding_case ~name:"hidden-law-predecessor-rejected"
        ~nested:false ~accepted:false
        (fun role -> old_law ^ numeric_operation role ^ exported_law);
      exported_binding_case ~name:"final-shadowing-law-accepted"
        ~nested:false ~accepted:true
        (fun role -> old_law ^ exported_law ^ numeric_operation role);
      exported_binding_case ~name:"nested-hidden-law-predecessor-rejected"
        ~nested:true ~accepted:false
        (fun role -> old_law ^ numeric_operation role ^ exported_law);
      exported_binding_case ~name:"nested-final-shadowing-law-accepted"
        ~nested:true ~accepted:true
        (fun role -> old_law ^ exported_law ^ numeric_operation role);
      exported_binding_case ~name:"later-open-does-not-change-exported-law"
        ~nested:false ~accepted:true
        (fun role -> old_law ^ numeric_operation role
          ^ "module Other = struct let law x = x + 2 end\nopen Other\n");
      exported_binding_case ~name:"hidden-callable-marker-rejected"
        ~nested:false ~accepted:false
        (fun role -> old_law ^ numeric_operation role ^ "let operation x = x + 2\n");
      exported_binding_case ~name:"final-shadowing-callable-accepted"
        ~nested:false ~accepted:true
        (fun role -> old_law ^ "let operation x = x\n" ^ numeric_operation role);
      exported_binding_case ~name:"mli-only-final-shadowing-callable-accepted"
        ~nested:false ~accepted:true
        (fun _ -> old_law ^ "let operation x = x\nlet operation x = law x\n");
      carrier_layout_case ~name:"inferred-immediate-carrier"
        ~interface:"type carrier = int\n" ~implementation:"type carrier = int\n"
        ~request:"immediate" (Some Cmt_input.Artifact_immediate);
      carrier_layout_case ~name:"chained-alias-carrier-layout"
        ~interface:"type base = int\ntype carrier = base\n"
        ~implementation:"type base = int\ntype carrier = base\n"
        ~request:"immediate" (Some Cmt_input.Artifact_immediate);
      carrier_layout_case ~name:"same-type-group-carrier-layout"
        ~interface:"type backing = int and carrier = backing\n"
        ~implementation:"type backing = int and carrier = backing\n"
        ~request:"immediate" (Some Cmt_input.Artifact_immediate);
      carrier_layout_case ~name:"forward-type-group-carrier-layout"
        ~interface:"type carrier = backing\n"
        ~following_declarations:"and backing = int\n"
        ~implementation:"type carrier = backing and backing = int\n"
        ~request:"immediate" (Some Cmt_input.Artifact_immediate);
      carrier_layout_case ~name:"boxed-float-carrier-layout"
        ~interface:"type carrier = float\n" ~implementation:"type carrier = float\n"
        ~request:"boxed" (Some Cmt_input.Artifact_boxed);
      carrier_layout_case ~name:"abstract-carrier-does-not-leak-implementation-layout"
        ~interface:"type carrier\n" ~implementation:"type carrier = int\n"
        ~request:"boxed" (Some Cmt_input.Artifact_boxed);
      carrier_layout_case ~name:"unboxed-carrier-layout-not-yet-admitted"
        ~interface:"type carrier = int64#\n" ~implementation:"type carrier = int64#\n"
        ~request:"boxed" None;
      cmti_semantic_substitution_case;
      numeric_provenance_budget_case;
      bounded_numeric_provenance_case ])
