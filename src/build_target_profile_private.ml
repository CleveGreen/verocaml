type profile = {
  profile_claim : Target_profile_private.claim;
  build_declaration_full_key : string;
  full_key : string;
  checked_digest : string;
}

type instance = {
  profile_full_key : string;
  profile_checked_digest : string;
  target_claim : Target_profile_private.instance_claim;
  full_key : string;
  checked_digest : string;
}

type capability = { token : unit ref }

let profile_schema = "verocaml.sealed-build-target-profile.v2"
let instance_schema = "verocaml.sealed-build-target-instance.v2"
let declaration_schema = "verocaml.build-produced-target-declaration.v2"
let profile_digest_domain = "verocaml.sealed-build-target-profile.full-key.v2"
let instance_digest_domain = "verocaml.sealed-build-target-instance.full-key.v2"

let ( let* ) value continuation =
  match value with Ok value -> continuation value | Error _ as error -> error

let compiler_abi () =
  Numeric_receipt_private.encode ~schema:"verocaml.build-compiler-abi.v1"
    [ Config.version; Sys.ocaml_version; Config.cmi_magic_number;
      Config.cmt_magic_number; Config.ast_intf_magic_number;
      Config.ast_impl_magic_number; Config.cmx_magic_number;
      string_of_bool Config.native_compiler; string_of_bool Config.flambda;
      string_of_bool Config.flambda2; string_of_bool Config.flat_float_array ]

let toolchain_abi () =
  Numeric_receipt_private.encode ~schema:"verocaml.build-toolchain-abi.v1"
    [ Config.host; Config.target; Config.architecture; Config.model;
      Config.system; Config.ccomp_type; Config.ext_obj; Config.ext_asm;
      Config.ext_lib; Config.ext_dll; Config.ext_exe ]

let target_identity () =
  Numeric_receipt_private.encode ~schema:"verocaml.build-target-identity.v1"
    [ Numeric_build_target_generated.producer_schema;
      Numeric_build_target_generated.profile_identity; Config.target;
      Config.architecture; Config.model; Config.system;
      string_of_int Config.max_tag; string_of_bool Config.native_compiler ]

let layout_material (layout : Target_profile_private.layout) =
  Numeric_receipt_private.encode ~schema:"verocaml.build-produced-layout.v1"
    [ layout.layout_class; layout.layout_abi; string_of_int layout.width;
      string_of_bool layout.signed ]

let derive () =
  let result =
    let compiler_abi = compiler_abi ()
    and toolchain_abi = toolchain_abi ()
    and target_identity = target_identity () in
    let rec layouts values = function
      | [] -> Ok (List.rev values)
      | (layout_class, declared_layout_abi, width, signed) :: rest ->
          let layout_abi =
            Numeric_receipt_private.encode
              ~schema:"verocaml.build-layout-abi.v1"
              [ target_identity; declared_layout_abi; string_of_int width;
                string_of_bool signed ]
          in
          let* layout =
            Target_profile_private.layout ~layout_class ~layout_abi ~width
              ~signed
          in
          [%log.trace "validated build-produced target layout fact"
            ~stage:(Delator.Field.string "build-target-profile-derivation")
            ~layout_class:(Delator.Field.string layout_class)
            ~width:(Delator.Field.int width)
            ~signed:(Delator.Field.bool signed)
            ~authority:(Delator.Field.string "compiled-target-matrix")
            ~decision:(Delator.Field.string "accepted")];
          layouts (layout :: values) rest
    in
    let* layouts = layouts [] Numeric_build_target_generated.layouts in
    let layouts =
      List.sort
        (fun left right ->
          String.compare (layout_material left) (layout_material right))
        layouts
    in
    let layout_keys = List.map layout_material layouts in
    let declared_widths =
      List.map (fun (layout : Target_profile_private.layout) -> layout.width)
        layouts
      |> List.sort Int.compare
    in
    let supported_widths = List.sort_uniq Int.compare declared_widths
    in
    if Numeric_build_target_generated.profile_identity = "" then
      Error "build-produced target profile identity is empty"
    else if
      List.length layout_keys
      <> List.length (List.sort_uniq String.compare layout_keys)
    then Error "build-produced target profile has duplicate complete layouts"
    else if
      supported_widths = []
      || List.exists (fun width -> width <= 0) supported_widths
    then Error "build-produced target profile has no positive target widths"
    else if
      not
        (String.equal
           Numeric_build_target_generated.logical_bv_width_permission
           "positive")
    then Error "build-produced logical BV width permission is unsupported"
    else
      let build_declaration_full_key =
        Numeric_receipt_private.encode ~schema:declaration_schema
          [ Numeric_build_target_generated.producer_schema;
            Numeric_build_target_generated.profile_identity; compiler_abi;
            toolchain_abi; target_identity;
            Numeric_receipt_private.list layout_keys;
            Numeric_receipt_private.list
              (List.map string_of_int supported_widths) ]
      in
      let* logical_bv_width_permission =
        Target_profile_private.issue_logical_bv_width_permission
          ~issuer_claim:build_declaration_full_key
      in
      let* profile_claim =
        Target_profile_private.issue_claim ~compiler_abi ~toolchain_abi
          ~target_identity ~layouts ~supported_widths
          ~logical_bv_width_permission
          ~issuer_claim:build_declaration_full_key
      in
      let* decoded_claim =
        Target_profile_private.decode_claim profile_claim.full_key
      in
      if not (Target_profile_private.equal_claim profile_claim decoded_claim) then
        Error "build-produced target profile claim is not canonical"
      else
        let full_key =
          Numeric_receipt_private.encode ~schema:profile_schema
            [ build_declaration_full_key; profile_claim.full_key;
              profile_claim.checked_digest ]
        in
        let checked_digest =
          Numeric_receipt_private.digest ~domain:profile_digest_domain full_key
        in
        let profile =
          { profile_claim; build_declaration_full_key; full_key;
            checked_digest }
        in
        let rec instances values = function
          | [] -> Ok (List.rev values)
          | layout :: rest ->
              let* target_claim =
                Target_profile_private.instantiate_claim profile_claim ~layout
              in
              let* decoded_target =
                Target_profile_private.decode_instance_claim ~profile:profile_claim
                  target_claim.full_key
              in
              if
                not
                  (Target_profile_private.equal_instance_claim target_claim
                     decoded_target)
              then Error "build-produced target instance claim is not canonical"
              else
                let full_key =
                  Numeric_receipt_private.encode ~schema:instance_schema
                    [ profile.full_key; profile.checked_digest;
                      target_claim.full_key; target_claim.checked_digest ]
                in
                let checked_digest =
                  Numeric_receipt_private.digest ~domain:instance_digest_domain
                    full_key
                in
                instances
                  ({ profile_full_key = profile.full_key;
                     profile_checked_digest = profile.checked_digest;
                     target_claim; full_key; checked_digest }
                  :: values)
                  rest
        in
        let* instances = instances [] layouts in
        let original_count = List.length instances in
        let instances = Numeric_receipt_private.canonical_members
          ~full_key:(fun (instance : instance) -> instance.full_key) instances in
        if
          instances = []
          || List.length instances <> original_count
        then Error "build-produced target instance set is empty or duplicate"
        else Ok (profile, instances)
  in
  (match result with
  | Ok
      ( (profile [@log_value.info]),
        (instances [@log_value.info]) ) ->
      [%log.info "sealed build-produced target profile and instances"
        ~stage:(Delator.Field.string "build-target-profile-sealing")
        ~target_count:
          (Delator.Field.int
             (List.length (instances [@log_value.info])))
        ~width_count:
          (Delator.Field.int
             (List.length
                (profile [@log_value.info]).profile_claim.supported_widths))
        ~logical_bv_width_domain:
          (Delator.Field.string
             (profile [@log_value.info]).profile_claim
               .logical_bv_width_permission.domain)
        ~authority:(Delator.Field.string "compiled-target-matrix-capability")
        ~decision:(Delator.Field.string "sealed")]
  | Error (reason [@log_value.warn]) ->
      [%log.warn "rejected build-produced target profile sealing"
        ~stage:(Delator.Field.string "build-target-profile-sealing")
        ~reason_class:(Delator.Field.string (reason [@log_value.warn]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level info]

(** This eager immutable snapshot lets portable workers authenticate against
    compiled profile facts without capturing the issuer capability reference. *)
let worker_sealed_profile = derive ()

let live_token = ref ()
let embedded_capability = { token = live_token }

let capability () =
  [%log.trace "retrieved embedded build-target capability"
    ~stage:(Delator.Field.string "build-target-capability-retrieval")
    ~authority:(Delator.Field.string "compiled-target-matrix")
    ~decision:(Delator.Field.string "retrieved")];
  embedded_capability
[@@delator.instrument] [@@delator.level trace]

let authenticate capability =
  let result =
    if capability != embedded_capability || capability.token != live_token then
      Error "build-target capability is not the embedded live build capability"
    else worker_sealed_profile
  in
  (match result with
  | Ok (_, (instances [@log_value.info])) ->
      [%log.info "authenticated embedded build-target capability"
        ~stage:(Delator.Field.string "build-target-capability-authentication")
        ~target_count:
          (Delator.Field.int
             (List.length (instances [@log_value.info])))
        ~authority:(Delator.Field.string "compiled-target-matrix")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.warn]) ->
      [%log.warn "rejected build-target capability authentication"
        ~stage:(Delator.Field.string "build-target-capability-authentication")
        ~reason_class:(Delator.Field.string (reason [@log_value.warn]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level info]

let authenticate_profile capability =
  let* profile, _ = authenticate capability in
  Ok profile

let authenticate_instances capability =
  let* _, instances = authenticate capability in
  Ok instances

let logical_bv_width_permitted profile width =
  Target_profile_private.logical_bv_width_permitted
    profile.profile_claim.logical_bv_width_permission width

let instance_logical_bv_width_permitted instance width =
  match authenticate (capability ()) with
  | Error _ -> false
  | Ok (profile, instances) ->
      List.exists
        (fun candidate -> String.equal candidate.full_key instance.full_key)
        instances
      &&
      String.equal
        instance.target_claim.logical_bv_width_permission_full_key
        profile.profile_claim.logical_bv_width_permission.full_key
      && String.equal
           instance.target_claim.logical_bv_width_permission_checked_digest
           profile.profile_claim.logical_bv_width_permission.checked_digest
      && logical_bv_width_permitted profile width

let decode_profile capability encoded =
  let result =
    let* profile = authenticate_profile capability in
    if String.equal encoded profile.full_key then Ok profile
    else Error "serialized build-target profile is stale or substituted"
  in
  (match result with
  | Ok _ ->
      [%log.trace "authenticated serialized build-target profile"
        ~stage:(Delator.Field.string "build-target-profile-decode")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.warn]) ->
      [%log.warn "rejected serialized build-target profile"
        ~stage:(Delator.Field.string "build-target-profile-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.warn]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let decode_instance capability encoded =
  let result =
    let* instances = authenticate_instances capability in
    match List.filter (fun instance -> String.equal instance.full_key encoded) instances with
    | [ instance ] -> Ok instance
    | [] -> Error "serialized build-target instance is stale or substituted"
    | _ :: _ :: _ -> Error "serialized build-target instance is ambiguous"
  in
  (match result with
  | Ok (instance [@log_value.trace]) ->
      [%log.trace "authenticated serialized build-target instance"
        ~stage:(Delator.Field.string "build-target-instance-decode")
        ~width:
          (Delator.Field.int
             (instance [@log_value.trace]).target_claim.width)
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.warn]) ->
      [%log.warn "rejected serialized build-target instance"
        ~stage:(Delator.Field.string "build-target-instance-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.warn]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let decode_profile_for_worker @ portable = fun encoded ->
  let* profile, _ = worker_sealed_profile in
  if String.equal encoded profile.full_key then Ok profile
  else Error "detached build-target profile is stale or substituted"

let decode_instance_for_worker @ portable = fun encoded ->
  let* profile, instances = worker_sealed_profile in
  match
    List.filter (fun instance -> String.equal instance.full_key encoded) instances
  with
  | [ instance ]
    when String.equal instance.profile_full_key profile.full_key
         && String.equal
              instance.target_claim.logical_bv_width_permission_full_key
              profile.profile_claim.logical_bv_width_permission.full_key
         && String.equal
              instance.target_claim.logical_bv_width_permission_checked_digest
              profile.profile_claim.logical_bv_width_permission.checked_digest ->
      Ok instance
  | [] -> Error "detached build-target instance is stale or substituted"
  | [ _ ] ->
      Error "detached build-target instance has conflicting profile authority"
  | _ :: _ :: _ -> Error "detached build-target instance is ambiguous"

let instance_logical_bv_width_permitted_for_worker @ portable =
 fun instance width ->
  match worker_sealed_profile with
  | Error _ -> false
  | Ok (profile, instances) ->
      List.exists
        (fun candidate -> String.equal candidate.full_key instance.full_key)
        instances
      && String.equal instance.profile_full_key profile.full_key
      && String.equal
           profile.profile_claim.logical_bv_width_permission.domain
           "positive-mathematical-widths"
      && width > 0

let compare_profile (left : profile) (right : profile) =
  String.compare left.full_key right.full_key

let equal_profile left right = compare_profile left right = 0

let compare_instance (left : instance) (right : instance) =
  String.compare left.full_key right.full_key

let equal_instance left right = compare_instance left right = 0
