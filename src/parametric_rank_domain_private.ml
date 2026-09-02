module Issuance = Typedtree_adapter_issuance_private
type artifact_identity = {
  unit_name : string;
  source_file : string;
  source_digest : string;
  interface_digest : string;
  raw_cmt_digest : string;
  import_digest : string;
}
type schema_capability = {
  issuer : unit ref;
  token : unit ref;
  artifact : artifact_identity;
  descriptor : Parametric_adt.t;
  schema_digest : string;
  profile_digest : string;
  program : Sst.program Weak.t;
  program_snapshot : string;
  program_digest : string;
}
type validated_rank_domain = {
  issuer : unit ref;
  token : unit ref;
  capability : schema_capability option;
  program : Sst.program Weak.t;
  program_snapshot : string;
  domain_id : string;
  domain_version : string;
  snapshot_digest : string;
  component : Issuance.rank_type_identity list;
  positive_children : Issuance.rank_positive_child list;
  ground_witnesses : Issuance.rank_ground_witness list;
  actual_evidence : string list;
  application : Parametric_type.t option;
  immutable : bool;
}
type portable_claim = {
  claim_version : string;
  provider_unit : string;
  provider_artifact : string;
  schema_digest : string;
  compiler_uid : string;
  profile_digest : string;
  application : Parametric_type.t option;
  actual_evidence : string list;
  claim_digest : string;
}
type nominal_spec = {
  component : Issuance.rank_type_identity list;
  positive_children : Issuance.rank_positive_child list;
  ground_witnesses : Issuance.rank_ground_witness list;
  actual_evidence : string list;
}
type error = { span : Diagnostic.span; detail : string }
let ( let* ) result continuation = match result with Ok value -> continuation value | Error _ as error -> error
let issuer = ref ()
let domain_version_value = "structural-height-v1"
let schema_version = "parametric-schema-rank-v1"
let capabilities : schema_capability list ref = ref []
let nominal : validated_rank_domain list ref = ref []
let profiles : Issuance.issued_rank_profile list ref = ref []
let digest fields = Digest.string (String.concat "\000" fields) |> Digest.to_hex
let option value = Option.value ~default:"-" value
let import_identity (item : Cmt_input.import) =
  item.unit_name ^ "\001" ^ option item.crc
let compare_import (left : Cmt_input.import) (right : Cmt_input.import) =
  let unit_order = String.compare left.unit_name right.unit_name in
  if unit_order <> 0 then unit_order
  else Option.compare String.compare left.crc right.crc
let import_digest imports =
  Array.to_list imports
  |> List.sort compare_import |> List.map import_identity |> digest
let artifact_identity (implementation : Cmt_input.implementation) =
  match (implementation.source_digest, implementation.interface_digest) with
  | Some source_digest, Some interface_digest
    when implementation.unit_name <> ""
         && implementation.has_implementation_shape ->
      Ok
        {
          unit_name = implementation.unit_name;
          source_file = implementation.source_file;
          source_digest = Digest.to_hex source_digest;
          interface_digest;
          raw_cmt_digest = implementation.raw_artifact_digest;
          import_digest = import_digest implementation.imports;
        }
  | None, _ -> Error "schema-rank CMT has no compiler source digest"
  | _, None -> Error "schema-rank CMT has no compiler interface digest"
  | Some _, Some _ ->
      Error "schema-rank CMT is not an implementation compilation"
let artifact_snapshot artifact =
  String.concat "\001"
    [
      artifact.unit_name;
      artifact.source_file;
      artifact.source_digest;
      artifact.interface_digest;
      artifact.raw_cmt_digest;
      artifact.import_digest;
    ]
let binder_identity (binder : Parametric_type.binder) =
  Printf.sprintf "%s#%d/%d" binder.owner.owner_name binder.owner.owner_index
    binder.ordinal
let field_identity (field : Parametric_adt.field) =
  String.concat "\001"
    [
      string_of_int field.field_index;
      field.field_name;
      field.field_uid;
      Parametric_type.to_string field.field_type;
      string_of_bool field.field_mutable;
    ]
let descriptor_profile descriptor =
  let kind =
    match Parametric_adt.kind descriptor with
    | Parametric_adt.Record fields ->
        "record:" ^ String.concat "\002" (List.map field_identity fields)
    | Parametric_adt.Variant constructors ->
        constructors
        |> List.map (fun (constructor : Parametric_adt.constructor) ->
               String.concat "\001"
                 [
                   string_of_int constructor.constructor_index;
                   constructor.constructor_name;
                   constructor.constructor_uid;
                   String.concat "\002"
                     (List.map field_identity constructor.constructor_fields);
                 ])
        |> String.concat "\003" |> fun value -> "variant:" ^ value
  in
  digest
    [
      schema_version;
      Parametric_adt.to_string descriptor;
      Parametric_adt.compiler_uid descriptor;
      Parametric_adt.binders descriptor
      |> List.map binder_identity |> String.concat "\002";
      kind;
    ]
let live_program weak = Weak.get weak 0
let exact_program program weak snapshot =
  match live_program weak with
  | Some candidate ->
      candidate == program && String.equal snapshot (Sst.to_string program)
  | None -> false
let capability_authenticates program (capability : schema_capability) =
  capability.issuer == issuer
  && !(capability.token) = ()
  && exact_program program capability.program capability.program_snapshot
  && String.equal capability.schema_digest
       (digest
          [
            Parametric_adt.to_string capability.descriptor;
            Parametric_adt.compiler_uid capability.descriptor;
          ])
  && String.equal capability.profile_digest
       (descriptor_profile capability.descriptor)
  && String.equal capability.program_digest
       (digest
          [ artifact_snapshot capability.artifact; capability.program_snapshot ])
let weak_program program =
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some program);
  weak
let fields = function
  | Parametric_adt.Record fields -> fields
  | Parametric_adt.Variant constructors ->
      List.concat_map
        (fun (constructor : Parametric_adt.constructor) ->
          constructor.constructor_fields)
        constructors
let self_application descriptor typ =
  match typ with
  | Parametric_type.Application (constructor, _) ->
      Parametric_type.compare_constructor constructor
        (Parametric_adt.type_constructor descriptor)
      = 0
  | Unit | Bool | Int | Mathematical_int | Tuple _ | Aggregate _ | Parameter _ ->
      false
let direct_self descriptor typ =
  match typ with
  | Parametric_type.Application (constructor, arguments) ->
      Parametric_type.compare_constructor constructor
        (Parametric_adt.type_constructor descriptor)
      = 0
      && List.length arguments = List.length (Parametric_adt.binders descriptor)
      && List.for_all2
           (fun argument binder ->
             match argument with
             | Parametric_type.Parameter candidate ->
                 Parametric_type.compare_binder candidate binder = 0
             | Unit | Bool | Int | Mathematical_int | Tuple _ | Aggregate _
             | Application _ ->
                 false)
           arguments (Parametric_adt.binders descriptor)
  | Unit | Bool | Int | Mathematical_int | Tuple _ | Aggregate _ | Parameter _ ->
      false
let supported_schema descriptor =
  match Parametric_adt.kind descriptor with
  | Parametric_adt.Record _ -> false
  | Parametric_adt.Variant constructors ->
      let descriptor_fields = fields (Parametric_adt.kind descriptor) in
      constructors <> []
      && List.exists
           (fun (constructor : Parametric_adt.constructor) ->
             not
               (List.exists
                  (fun field -> direct_self descriptor field.Parametric_adt.field_type)
                  constructor.constructor_fields))
           constructors
      && List.exists
           (fun field -> direct_self descriptor field.Parametric_adt.field_type)
           descriptor_fields
      && List.for_all
           (fun field ->
             (not field.Parametric_adt.field_mutable)
             &&
             match field.field_type with
             | Parametric_type.Unit | Bool | Int | Mathematical_int
             | Parameter _ ->
                 true
             | Application _ as typ -> direct_self descriptor typ
             | Tuple _ | Aggregate _ -> false)
           descriptor_fields
      && not
           (List.exists
              (fun field ->
                self_application descriptor field.Parametric_adt.field_type
                && not (direct_self descriptor field.field_type))
              descriptor_fields)
let seal_local_schemas ~implementation ~program =
  let span = Diagnostic.file_span implementation.Cmt_input.source_file in
  match artifact_identity implementation with
  | Error detail -> Error { span; detail }
  | Ok artifact ->
      capabilities :=
        List.filter
          (fun (capability : schema_capability) ->
            match live_program capability.program with
            | Some candidate -> candidate != program
            | None -> false)
          !capabilities;
      let program_snapshot = Sst.to_string program in
      program.parametric_adts
      |> List.filter supported_schema
      |> List.iter (fun descriptor ->
             let schema_digest =
               digest
                 [
                   Parametric_adt.to_string descriptor;
                   Parametric_adt.compiler_uid descriptor;
                 ]
             in
             capabilities :=
               {
                 issuer;
                 token = ref ();
                 artifact;
                 descriptor;
                 schema_digest;
                 profile_digest = descriptor_profile descriptor;
                 program = weak_program program;
                 program_snapshot;
                 program_digest =
                   digest [ artifact_snapshot artifact; program_snapshot ];
               }
               :: !capabilities);
      Ok ()
let identity_snapshot identity =
  (* Live capability authentication, not the portable ID, binds compiler UIDs. *)
  String.concat "\001"
    [
      identity.Issuance.rank_type_id.type_name;
      string_of_int identity.rank_type_id.type_index;
      identity.rank_path;
    ]
let child_snapshot child =
  (* Diagnostic expansion traces may contain nonportable compiler UIDs. *)
  String.concat "\001"
    [
      child.Issuance.rank_constructor.constructor_name;
      string_of_int child.rank_constructor.constructor_index;
      string_of_int child.rank_field.field_index;
      String.concat "." (List.map string_of_int child.rank_child_path);
      child.rank_child_type.type_name;
      string_of_int child.rank_child_type.type_index;
    ]
let ground_snapshot witness =
  String.concat "\001"
    [
      witness.Issuance.rank_ground_constructor.constructor_name;
      string_of_int witness.rank_ground_constructor.constructor_index;
    ]
let make_domain ?capability ?application ~program ~component
    ~positive_children ~ground_witnesses ~actual_evidence () =
  let program_snapshot = Sst.to_string program in
  let snapshot =
    String.concat "\000"
      [
        domain_version_value;
        String.concat "\003" (List.map identity_snapshot component);
        String.concat "\003" (List.map child_snapshot positive_children);
        String.concat "\003" (List.map ground_snapshot ground_witnesses);
        String.concat "\003" actual_evidence;
      ]
  in
  let snapshot_digest = digest [ snapshot ] in
  {
    issuer;
    token = ref ();
    capability;
    program = weak_program program;
    program_snapshot;
    domain_id = "rank-domain/v1/" ^ snapshot_digest;
    domain_version = domain_version_value;
    snapshot_digest;
    component;
    positive_children;
    ground_witnesses;
    actual_evidence;
    application;
    immutable = true;
  }
let issue_nominal ~program specs =
  nominal :=
    List.filter
      (fun domain ->
        match live_program domain.program with
        | Some candidate -> candidate != program
        | None -> false)
      !nominal;
  let issued =
    List.map
      (fun spec ->
        make_domain ~program ~component:spec.component
          ~positive_children:spec.positive_children
          ~ground_witnesses:spec.ground_witnesses
          ~actual_evidence:spec.actual_evidence ())
      specs
  in
  nominal := List.rev_append issued !nominal;
  issued
let authenticate ~program domain =
  domain.issuer == issuer
  && !(domain.token) = ()
  && domain.immutable
  && String.equal domain.domain_version domain_version_value
  && exact_program program domain.program domain.program_snapshot
  &&
  match domain.capability with
  | None -> true
  | Some capability -> capability_authenticates program capability
let nominal_domains program =
  let live, selected =
    List.fold_left
      (fun (live, selected) domain ->
        match live_program domain.program with
        | None -> (live, selected)
        | Some candidate ->
            ( domain :: live,
              if candidate == program && authenticate ~program domain then
                domain :: selected
              else selected ))
      ([], []) !nominal
  in
  nominal := List.rev live;
  List.rev selected
let find_capability program constructor =
  List.find_opt
    (fun capability ->
      capability_authenticates program capability
      && Parametric_type.compare_constructor
           (Parametric_adt.type_constructor capability.descriptor)
           constructor
         = 0)
    !capabilities
let rec actual_allowed program opaque_binders visiting = function
  | Parametric_type.Unit | Bool | Int | Mathematical_int -> true
  | Parameter binder ->
      List.exists
        (fun candidate -> Parametric_type.compare_binder candidate binder = 0)
        opaque_binders
  | Aggregate type_id ->
      nominal_domains program
      |> List.exists (fun (domain : validated_rank_domain) ->
             List.exists
               (fun identity -> identity.Issuance.rank_type_id = type_id)
               domain.component)
  | Application (constructor, arguments) ->
      let key =
        constructor.constructor_identity ^ "<"
        ^ String.concat "," (List.map Parametric_type.to_string arguments)
        ^ ">"
      in
      if List.mem key visiting then false
      else
        Option.is_some (find_capability program constructor)
        && List.for_all (actual_allowed program [] (key :: visiting)) arguments
  | Tuple _ -> false
let constructor_id type_id (constructor : Parametric_adt.constructor) =
  {
    Sst.constructor_type = type_id;
    constructor_index = constructor.constructor_index;
    constructor_name = constructor.constructor_name;
  }
let field_id type_id constructor (field : Parametric_adt.field) =
  {
    Sst.field_owner =
      Sst.Constructor_owner (constructor_id type_id constructor);
    field_index = field.field_index;
    field_name = Printf.sprintf "$arg%d" field.field_index;
  }
let application_layout capability span arguments =
  let descriptor = capability.descriptor in
  let type_id = Parametric_adt.type_id descriptor in
  let opaque_binders =
    Parametric_adt.binders descriptor
    |> List.filter (fun binder ->
           fields (Parametric_adt.kind descriptor)
           |> List.for_all (fun field ->
                  let typ = field.Parametric_adt.field_type in
                  if direct_self descriptor typ then true
                  else
                    match typ with
                    | Parametric_type.Parameter candidate ->
                      Parametric_type.compare_binder candidate binder = 0
                      || not
                           (List.exists
                              (fun parameter ->
                                Parametric_type.compare_binder parameter binder
                                = 0)
                              (Parametric_type.parameters field.field_type))
                    | Unit | Bool | Int | Mathematical_int | Tuple _ | Aggregate _
                    | Application _ ->
                        not
                          (List.exists
                             (fun parameter ->
                               Parametric_type.compare_binder parameter binder
                               = 0)
                             (Parametric_type.parameters typ))))
  in
  if List.length arguments <> List.length (Parametric_adt.binders descriptor)
  then Error { span; detail = "schema-rank application arity mismatch" }
  else if
    not
      (List.for_all2
         (fun binder argument ->
           match argument with
           | Parametric_type.Parameter _ ->
               List.exists
                 (fun candidate ->
                   Parametric_type.compare_binder candidate binder = 0)
                 opaque_binders
           | Unit | Bool | Int | Mathematical_int | Tuple _ | Aggregate _
           | Application _ ->
               actual_allowed
                 (Option.get (live_program capability.program))
                 [] [] argument)
         (Parametric_adt.binders descriptor) arguments)
  then
    Error
      {
        span;
        detail =
          "schema-rank application actual is outside the scalar, opaque \
           binder, or independently validated nominal policy";
      }
  else
    let component =
      [
        {
          Issuance.rank_type_id = type_id;
          rank_path =
            (Parametric_adt.type_constructor descriptor).constructor_path;
          rank_uid = Parametric_adt.compiler_uid descriptor;
          rank_span = span;
        };
      ]
    in
    let constructors =
      match Parametric_adt.kind descriptor with
      | Parametric_adt.Variant constructors -> constructors
      | Record _ -> []
    in
    let positive_children =
      constructors
      |> List.concat_map (fun (constructor : Parametric_adt.constructor) ->
             constructor.Parametric_adt.constructor_fields
             |> List.filter_map (fun (field : Parametric_adt.field) ->
                    if direct_self descriptor field.field_type then
                      Some
                        {
                          Issuance.rank_constructor =
                            constructor_id type_id constructor;
                          rank_constructor_uid = constructor.constructor_uid;
                          rank_field = field_id type_id constructor field;
                          rank_field_uid = field.field_uid;
                          rank_child_path = [];
                          rank_child_type = type_id;
                          rank_expansion_trace =
                            [
                              "schema:" ^ Parametric_adt.to_string descriptor;
                              "direct-uniform-self";
                            ];
                        }
                    else None))
    in
    let ground_witnesses =
      constructors
      |> List.filter_map (fun (constructor : Parametric_adt.constructor) ->
             if
               List.exists
                 (fun (field : Parametric_adt.field) ->
                   direct_self descriptor field.Parametric_adt.field_type)
                 constructor.constructor_fields
             then None
             else
               Some
                 {
                   Issuance.rank_ground_constructor =
                     constructor_id type_id constructor;
                   rank_ground_constructor_uid = constructor.constructor_uid;
                 })
    in
    Ok (component, positive_children, ground_witnesses)
let derive_application ~program ~span = function
  | Parametric_type.Application (constructor, arguments) as application -> (
      match find_capability program constructor with
      | None ->
          Error
            {
              span;
              detail =
                "parametric rank schema has no exact compiler/program \
                 capability";
            }
      | Some capability ->
          (match application_layout capability span arguments with
          | Error _ as error -> error
          | Ok (component, positive_children, ground_witnesses) ->
              let actual_evidence =
                List.map Parametric_type.to_string arguments
              in
              Ok
                (make_domain ~capability ~application ~program ~component
                   ~positive_children ~ground_witnesses ~actual_evidence ())))
  | Unit | Bool | Int | Mathematical_int | Tuple _ | Aggregate _ | Parameter _ ->
      Error { span; detail = "schema-rank view requires an exact application" }
let domain_id (domain : validated_rank_domain) = domain.domain_id
let domain_version (domain : validated_rank_domain) = domain.domain_version
let snapshot_digest (domain : validated_rank_domain) = domain.snapshot_digest
let component (domain : validated_rank_domain) = domain.component
let positive_children (domain : validated_rank_domain) =
  domain.positive_children
let ground_witnesses (domain : validated_rank_domain) =
  domain.ground_witnesses
let immutable (domain : validated_rank_domain) = domain.immutable
let application (domain : validated_rank_domain) = domain.application
let actual_arguments domain =
  match application domain with
  | Some (Parametric_type.Application (_, arguments)) -> arguments
  | Some
      (Unit | Bool | Int | Mathematical_int | Tuple _ | Aggregate _ | Parameter _)
  | None ->
      []
let authenticate_profile ~structure profile =
  profile.Issuance.rank_profile_structure == structure
  && !(profile.rank_profile_token) = ()
  && String.equal profile.rank_profile_snapshot_digest
       (Digest.to_hex
          (Digest.string (profile.rank_profile_structure_snapshot ())))
  && List.exists (fun issued -> issued == profile) !profiles
let register_profiles ~structure issued =
  profiles :=
    List.filter
      (fun profile -> profile.Issuance.rank_profile_structure != structure)
      !profiles;
  profiles := List.rev_append issued !profiles
let profiles structure =
  List.filter (authenticate_profile ~structure) !profiles |> List.rev
let claim_snapshot claim =
  [
    claim.claim_version;
    claim.provider_unit;
    claim.provider_artifact;
    claim.schema_digest;
    claim.compiler_uid;
    claim.profile_digest;
    Option.fold ~none:"-" ~some:Parametric_type.to_string claim.application;
    String.concat "\002" claim.actual_evidence;
  ]
let portable_claim ~provider_unit ~provider_artifact domain =
  let capability = Option.get domain.capability in
  let base =
    {
      claim_version = schema_version;
      provider_unit;
      provider_artifact;
      schema_digest = capability.schema_digest;
      compiler_uid = Parametric_adt.compiler_uid capability.descriptor;
      profile_digest = capability.profile_digest;
      application = domain.application;
      actual_evidence = domain.actual_evidence;
      claim_digest = "";
    }
  in
  { base with claim_digest = digest (claim_snapshot base) }
let portable_claim_digest claim = claim.claim_digest
let portable_claim_application claim = claim.application
let reauthenticate_claim ~implementation ~program ~descriptor claim =
  let span = Diagnostic.file_span implementation.Cmt_input.source_file in
  let expected_schema =
    digest
      [
        Parametric_adt.to_string descriptor;
        Parametric_adt.compiler_uid descriptor;
      ]
  in
  if not (String.equal claim.claim_version schema_version) then
    Error { span; detail = "retained schema-rank claim version mismatch" }
  else if not (String.equal claim.claim_digest (digest (claim_snapshot claim)))
  then Error { span; detail = "retained schema-rank claim digest mismatch" }
  else if
    not
      (String.equal claim.schema_digest expected_schema
      && String.equal claim.compiler_uid (Parametric_adt.compiler_uid descriptor)
      && String.equal claim.profile_digest (descriptor_profile descriptor))
  then Error { span; detail = "retained schema-rank claim schema mismatch" }
  else
    match claim.application with
    | None ->
        Error { span; detail = "retained schema-rank claim has no application" }
    | Some application ->
        let program_snapshot = Sst.to_string program in
        let* artifact =
          match artifact_identity implementation with
          | Ok artifact -> Ok artifact
          | Error detail -> Error { span; detail }
        in
        let capability =
          {
            issuer;
            token = ref ();
            artifact;
            descriptor;
            schema_digest = expected_schema;
            profile_digest = descriptor_profile descriptor;
            program = weak_program program;
            program_snapshot;
            program_digest =
              digest [ artifact_snapshot artifact; program_snapshot ];
          }
        in
        capabilities := capability :: !capabilities;
        derive_application ~program ~span application
