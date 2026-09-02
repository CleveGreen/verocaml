type conversion_identity = {
  provider_origin : string;
  callable_path : string;
  callable_uid : string;
  logical_sort_key : string;
  issuer_authority : string;
  canonical_material : string;
  checked_digest : string;
}

type application_reference =
  | Missing_reference
  | Callable_reference of {
      callable_path : string;
      callable_uid : string;
    }
  | Authenticated_reference of {
      callable_path : string;
      callable_uid : string;
      canonical_material : string;
      checked_digest : string;
    }

type rejection =
  | Missing_identity
  | Malformed_identity
  | Forged_identity
  | Ambiguous_identity
  | Invalid_semantic_boundary

let schema = "verocaml.explicit-int-narrowing-application.v1"

let frame tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let digest material =
  "narrowing-v1:" ^ Digest.to_hex (Digest.string material)

let issue_authenticated ~provider_origin ~callable_path ~callable_uid
    ~logical_sort ~issuer_authority =
  let logical_sort_key =
    Logical_sort_private.canonical_material logical_sort
  in
  if
    provider_origin = "" || callable_path = "" || callable_uid = ""
    || logical_sort_key = "" || issuer_authority = ""
  then Error "explicit narrowing conversion identity has an empty authority field"
  else
    let canonical_material =
      frame schema
        [
          provider_origin;
          callable_path;
          callable_uid;
          logical_sort_key;
          issuer_authority;
        ]
    in
    let checked_digest = digest canonical_material in
    [%log.debug "issued authenticated explicit integer narrowing identity"
      ~stage:(Delator.Field.string "integer-narrowing-boundary")
      ~schema:(Delator.Field.string schema)
      ~provider:(Delator.Field.string provider_origin)
      ~decision:(Delator.Field.string "accepted")];
    Ok
      {
        provider_origin;
        callable_path;
        callable_uid;
        logical_sort_key;
        issuer_authority;
        canonical_material;
        checked_digest;
      }

let rejection_name = function
  | Missing_identity -> "missing-identity"
  | Malformed_identity -> "malformed-identity"
  | Forged_identity -> "forged-identity"
  | Ambiguous_identity -> "ambiguous-identity"
  | Invalid_semantic_boundary -> "invalid-semantic-boundary"

let same_callable identity callable_path callable_uid =
  String.equal identity.callable_path callable_path
  && String.equal identity.callable_uid callable_uid

let valid_identity identity =
  String.equal identity.checked_digest (digest identity.canonical_material)
  &&
  String.equal identity.canonical_material
    (frame schema
       [
         identity.provider_origin;
         identity.callable_path;
         identity.callable_uid;
         identity.logical_sort_key;
         identity.issuer_authority;
       ])

let validate_application ~candidates ~reference ~source ~target =
  let reject reason
      (candidates : conversion_identity list [@log_value.debug]) =
    [%log.debug "rejected explicit integer narrowing application"
      ~stage:(Delator.Field.string "integer-narrowing-boundary")
      ~schema:(Delator.Field.string schema)
      ~candidate_count:
        (Delator.Field.int
           (List.length (candidates [@log_value.debug])))
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string (rejection_name reason))];
    Error reason
  in
  if
    source <> Parametric_type.Mathematical_int
    || target <> Parametric_type.Int
  then reject Invalid_semantic_boundary (candidates [@log_value.debug])
  else if List.exists (fun candidate -> not (valid_identity candidate)) candidates
  then reject Malformed_identity (candidates [@log_value.debug])
  else
    let candidates =
      List.sort_uniq
        (fun left right ->
          String.compare left.canonical_material right.canonical_material)
        candidates
    in
    match reference with
    | Missing_reference ->
        reject Missing_identity (candidates [@log_value.debug])
    | Callable_reference { callable_path; callable_uid } -> (
        match
          List.filter
            (fun candidate ->
              same_callable candidate callable_path callable_uid)
            candidates
        with
        | [] | [ _ ] ->
            reject Missing_identity (candidates [@log_value.debug])
        | _ :: _ :: _ ->
            reject Ambiguous_identity (candidates [@log_value.debug]))
    | Authenticated_reference
        {
          callable_path;
          callable_uid;
          canonical_material;
          checked_digest;
        } ->
        if not (String.equal checked_digest (digest canonical_material)) then
          reject Malformed_identity (candidates [@log_value.debug])
        else
          let matches =
            List.filter
              (fun candidate ->
                same_callable candidate callable_path callable_uid
                && String.equal candidate.canonical_material canonical_material
                && String.equal candidate.checked_digest checked_digest)
              candidates
          in
          (match matches with
          | [ candidate ] ->
              [%log.debug "accepted explicit integer narrowing application"
                ~stage:(Delator.Field.string "integer-narrowing-boundary")
                ~schema:(Delator.Field.string schema)
                ~provider:(Delator.Field.string candidate.provider_origin)
                ~candidate_count:(Delator.Field.int (List.length candidates))
                ~decision:(Delator.Field.string "accepted")];
              Ok candidate
          | [] -> reject Forged_identity (candidates [@log_value.debug])
          | _ :: _ :: _ ->
              reject Ambiguous_identity (candidates [@log_value.debug]))

module For_testing = struct
  let reference identity =
    Authenticated_reference
      {
        callable_path = identity.callable_path;
        callable_uid = identity.callable_uid;
        canonical_material = identity.canonical_material;
        checked_digest = identity.checked_digest;
      }

  let malformed_reference identity =
    Authenticated_reference
      {
        callable_path = identity.callable_path;
        callable_uid = identity.callable_uid;
        canonical_material = identity.canonical_material;
        checked_digest = identity.checked_digest ^ "-malformed";
      }

  let callable_reference identity =
    Callable_reference
      {
        callable_path = identity.callable_path;
        callable_uid = identity.callable_uid;
      }
end
