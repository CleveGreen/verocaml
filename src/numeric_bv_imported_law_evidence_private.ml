type semantic_authority = Checked_proof | Explicit_axiom
type trusted_dependency = {
  compiler_identity : string;
  evidence_full_key : string;
}

type t = {
  target_full_key : string;
  issuer_unit : string;
  outer_callable_uid : string;
  modular_callable_uid : string;
  outer_callable_abi : string;
  modular_callable_abi : string;
  outer_law_full_key : string;
  modular_law_full_key : string;
  carrier_binding_full_key : string;
  base_int_full_key : string;
  semantic_width : int;
  outer_authority : semantic_authority;
  modular_authority : semantic_authority;
  trusted_dependencies : trusted_dependency list;
  full_key : string;
}

let authority_key = function
  | Checked_proof -> "checked-proof"
  | Explicit_axiom -> "explicit-axiom"

let trusted_dependency_key dependency =
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-bv-imported-trusted-dependency.v1"
    [ dependency.compiler_identity; dependency.evidence_full_key ]

let identity ~target_full_key ~issuer_unit ~outer_callable_uid
    ~modular_callable_uid ~outer_callable_abi ~modular_callable_abi
    ~outer_law_full_key ~modular_law_full_key
    ~carrier_binding_full_key ~base_int_full_key ~semantic_width
    ~outer_authority ~modular_authority ~trusted_dependencies =
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-bv-imported-law-evidence.v3"
    [ target_full_key; issuer_unit; outer_callable_uid; modular_callable_uid;
      outer_callable_abi; modular_callable_abi;
      outer_law_full_key; modular_law_full_key; carrier_binding_full_key;
      base_int_full_key; string_of_int semantic_width;
      authority_key outer_authority; authority_key modular_authority;
      Numeric_receipt_private.list
        (List.map trusted_dependency_key trusted_dependencies) ]

module For_registry = struct
  let issue ~(target : Build_target_profile_private.instance) ~issuer_unit
      ~outer_callable_uid ~modular_callable_uid ~outer_callable_abi
      ~modular_callable_abi ~outer_law_full_key
      ~modular_law_full_key ~carrier_binding_full_key ~base_int_full_key
      ~semantic_width ~outer_authority ~modular_authority
      ~trusted_dependencies =
    let target_full_key = target.full_key in
    let trusted_dependencies =
      trusted_dependencies
      |> List.map (fun (compiler_identity, evidence_full_key) ->
             { compiler_identity; evidence_full_key })
      |> List.sort_uniq (fun left right ->
             String.compare (trusted_dependency_key left)
               (trusted_dependency_key right))
    in
    let full_key =
      identity ~target_full_key ~issuer_unit ~outer_callable_uid
        ~modular_callable_uid ~outer_callable_abi ~modular_callable_abi
        ~outer_law_full_key ~modular_law_full_key
        ~carrier_binding_full_key ~base_int_full_key ~semantic_width
        ~outer_authority ~modular_authority ~trusted_dependencies
    in
    let evidence =
      { target_full_key; issuer_unit; outer_callable_uid; modular_callable_uid;
      outer_callable_abi; modular_callable_abi;
      outer_law_full_key; modular_law_full_key; carrier_binding_full_key;
      base_int_full_key; semantic_width; outer_authority; modular_authority;
      trusted_dependencies; full_key }
    in
    [%log.debug "issued imported BV law evidence from admitted descriptors"
      ~stage:(Delator.Field.string "numeric-bv-imported-law-evidence")
      ~provider:(Delator.Field.string issuer_unit)
      ~semantic_width:(Delator.Field.int semantic_width)
      ~admitted_descriptors:(Delator.Field.int 2)
      ~decision:(Delator.Field.string "sealed-law-pair")];
    evidence
end

let validate ~(target : Build_target_profile_private.instance) evidence =
  let expected =
    identity ~target_full_key:evidence.target_full_key
      ~issuer_unit:evidence.issuer_unit
      ~outer_callable_uid:evidence.outer_callable_uid
      ~modular_callable_uid:evidence.modular_callable_uid
      ~outer_callable_abi:evidence.outer_callable_abi
      ~modular_callable_abi:evidence.modular_callable_abi
      ~outer_law_full_key:evidence.outer_law_full_key
      ~modular_law_full_key:evidence.modular_law_full_key
      ~carrier_binding_full_key:evidence.carrier_binding_full_key
      ~base_int_full_key:evidence.base_int_full_key
      ~semantic_width:evidence.semantic_width
      ~outer_authority:evidence.outer_authority
      ~modular_authority:evidence.modular_authority
      ~trusted_dependencies:evidence.trusted_dependencies
  in
  let result =
    if not (String.equal target.full_key evidence.target_full_key) then
      Error "imported BV law evidence belongs to another exact target"
    else if not (String.equal expected evidence.full_key) then
      Error "imported BV law evidence identity changed before source admission"
    else Ok ()
  in
  [%log.trace "validated sealed imported BV law evidence"
    ~stage:(Delator.Field.string "numeric-bv-imported-law-evidence")
    ~provider:(Delator.Field.string evidence.issuer_unit)
    ~semantic_width:(Delator.Field.int evidence.semantic_width)
    ~admitted:(Delator.Field.bool (Result.is_ok result))
    ~trusted_dependencies:
      (Delator.Field.int (List.length evidence.trusted_dependencies))
    ~decision:
      (Delator.Field.string
         (match result with Ok () -> "consume" | Error _ -> "reject"))];
  result
[@@delator.instrument] [@@delator.level trace]

let issuer_unit evidence = evidence.issuer_unit
let outer_callable_uid evidence = evidence.outer_callable_uid
let modular_callable_uid evidence = evidence.modular_callable_uid
let outer_callable_abi evidence = evidence.outer_callable_abi
let modular_callable_abi evidence = evidence.modular_callable_abi
let outer_law_full_key evidence = evidence.outer_law_full_key
let modular_law_full_key evidence = evidence.modular_law_full_key
let carrier_binding_full_key evidence = evidence.carrier_binding_full_key
let base_int_full_key evidence = evidence.base_int_full_key
let semantic_width evidence = evidence.semantic_width
let outer_authority evidence = evidence.outer_authority
let modular_authority evidence = evidence.modular_authority
let trusted_dependencies evidence = evidence.trusted_dependencies
let full_key evidence = evidence.full_key
