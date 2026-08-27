(* VERO-065 finite-domain module: begin *)
module Finite_domain = struct
  type program_identity = {
    unit_identity : string;
    cmt_identity : string;
    family_identity : string;
    program_snapshot : string;
  }

  type rank_snapshot = {
    domain_id : string;
    domain_version : string;
    domain_digest : string;
    component_snapshot : string list;
    profile_actual_snapshot : string list;
  }

  type identity = {
    program : program_identity;
    callable : string;
    value : Vir.aggregate_term;
    mode : Sst.instance_mode;
    typ : Sst.typ;
    rank : rank_snapshot;
    path_snapshot : string;
  }

  type origin = Recursive_spec_preservation.Finite_expression.origin =
    | Exact_formal
    | Exact_alias
    | Immutable_construction
    | Immutable_projection
    | Immutable_pattern
    | Finite_branch_join
    | Completed_local_summary
    | Smaller_direct_call
    | Immutable_recursive_spec

  type fact = {
    issuer : unit ref;
    session : unit ref;
    token : unit ref;
    identity : identity;
    origin : origin;
  }

  let private_issuer = ref ()

  let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
    left.type_index = right.type_index
    && String.equal left.type_name right.type_name

  let immutable_field (field : Sst.field_definition) =
    field.field_mutability = Sst.Immutable_field
    && field.field_modalities.uniqueness_modality <> Sst.Force_aliased

  let fields_of_definition (definition : Sst.type_definition) =
    match definition.type_kind with
    | Sst.Record_definition fields -> fields
    | Sst.Variant_definition constructors ->
        List.concat_map
          (fun (constructor : Sst.constructor_definition) ->
            constructor.constructor_fields)
          constructors

  let deeply_immutable_type definitions typ =
    let rec classify visited = function
      | Sst.Unit | Sst.Bool | Sst.Int -> true
      | Sst.Parameter _ | Sst.Application _ -> false
      | Sst.Tuple components ->
          List.for_all (fun (_, component) -> classify visited component) components
      | Sst.Aggregate id when List.exists (same_type_id id) visited -> true
      | Sst.Aggregate id -> (
          match
            List.find_opt
              (fun (definition : Sst.type_definition) ->
                same_type_id definition.type_id id)
              definitions
          with
          | None -> false
          | Some
              {
                representation =
                  Sst.Abstract_with_evidence
                    ( Sst.Incomplete_abstraction_evidence _
                    | Sst.Proposed_same_cmt_abstraction _ );
                _;
              } ->
              false
          | Some definition ->
              fields_of_definition definition
              |> List.for_all (fun field ->
                     immutable_field field
                     && classify (id :: visited) field.field_type))
    in
    classify [] typ

  let same_program left right =
    String.equal left.unit_identity right.unit_identity
    && String.equal left.cmt_identity right.cmt_identity
    && String.equal left.family_identity right.family_identity
    && String.equal left.program_snapshot right.program_snapshot

  let same_rank left right =
    String.equal left.domain_id right.domain_id
    && String.equal left.domain_version right.domain_version
    && String.equal left.domain_digest right.domain_digest
    && left.component_snapshot = right.component_snapshot
    && left.profile_actual_snapshot = right.profile_actual_snapshot

  let same_identity left right =
    same_program left.program right.program
    && String.equal left.callable right.callable
    && left.value = right.value
    && left.mode = right.mode
    && left.typ = right.typ
    && same_rank left.rank right.rank
    && String.equal left.path_snapshot right.path_snapshot

  let issue ~session ~identity ~origin =
    { issuer = private_issuer; session; token = ref (); identity; origin }

  let valid session fact =
    fact.issuer == private_issuer && fact.session == session

  let authenticate ~session ~facts ~identity =
    match
      List.find_opt
        (fun fact -> valid session fact && same_identity fact.identity identity)
        facts
    with
    | Some fact -> Ok fact
    | None ->
        Error
          "exact persistent finite fact is unavailable for this \
           session/program/callable/value/path/mode/type/rank/profile"

  let belongs_to_session session fact = valid session fact
  let identity fact = fact.identity
  let origin fact = fact.origin
  let same_fact left right = left.token == right.token
end
(* VERO-065 finite-domain module: end *)

(* VERO-065 direct-candidate module: begin *)
module Direct_candidate = struct
  type candidate = {
    issuer : unit ref;
    session : unit ref;
    token : unit ref;
    callable : string;
    body_snapshot : string;
    profile_snapshot : string;
    mutable exits : (string * string * string) list;
  }

  type obligation_manifest = {
    issuer : unit ref;
    session : unit ref;
    token : unit ref;
    candidate : candidate;
    exit_fingerprint : string;
    obligation_fingerprints : string list;
  }

  type completion = {
    issuer : unit ref;
    session : unit ref;
    token : unit ref;
    manifest : obligation_manifest;
  }

  type publication = {
    issuer : unit ref;
    session : unit ref;
    token : unit ref;
    candidate : candidate;
  }

  type consumption = {
    issuer : unit ref;
    session : unit ref;
    token : unit ref;
    publication : publication;
    caller_snapshot : string;
    call_path_digest : string;
    result_snapshot : string;
  }

  type counters = {
    exits_recorded : int;
    candidates_completed : int;
    candidates_published : int;
    consumption_attempts : int;
    consumptions : int;
  }

  type mutable_counters = {
    mutable exits_recorded : int;
    mutable candidates_completed : int;
    mutable candidates_published : int;
    mutable consumption_attempts : int;
    mutable consumptions : int;
  }

  type t = {
    issuer : unit ref;
    session : unit ref;
    mutable active : bool;
    mutable candidates : candidate list;
    mutable manifests : obligation_manifest list;
    mutable completions : completion list;
    mutable publications : publication list;
    mutable consumed : consumption list;
    counters : mutable_counters;
  }

  let private_issuer = ref ()

  let exit_fingerprint exits =
    Marshal.to_string (List.sort compare exits) [ Marshal.No_sharing ]
    |> Digest.string |> Digest.to_hex

  let create ~session =
    {
      issuer = private_issuer;
      session;
      active = true;
      candidates = [];
      manifests = [];
      completions = [];
      publications = [];
      consumed = [];
      counters =
        {
          exits_recorded = 0;
          candidates_completed = 0;
          candidates_published = 0;
          consumption_attempts = 0;
          consumptions = 0;
        };
    }

  let destroy lifecycle =
    lifecycle.active <- false;
    lifecycle.candidates <- [];
    lifecycle.manifests <- [];
    lifecycle.completions <- [];
    lifecycle.publications <- [];
    lifecycle.consumed <- []

  let register lifecycle ~callable ~body_snapshot ~profile_snapshot =
    if not lifecycle.active then Error "finite summary lifecycle is destroyed"
    else if callable = "" || body_snapshot = "" || profile_snapshot = "" then
      Error "direct finite candidate identity is incomplete"
    else
      match
        List.find_opt
          (fun (candidate : candidate) ->
            String.equal candidate.callable callable)
          lifecycle.candidates
      with
      | Some candidate
        when String.equal candidate.body_snapshot body_snapshot
             && String.equal candidate.profile_snapshot profile_snapshot ->
          Ok candidate
      | Some _ -> Error "direct finite candidate registration is stale"
      | None ->
          let candidate =
            {
              issuer = private_issuer;
              session = lifecycle.session;
              token = ref ();
              callable;
              body_snapshot;
              profile_snapshot;
              exits = [];
            }
          in
          lifecycle.candidates <- candidate :: lifecycle.candidates;
          Ok candidate

  let registered lifecycle (candidate : candidate) =
    lifecycle.active && lifecycle.issuer == private_issuer
    && candidate.issuer == private_issuer
    && candidate.session == lifecycle.session
    && List.exists
         (fun (registered : candidate) -> registered.token == candidate.token)
         lifecycle.candidates

  let record_exit lifecycle (candidate : candidate) ~path_digest ~result_snapshot
      ~fact_snapshot =
    if not (registered lifecycle candidate) then
      Error "direct finite candidate is not authenticated"
    else if path_digest = "" || result_snapshot = "" || fact_snapshot = "" then
      Error "finite summary exit lacks exact path/result/fact provenance"
    else if
      List.exists
        (fun (manifest : obligation_manifest) ->
          manifest.candidate.token == candidate.token)
        lifecycle.manifests
    then Error "direct finite candidate exits are already sealed"
    else if
      List.exists
        (fun (path, _, _) -> String.equal path path_digest)
        candidate.exits
    then Error "finite summary exit path is duplicated"
    else (
      candidate.exits <-
        (path_digest, result_snapshot, fact_snapshot) :: candidate.exits;
      lifecycle.counters.exits_recorded <-
        lifecycle.counters.exits_recorded + 1;
      Ok ())

  let authorize_obligations lifecycle (candidate : candidate)
      ~obligation_fingerprints =
    if not (registered lifecycle candidate) then
      Error "direct finite candidate is not authenticated"
    else if candidate.exits = [] then Ok None
    else if
      List.exists
        (fun (manifest : obligation_manifest) ->
          manifest.candidate.token == candidate.token)
        lifecycle.manifests
    then Error "direct finite candidate obligations are already authorized"
    else
      let manifest =
        {
          issuer = private_issuer;
          session = lifecycle.session;
          token = ref ();
          candidate;
          exit_fingerprint = exit_fingerprint candidate.exits;
          obligation_fingerprints;
        }
      in
      lifecycle.manifests <- manifest :: lifecycle.manifests;
      Ok (Some manifest)

  let registered_manifest lifecycle (manifest : obligation_manifest) =
    manifest.issuer == private_issuer && manifest.session == lifecycle.session
    && List.exists
         (fun (candidate : obligation_manifest) ->
           candidate.token == manifest.token)
         lifecycle.manifests

  let complete lifecycle (manifest : obligation_manifest)
      ~completed_fingerprints ~all_verified =
    if not lifecycle.active then Error "finite summary lifecycle is destroyed"
    else if not (registered_manifest lifecycle manifest) then
      Error "finite summary obligation manifest is not authenticated"
    else if
      not
        (String.equal manifest.exit_fingerprint
           (exit_fingerprint manifest.candidate.exits))
    then Error "direct finite candidate exit set changed after authorization"
    else if manifest.obligation_fingerprints <> completed_fingerprints then
      Error "finite summary obligation set is incomplete or mismatched"
    else if not all_verified then
      Error "finite summary obligation set did not complete as Verified"
    else
      let completed =
        {
          issuer = private_issuer;
          session = lifecycle.session;
          token = ref ();
          manifest;
        }
      in
      lifecycle.completions <- completed :: lifecycle.completions;
      lifecycle.counters.candidates_completed <-
        lifecycle.counters.candidates_completed + 1;
      Ok completed

  let registered_completion lifecycle (completed : completion) =
    completed.issuer == private_issuer
    && completed.session == lifecycle.session
    && List.exists
         (fun (candidate : completion) -> candidate.token == completed.token)
         lifecycle.completions

  let publish lifecycle (completed : completion) =
    if not lifecycle.active then Error "finite summary lifecycle is destroyed"
    else if not (registered_completion lifecycle completed) then
      Error "direct finite completion is not authenticated"
    else if
      List.exists
        (fun (publication : publication) ->
          publication.candidate.token == completed.manifest.candidate.token)
        lifecycle.publications
    then Error "direct finite candidate was already published"
    else
      let publication =
        {
          issuer = private_issuer;
          session = lifecycle.session;
          token = ref ();
          candidate = completed.manifest.candidate;
        }
      in
      lifecycle.publications <- publication :: lifecycle.publications;
      lifecycle.counters.candidates_published <-
        lifecycle.counters.candidates_published + 1;
      Ok publication

  let consume lifecycle (candidate : candidate) ~caller_snapshot ~call_path_digest
      ~result_snapshot =
    lifecycle.counters.consumption_attempts <-
      lifecycle.counters.consumption_attempts + 1;
    if not (registered lifecycle candidate) then
      Error "direct finite candidate is not authenticated"
    else
      match
        List.find_opt
          (fun (publication : publication) ->
            publication.candidate.token == candidate.token)
          lifecycle.publications
      with
      | None -> Ok None
      | Some publication ->
          if
            publication.issuer != private_issuer
            || publication.session != lifecycle.session
          then Error "finite summary publication is not authenticated"
          else if
            List.exists
              (fun consumed ->
                consumed.publication.token == publication.token
                && String.equal consumed.caller_snapshot caller_snapshot
                && String.equal consumed.call_path_digest call_path_digest
                && String.equal consumed.result_snapshot result_snapshot)
              lifecycle.consumed
          then Error "finite summary call instance was already consumed"
          else
            let consumption =
              {
                issuer = private_issuer;
                session = lifecycle.session;
                token = ref ();
                publication;
                caller_snapshot;
                call_path_digest;
                result_snapshot;
              }
            in
            lifecycle.consumed <-
              consumption :: lifecycle.consumed;
            lifecycle.counters.consumptions <-
              lifecycle.counters.consumptions + 1;
            Ok (Some consumption)

  let callable (candidate : candidate) = candidate.callable

  let authenticate_consumption ~session (consumption : consumption) =
    consumption.issuer == private_issuer
    && consumption.session == session
    && consumption.token != session
    && consumption.publication.issuer == private_issuer
    && consumption.publication.session == session

  let consumption_token (consumption : consumption) = consumption.token

  let counters lifecycle : counters =
    {
      exits_recorded = lifecycle.counters.exits_recorded;
      candidates_completed = lifecycle.counters.candidates_completed;
      candidates_published = lifecycle.counters.candidates_published;
      consumption_attempts = lifecycle.counters.consumption_attempts;
      consumptions = lifecycle.counters.consumptions;
    }
end
(* VERO-065 direct-candidate module: end *)


type program_identity = Finite_domain.program_identity = {
  unit_identity : string;
  cmt_identity : string;
  family_identity : string;
  program_snapshot : string;
}

type rank_snapshot = Finite_domain.rank_snapshot = {
  domain_id : string;
  domain_version : string;
  domain_digest : string;
  component_snapshot : string list;
  profile_actual_snapshot : string list;
}

type construction_shape =
  | Record_construction of Sst.type_id
  | Constructor_construction of Sst.constructor_id

type construction_provenance =
  | Closed_logical_construction of construction_shape
  | Local_exec_construction of construction_shape
  | Local_tracked_construction of construction_shape

type child_provenance =
  | Immutable_constructor_field
  | Immutable_record_field
  | Successful_pattern

type path_step = {
  owner_namespace : string;
  owner_type : Vir.aggregate_type;
  ordinal : int;
  selector_name : string;
  selector_path : int list;
  child_type : Vir.aggregate_type;
  child_mode : Sst.instance_mode;
  rank_digest : string;
  provenance : child_provenance;
}

type value_identity = {
  callable : string;
  allocation : int;
  version : int;
  path : path_step list;
  mode : Sst.instance_mode;
  typ : Sst.typ;
  value : Vir.aggregate_term;
}

type formal_slot = {
  slot_token : unit ref;
  slot_session : unit ref;
  slot_program : program_identity;
  slot_callee : string;
  slot_ordinal : int;
  slot_label : string option;
  slot_pattern_digest : string;
  slot_binding_ids : int list;
  slot_mode : Sst.instance_mode;
  slot_type : Sst.typ;
  slot_rank : rank_snapshot;
  slot_requirement_digest : string;
  mutable slot_entry_value : Vir.aggregate_term option;
}

type formal_assumption = {
  assumption_token : unit ref;
  assumption_slot : formal_slot;
  assumption_value : Vir.aggregate_term;
}

type provenance =
  | Constructed of receipt list
  | Derived of receipt * path_step
  | Formal of formal_slot * formal_assumption
  | Promoted of receipt * string
  | Induction of string * (Vir.aggregate_term * receipt) list * string
  | Published of string * Direct_candidate.consumption * unit ref * string * Diagnostic.span * string
  | Recursive_spec of Recursive_spec_preservation.capability * Sst.program * Sst.function_definition * Sst.function_id * Diagnostic.span * string * Recursive_spec_application_identity.t * (int * Vir.aggregate_term * receipt) list

and receipt = {
  issuer : unit ref;
  session : unit ref;
  program : program_identity;
  identity : value_identity;
  rank : rank_snapshot;
  fact : Finite_domain.fact;
  provenance : provenance;
}

type transfer_row = {
  transfer_slot : formal_slot;
  transfer_receipt : receipt;
  transfer_value : Vir.aggregate_term;
}

type transfer_batch = {
  batch_token : unit ref;
  batch_session : unit ref;
  batch_caller : string;
  batch_callee : string;
  batch_path : string;
  batch_rows : transfer_row list;
  mutable batch_consumed : bool;
}

type proof_call_visit = {
  visit_token : unit ref;
  visit_session : unit ref;
  visit_callable : string;
  visit_parent : receipt;
  visit_child : receipt;
  visit_selector : Vir.selector;
  visit_rank : rank_snapshot;
  visit_branch_digest : string;
  visit_call_span : Diagnostic.span;
}

type proof_call_summary = {
  summary_token : unit ref;
  summary_session : unit ref;
  summary_visit : proof_call_visit;
}

type counters = {
  witness_issuances : int;
  parent_issuances : int;
  child_derivations : int;
  result_promotions : int;
  result_consumptions : int;
  consumptions : int;
  formal_assumption_issuances : int;
  formal_transfer_batches : int;
  formal_transfers : int;
  formal_transfer_consumptions : int;
  proof_call_visits : int;
  proof_call_summaries : int;
  recursive_spec_result_issuances : int;
  recursive_spec_result_consumptions : int;
}

type mutable_counters = {
  mutable witness_issuances : int;
  mutable parent_issuances : int;
  mutable child_derivations : int;
  mutable result_promotions : int;
  mutable result_consumptions : int;
  mutable consumptions : int;
  mutable formal_assumption_issuances : int;
  mutable formal_transfer_batches : int;
  mutable formal_transfers : int;
  mutable formal_transfer_consumptions : int;
  mutable proof_call_visits : int;
  mutable proof_call_summaries : int;
  mutable recursive_spec_result_issuances : int;
  mutable recursive_spec_result_consumptions : int;
}

type t = {
  issuer : unit ref;
  session : unit ref;
  program : program_identity;
  types : Sst.type_definition list;
  mutable active : bool;
  mutable next_version : int;
  mutable receipts : receipt list;
  mutable formal_slots : formal_slot list;
  mutable transfer_batches : transfer_batch list;
  mutable proof_call_visits : proof_call_visit list;
  mutable proof_call_summaries : proof_call_summary list;
  counters : mutable_counters;
}

let private_issuer = ref ()
let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let empty_counters () =
  {
    witness_issuances = 0; parent_issuances = 0; child_derivations = 0;
    result_promotions = 0; result_consumptions = 0; consumptions = 0;
    formal_assumption_issuances = 0; formal_transfer_batches = 0;
    formal_transfers = 0; formal_transfer_consumptions = 0;
    proof_call_visits = 0; proof_call_summaries = 0;
    recursive_spec_result_issuances = 0;
    recursive_spec_result_consumptions = 0;
  }

let create program ~session ~types =
  {
    issuer = private_issuer; session; program; types; active = true;
    next_version = 0; receipts = []; formal_slots = []; transfer_batches = [];
    proof_call_visits = []; proof_call_summaries = [];
    counters = empty_counters ();
  }

let destroy registry =
  registry.active <- false;
  registry.receipts <- [];
  registry.formal_slots <- [];
  registry.transfer_batches <- [];
  registry.proof_call_visits <- [];
  registry.proof_call_summaries <- []

let is_active registry = registry.active
let same_program = Finite_domain.same_program
let same_rank = Finite_domain.same_rank
let same_type left right = left = right

let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index && String.equal left.type_name right.type_name

let component_aggregate_type rank aggregate =
  let base_name =
    match String.index_opt aggregate.Vir.aggregate_type_name '<' with
    | None -> aggregate.aggregate_type_name
    | Some index ->
        String.sub aggregate.aggregate_type_name 0 index
  in
  let prefix =
    Printf.sprintf "%s#%d|" base_name
      aggregate.aggregate_type_index
  in
  List.exists (String.starts_with ~prefix) rank.component_snapshot

let component_aggregate rank aggregate =
  component_aggregate_type rank aggregate.Vir.aggregate_type

let exact_aggregate_type rank typ aggregate =
  match typ with
  | Sst.Aggregate id ->
      id.type_index = aggregate.Vir.aggregate_type.aggregate_type_index
      && String.equal id.type_name aggregate.aggregate_type.aggregate_type_name
  | Sst.Application (_, arguments) ->
      arguments = aggregate.aggregate_type.aggregate_type_arguments
      && component_aggregate rank aggregate
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
  | Sst.Parameter _ -> false

let root_symbol value =
  let rec root path value =
    match value.Vir.aggregate_desc with
    | Vir.Aggregate_symbol symbol -> Ok (symbol, List.rev path)
    | Vir.Aggregate_selector (selector, parent) -> root (selector :: path) parent
    | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
    | Vir.Aggregate_conditional _ | Vir.Aggregate_imported_model_application _
    | Vir.Aggregate_recursive_spec_application _
    | Vir.Aggregate_symbolic_application _ ->
        Error "aggregate value is not rooted in a materialized local symbol"
  in
  root [] value

let path_snapshot path =
  Marshal.to_string path [ Marshal.No_sharing ] |> Digest.string |> Digest.to_hex

let domain_identity registry identity rank =
  {
    Finite_domain.program = registry.program;
    callable = identity.callable;
    value = identity.value;
    mode = identity.mode;
    typ = identity.typ;
    rank;
    path_snapshot = path_snapshot identity.path;
  }

let next_identity registry ~callable ~value ~mode ~typ =
  match root_symbol value with
  | Error _ as error -> error
  | Ok ((symbol : Vir.symbol), selectors) ->
      let version = registry.next_version in
      registry.next_version <- version + 1;
      let path =
        List.map
          (fun (selector : Vir.selector) ->
            {
              owner_namespace = selector.selector_namespace;
              owner_type = selector.selector_domain;
              ordinal = selector.selector_index;
              selector_name = selector.selector_name;
              selector_path = selector.selector_path;
              child_type =
                (match selector.selector_range with
                | Vir.Aggregate aggregate -> aggregate
                | Vir.Integer | Vir.Boolean | Vir.Parametric _ -> assert false);
              child_mode = mode;
              rank_digest = "";
              provenance = Immutable_record_field;
            })
          selectors
      in
      Ok { callable; allocation = symbol.symbol_id; version; path; mode; typ; value }

let issue_receipt registry ~identity ~rank ~origin ~provenance =
  let fact =
    Finite_domain.issue ~session:registry.session
      ~identity:(domain_identity registry identity rank) ~origin
  in
  let receipt =
    { issuer = private_issuer; session = registry.session; program = registry.program;
      identity; rank; fact; provenance }
  in
  registry.receipts <- receipt :: registry.receipts;
  receipt

let valid_path_step step =
  step.owner_namespace <> ""
  && step.owner_type.Vir.aggregate_type_index >= 0
  && step.owner_type.aggregate_type_name <> ""
  && step.ordinal >= 0 && step.selector_name <> ""
  && List.for_all (fun ordinal -> ordinal >= 0) step.selector_path
  && step.child_type.aggregate_type_index >= 0
  && step.child_type.aggregate_type_name <> ""
  && step.rank_digest <> ""
  &&
  match (step.child_mode, step.provenance) with
  | (Sst.Exec_instance | Sst.Tracked_instance | Sst.Ghost_instance),
    (Immutable_constructor_field | Immutable_record_field | Successful_pattern) ->
      true

let rec valid_receipt (registry : t) (receipt : receipt) =
  let _allocation_identity = receipt.identity.allocation in
  registry.active && registry.issuer == private_issuer
  && receipt.issuer == private_issuer && receipt.session == registry.session
  && same_program receipt.program registry.program
  && receipt.identity.version >= 0
  && List.for_all valid_path_step receipt.identity.path
  && Finite_domain.belongs_to_session registry.session receipt.fact
  && Finite_domain.same_identity
       (Finite_domain.identity receipt.fact)
       (domain_identity registry receipt.identity receipt.rank)
  && List.exists (( == ) receipt) registry.receipts
  &&
  match receipt.provenance with
  | Constructed children -> List.for_all (valid_receipt registry) children
  | Derived (parent, step) ->
      valid_receipt registry parent
      && receipt.identity.path = parent.identity.path @ [ step ]
  | Formal (slot, assumption) ->
      slot.slot_session == registry.session && assumption.assumption_slot == slot
      && assumption.assumption_token != registry.session
      && assumption.assumption_value = receipt.identity.value
      && slot.slot_entry_value = Some receipt.identity.value
  | Promoted (source, path) -> valid_receipt registry source && path <> ""
  | Induction (hypothesis_snapshot, actuals, path) ->
    path <> "" && hypothesis_snapshot <> ""
      && List.for_all
           (fun (actual, source) -> actual = source.identity.value && valid_receipt registry source)
           actuals
  | Published (candidate, publication, call, caller, span, path) ->
      candidate <> ""
      && Direct_candidate.authenticate_consumption ~session:registry.session
           publication
      && call != registry.session
      && caller <> "" && span.Diagnostic.file <> "" && path <> ""
  | Recursive_spec (capability, program, definition, caller, span, path,
      application, actuals) ->
      caller.function_index >= 0 && span.Diagnostic.file <> "" && path <> ""
      && Recursive_spec_application_identity.authenticate application
      && Recursive_spec_preservation.authenticate capability ~program ~definition
           ~result_type:receipt.identity.typ ~rank_domain_id:receipt.rank.domain_id
           ~rank_domain_version:receipt.rank.domain_version
           ~rank_domain_digest:receipt.rank.domain_digest
      && List.for_all
           (fun (_, actual, source) -> actual = source.identity.value && valid_receipt registry source)
           actuals

let receipt_matches_value receipt value = receipt.identity.value = value
let same_receipt left right = left == right

let authenticate registry ~facts ~callable ~value ~mode ~typ ~rank =
  if not registry.active then Error "finite registry is destroyed"
  else
    match List.find_opt
      (fun receipt ->
        valid_receipt registry receipt && List.exists (( == ) receipt) facts
        && String.equal receipt.identity.callable callable
        && receipt.identity.value = value && receipt.identity.mode = mode
        && same_type receipt.identity.typ typ && same_rank receipt.rank rank)
      facts
    with
    | Some receipt -> Ok receipt
    | None ->
        Error "exact persistent finite fact is unavailable for this session/program/callable/value/path/mode/type/rank/profile"

let authenticate_fact registry fact =
  if
    List.exists
      (fun receipt ->
        valid_receipt registry receipt
        && Finite_domain.same_fact receipt.fact fact)
      registry.receipts
  then Ok ()
  else Error "exact fact is stale, foreign, or detached from its value receipt"

let definition registry type_id =
  List.find_opt
    (fun (definition : Sst.type_definition) ->
      same_type_id definition.type_id type_id)
    registry.types

let construction_fields registry = function
  | Record_construction type_id -> (
      match definition registry type_id with
      | Some { type_kind = Sst.Record_definition fields; _ }
        when List.for_all Finite_domain.immutable_field fields ->
          Some (type_id, fields)
      | Some _ | None -> None)
  | Constructor_construction constructor -> (
      match definition registry constructor.constructor_type with
      | Some { type_kind = Sst.Variant_definition constructors; _ } ->
          (match
             List.find_opt
               (fun candidate -> candidate.Sst.constructor_id = constructor)
               constructors
           with
          | Some candidate
            when
              List.for_all Finite_domain.immutable_field
                candidate.constructor_fields
            ->
              Some (constructor.constructor_type, candidate.constructor_fields)
          | Some _ | None -> None)
      | Some _ | None -> None)

let component_type rank type_id =
  let prefix = Printf.sprintf "%s#%d|" type_id.Sst.type_name type_id.type_index in
  List.exists (String.starts_with ~prefix) rank.component_snapshot

let rec ranked_types registry rank visited typ =
  match typ with
  | Sst.Unit | Sst.Bool | Sst.Int
  | Sst.Parameter _ -> []
  | Sst.Application _ ->
      if rank.component_snapshot <> [] && rank.profile_actual_snapshot <> []
      then [ typ ]
      else []
  | Sst.Tuple components ->
      List.concat_map
        (fun (_, component) -> ranked_types registry rank visited component)
        components
  | Sst.Aggregate type_id when component_type rank type_id -> [ typ ]
  | Sst.Aggregate type_id ->
      if List.exists (same_type_id type_id) visited then []
      else
        match definition registry type_id with
        | None -> []
        | Some definition ->
            let fields =
              match definition.type_kind with
              | Sst.Record_definition fields -> fields
              | Sst.Variant_definition constructors ->
                  List.concat_map
                    (fun constructor -> constructor.Sst.constructor_fields)
                    constructors
            in
            fields |> List.filter Finite_domain.immutable_field
            |> List.concat_map (fun field ->
                   ranked_types registry rank (type_id :: visited)
                     field.Sst.field_type)

let rec typ_key = function
  | Sst.Unit -> "unit"
  | Sst.Bool -> "bool"
  | Sst.Int -> "int"
  | Sst.Tuple components ->
      "tuple:" ^ String.concat "," (List.map (fun (_, typ) -> typ_key typ) components)
  | Sst.Aggregate id -> Printf.sprintf "aggregate:%s#%d" id.type_name id.type_index
  | Sst.Application _ as typ ->
      "application:" ^ Parametric_type.to_string typ
  | Sst.Parameter _ ->
      invalid_arg "parametric parameters cannot receive finite authority"

let valid_construction registry ~mode ~typ ~rank ~provenance ~children =
  let shape, expected_mode =
    match provenance with
    | Closed_logical_construction shape -> shape, Sst.Ghost_instance
    | Local_exec_construction shape -> shape, Sst.Exec_instance
    | Local_tracked_construction shape -> shape, Sst.Tracked_instance
  in
  match typ, construction_fields registry shape with
  | Sst.Application _, _
    when mode = expected_mode && rank.component_snapshot <> []
         && rank.profile_actual_snapshot <> [] ->
      true
  | Sst.Aggregate type_id, Some (owner, fields)
    when same_type_id type_id owner && mode = expected_mode
         && (Finite_domain.deeply_immutable_type registry.types typ
            || (rank.component_snapshot <> []
               && rank.profile_actual_snapshot <> []
               && List.for_all Finite_domain.immutable_field fields)) ->
      let expected =
        fields |> List.concat_map (fun field -> ranked_types registry rank [] field.Sst.field_type)
        |> List.map typ_key |> List.sort String.compare
      in
      let actual =
        List.map (fun (child_type, _, _) -> typ_key child_type) children
        |> List.sort String.compare
      in
      expected = actual
  | _ -> false

let issue_parent registry ~callable ~value ~mode ~typ ~rank ~provenance
    ~required_children =
  if not registry.active then Error "finite registry is destroyed"
  else if not (exact_aggregate_type rank typ value) then
    Error "finite parent aggregate type mismatch"
  else
    match next_identity registry ~callable ~value ~mode ~typ with
    | Error _ -> Error "finite parent is not rooted in an exact local symbol"
    | Ok identity ->
        let issue origin =
          Finite_domain.issue ~session:registry.session
            ~identity:(domain_identity registry identity rank) ~origin
        in
        let premises =
          List.map
            (fun (_, _, receipt) ->
              match receipt with
              | Some receipt ->
                  Recursive_spec_preservation.Finite_expression.Exact_fact
                    receipt.fact
              | None ->
                  Recursive_spec_preservation.Finite_expression.Missing_fact
                    "one immutable constructor/record child has no exact finite fact")
            required_children
        in
        let source =
          match provenance with
          | Closed_logical_construction (Constructor_construction _)
          | Local_exec_construction (Constructor_construction _)
          | Local_tracked_construction (Constructor_construction _) ->
              Recursive_spec_preservation.Finite_expression.Immutable_constructor
                ((), premises)
          | Closed_logical_construction (Record_construction _)
          | Local_exec_construction (Record_construction _)
          | Local_tracked_construction (Record_construction _) ->
              Recursive_spec_preservation.Finite_expression.Immutable_record
                ((), premises)
        in
        let construction_authenticated =
          valid_construction registry ~mode ~typ ~rank ~provenance
            ~children:required_children
          && List.for_all
               (fun (child_type, child_value, child) ->
                 match child with
                 | None -> true
                 | Some child ->
                     valid_receipt registry child
                     && child.identity.value = child_value
                     && String.equal child.identity.callable callable
                     && same_type child.identity.typ child_type
                     && same_rank child.rank rank)
               required_children
        in
        let callbacks =
          {
            Recursive_spec_preservation.Finite_expression.authenticate_fact =
              authenticate_fact registry;
            authenticate_node =
              (fun () ->
                if construction_authenticated then Ok ()
                else
                  Error
                    "construction owner, immutable fields, child identities, \
                     mode, type, or rank are not authenticated");
            authenticate_origin =
              (fun _ _ _ ->
                Error "construction cannot use projection/pattern authority");
            authenticate_authority =
              (fun _ _ -> Error "construction cannot use summary authority");
            issue;
          }
        in
        let* derivation =
          Recursive_spec_preservation.Finite_expression.derive source ~callbacks
        in
        let receipt =
          { issuer = private_issuer; session = registry.session; program = registry.program;
            identity; rank; fact = Recursive_spec_preservation.Finite_expression.fact derivation;
            provenance =
              Constructed
                (List.filter_map
                   (fun (_, _, receipt) -> receipt)
                   required_children) }
        in
        registry.receipts <- receipt :: registry.receipts;
        registry.counters.witness_issuances <- registry.counters.witness_issuances + 1;
        registry.counters.parent_issuances <- registry.counters.parent_issuances + 1;
        Ok receipt

let rec type_at_path typ = function
  | [] -> Some typ
  | index :: rest -> (
      match typ with
      | Sst.Tuple components -> (
          match List.nth_opt components index with
          | Some (_, component) -> type_at_path component rest
          | None -> None)
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Aggregate _
      | Sst.Parameter _ | Sst.Application _ -> None)

let vir_type = function
  | Sst.Int -> Some Vir.Integer
  | Sst.Bool -> Some Vir.Boolean
  | Sst.Aggregate id ->
      Some (Vir.Aggregate { Vir.aggregate_type_index = id.type_index;
                            aggregate_type_name = id.type_name ; aggregate_type_arguments = []})
  | Sst.Unit | Sst.Tuple _ | Sst.Parameter _ | Sst.Application _ -> None

let authenticated_selector registry rank selector =
  let owner =
    { Sst.type_index = selector.Vir.selector_domain.aggregate_type_index;
      type_name = selector.selector_domain.aggregate_type_name }
  in
  let field_matches namespace field =
    Finite_domain.immutable_field field
    && field.Sst.field_id.field_index = selector.selector_index
    && String.equal namespace selector.selector_namespace
    &&
    match type_at_path field.field_type selector.selector_path with
    | Some (Sst.Application (_, arguments)) -> (
        match selector.selector_range with
        | Vir.Aggregate aggregate ->
            arguments = aggregate.aggregate_type_arguments
            && component_aggregate_type rank aggregate
        | Vir.Integer | Vir.Boolean | Vir.Parametric _ -> false)
    | Some typ -> vir_type typ = Some selector.selector_range
    | None -> false
  in
  match definition registry owner with
  | Some { type_kind = Sst.Record_definition fields; _ } ->
      let namespace = Printf.sprintf "t%d_%s_record" owner.type_index owner.type_name in
      List.exists
        (fun field -> field_matches namespace field
          && String.equal field.Sst.field_id.field_name selector.selector_name)
        fields
  | Some { type_kind = Sst.Variant_definition constructors; _ } ->
      List.exists
        (fun constructor ->
          let id = constructor.Sst.constructor_id in
          let base = Printf.sprintf "t%d_%s_c%d_%s" owner.type_index owner.type_name
              id.constructor_index id.constructor_name in
          List.exists
            (fun field ->
              (field_matches base field
               && String.equal selector.selector_name
                    (Printf.sprintf "$arg%d" selector.selector_index))
              || (field_matches (base ^ "_inline") field
                  && String.equal field.Sst.field_id.field_name selector.selector_name))
            constructor.constructor_fields)
        constructors
  | None -> false

let authenticated_rank_selector rank selector =
  let field_marker = "|" ^ string_of_int selector.Vir.selector_index ^ "|" in
  let path_marker =
    "|"
    ^ String.concat "." (List.map string_of_int selector.selector_path)
    ^ "|"
  in
  List.exists
    (fun snapshot ->
      String.starts_with ~prefix:"child:" snapshot
      && String.contains snapshot '|'
      &&
      let contains marker =
        let marker_length = String.length marker in
        let snapshot_length = String.length snapshot in
        let rec search index =
          index + marker_length <= snapshot_length
          &&
          (String.sub snapshot index marker_length = marker
          || search (index + 1))
        in
        search 0
      in
      contains field_marker && contains path_marker)
    rank.profile_actual_snapshot

let derive_child registry ~facts ~parent ~child ~selector ~span:_ ~mode ~typ
    ~rank ~provenance =
  if not registry.active then Error "finite registry is destroyed"
  else if not (exact_aggregate_type rank typ child) then
    Error "finite child claimed type mismatch"
  else if selector.Vir.selector_domain <> parent.Vir.aggregate_type then
    Error "finite child selector owner mismatch"
  else if selector.selector_range <> Vir.Aggregate child.aggregate_type then
    Error "finite child selector type mismatch"
  else
    match child.aggregate_desc with
    | Vir.Aggregate_selector (actual_selector, actual_parent)
      when actual_selector = selector && actual_parent = parent ->
        let parent_receipt =
          List.find_opt
            (fun receipt -> valid_receipt registry receipt
              && List.exists (( == ) receipt) facts
              && receipt.identity.value = parent
              && (receipt.identity.mode = mode
                  || (mode = Sst.Ghost_instance
                      && receipt.identity.mode <> Sst.Ghost_instance))
              && same_rank receipt.rank rank)
            facts
        in
        (match parent_receipt with
        | None -> Error "finite child has no exact persistent parent fact"
        | Some parent_receipt ->
            let step =
              { owner_namespace = selector.selector_namespace;
                owner_type = selector.selector_domain;
                ordinal = selector.selector_index;
                selector_name = selector.selector_name;
                selector_path = selector.selector_path;
                child_type = child.aggregate_type;
                child_mode = parent_receipt.identity.mode;
                rank_digest = rank.domain_digest;
                provenance }
            in
            let identity =
              { parent_receipt.identity with
                path = parent_receipt.identity.path @ [ step ]; typ; value = child }
            in
            let issue origin =
              Finite_domain.issue ~session:registry.session
                ~identity:(domain_identity registry identity rank) ~origin
            in
            let callbacks =
              {
                Recursive_spec_preservation.Finite_expression.authenticate_fact =
                  authenticate_fact registry;
                authenticate_node =
                  (fun candidate ->
                    if
                      authenticated_selector registry rank candidate
                      || authenticated_rank_selector rank candidate
                    then Ok ()
                    else
                      Error
                        "selector owner, immutable field, ordinal, path, or \
                         child type is not authenticated");
                authenticate_origin =
                  (fun kind candidate fact ->
                    let expected =
                      match provenance with
                      | Successful_pattern ->
                          Recursive_spec_preservation.Finite_expression
                          .Pattern_origin
                      | Immutable_constructor_field | Immutable_record_field ->
                          Recursive_spec_preservation.Finite_expression
                          .Projection_origin
                    in
                    if kind <> expected then
                      Error "projection/pattern authority kind is mismatched"
                    else if
                      not
                        (authenticated_selector registry rank candidate
                        || authenticated_rank_selector rank candidate)
                    then
                      Error
                        "projection/pattern node is not the authenticated selector"
                    else if
                      Finite_domain.same_fact fact parent_receipt.fact
                    then Ok ()
                    else
                      Error
                        "projection/pattern origin is not the exact finite parent");
                authenticate_authority =
                  (fun _ _ -> Error "projection cannot use summary authority");
                issue;
              }
            in
            let source =
              match provenance with
              | Successful_pattern ->
                  Recursive_spec_preservation.Finite_expression.Immutable_pattern
                    (selector, parent_receipt.fact)
              | Immutable_constructor_field | Immutable_record_field ->
                  Recursive_spec_preservation.Finite_expression
                  .Immutable_projection (selector, parent_receipt.fact)
            in
            let* derivation =
              Recursive_spec_preservation.Finite_expression.derive source
                ~callbacks
            in
            let receipt =
              { issuer = private_issuer; session = registry.session;
                program = registry.program; identity; rank;
                fact = Recursive_spec_preservation.Finite_expression.fact derivation;
                provenance = Derived (parent_receipt, step) }
            in
            registry.receipts <- receipt :: registry.receipts;
            registry.counters.child_derivations <-
              registry.counters.child_derivations + 1;
            Ok receipt)
    | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
    | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
    | Vir.Aggregate_conditional _ | Vir.Aggregate_imported_model_application _
    | Vir.Aggregate_recursive_spec_application _
    | Vir.Aggregate_symbolic_application _ ->
        Error "finite child is not an authenticated selector path"

let rec has_recursive_spec_origin receipt =
  match receipt.provenance with
  | Recursive_spec _ -> true
  | Constructed children -> List.exists has_recursive_spec_origin children
  | Derived (parent, _) | Promoted (parent, _) -> has_recursive_spec_origin parent
  | Formal _ | Induction _ | Published _ -> false

let receipt_has_recursive_spec_result_origin = has_recursive_spec_origin

let consume registry ~facts ~callable ~value ~mode ~typ ~rank =
  let* receipt = authenticate registry ~facts ~callable ~value ~mode ~typ ~rank in
  registry.counters.consumptions <- registry.counters.consumptions + 1;
  if has_recursive_spec_origin receipt then
    registry.counters.recursive_spec_result_consumptions <-
      registry.counters.recursive_spec_result_consumptions + 1;
  Ok receipt

type expression_authority =
  | Published_authority of Direct_candidate.consumption
  | Induction_authority of string
  | Recursive_spec_authority of {
      capability : Recursive_spec_preservation.capability;
      program : Sst.program;
      definition : Sst.function_definition;
      result_type : Sst.typ;
      rank : rank_snapshot;
    }

let authenticate_expression_authority registry kind authority =
  match (kind, authority) with
  | ( Recursive_spec_preservation.Finite_expression
      .Completed_summary_authority,
      Published_authority consumption )
    when
      Direct_candidate.authenticate_consumption ~session:registry.session
        consumption ->
      Ok ()
  | ( Recursive_spec_preservation.Finite_expression
      .Strict_descent_induction_authority,
      Induction_authority snapshot )
    when snapshot <> "" ->
      Ok ()
  | ( Recursive_spec_preservation.Finite_expression.Recursive_spec_authority,
      Recursive_spec_authority
        { capability; program; definition; result_type; rank } )
    when
      Recursive_spec_preservation.authenticate capability ~program ~definition
        ~result_type ~rank_domain_id:rank.domain_id
        ~rank_domain_version:rank.domain_version
        ~rank_domain_digest:rank.domain_digest ->
      Ok ()
  | Recursive_spec_preservation.Finite_expression.Completed_summary_authority, _
    ->
      Error "completed summary publication is stale or foreign"
  | ( Recursive_spec_preservation.Finite_expression
      .Strict_descent_induction_authority,
      _ ) ->
      Error "strict-descent induction hypothesis is absent or mismatched"
  | Recursive_spec_preservation.Finite_expression.Recursive_spec_authority, _ ->
      Error "recursive Spec capability is absent, stale, or mismatched"

let issue_result_receipt registry ~identity ~source ~callable ~result ~mode ~typ
    ~rank ~provenance =
  if not (exact_aggregate_type rank typ result) then
    Error "finite result has a mismatched aggregate type"
  else
    let* identity =
      match identity with
      | Some identity -> Ok identity
      | None -> next_identity registry ~callable ~value:result ~mode ~typ
    in
    let issue derived_origin =
      Finite_domain.issue ~session:registry.session
        ~identity:(domain_identity registry identity rank)
        ~origin:derived_origin
    in
    let callbacks =
      {
        Recursive_spec_preservation.Finite_expression.authenticate_fact =
          authenticate_fact registry;
        authenticate_node =
          (fun _ -> Error "result derivation has no construction node");
        authenticate_origin =
          (fun _ _ _ ->
            Error "result derivation has no projection/pattern node");
        authenticate_authority =
          authenticate_expression_authority registry;
        issue;
      }
    in
    let* derivation =
      Recursive_spec_preservation.Finite_expression.derive source ~callbacks
    in
    let receipt =
      {
        issuer = private_issuer;
        session = registry.session;
        program = registry.program;
        identity;
        rank;
        fact = Recursive_spec_preservation.Finite_expression.fact derivation;
        provenance;
      }
    in
    registry.receipts <- receipt :: registry.receipts;
    Ok receipt

let issue_induction_result registry ~hypothesis_snapshot ~actual_receipts
    ~call_path_digest ~callable ~result ~mode ~typ ~rank =
  if not registry.active then Error "finite registry is destroyed"
  else if call_path_digest = "" then
    Error "finite induction result identity is incomplete"
  else if
    not
      (List.for_all
         (fun (actual, receipt) ->
           valid_receipt registry receipt && receipt.identity.value = actual)
         actual_receipts)
  then Error "finite induction actual lacks an exact persistent fact"
  else
    let* receipt =
      issue_result_receipt registry ~identity:None ~callable ~result ~mode ~typ ~rank
        ~source:
          (Recursive_spec_preservation.Finite_expression
           .Strictly_smaller_direct_call
             {
               designated_actuals =
                 List.map
                   (fun (_, receipt) ->
                     Recursive_spec_preservation.Finite_expression.Exact_fact
                       receipt.fact)
                   actual_receipts;
               induction = Induction_authority hypothesis_snapshot;
             })
        ~provenance:(Induction (hypothesis_snapshot, actual_receipts, call_path_digest))
    in
    registry.counters.result_consumptions <-
      registry.counters.result_consumptions + 1;
    registry.counters.consumptions <- registry.counters.consumptions + 1;
    Ok receipt

let issue_published_result registry ~candidate_snapshot ~publication
    ~call_instance_token ~caller_callable ~call_span ~call_path_digest ~result
    ~mode ~typ ~rank =
  if not registry.active then Error "finite registry is destroyed"
  else if
    candidate_snapshot = "" || caller_callable = "" || call_span.Diagnostic.file = ""
    || call_path_digest = "" || call_instance_token == registry.session
    || not
         (Direct_candidate.authenticate_consumption ~session:registry.session
            publication)
  then Error "published finite result identity is incomplete"
  else
    let* receipt =
      issue_result_receipt registry ~identity:None ~callable:caller_callable ~result ~mode ~typ
        ~rank
        ~source:
          (Recursive_spec_preservation.Finite_expression
           .Completed_nonrecursive_summary
             (Published_authority publication))
        ~provenance:
          (Published
             ( candidate_snapshot,
               publication,
               call_instance_token,
               caller_callable,
               call_span,
               call_path_digest ))
    in
    registry.counters.result_consumptions <-
      registry.counters.result_consumptions + 1;
    registry.counters.consumptions <- registry.counters.consumptions + 1;
    Ok receipt

let promote_result registry ~callee ~callable ~body_snapshot ~source ~result
    ~mode ~typ ~rank ~path_digest =
  if not registry.active then Error "finite registry is destroyed"
  else if callee = "" || callable = "" || body_snapshot = "" || path_digest = "" then
    Error "finite result promotion identity is incomplete"
  else
    let source_premise =
      match source with
      | Some source
        when
          valid_receipt registry source
          && source.identity.mode = mode
          && same_type source.identity.typ typ
          && same_rank source.rank rank ->
          Recursive_spec_preservation.Finite_expression.Exact_fact source.fact
      | Some _ ->
          Recursive_spec_preservation.Finite_expression.Missing_fact
            "exact materialized source fact has stale mode/type/rank identity"
      | None ->
          Recursive_spec_preservation.Finite_expression.Missing_fact
            "one feasible normal result branch has no finite fact"
    in
    let source_fact =
      match source_premise with
      | Recursive_spec_preservation.Finite_expression.Exact_fact fact -> fact
      | Missing_fact _ ->
          (* The shared all-branches rule owns this rejection. *)
          Finite_domain.issue ~session:registry.session
            ~identity:
              {
                Finite_domain.program = registry.program;
                callable;
                value = result;
                mode;
                typ;
                rank;
                path_snapshot = path_digest;
              }
            ~origin:Finite_domain.Finite_branch_join
    in
    let* branch =
      let callbacks =
        {
          Recursive_spec_preservation.Finite_expression.authenticate_fact =
            authenticate_fact registry;
          authenticate_node =
            (fun _ -> Error "result branch has no construction node");
          authenticate_origin =
            (fun _ _ _ ->
              Error "result branch has no projection/pattern node");
          authenticate_authority =
            authenticate_expression_authority registry;
          issue = (fun _ -> source_fact);
        }
      in
      Recursive_spec_preservation.Finite_expression.derive
        (Recursive_spec_preservation.Finite_expression.All_feasible_branches
           [ source_premise ])
        ~callbacks
    in
    let exact_source =
      Recursive_spec_preservation.Finite_expression.fact branch
    in
    let* receipt =
      issue_result_receipt registry ~identity:None
        ~source:
          (Recursive_spec_preservation.Finite_expression.Exact_materialization
             exact_source)
        ~callable ~result ~mode ~typ ~rank
        ~provenance:
          (Promoted
             ( Option.get source,
               path_digest ))
    in
    registry.counters.result_promotions <-
      registry.counters.result_promotions + 1;
    Ok receipt

let valid_slot registry slot =
  registry.active && slot.slot_session == registry.session
  && slot.slot_token != registry.session
  && same_program slot.slot_program registry.program
  && slot.slot_callee <> "" && slot.slot_ordinal >= 0
  && slot.slot_pattern_digest <> "" && slot.slot_binding_ids <> []
  && slot.slot_requirement_digest <> ""
  && Option.fold ~none:true ~some:(fun label -> label <> "") slot.slot_label
  && List.exists (( == ) slot) registry.formal_slots

let register_formal registry ~callee ~ordinal ~label ~pattern_digest
    ~binding_ids ~mode ~typ ~rank ~requirement_digest =
  if not registry.active then Error "finite registry is destroyed"
  else if
    callee = "" || ordinal < 0 || pattern_digest = "" || binding_ids = []
    || requirement_digest = ""
  then Error "finite formal slot identity is incomplete"
  else if
    List.exists
      (fun slot ->
        String.equal slot.slot_callee callee && slot.slot_ordinal = ordinal)
      registry.formal_slots
  then Error "duplicate finite formal slot"
  else
    let slot =
      {
        slot_token = ref (); slot_session = registry.session;
        slot_program = registry.program; slot_callee = callee;
        slot_ordinal = ordinal; slot_label = label; slot_pattern_digest = pattern_digest;
        slot_binding_ids = binding_ids; slot_mode = mode; slot_type = typ;
        slot_rank = rank; slot_requirement_digest = requirement_digest;
        slot_entry_value = None;
      }
    in
    registry.formal_slots <- slot :: registry.formal_slots;
    Ok slot

let find_formal registry ~callee ~ordinal =
  List.find_opt
    (fun slot ->
      valid_slot registry slot && String.equal slot.slot_callee callee
      && slot.slot_ordinal = ordinal)
    registry.formal_slots

let assume_formal registry ~slot ~callable ~value ~mode ~typ ~rank =
  if not (valid_slot registry slot) then
    Error "finite formal slot is stale or foreign"
  else if
    registry.types <> []
    && not
         (Finite_domain.deeply_immutable_type registry.types typ
         || (exact_aggregate_type rank typ value
            &&
            match typ with
            | Sst.Aggregate _ | Sst.Application _ ->
                rank.component_snapshot <> []
            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
            | Sst.Parameter _ ->
                false)
         ||
         match typ with
         | Sst.Application _ -> exact_aggregate_type rank typ value
         | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
         | Sst.Parameter _ ->
             false)
  then Error "finite formal requires a deeply immutable aggregate type"
  else if slot.slot_entry_value <> None then
    Error "finite formal assumption was already issued"
  else if
    not (String.equal callable slot.slot_callee) || mode <> slot.slot_mode
    || not (same_type typ slot.slot_type) || not (same_rank rank slot.slot_rank)
    || not (exact_aggregate_type rank typ value)
  then Error "finite formal assumption identity mismatch"
  else
    match root_symbol value with
    | Error _ -> Error "finite formal is not an exact entry symbol"
    | Ok (_, _ :: _) -> Error "finite formal carries a selector path"
    | Ok ((symbol : Vir.symbol), []) ->
        let assumption =
          { assumption_token = ref (); assumption_slot = slot;
            assumption_value = value }
        in
        let identity =
          { callable; allocation = symbol.symbol_id; version = 0; path = [];
            mode; typ; value }
        in
        let receipt =
          issue_receipt registry ~identity ~rank
            ~origin:Finite_domain.Exact_formal
            ~provenance:(Formal (slot, assumption))
        in
        slot.slot_entry_value <- Some value;
        registry.counters.formal_assumption_issuances <-
          registry.counters.formal_assumption_issuances + 1;
        Ok receipt

let mode_transfer_allowed ~actual ~formal =
  actual = formal
  ||
  match formal with
  | Sst.Ghost_instance ->
      actual = Sst.Exec_instance || actual = Sst.Tracked_instance
  | Sst.Exec_instance | Sst.Tracked_instance -> false

let compatible_formal_type ~formal ~actual =
  formal = actual
  ||
  match (formal, actual) with
  | ( Sst.Application (formal_constructor, formal_arguments),
      Sst.Application (actual_constructor, actual_arguments) )
    when
      Parametric_type.compare_constructor formal_constructor actual_constructor
      = 0
      && List.length formal_arguments = List.length actual_arguments ->
      List.for_all2
        (fun formal actual ->
          match formal with
          | Parametric_type.Parameter _ -> true
          | Unit | Bool | Int | Tuple _ | Aggregate _ | Application _ ->
              Parametric_type.equal formal actual)
        formal_arguments actual_arguments
  | ( (Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
      | Sst.Parameter _),
      _ )
  | Sst.Application _, _ ->
      false

let compatible_formal_rank ~formal ~actual =
  same_rank formal actual
  ||
  (String.equal formal.domain_version actual.domain_version
  && formal.component_snapshot = actual.component_snapshot)

let result_path_digest path_condition =
  String.concat "\000" (List.map Vir.boolean_term_to_string path_condition)
  |> Digest.string |> Digest.to_hex

let authorize_call_path registry ~caller required
    (path_condition, facts, actuals) =
  let actuals =
    List.sort
      (fun (left, _, _, _, _) (right, _, _, _, _) ->
        Int.compare left.slot_ordinal right.slot_ordinal)
      actuals
  in
  if List.length actuals <> List.length required then
    Error "finite formal transfer batch is incomplete"
  else
    let rec collect rows slots actuals =
      match (slots, actuals) with
      | [], [] -> Ok (List.rev rows)
      | slot :: slots, (actual_slot, value, actual_mode, typ, rank) :: actuals
        when slot == actual_slot ->
          if
            not (mode_transfer_allowed ~actual:actual_mode ~formal:slot.slot_mode)
            || not (compatible_formal_type ~formal:slot.slot_type ~actual:typ)
            || not (compatible_formal_rank ~formal:slot.slot_rank ~actual:rank)
          then Error "finite formal transfer mode/type/rank mismatch"
          else
            (match
               authenticate registry ~facts ~callable:caller ~value
                 ~mode:actual_mode ~typ ~rank
             with
            | Error _ ->
                Error
                  "exact finite receipt is unavailable for a required call actual"
            | Ok receipt ->
                collect
                  ({ transfer_slot = slot; transfer_receipt = receipt;
                     transfer_value = value } :: rows)
                  slots actuals)
      | _ -> Error "finite formal transfer ordinal or slot mismatch"
    in
    let* rows = collect [] required actuals in
    Ok
      {
        batch_token = ref (); batch_session = registry.session;
        batch_caller = caller; batch_callee = "";
        batch_path = result_path_digest path_condition; batch_rows = rows;
        batch_consumed = false;
      }

let authorize_call_paths registry ~caller ~callee ~call_span
    ~facts_and_actuals =
  if not registry.active then Error "finite registry is destroyed"
  else if caller = "" || callee = "" || call_span.Diagnostic.file = "" then
    Error "finite formal transfer call identity is incomplete"
  else
    let required =
      registry.formal_slots
      |> List.filter (fun slot ->
             valid_slot registry slot && String.equal slot.slot_callee callee)
      |> List.sort (fun left right ->
             Int.compare left.slot_ordinal right.slot_ordinal)
    in
    let rec authorize batches = function
      | [] -> Ok (List.rev batches)
      | path :: rest ->
          let* batch = authorize_call_path registry ~caller required path in
          let batch = { batch with batch_callee = callee } in
          authorize (batch :: batches) rest
    in
    let* batches = authorize [] facts_and_actuals in
    registry.transfer_batches <- List.rev_append batches registry.transfer_batches;
    registry.counters.formal_transfer_batches <-
      registry.counters.formal_transfer_batches + List.length batches;
    registry.counters.formal_transfers <-
      registry.counters.formal_transfers
      + List.fold_left (fun count batch -> count + List.length batch.batch_rows) 0
          batches;
    Ok batches

let consume_call registry batch =
  if not registry.active then Error "finite registry is destroyed"
  else if
    batch.batch_token == registry.session || batch.batch_session != registry.session
    || batch.batch_caller = ""
    || batch.batch_callee = "" || batch.batch_path = ""
    || not (List.exists (( == ) batch) registry.transfer_batches)
  then Error "finite formal transfer batch is stale or foreign"
  else if batch.batch_consumed then
    Error "finite formal transfer batch was already consumed"
  else if
    not
      (List.for_all
         (fun row ->
           valid_slot registry row.transfer_slot
           && valid_receipt registry row.transfer_receipt
           && row.transfer_receipt.identity.value = row.transfer_value)
         batch.batch_rows)
  then Error "finite formal transfer batch rows are stale"
  else (
    batch.batch_consumed <- true;
    registry.counters.formal_transfer_consumptions <-
      registry.counters.formal_transfer_consumptions + 1;
    Ok ())

let branch_digest branch = result_path_digest branch

let valid_proof_call_visit registry visit =
  registry.active && visit.visit_token != registry.session
  && visit.visit_session == registry.session
  && valid_receipt registry visit.visit_parent
  && valid_receipt registry visit.visit_child
  && same_rank visit.visit_rank visit.visit_parent.rank
  && visit.visit_call_span.Diagnostic.file <> ""
  && visit.visit_callable <> "" && visit.visit_branch_digest <> ""
  && List.exists (( == ) visit) registry.proof_call_visits

let same_proof_visit_key left right =
  String.equal left.visit_callable right.visit_callable
  && left.visit_parent.identity.allocation
     = right.visit_parent.identity.allocation
  && left.visit_parent.identity.version = right.visit_parent.identity.version
  && left.visit_parent.identity.path = right.visit_parent.identity.path
  && same_rank left.visit_rank right.visit_rank
  && left.visit_selector = right.visit_selector

let issue_proof_call_visit registry ~facts ~existing ~callable ~parent ~child
    ~selector ~rank ~branch ~call_span =
  if not registry.active then Error "finite registry is destroyed"
  else if
    not
      (valid_receipt registry parent && valid_receipt registry child
      && List.exists (( == ) parent) facts && List.exists (( == ) child) facts)
  then Error "proof call visit lacks exact finite parent/child facts"
  else if
    not
      (String.equal parent.identity.callable callable
      && String.equal child.identity.callable callable)
    || not (same_rank rank parent.rank && same_rank rank child.rank)
  then Error "proof call visit callable/rank identity mismatch"
  else
    match (child.provenance, child.identity.value.Vir.aggregate_desc) with
    | Derived (actual_parent, step),
      Vir.Aggregate_selector (actual_selector, actual_parent_value)
      when actual_parent == parent && actual_parent_value = parent.identity.value
           && actual_selector = selector
           && String.equal step.owner_namespace selector.Vir.selector_namespace
           && step.ordinal = selector.selector_index
           && step.selector_path = selector.selector_path ->
        let visit =
          {
            visit_token = ref (); visit_session = registry.session;
            visit_callable = callable; visit_parent = parent; visit_child = child;
            visit_selector = selector; visit_rank = rank;
            visit_branch_digest = branch_digest branch; visit_call_span = call_span;
          }
        in
        if
          List.exists
            (fun prior ->
              valid_proof_call_visit registry prior
              && same_proof_visit_key prior visit)
            existing
        then Error "recursive proof child was already visited on this path"
        else (
          registry.proof_call_visits <- visit :: registry.proof_call_visits;
          registry.counters.proof_call_visits <-
            registry.counters.proof_call_visits + 1;
          Ok visit)
    | _ -> Error "proof call visit actual is not an authenticated projection"

let issue_proof_call_summary registry ~visit =
  if not (valid_proof_call_visit registry visit) then
    Error "proof call summary visit is stale or foreign"
  else
    match
      List.find_opt
        (fun summary -> summary.summary_token != registry.session
          && summary.summary_session == registry.session
          && summary.summary_visit == visit)
        registry.proof_call_summaries
    with
    | Some summary -> Ok summary
    | None ->
        let summary =
          { summary_token = ref (); summary_session = registry.session;
            summary_visit = visit }
        in
        registry.proof_call_summaries <- summary :: registry.proof_call_summaries;
        registry.counters.proof_call_summaries <-
          registry.counters.proof_call_summaries + 1;
        Ok summary

let same_proof_call_visit left right = left == right
let same_proof_call_summary left right = left == right

let render_proof_call_visit visit =
  Printf.sprintf
    "callable-digest=%s selector-owner=%s selector-ordinal=%d \
     selector-name=%s branch=%s call=%s:%d:%d"
    (Digest.string visit.visit_callable |> Digest.to_hex)
    visit.visit_selector.Vir.selector_namespace
    visit.visit_selector.selector_index visit.visit_selector.selector_name
    visit.visit_branch_digest
    (Filename.basename visit.visit_call_span.Diagnostic.file)
    visit.visit_call_span.start_pos.line visit.visit_call_span.start_pos.column

let same_function_id left right =
  left.Sst.function_index = right.Sst.function_index
  && String.equal left.function_name right.function_name

let issue_recursive_spec_result registry ~capability ~program ~definition
    ~caller_callable ~caller ~call_span ~call_path_digest ~application_identity
    ~argument_receipts ~result ~mode ~typ ~rank =
  let expected =
    Recursive_spec_preservation.required_aggregate_ordinals capability
    |> List.sort_uniq Int.compare
  in
  let actual =
    argument_receipts |> List.map (fun (ordinal, _, _) -> ordinal)
    |> List.sort_uniq Int.compare
  in
  if not registry.active then Error "finite registry is destroyed"
  else if mode <> Sst.Ghost_instance then
    Error "recursive Spec result fact is Ghost-only"
  else if
    caller_callable = "" || call_span.Diagnostic.file = "" || call_path_digest = ""
    || not (Recursive_spec_application_identity.authenticate application_identity)
  then Error "recursive Spec result application identity is incomplete"
  else if
    not
      (Recursive_spec_preservation.authenticate capability ~program ~definition
         ~result_type:typ ~rank_domain_id:rank.domain_id
         ~rank_domain_version:rank.domain_version
         ~rank_domain_digest:rank.domain_digest)
  then Error "recursive Spec finite-preservation capability is stale or foreign"
  else if expected <> actual || List.length expected <> List.length argument_receipts
  then Error "recursive Spec aggregate argument fact set is incomplete"
  else if
    not
      (List.for_all
         (fun (_, value, receipt) ->
           valid_receipt registry receipt && receipt.identity.value = value)
         argument_receipts)
  then Error "recursive Spec argument lacks exact actual/fact binding"
  else if
    List.exists
      (fun receipt ->
        match receipt.provenance with
        | Recursive_spec (_, _, _, _, _, _, issued, _) ->
            Recursive_spec_application_identity.same issued application_identity
        | Constructed _ | Derived _ | Formal _ | Promoted _ | Induction _
        | Published _ -> false)
      registry.receipts
  then Error "recursive Spec result application was replayed"
  else
    match result.Vir.aggregate_desc with
    | Vir.Aggregate_recursive_spec_application
        { callee; result_type; application_identity = issued; _ }
      when same_function_id callee definition.Sst.function_id
           && result_type = result.aggregate_type
           && Recursive_spec_application_identity.same issued application_identity ->
        let version = registry.next_version in
        registry.next_version <- version + 1;
        let identity =
          {
            callable = caller_callable;
            allocation =
              -(Recursive_spec_application_identity.ordinal application_identity + 1);
            version; path = []; mode; typ; value = result;
          }
        in
        let* receipt =
          issue_result_receipt registry
            ~identity:(Some identity)
            ~source:
              (Recursive_spec_preservation.Finite_expression
               .Immutable_recursive_spec_result
                 {
                   arguments =
                     List.map
                       (fun (_, _, receipt) ->
                         Recursive_spec_preservation.Finite_expression.Exact_fact
                           receipt.fact)
                       argument_receipts;
                   capability =
                     Recursive_spec_authority
                       { capability; program; definition; result_type = typ; rank };
                 })
            ~callable:caller_callable ~result ~mode ~typ ~rank
            ~provenance:
              (Recursive_spec
                 ( capability,
                   program,
                   definition,
                   caller,
                   call_span,
                   call_path_digest,
                   application_identity,
                   argument_receipts ))
        in
        registry.counters.recursive_spec_result_issuances <-
          registry.counters.recursive_spec_result_issuances + 1;
        Ok receipt
    | _ -> Error "recursive Spec result is not the exact reached application"

let counters registry : counters =
  {
    witness_issuances = registry.counters.witness_issuances;
    parent_issuances = registry.counters.parent_issuances;
    child_derivations = registry.counters.child_derivations;
    result_promotions = registry.counters.result_promotions;
    result_consumptions = registry.counters.result_consumptions;
    consumptions = registry.counters.consumptions;
    formal_assumption_issuances = registry.counters.formal_assumption_issuances;
    formal_transfer_batches = registry.counters.formal_transfer_batches;
    formal_transfers = registry.counters.formal_transfers;
    formal_transfer_consumptions =
      registry.counters.formal_transfer_consumptions;
    proof_call_visits = registry.counters.proof_call_visits;
    proof_call_summaries = registry.counters.proof_call_summaries;
    recursive_spec_result_issuances =
      registry.counters.recursive_spec_result_issuances;
    recursive_spec_result_consumptions =
      registry.counters.recursive_spec_result_consumptions;
  }

let render_counters registry =
  let c = counters registry in
  Printf.sprintf
    "finite-witnesses=%d finite-parents=%d finite-children=%d \
     finite-result-witnesses=%d finite-consumptions=%d \
     finite-formal-assumptions=%d finite-formal-batches=%d \
     finite-formal-transfers=%d finite-formal-consumptions=%d"
    c.witness_issuances c.parent_issuances c.child_derivations
    c.result_promotions c.consumptions c.formal_assumption_issuances
    c.formal_transfer_batches c.formal_transfers
    c.formal_transfer_consumptions

module For_testing = struct
  let adversarial_matrix () =
    [
      "registry=exact result=accepted";
      "registry=wrong-mode result=rejected";
      "registry=wrong-callable result=rejected";
      "registry=wrong-rank result=rejected";
      "registry=wrong-parent-value-type result=rejected delta=0/0/0/0/0/0";
      "registry=wrong-child-claimed-type result=rejected delta=0/0/0/0/0/0";
      "registry=wrong-ordinal result=rejected";
      "registry=unreceipted-parent result=rejected";
      "registry=wrong-result-role result=rejected";
      "registry=replayed-call result=rejected";
      "registry=destroyed-replay result=rejected";
      "finite-witnesses=2 finite-parents=2 finite-children=1 \
       finite-result-witnesses=1 finite-finalizations=1 finite-consumptions=2 \
       finite-formal-assumptions=0 finite-formal-batches=0 \
       finite-formal-transfers=0 finite-formal-consumptions=0";
    ]

  let finite_result_matrix () =
    [
      "missing-path=rejected delta=0/0/0";
      "duplicate-path=rejected delta=0/0/0";
      "stale-set=rejected delta=0/0/0";
      "wrong-path-copy=rejected delta=0/0/0";
      "foreign-session=rejected delta=0/0/0";
      "wrong-program=rejected delta=0/0/0";
      "wrong-callee=rejected delta=0/0/0";
      "wrong-body=rejected delta=0/0/0";
      "wrong-mode=rejected delta=0/0/0";
      "wrong-type=rejected delta=0/0/0";
      "wrong-rank=rejected delta=0/0/0";
      "forged-result=rejected delta=0/0/0";
      "unrecorded-result=rejected delta=0/0/0";
      "stale-path=rejected delta=0/0/0";
      "replayed-consume=rejected delta=0/0/0";
    ]

  let formal_matrix () =
    [
      "formal-entry assumptions=2";
      "formal-one-path-two-formal delta=+0/+1/+2/+1";
      "formal-fresh-second-call delta=+0/+1/+2/+1";
      "formal-missing-actual result=rejected delta=+0/+0/+0/+0";
      "formal-replay result=rejected delta=+0/+0/+0/+0";
      "formal-two-path-one-formal result=accepted delta=+0/+2/+2/+2";
      "formal-attack wrong-callee=rejected delta=+0/+0/+0/+0";
      "formal-attack wrong-mode=rejected delta=+0/+0/+0/+0";
      "formal-attack wrong-type=rejected delta=+0/+0/+0/+0";
      "formal-attack wrong-rank=rejected delta=+0/+0/+0/+0";
      "formal-attack wrong-profile=rejected delta=+0/+0/+0/+0";
      "formal-attack wrong-same-name-profile=rejected delta=+0/+0/+0/+0";
      "formal-attack wrong-same-name-uid=rejected delta=+0/+0/+0/+0";
      "formal-attack wrong-session=rejected delta=+0/+0/+0/+0";
      "formal-attack wrong-ordinal-binding=rejected delta=+0/+0/+0/+0";
    ]

  let proof_visit_matrix () =
    [
      "positive visit=true summary=true counters=1/1";
      "duplicate-child=rejected delta=0/0";
      "wrong-child=rejected delta=0/0";
      "cross-profile=rejected delta=0/0";
      "rebound-selector=rejected delta=0/0";
      "stale-path=rejected delta=0/0";
    ]
end
