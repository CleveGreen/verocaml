(** Private session-local authority for persistent finite immutable values. *)

type t
type receipt
type formal_slot
type formal_assumption
type transfer_batch
type proof_call_visit
type proof_call_summary

module Finite_domain : sig
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

  type fact

  val immutable_field : Sst.field_definition -> bool
  val deeply_immutable_type : Sst.type_definition list -> Sst.typ -> bool
  val same_program : program_identity -> program_identity -> bool
  val same_rank : rank_snapshot -> rank_snapshot -> bool
  val same_identity : identity -> identity -> bool
  val issue : session:unit ref -> identity:identity -> origin:origin -> fact

  val authenticate :
    session:unit ref ->
    facts:fact list ->
    identity:identity ->
    (fact, string) result

  val belongs_to_session : unit ref -> fact -> bool
  val identity : fact -> identity
  val origin : fact -> origin
  val same_fact : fact -> fact -> bool
end

module Direct_candidate : sig
  type t
  type candidate
  type obligation_manifest
  type completion
  type publication
  type consumption

  type counters = {
    exits_recorded : int;
    candidates_completed : int;
    candidates_published : int;
    consumption_attempts : int;
    consumptions : int;
  }

  val create : session:unit ref -> t
  val destroy : t -> unit

  val register :
    t ->
    callable:string ->
    body_snapshot:string ->
    profile_snapshot:string ->
    (candidate, string) result

  val record_exit :
    t ->
    candidate ->
    path_digest:string ->
    result_snapshot:string ->
    fact_snapshot:string ->
    (unit, string) result

  val authorize_obligations :
    t ->
    candidate ->
    obligation_fingerprints:string list ->
    (obligation_manifest option, string) result

  val complete :
    t ->
    obligation_manifest ->
    completed_fingerprints:string list ->
    all_verified:bool ->
    (completion, string) result

  val publish : t -> completion -> (publication, string) result

  val consume :
    t ->
    candidate ->
    caller_snapshot:string ->
    call_path_digest:string ->
    result_snapshot:string ->
    (consumption option, string) result

  val callable : candidate -> string
  val authenticate_consumption : session:unit ref -> consumption -> bool
  val consumption_token : consumption -> unit ref
  val counters : t -> counters
end

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

val create :
  program_identity -> session:unit ref -> types:Sst.type_definition list -> t
val destroy : t -> unit
val is_active : t -> bool

val issue_parent :
  t ->
  callable:string ->
  value:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  provenance:construction_provenance ->
  required_children:(Sst.typ * Vir.aggregate_term * receipt option) list ->
  (receipt, string) result

val derive_child :
  t ->
  facts:receipt list ->
  parent:Vir.aggregate_term ->
  child:Vir.aggregate_term ->
  selector:Vir.selector ->
  span:Diagnostic.span ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  provenance:child_provenance ->
  (receipt, string) result

val authenticate :
  t ->
  facts:receipt list ->
  callable:string ->
  value:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  (receipt, string) result

val consume :
  t ->
  facts:receipt list ->
  callable:string ->
  value:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  (receipt, string) result

val issue_induction_result :
  t ->
  hypothesis_snapshot:string ->
  actual_receipts:(Vir.aggregate_term * receipt) list ->
  call_path_digest:string ->
  callable:string ->
  result:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  (receipt, string) result

val issue_published_result :
  t ->
  candidate_snapshot:string ->
  publication:Direct_candidate.consumption ->
  call_instance_token:unit ref ->
  caller_callable:string ->
  call_span:Diagnostic.span ->
  call_path_digest:string ->
  result:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  (receipt, string) result

val issue_recursive_spec_result :
  t ->
  capability:Recursive_spec_preservation.capability ->
  program:Sst.program ->
  definition:Sst.function_definition ->
  caller_callable:string ->
  caller:Sst.function_id ->
  call_span:Diagnostic.span ->
  call_path_digest:string ->
  application_identity:Recursive_spec_application_identity.t ->
  argument_receipts:(int * Vir.aggregate_term * receipt) list ->
  result:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  (receipt, string) result

val promote_result :
  t ->
  callee:string ->
  callable:string ->
  body_snapshot:string ->
  source:receipt option ->
  result:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  path_digest:string ->
  (receipt, string) result

val receipt_matches_value : receipt -> Vir.aggregate_term -> bool
val receipt_has_recursive_spec_result_origin : receipt -> bool
val same_receipt : receipt -> receipt -> bool

val register_formal :
  t ->
  callee:string ->
  ordinal:int ->
  label:string option ->
  pattern_digest:string ->
  binding_ids:int list ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  requirement_digest:string ->
  (formal_slot, string) result

val find_formal : t -> callee:string -> ordinal:int -> formal_slot option

val assume_formal :
  t ->
  slot:formal_slot ->
  callable:string ->
  value:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:rank_snapshot ->
  (receipt, string) result

val authorize_call_paths :
  t ->
  caller:string ->
  callee:string ->
  call_span:Diagnostic.span ->
  facts_and_actuals:
    (Vir.boolean_term list * receipt list
    * (formal_slot * Vir.aggregate_term * Sst.instance_mode * Sst.typ
      * rank_snapshot)
      list)
    list ->
  (transfer_batch list, string) result

val consume_call : t -> transfer_batch -> (unit, string) result

val issue_proof_call_visit :
  t ->
  facts:receipt list ->
  existing:proof_call_visit list ->
  callable:string ->
  parent:receipt ->
  child:receipt ->
  selector:Vir.selector ->
  rank:rank_snapshot ->
  branch:Vir.boolean_term list ->
  call_span:Diagnostic.span ->
  (proof_call_visit, string) result

val issue_proof_call_summary :
  t -> visit:proof_call_visit -> (proof_call_summary, string) result
val same_proof_call_visit : proof_call_visit -> proof_call_visit -> bool
val same_proof_call_summary : proof_call_summary -> proof_call_summary -> bool
val render_proof_call_visit : proof_call_visit -> string

val result_path_digest : Vir.boolean_term list -> string
val counters : t -> counters
val render_counters : t -> string

module For_testing : sig
  val adversarial_matrix : unit -> string list
  val finite_result_matrix : unit -> string list
  val formal_matrix : unit -> string list
  val proof_visit_matrix : unit -> string list
end
