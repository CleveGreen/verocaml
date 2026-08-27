(** Reusable internal derivation for persistent immutable finite facts. *)
module Finite_expression : sig
  type origin =
    | Exact_formal
    | Exact_alias
    | Immutable_construction
    | Immutable_projection
    | Immutable_pattern
    | Finite_branch_join
    | Completed_local_summary
    | Smaller_direct_call
    | Immutable_recursive_spec

  type 'fact premise =
    | Exact_fact of 'fact
    | Missing_fact of string

  type origin_kind =
    | Projection_origin
    | Pattern_origin

  type authority_kind =
    | Completed_summary_authority
    | Strict_descent_induction_authority
    | Recursive_spec_authority

  type ('fact, 'authority, 'node) source =
    | Existing_fact of 'fact
    | Exact_alias of 'fact
    | Exact_let of 'fact
    | Exact_materialization of 'fact
    | Immutable_constructor of 'node * 'fact premise list
    | Immutable_record of 'node * 'fact premise list
    | Immutable_projection of 'node * 'fact
    | Immutable_pattern of 'node * 'fact
    | All_feasible_branches of 'fact premise list
    | Completed_nonrecursive_summary of 'authority
    | Strictly_smaller_direct_call of {
        designated_actuals : 'fact premise list;
        induction : 'authority;
      }
    | Immutable_recursive_spec_result of {
        arguments : 'fact premise list;
        capability : 'authority;
      }

  type 'fact derivation

  type ('fact, 'authority, 'node) callbacks = {
    authenticate_fact : 'fact -> (unit, string) result;
    authenticate_node : 'node -> (unit, string) result;
    authenticate_origin :
      origin_kind -> 'node -> 'fact -> (unit, string) result;
    authenticate_authority :
      authority_kind -> 'authority -> (unit, string) result;
    issue : origin -> 'fact;
  }

  val derive :
    ('fact, 'authority, 'node) source ->
    callbacks:('fact, 'authority, 'node) callbacks ->
    ('fact derivation, string) result

  val fact : 'fact derivation -> 'fact

  type route_counts = { base : int; recursive : int; construction : int }

  val check_immutable_recursive_spec :
    definition:Sst.function_definition ->
    expanded_body:Sst.expression ->
    strict_edge_spans:Diagnostic.span list ->
    (route_counts, string) result

  val ordinary_checker_entry : string
  val immutable_recursive_spec_checker_entry : string
end

(** Private all-path finite-preservation authority for aggregate recursive
    specifications.  A capability is issued only after the exact definition's
    termination obligations have verified. *)
type capability

val analyze :
  program:Sst.program ->
  definition:Sst.function_definition ->
  expanded_body:Sst.expression ->
  helper_closure_snapshot:string ->
  rank_domains:Vir.rank_domain list ->
  termination_obligations:Vir.obligation list ->
  (capability option, string) result

val authenticate :
  capability ->
  program:Sst.program ->
  definition:Sst.function_definition ->
  result_type:Sst.typ ->
  rank_domain_id:string ->
  rank_domain_version:string ->
  rank_domain_digest:string ->
  bool

val function_id : capability -> Sst.function_id
val body_snapshot : capability -> string
val helper_closure_snapshot : capability -> string
val result_type : capability -> Sst.typ
val rank_domain_id : capability -> string
val rank_domain_digest : capability -> string
val required_aggregate_ordinals : capability -> int list
val base_route_count : capability -> int
val recursive_route_count : capability -> int
val construction_route_count : capability -> int

(** One-shot handoff from verified termination preflight to the session that
    owns the same physical semantic program. *)
val clear_pending : Sst.program -> unit
val retain_pending : Sst.program -> capability list -> unit
val take_pending : Sst.program -> capability list

module For_testing : sig
  val forged : capability -> capability
end
