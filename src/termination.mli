type certified_structural_rank

type decrease_domain =
  | Integer_height
  | Structural_rank of certified_structural_rank
  | Parametric_direct_edge of Parametric_adt.t
  | Frozen_spine_direct_edge of Sst.frozen_spine_prerequisite

type analysis
type call_graph
type callable
type call_edge
type scc
type plan
type pending_summary
type entry_intent
type edge_intent

type recursion_kind =
  | Direct_self
  | Mutual

type error_kind =
  | Missing_measure
  | Duplicate_measure
  | Inapplicable_measure
  | Non_integer_measure
  | Recursive_measure
  | Unsupported_mutual_scc of Sst.function_id list
  | Unsupported_recursive_mode of
      Sst.verification_mode * Sst.function_id list
  | Raw_analysis_only

type error = {
  function_id : Sst.function_id option;
  span : Diagnostic.span;
  kind : error_kind;
}

val analyze : Sst_validation.validated_program -> analysis
(* [analyze_raw] preserves graph facts for diagnostics before legacy cycle
   gates. Its handles carry no validated semantic authority, and [prepare]
   rejects the resulting analysis. *)
val analyze_raw : Sst.program -> analysis
val graph : analysis -> call_graph
val graph_callables : call_graph -> callable list
val graph_edges : call_graph -> call_edge list
val callable_id : callable -> Sst.function_id
val callable_mode : callable -> Sst.verification_mode
val call_edge_caller : call_edge -> callable
val call_edge_callee : call_edge -> callable
val call_edge_form : call_edge -> Sst.call_form
val call_edge_recursive : call_edge -> bool
val call_edge_span : call_edge -> Diagnostic.span
val call_edge_region : call_edge -> Sst_validation.call_edge_region

val sccs : analysis -> scc list
val scc_members : scc -> callable list
val scc_edges : scc -> call_edge list
val scc_modes : scc -> Sst.verification_mode list
val scc_is_recursive : scc -> bool
val scc_recursion_kind : scc -> recursion_kind option

val precheck : analysis -> (unit, error) result
val prepare : analysis -> (plan, error) result
val pending_summaries : plan -> pending_summary list
val pending_callable : pending_summary -> callable
val pending_group : pending_summary -> scc
val pending_domain : pending_summary -> decrease_domain
val structural_rank_id : certified_structural_rank -> string

val find_pending_summary :
  plan -> Sst.function_id -> pending_summary option
val find_entry_intent :
  plan -> Sst.function_id -> entry_intent option
val find_edge_intent :
  plan ->
  caller:Sst.function_id ->
  callee:Sst.function_id ->
  span:Diagnostic.span ->
  edge_intent option

val entry_callable : entry_intent -> callable
val entry_measure : entry_intent -> Sst_validation.decrease_descriptor
val entry_domain : entry_intent -> decrease_domain

val edge_descriptor : edge_intent -> call_edge
val edge_measure : edge_intent -> Sst_validation.decrease_descriptor
val edge_domain : edge_intent -> decrease_domain

val error_to_string : error -> string
