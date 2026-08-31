type status =
  | Verified
  | Counterexample
  | Inconclusive
  | Incomplete_source
  | Frontend_rejected

type operation =
  | Add
  | Subtract
  | Negate
  | Multiply_constant
  | Successor
  | Predecessor
  | Absolute_value

type violated_bound = Lower | Upper

type semantic_kind =
  | Arithmetic_safety of operation * violated_bound
  | Assertion
  | Local_assertion
  | Postcondition
  | Call_precondition of { callee : string }
  | Callback_precondition of { callback_name : string }
  | Invariant_validity of { invariant_id : string; boundary : string }
  | Entry_measure_nonnegative
  | Recursive_call_measure_nonnegative of { callee : string }
  | Recursive_call_strict_descent of { callee : string }

type semantic_fact = { function_name : string; kind : semantic_kind }

type unit_disposition =
  | Unit_verified
  | Unit_counterexample
  | Unit_inconclusive
  | Unit_incomplete_source
  | Unit_frontend_rejected
  | Unit_dependency_success
  | Unit_skipped

type named_fact =
  | Function_exists of string
  | Obligation_kind_exists of {
      function_name : string;
      kind : semantic_kind;
    }

type exit_class = Exited of int | Signaled | Stopped

type process_fact =
  | Exit_class of exit_class
  | Stable_code of string
  | Forwarded of string
  | Cleaned of string
  | Adjacent of string

type observation
type t

val observation :
  status:status ->
  ?frontend_codes:string list ->
  ?semantic_facts:semantic_fact list ->
  ?units:(string * unit_disposition) list ->
  ?named_facts:(string * named_fact) list ->
  ?process_facts:process_fact list ->
  unit ->
  observation

val project : observation -> t
val of_verifier_result : Verifier_service.result -> t
val frontend_rejection : code:string -> t
val with_unit : string -> unit_disposition -> t -> t
val merge : t list -> t
val status : t -> status
val frontend_codes : t -> string list
val semantic_facts : t -> semantic_fact list
val units : t -> (string * unit_disposition) list
val named_facts : t -> (string * named_fact) list
val process_facts : t -> process_fact list

val resource_at_most :
  [ `Functions | `Obligations ] ->
  maximum:int ->
  baseline:int ->
  rationale:string ->
  t ->
  (unit, string) result

val semantic_parity : except:string list -> t -> t -> (unit, string) result
val to_string : t -> string
val status_name : status -> string
val semantic_kind_name : semantic_kind -> string
val unit_disposition_name : unit_disposition -> string
val named_fact_name : named_fact -> string
val process_fact_name : process_fact -> string
