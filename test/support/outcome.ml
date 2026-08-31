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

type observation = {
  status : status;
  frontend_codes : string list;
  semantic_facts : semantic_fact list;
  units : (string * unit_disposition) list;
  named_facts : (string * named_fact) list;
  process_facts : process_fact list;
  functions : int option;
  obligations : int option;
}

type t = observation

let sort_uniq compare values = List.sort_uniq compare values

let normalize observation =
  {
    observation with
    frontend_codes = sort_uniq String.compare observation.frontend_codes;
    semantic_facts = sort_uniq Stdlib.compare observation.semantic_facts;
    units = sort_uniq Stdlib.compare observation.units;
    named_facts = sort_uniq Stdlib.compare observation.named_facts;
    process_facts = sort_uniq Stdlib.compare observation.process_facts;
  }

let observation ~status ?(frontend_codes = []) ?(semantic_facts = [])
    ?(units = []) ?(named_facts = []) ?(process_facts = []) () =
  normalize
    {
      status;
      frontend_codes;
      semantic_facts;
      units;
      named_facts;
      process_facts;
      functions = None;
      obligations = None;
    }

let measured_observation ~status ?(frontend_codes = []) ?(semantic_facts = [])
    ?(units = []) ?(named_facts = []) ?(process_facts = []) ~functions
    ~obligations () =
  normalize
    {
      status;
      frontend_codes;
      semantic_facts;
      units;
      named_facts;
      process_facts;
      functions = Some functions;
      obligations = Some obligations;
    }

let project = normalize

let status_of_service = function
  | Verifier_service.Verified -> Verified
  | Counterexample -> Counterexample
  | Inconclusive -> Inconclusive
  | Incomplete_source -> Incomplete_source

let operation_of_service = function
  | Verifier_service.Add -> Add
  | Subtract -> Subtract
  | Negate -> Negate
  | Multiply_constant _ -> Multiply_constant
  | Successor -> Successor
  | Predecessor -> Predecessor
  | Absolute_value -> Absolute_value

let bound_of_service = function
  | Verifier_service.Lower -> Lower
  | Upper -> Upper

let boundary_name = function
  | Verifier_service.Constructor_establishment -> "constructor-establishment"
  | Transition_preservation -> "transition-preservation"
  | Call_argument -> "call-argument"
  | Call_result -> "call-result"
  | Function_return -> "function-return"
  | Shared_invariant_close -> "shared-invariant-close"
  | Terminal_observation -> "terminal-observation"

let semantic_kind_of_service = function
  | Verifier_service.Arithmetic_safety
      { operation; mathematical_result = _; violated_bound } ->
      Arithmetic_safety
        (operation_of_service operation, bound_of_service violated_bound)
  | Assertion _ -> Assertion
  | Local_assertion _ -> Local_assertion
  | Postcondition _ -> Postcondition
  | Call_precondition { callee; precondition_ordinal = _ } ->
      Call_precondition
        { callee = Verifier_service.function_name callee }
  | Callback_precondition { callback_name; callback_id = _ } ->
      Callback_precondition { callback_name }
  | Invariant_validity { invariant_id; boundary } ->
      Invariant_validity { invariant_id; boundary = boundary_name boundary }
  | Entry_measure_nonnegative -> Entry_measure_nonnegative
  | Recursive_call_measure_nonnegative { callee } ->
      Recursive_call_measure_nonnegative
        { callee = Verifier_service.function_name callee }
  | Recursive_call_strict_descent { callee } ->
      Recursive_call_strict_descent
        { callee = Verifier_service.function_name callee }

let function_named_fact execution =
  Function_exists execution.Vir.function_ref.function_name

let obligation_named_fact (obligation : Vir.obligation) =
  let function_name = obligation.Vir.function_ref.function_name in
  let kind =
    match obligation.Vir.kind with
    | Vir.Arithmetic_safety { operation; violated_bound; _ } ->
        let operation =
          match operation with
          | Vir.Add -> Add
          | Subtract -> Subtract
          | Negate -> Negate
          | Multiply_constant _ -> Multiply_constant
          | Successor -> Successor
          | Predecessor -> Predecessor
          | Absolute_value -> Absolute_value
        in
        let violated_bound =
          match violated_bound with Vir.Lower_bound -> Lower | Upper_bound -> Upper
        in
        Arithmetic_safety (operation, violated_bound)
    | Assertion _ -> Assertion
    | Local_assertion _ -> Local_assertion
    | Postcondition _ -> Postcondition
    | Call_precondition { callee; _ } ->
        Call_precondition { callee = callee.function_name }
    | Callback_precondition { callback; _ } ->
        Callback_precondition { callback_name = callback.Sst.callback_name }
    | Invariant_validity { invariant_id; boundary; _ } ->
        let boundary =
          match boundary with
          | Vir.Constructor_establishment -> "constructor-establishment"
          | Transition_preservation _ -> "transition-preservation"
          | Call_argument _ -> "call-argument"
          | Call_result _ -> "call-result"
          | Function_return -> "function-return"
          | Shared_invariant_close _ -> "shared-invariant-close"
          | Terminal_observation _ -> "terminal-observation"
        in
        Invariant_validity { invariant_id; boundary }
    | Entry_measure_nonnegative _ -> Entry_measure_nonnegative
    | Recursive_call_measure_nonnegative { callee; _ } ->
        Recursive_call_measure_nonnegative { callee = callee.function_name }
    | Recursive_call_strict_descent { callee; _ } ->
        Recursive_call_strict_descent { callee = callee.function_name }
  in
  Obligation_kind_exists { function_name; kind }

let of_verifier_result result =
  let semantic_facts =
    Verifier_service.diagnostics result
    |> List.map (fun diagnostic ->
           {
             function_name =
               Verifier_service.diagnostic_function diagnostic
               |> Verifier_service.function_name;
             kind =
               Verifier_service.diagnostic_kind diagnostic
               |> semantic_kind_of_service;
           })
  in
  let named_facts =
    (Verifier_service.vir result).Vir.functions
    |> List.concat_map (fun (execution : Vir.function_execution) ->
           ("function:" ^ execution.function_ref.function_name,
            function_named_fact execution)
           :: List.map
                (fun (obligation : Vir.obligation) ->
                  ( "obligation-kind:"
                    ^ obligation.Vir.function_ref.function_name,
                    obligation_named_fact obligation ))
                execution.obligations)
  in
  measured_observation ~status:(status_of_service (Verifier_service.status result))
    ~semantic_facts ~named_facts
    ~functions:(Verifier_service.functions result)
    ~obligations:(Verifier_service.obligations result) ()

let frontend_rejection ~code =
  observation ~status:Frontend_rejected ~frontend_codes:[ code ] ()

let with_unit unit_name disposition projection =
  normalize
    { projection with units = (unit_name, disposition) :: projection.units }

let merge projections =
  let sum_options left right =
    match (left, right) with
    | None, None -> None
    | _ -> Some (Option.value left ~default:0 + Option.value right ~default:0)
  in
  match projections with
  | [] -> observation ~status:Verified ()
  | first :: rest ->
      List.fold_left
        (fun left right ->
          normalize
            {
              status =
                (if left.status = Verified then right.status else left.status);
              frontend_codes = left.frontend_codes @ right.frontend_codes;
              semantic_facts = left.semantic_facts @ right.semantic_facts;
              units = left.units @ right.units;
              named_facts = left.named_facts @ right.named_facts;
              process_facts = left.process_facts @ right.process_facts;
              functions = sum_options left.functions right.functions;
              obligations = sum_options left.obligations right.obligations;
            })
        first rest

let status projection = projection.status
let frontend_codes projection = projection.frontend_codes
let semantic_facts projection = projection.semantic_facts
let units projection = projection.units
let named_facts projection = projection.named_facts
let process_facts projection = projection.process_facts

let resource_at_most kind ~maximum ~baseline ~rationale projection =
  if maximum < 0 then Error "maximum must be non-negative"
  else if baseline < 0 then Error "measured baseline must be non-negative"
  else if String.trim rationale = "" then Error "resource rationale must be non-empty"
  else
    let label, measured =
      match kind with
      | `Functions -> ("functions", projection.functions)
      | `Obligations -> ("obligations", projection.obligations)
    in
    match measured with
    | None -> Error "resource measurement unavailable"
    | Some value when value <= maximum -> Ok ()
    | Some value ->
        Error
          (Printf.sprintf "%s=%d exceeds inclusive maximum=%d baseline=%d rationale=%s"
             label value maximum baseline rationale)

let semantic_parity ~except left right =
  let keep (name, _) = not (List.mem name except) in
  let strip projection =
    normalize
      {
        projection with
        named_facts = List.filter keep projection.named_facts;
        functions = None;
        obligations = None;
      }
  in
  if strip left = strip right then Ok ()
  else Error "semantic-parity: selected closed projection differs"

let status_name = function
  | Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete-source"
  | Frontend_rejected -> "frontend-rejected"

let operation_name = function
  | Add -> "add"
  | Subtract -> "subtract"
  | Negate -> "negate"
  | Multiply_constant -> "multiply-constant"
  | Successor -> "successor"
  | Predecessor -> "predecessor"
  | Absolute_value -> "absolute-value"

let semantic_kind_name = function
  | Arithmetic_safety (operation, violated_bound) ->
      Printf.sprintf "arithmetic-safety(%s,%s)" (operation_name operation)
        (match violated_bound with Lower -> "lower" | Upper -> "upper")
  | Assertion -> "assertion"
  | Local_assertion -> "local-assertion"
  | Postcondition -> "postcondition"
  | Call_precondition { callee } -> "call-precondition(" ^ callee ^ ")"
  | Callback_precondition { callback_name } ->
      "callback-precondition(" ^ callback_name ^ ")"
  | Invariant_validity { invariant_id; boundary } ->
      Printf.sprintf "invariant-validity(%s,%s)" invariant_id boundary
  | Entry_measure_nonnegative -> "entry-measure-nonnegative"
  | Recursive_call_measure_nonnegative { callee } ->
      "recursive-call-measure-nonnegative(" ^ callee ^ ")"
  | Recursive_call_strict_descent { callee } ->
      "recursive-call-strict-descent(" ^ callee ^ ")"

let unit_disposition_name = function
  | Unit_verified -> "verified"
  | Unit_counterexample -> "counterexample"
  | Unit_inconclusive -> "inconclusive"
  | Unit_incomplete_source -> "incomplete-source"
  | Unit_frontend_rejected -> "frontend-rejected"
  | Unit_dependency_success -> "dependency-success"
  | Unit_skipped -> "skipped"

let named_fact_name = function
  | Function_exists function_name -> "function-exists(" ^ function_name ^ ")"
  | Obligation_kind_exists { function_name; kind } ->
      "obligation-kind-exists(" ^ function_name ^ "," ^ semantic_kind_name kind ^ ")"

let process_fact_name = function
  | Exit_class (Exited code) -> "exit(" ^ string_of_int code ^ ")"
  | Exit_class Signaled -> "signaled"
  | Exit_class Stopped -> "stopped"
  | Stable_code code -> "stable-code(" ^ code ^ ")"
  | Forwarded name -> "forwarded(" ^ name ^ ")"
  | Cleaned path -> "cleaned(" ^ path ^ ")"
  | Adjacent name -> "adjacent(" ^ name ^ ")"

let to_string projection =
  let codes = String.concat "," projection.frontend_codes in
  let semantics =
    projection.semantic_facts
    |> List.map (fun fact ->
           fact.function_name ^ ":" ^ semantic_kind_name fact.kind)
    |> String.concat ","
  in
  let units =
    projection.units
    |> List.map (fun (name, disposition) ->
           name ^ ":" ^ unit_disposition_name disposition)
    |> String.concat ","
  in
  let named =
    projection.named_facts
    |> List.map (fun (name, fact) -> name ^ ":" ^ named_fact_name fact)
    |> String.concat ","
  in
  let process =
    projection.process_facts |> List.map process_fact_name |> String.concat ","
  in
  Printf.sprintf
    "status=%s;codes=[%s];semantic=[%s];units=[%s];facts=[%s];process=[%s]"
    (status_name projection.status) codes semantics units named process
