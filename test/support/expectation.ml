type bound_kind = Functions | Obligations

type bound = {
  kind : bound_kind;
  maximum : int;
  baseline : int;
  rationale : string;
}

type t = {
  status : Outcome.status option;
  frontend_codes : string list;
  semantic_facts : Outcome.semantic_fact list;
  units : (string * Outcome.unit_disposition) list;
  named_facts : (string * Outcome.named_fact) list;
  process_facts : Outcome.process_fact list;
  bounds : bound list;
}

let empty =
  {
    status = None;
    frontend_codes = [];
    semantic_facts = [];
    units = [];
    named_facts = [];
    process_facts = [];
    bounds = [];
  }

let status status expectation = { expectation with status = Some status }

let require_frontend_code code expectation =
  { expectation with frontend_codes = code :: expectation.frontend_codes }

let require_semantic ~function_name kind expectation =
  {
    expectation with
    semantic_facts = { Outcome.function_name; kind } :: expectation.semantic_facts;
  }

let require_unit name disposition expectation =
  { expectation with units = (name, disposition) :: expectation.units }

let require_named_fact name fact expectation =
  { expectation with named_facts = (name, fact) :: expectation.named_facts }

let require_process_fact fact expectation =
  { expectation with process_facts = fact :: expectation.process_facts }

let add_bound kind ~maximum ~baseline ~rationale expectation =
  if maximum < 0 then Error "maximum must be non-negative"
  else if baseline < 0 then Error "measured baseline must be non-negative"
  else if String.trim rationale = "" then Error "resource rationale must be non-empty"
  else
    Ok
      {
        expectation with
        bounds = { kind; maximum; baseline; rationale } :: expectation.bounds;
      }

let functions_at_most = add_bound Functions
let obligations_at_most = add_bound Obligations

let missing label values =
  match values with
  | [] -> Ok ()
  | _ -> Error (Printf.sprintf "missing selected %s fact" label)

let check_bound projection bound =
  Outcome.resource_at_most
    (match bound.kind with Functions -> `Functions | Obligations -> `Obligations)
    ~maximum:bound.maximum ~baseline:bound.baseline ~rationale:bound.rationale
    projection

let check expectation projection =
  let ( let* ) result next = Result.bind result next in
  let* () =
    match expectation.status with
    | None -> Ok ()
    | Some expected when expected = Outcome.status projection -> Ok ()
    | Some _ -> Error "selected status differs"
  in
  let* () =
    expectation.frontend_codes
    |> List.filter (fun value -> not (List.mem value (Outcome.frontend_codes projection)))
    |> missing "frontend-code"
  in
  let* () =
    expectation.semantic_facts
    |> List.filter (fun value -> not (List.mem value (Outcome.semantic_facts projection)))
    |> missing "semantic"
  in
  let* () =
    expectation.units
    |> List.filter (fun value -> not (List.mem value (Outcome.units projection)))
    |> missing "unit"
  in
  let* () =
    expectation.named_facts
    |> List.filter (fun value -> not (List.mem value (Outcome.named_facts projection)))
    |> missing "named"
  in
  let* () =
    expectation.process_facts
    |> List.filter (fun value -> not (List.mem value (Outcome.process_facts projection)))
    |> missing "process"
  in
  List.fold_left
    (fun result bound -> Result.bind result (fun () -> check_bound projection bound))
    (Ok ()) expectation.bounds

let describe expectation =
  let status =
    expectation.status |> Option.map Outcome.status_name
    |> Option.value ~default:"unselected"
  in
  let semantic =
    expectation.semantic_facts
    |> List.map (fun fact ->
           fact.Outcome.function_name ^ ":" ^ Outcome.semantic_kind_name fact.kind)
    |> String.concat ","
  in
  let units =
    expectation.units
    |> List.map (fun (name, disposition) ->
           name ^ ":" ^ Outcome.unit_disposition_name disposition)
    |> String.concat ","
  in
  let named =
    expectation.named_facts
    |> List.map (fun (name, fact) -> name ^ ":" ^ Outcome.named_fact_name fact)
    |> String.concat ","
  in
  let process =
    expectation.process_facts |> List.map Outcome.process_fact_name |> String.concat ","
  in
  let bounds =
    expectation.bounds
    |> List.map (fun bound ->
           Printf.sprintf "%s<=%d baseline=%d rationale=%s"
             (match bound.kind with Functions -> "functions" | Obligations -> "obligations")
             bound.maximum bound.baseline bound.rationale)
    |> String.concat ","
  in
  Printf.sprintf
    "status=%s;codes=[%s];semantic=[%s];units=[%s];facts=[%s];process=[%s];bounds=[%s]"
    status (String.concat "," expectation.frontend_codes) semantic units named process
    bounds
