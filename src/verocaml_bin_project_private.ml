open Verocaml_bin_render

type options = {
  inventory : (Verifier_service.scope_role * string * string) list;
  timeout_ms : int;
  rlimit : int option;
  threads : int;
}

type parsed = (options, string) result

let default_timeout_ms = 5000
let render_message = Verocaml_bin_render.message

let parse_positive_int flag value =
  match int_of_string_opt value with
  | Some value when value > 0 -> Ok value
  | _ -> Error (render_message (Positive_integer flag))

let parse_positive_decimal_int flag value =
  if
    String.length value > 0
    && String.for_all
         (fun character -> character >= '0' && character <= '9')
         value
  then parse_positive_int flag value
  else Error (render_message (Positive_integer flag))

let parse ~default_threads argv : parsed =
  let rec loop inventory timeout_ms timeout_seen rlimit rlimit_seen threads
      threads_seen solver_seen = function
    | [] ->
        if
          not
            (List.exists
               (fun (role, _, _) -> role = Verifier_service.Scope_root)
               inventory)
        then Error (render_message Project_inventory_required)
        else
          Ok
            {
              inventory = List.rev inventory;
              timeout_ms;
              rlimit;
              threads = Option.value ~default:(default_threads ()) threads;
            }
    | (("--root" | "--dependency") as flag) :: cmt :: cmi :: rest
      when not (String.starts_with ~prefix:"--" cmt)
           && not (String.starts_with ~prefix:"--" cmi) ->
        let role =
          if String.equal flag "--root" then Verifier_service.Scope_root
          else Scope_dependency
        in
        loop ((role, cmt, cmi) :: inventory) timeout_ms timeout_seen rlimit
          rlimit_seen threads threads_seen solver_seen rest
    | (("--root" | "--dependency") as flag) :: _ ->
        Error (render_message (Project_entry_requires flag))
    | "--solver" :: value :: rest ->
        if solver_seen then Error (render_message (Option_once "--solver"))
        else if not (String.equal value "z3") then
          Error (render_message (Unsupported_solver value))
        else
          loop inventory timeout_ms timeout_seen rlimit rlimit_seen threads
            threads_seen true rest
    | "--timeout-ms" :: value :: rest ->
        if timeout_seen then
          Error (render_message (Option_once "--timeout-ms"))
        else (
          match parse_positive_int "--timeout-ms" value with
          | Error _ as error -> error
          | Ok timeout_ms ->
              loop inventory timeout_ms true rlimit rlimit_seen threads
                threads_seen solver_seen rest)
    | "--rlimit" :: value :: rest ->
        if rlimit_seen then Error (render_message (Option_once "--rlimit"))
        else (
          match parse_positive_decimal_int "--rlimit" value with
          | Error _ as error -> error
          | Ok rlimit ->
              loop inventory timeout_ms timeout_seen (Some rlimit) true threads
                threads_seen solver_seen rest)
    | "--threads" :: value :: rest ->
        if threads_seen then Error (render_message (Option_once "--threads"))
        else (
          match parse_positive_decimal_int "--threads" value with
          | Error _ as error -> error
          | Ok threads ->
              loop inventory timeout_ms timeout_seen rlimit rlimit_seen
                (Some threads) true solver_seen rest)
    | flag :: _ when String.starts_with ~prefix:"--" flag ->
        Error (render_message (Unknown_option flag))
    | argument :: _ -> Error (render_message (Unexpected_argument argument))
  in
  match Array.to_list argv with
  | _ :: "verify-project" :: arguments ->
      loop [] default_timeout_ms false None false None false false arguments
  | _ -> Error Verocaml_bin_render.usage

let load_inventory inventory =
  let rec load loaded = function
    | [] -> Ok (List.rev loaded)
    | (role, cmt, cmi) :: rest -> (
        match Cmt_input.load_with_interface ~cmt ~cmi () with
        | Error diagnostic ->
            prerr_endline (Verocaml_bin_render.frontend_error diagnostic);
            Error ()
        | Ok implementation ->
            load ((role, cmt, cmi, implementation) :: loaded) rest)
  in
  load [] inventory

let result_name result =
  match Verifier_service.status result with
  | Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete-source"

let stdout_line row =
  let unit_name = Verifier_service.scoped_row_unit_name row
  and file = Verifier_service.scoped_row_cmt row in
  match
    ( Verifier_service.scoped_row_classification row,
      Verifier_service.scoped_row_outcome row )
  with
  | Scoped_verified, Scoped_verification result ->
      let summary =
        Printf.sprintf
          "verocaml: verified unit=%s file=%s result=%s functions=%d obligations=%d"
          unit_name file (result_name result)
          (Verifier_service.functions result)
          (Verifier_service.obligations result)
      in
      let imported_uses =
        Verifier_service.trusted_external_observations result
        |> List.filter_map (fun observation ->
               match Verifier_service.trusted_external_view observation with
               | Trusted_external_target_specification_use _ as view ->
                   Some (Verocaml_bin_render.trusted_external_line view)
               | Trusted_external_specification_use _
               | Trusted_external_body_use _
               | Trusted_external_body_declaration _ ->
                   None)
      in
      String.concat "\n" (imported_uses @ [ summary ])
  | Scoped_verified_dependency, Scoped_dependency_success ->
      Printf.sprintf
        "verocaml: verified-dependency unit=%s file=%s result=verified"
        unit_name file
  | Scoped_skipped, Scoped_skip ->
      Printf.sprintf "verocaml: skipped unit=%s file=%s result=skipped"
        unit_name file
  | (Scoped_verified | Scoped_verified_dependency), Scoped_rejection _ ->
      let classification =
        match Verifier_service.scoped_row_classification row with
        | Scoped_verified -> "verified"
        | Scoped_verified_dependency -> "verified-dependency"
        | Scoped_skipped -> assert false
      in
      Printf.sprintf "verocaml: %s unit=%s file=%s result=rejected"
        classification unit_name file
  | Scoped_verified_dependency, Scoped_verification _
  | Scoped_verified_dependency, Scoped_skip
  | Scoped_verified, Scoped_dependency_success
  | Scoped_verified, Scoped_skip
  | Scoped_skipped, Scoped_verification _
  | Scoped_skipped, Scoped_dependency_success
  | Scoped_skipped, Scoped_rejection _ ->
      invalid_arg "inconsistent scoped verification row"

let stderr_lines row =
  match Verifier_service.scoped_row_outcome row with
  | Scoped_verification result -> verification_stderr_lines result
  | Scoped_rejection error -> (
      match Verifier_service.scoped_row_classification row with
      | Scoped_verified_dependency -> []
      | Scoped_skipped -> assert false
      | Scoped_verified -> (
          match Verifier_service.error_diagnostic error with
          | Some diagnostic -> [ frontend_error diagnostic ]
          | None when Verifier_service.error_is_internal error ->
              [
                internal_error
                  "VeroCaml could not prepare this verification request. This is a verifier bug, not a failed proof. Re-run with DELATOR_LOG=debug to capture diagnostic details.";
              ]
          | None ->
              [
                dependency_error
                  ~unit_name:(Verifier_service.error_unit_name error)
                  ~message:(Verifier_service.error_message error);
              ]))
  | Scoped_dependency_success | Scoped_skip -> []

let exit_code rows =
  let rejected = ref false and inconclusive = ref false
  and counterexample = ref false in
  List.iter
    (fun row ->
      match Verifier_service.scoped_row_outcome row with
      | Scoped_rejection _ -> rejected := true
      | Scoped_verification result -> (
          match Verifier_service.status result with
          | Verified -> ()
          | Counterexample -> counterexample := true
          | Inconclusive | Incomplete_source -> inconclusive := true)
      | Scoped_dependency_success | Scoped_skip -> ())
    rows;
  if !rejected then 2
  else if !inconclusive then 3
  else if !counterexample then 1
  else 0

let verify_inventory ~startup_classification ~configuration inventory =
  match startup_classification with
  | Error diagnostic ->
      prerr_endline (frontend_error diagnostic);
      2
  | Ok () -> (
      match Verifier_service.scoped_request ~configuration ~inventory with
          | Error error ->
              prerr_endline
                (dependency_error
                   ~unit_name:
                     (Verifier_service.scoped_plan_error_unit_name error)
                   ~message:(Verifier_service.scoped_plan_error_message error));
              2
          | Ok request ->
              let result = Verifier_service.verify_scope request in
              let rows = Verifier_service.scoped_rows result in
              List.iter
                (fun row -> stderr_lines row |> List.iter prerr_endline)
                rows;
              List.iter (fun row -> stdout_line row |> print_endline) rows;
              exit_code rows)
[@@delator.instrument] [@@delator.level info]

let verify startup_classification options configuration =
  match load_inventory options.inventory with
  | Error () -> 2
  | Ok inventory ->
      verify_inventory ~startup_classification ~configuration inventory

let main ~startup_classification ~default_threads argv =
  match parse ~default_threads argv with
  | Error message ->
      prerr_endline (cli_error message);
      2
  | Ok options -> (
      match
        Verifier_service.configuration ~threads:options.threads
          ~timeout_ms:options.timeout_ms ~rlimit:options.rlimit
      with
      | Error configuration_error ->
          prerr_endline
            (cli_error
               (Verifier_service.configuration_error_message
                  configuration_error));
          2
      | Ok configuration -> verify startup_classification options configuration)
