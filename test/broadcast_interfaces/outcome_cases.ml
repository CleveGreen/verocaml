open Outcome_test_support

let ( let* ) = Result.bind
let suite_path = "test/broadcast_interfaces/outcome_cases.ml"

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let require condition message = if condition then Ok () else mismatch "%s" message
let file path contents = { Fixture.path; contents }

let existing_file path =
  try Sys.file_exists path && not (Sys.is_directory path) with Sys_error _ -> false

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755)

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let replace_first_frame_length encoded replacement =
  let colon = String.index encoded ':' in
  let rec length_start index =
    if index = 0 then 0
    else
      match encoded.[index - 1] with
      | '0' .. '9' -> length_start (index - 1)
      | _ -> index
  in
  let start = length_start colon in
  if start = colon then invalid_arg "encoded authority has no initial frame length";
  String.sub encoded 0 start ^ replacement
  ^ String.sub encoded colon (String.length encoded - colon)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let () =
  if Array.length Sys.argv = 3 && String.equal Sys.argv.(1) "--oversize-vri" then (
    let filename = Sys.argv.(2) in
    let encoded = read_file filename in
    write_file filename
      (replace_first_frame_length encoded (string_of_int max_int));
    exit 0)

let copy_file source destination =
  let input = open_in_bin source in
  let contents =
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () -> really_input_string input (in_channel_length input))
  in
  write_file destination contents

type captured_process = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let process_environment forwarded =
  let names = List.map fst forwarded in
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun binding ->
           match String.index_opt binding '=' with
           | None -> true
           | Some index ->
               not (List.mem (String.sub binding 0 index) names))
  in
  List.map (fun (name, value) -> name ^ "=" ^ value) forwarded @ inherited
  |> Array.of_list

let run_captured ~workspace ?stdout_path ~program ~arguments ~forwarded () =
  let stdout_path =
    Option.value stdout_path
      ~default:(Filename.concat workspace "captured.stdout")
  and stderr_path = Filename.concat workspace "captured.stderr" in
  mkdir_p (Filename.dirname stdout_path);
  mkdir_p (Filename.dirname stderr_path);
  let stdout_channel = open_out_bin stdout_path
  and stderr_channel = open_out_bin stderr_path in
  let process =
    Unix.create_process_env program
      (Array.of_list (program :: arguments))
      (process_environment forwarded) Unix.stdin
      (Unix.descr_of_out_channel stdout_channel)
      (Unix.descr_of_out_channel stderr_channel)
  in
  close_out_noerr stdout_channel;
  close_out_noerr stderr_channel;
  let _, status = Unix.waitpid [] process in
  { status; stdout = read_file stdout_path; stderr = read_file stderr_path }

let process_exited code process = process.status = Unix.WEXITED code

let run_compile_rejection ~environment ~workspace ~name
    (project : Fixture.dune_project) =
  let root = Filename.concat workspace name in
  List.iter
    (fun source ->
      write_file (Filename.concat root source.Fixture.path) source.contents)
    project.files;
  let* process =
    Process_adapter.run ~cwd:root
      {
        program = Project_environment.dune_path environment;
        arguments =
          [
            "build";
            "--root";
            root;
            "--build-dir";
            Filename.concat root "_build";
            "--profile";
            "release";
          ]
          @ project.targets;
        forwarded =
          [
            ("PATH", Project_environment.tool_path environment);
            ("OCAMLPATH", Project_environment.ocaml_path environment);
            ("DUNE_CACHE", "disabled");
            ("HOME", root);
            ("TMPDIR", root);
          ];
        cleanup_paths = [];
        adjacency = [];
      }
  in
  let rejected =
    Outcome.process_facts process
    |> List.exists (function
         | Outcome.Exit_class (Outcome.Exited code) -> code <> 0
         | Exit_class Signaled | Exit_class Stopped -> true
         | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false)
  in
  if rejected then
    Ok
      (Outcome.observation ~status:Outcome.Frontend_rejected
         ~process_facts:(Outcome.process_facts process) ()
      |> Outcome.project)
  else mismatch "compiler accepted forbidden broadcast authority route %s" name

let contains source fragment =
  let source_length = String.length source and length = String.length fragment in
  let rec loop index =
    index + length <= source_length
    && (String.sub source index length = fragment || loop (index + 1))
  in
  fragment = "" || loop 0

let public_diagnostic process =
  process.stderr |> String.split_on_char '\n'
  |> List.find_opt (fun line -> contains line "VERO_DEPENDENCY")

let redacts secrets value =
  List.for_all (fun secret -> secret = "" || not (contains value secret)) secrets
  && not (contains value "verocaml.internal.broadcast")
  && not (contains value "verocaml:broadcast:carrier:")

let diagnostic_redacts secrets (diagnostic : Diagnostic.t) =
  let classification_values =
    match diagnostic.classification with
    | Diagnostic.Invalid_broadcast reason -> [ reason ]
    | Diagnostic.Invalid_broadcast_dependency { provider; failure = _ } ->
        [ provider ]
    | Diagnostic.Unsupported_target _ | Diagnostic.Unsupported_input _
    | Diagnostic.Malformed_input | Diagnostic.Incompatible_magic
    | Diagnostic.Input_io_error | Diagnostic.Invalid_recursive_rank _
    | Diagnostic.Invalid_symbolic_declaration _
    | Diagnostic.Invalid_symbolic_application _
    | Diagnostic.Invalid_symbolic_authentication _
    | Diagnostic.Invalid_symbolic_dependency _
    | Diagnostic.Invalid_logical_constant_declaration _
    | Diagnostic.Invalid_logical_constant_use _
    | Diagnostic.Invalid_logical_constant_authentication _
    | Diagnostic.Invalid_numeric_declaration _
    | Diagnostic.Executable_function_in_specification _
    | Diagnostic.Unannotated_erased_call _
    | Diagnostic.Invalid_verification_call _
    | Diagnostic.Invalid_imported_specification _
    | Diagnostic.Invalid_semantic_program _
    | Diagnostic.Unsupported_construct _ -> []
  in
  List.for_all (redacts secrets)
    (diagnostic.code :: diagnostic.message :: diagnostic.span.file
   :: classification_values)

type route = Standalone | Ppxlib

let ppx route mode =
  match (route, mode) with
  | Standalone, `Retained ->
      "(flags (:standard -ppx \"verocaml-ppx --keep-ghost\"))"
  | Standalone, `Ordinary ->
      "(flags (:standard -ppx \"verocaml-ppx\"))"
  | Ppxlib, `Retained ->
      "(preprocess (pps verocaml.ppx -- --verocaml-retained))"
  | Ppxlib, `Ordinary -> "(preprocess (pps verocaml.ppx))"

let declared_authority_rule ?(dependencies = []) ?(transport_units = []) ?package
    ?destination library unit_name =
  let stem = String.uncapitalize_ascii unit_name in
  let object_path = "." ^ library ^ ".objs/byte/" ^ stem in
  let dependency_files =
    dependencies
    |> List.mapi (fun index (dependency_library, dependency) ->
           let dependency = String.uncapitalize_ascii dependency in
           let dependency_object =
             "." ^ dependency_library ^ ".objs/byte/" ^ dependency
           in
           Printf.sprintf
             "  (:dependency_%d_cmt %s.cmt)\n  (:dependency_%d_cmi %s.cmi)\n  (:dependency_%d_cmti %s.cmti)\n  (:dependency_%d_vri %s.vri)\n  (:dependency_%d_manifest %s.verocaml-retained-interface)"
             index dependency_object index dependency_object index
             dependency_object index dependency index dependency)
    |> String.concat "\n"
  in
  let dependency_files =
    if dependency_files = "" then "" else "\n" ^ dependency_files
  in
  let transport_files =
    transport_units
    |> List.mapi (fun index transport_unit ->
           Printf.sprintf "  (:transport_%d_cmi .%s.objs/byte/%s.cmi)" index
             library (String.uncapitalize_ascii transport_unit))
    |> String.concat "\n"
  in
  let transport_files =
    if transport_files = "" then "" else "\n" ^ transport_files
  in
  let dependency_directories =
    dependencies
    |> List.map (fun (dependency_library, _) ->
           " --artifact-directory ." ^ dependency_library ^ ".objs/byte")
    |> String.concat ""
  in
  let build_manifest = stem ^ ".verocaml-retained-interface"
  and install_manifest = stem ^ ".verocaml-retained-interface.install" in
  let rule =
    Printf.sprintf
    {|(rule
 (targets
  %s.vri
  %s
  %s)
 (deps
  (sandbox always)
  (:emitter %%{bin:verocaml-retained-interface})
  (:cmt %s.cmt)
  (:cmi %s.cmi)
  (:cmti %s.cmti)%s%s)
 (action
  (progn
   (run %%{emitter} emit %%{cmt} %%{cmi} %%{cmti} %s.vri
    --artifact-directory .
    --artifact-directory .%s.objs/byte%s)
   (run %%{emitter} manifest %s %s.cmt %s.cmi %s.cmti %s.vri)
   (run %%{emitter} manifest %s %s.cmt %s.cmi %s.cmti %s.vri))))
(alias
 (name verocaml-retained-interfaces)
 (deps %s.vri %s))
(alias (name all) (deps %s.vri %s))
|}
      stem build_manifest install_manifest object_path object_path object_path
      transport_files dependency_files stem library dependency_directories
      build_manifest object_path
      object_path object_path stem install_manifest stem stem stem stem stem
      build_manifest stem build_manifest
  in
  match (package, destination) with
  | Some package, Some destination ->
      rule
      ^ Printf.sprintf
          {|(install
 (package %s)
 (section lib)
 (files
  (%s.vri as %s/%s.vri)
  (%s as %s/%s)))
|}
          package stem destination stem install_manifest destination build_manifest
  | None, None -> rule
  | Some _, None | None, Some _ -> invalid_arg "incomplete authority install"

let provider_a_mli =
  {|type 'a aurora_carrier = 'a option
[@@verocaml.external_type_specification]
type ('a, 'error) violet_carrier = ('a, 'error) result
[@@verocaml.external_type_specification]

[%%verocaml.symbolic val observed_zero : int -> bool]
[%%verocaml.symbolic val nested_present : ('a option, 'b option) result -> bool]
[%%verocaml.symbolic val nested_fact : ('a option, 'b option) result -> bool]
[%%verocaml.symbolic val reflexive_probe : 'a -> bool]

val zero_axiom : int -> unit
[@@verocaml.proof] [@@verocaml.broadcast]

val nested_axiom : ('a option, 'b option) result -> unit
[@@verocaml.proof] [@@verocaml.broadcast]

val reflexive_lemma : 'a -> unit
[@@verocaml.proof] [@@verocaml.broadcast]

val private_control : int -> unit [@@verocaml.proof]

[@@@verocaml.broadcast_group (trusted_facts, [zero_axiom; nested_axiom])]
[@@@verocaml.broadcast_group (proved_facts, [reflexive_lemma])]
|}

let provider_a_ml =
  {|type 'a aurora_carrier = 'a option
[@@verocaml.external_type_specification]
type ('a, 'error) violet_carrier = ('a, 'error) result
[@@verocaml.external_type_specification]

[%%verocaml.symbolic val observed_zero : int -> bool]
[%%verocaml.symbolic val nested_present : ('a option, 'b option) result -> bool]
[%%verocaml.symbolic val nested_fact : ('a option, 'b option) result -> bool]
[%%verocaml.symbolic val reflexive_probe : 'a -> bool]

let zero_axiom (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed_zero value) [@trigger])) || value = 0];
  ignore value
[@@verocaml.axiom] [@@verocaml.broadcast]

let nested_axiom (value : ('a option, 'b option) result) : unit =
  [%verocaml.ensures fun _ ->
    (not ((nested_present value) [@trigger])) || nested_fact value];
  ()
[@@verocaml.axiom] [@@verocaml.broadcast]

let reflexive_lemma (value : 'a) : unit =
  [%verocaml.ensures fun _ ->
    (not ((reflexive_probe value) [@trigger])) || value = value];
  ()
[@@verocaml.proof] [@@verocaml.broadcast]

let private_control (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value];
  ()
[@@verocaml.proof]

let private_broadcast (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed_zero value) [@trigger])) || value = value];
  ()
[@@verocaml.proof] [@@verocaml.broadcast]

[@@@verocaml.broadcast_group (trusted_facts, [nested_axiom; zero_axiom])]
[@@@verocaml.broadcast_group (proved_facts, [reflexive_lemma])]
|}

let forwarder_mli =
  {|module Alias = Provider_a
open Provider_a

[@@@verocaml.broadcast_group (alias_zero, [Alias.zero_axiom])]
[@@@verocaml.broadcast_group (opened_nested, [nested_axiom])]
[@@@verocaml.broadcast_group (forwarded_trusted, [Alias.trusted_facts])]
[@@@verocaml.broadcast_group (forwarded_proved, [Alias.proved_facts])]
|}

let forwarder_ml =
  {|module Alias = Provider_a
open Provider_a

[@@@verocaml.broadcast_group (alias_zero, [Alias.zero_axiom])]
[@@@verocaml.broadcast_group (opened_nested, [nested_axiom])]
[@@@verocaml.broadcast_group (forwarded_trusted, [Alias.trusted_facts])]
[@@@verocaml.broadcast_group (forwarded_proved, [proved_facts])]
|}

let consumer_ml =
  {|[@@@verocaml.activate
  [Forwarder.alias_zero; Forwarder.opened_nested;
   Forwarder.forwarded_trusted; Forwarder.forwarded_proved]]

let consume_zero (value : int) : unit =
  [%verocaml.requires Provider_a.observed_zero value];
  [%verocaml.assert value = 0];
  ()
[@@verocaml.proof]

let consume_nested_int (value : (int option, bool option) result) : unit =
  [%verocaml.requires Provider_a.nested_present value];
  [%verocaml.assert Provider_a.nested_fact value];
  ()
[@@verocaml.proof]

let consume_nested_bool (value : (bool option, int option) result) : unit =
  [%verocaml.requires Provider_a.nested_present value];
  [%verocaml.assert Provider_a.nested_fact value];
  ()
[@@verocaml.proof]

let consume_proved (value : 'a) : unit =
  [%verocaml.requires Provider_a.reflexive_probe value];
  [%verocaml.assert value = value];
  ()
[@@verocaml.proof]

|}

let replace_first ~sub ~by source =
  let source_length = String.length source and sub_length = String.length sub in
  let rec find index =
    if index + sub_length > source_length then None
    else if String.sub source index sub_length = sub then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> source
  | Some index ->
      String.sub source 0 index ^ by
      ^ String.sub source (index + sub_length)
          (source_length - index - sub_length)

let direct_consumer_ml =
  replace_first consumer_ml
    ~sub:
      "Forwarder.alias_zero; Forwarder.opened_nested;\n   Forwarder.forwarded_trusted; Forwarder.forwarded_proved"
    ~by:
      "Provider_a.zero_axiom; Provider_a.nested_axiom;\n   Provider_a.trusted_facts; Provider_a.proved_facts"

let positive_project route =
  let dune =
    Printf.sprintf
      {|(library
 (name provider_a_library)
 (wrapped false)
 (modules Provider_a)
 (libraries verocaml.ghost)
 %s)

(library
 (name forwarder_library)
 (wrapped false)
 (modules Forwarder)
 (libraries provider_a_library verocaml.ghost)
 %s)

(library
 (name consumer_library)
 (wrapped false)
 (modules Consumer)
 (libraries provider_a_library forwarder_library verocaml.ghost)
 %s)

(library
 (name direct_consumer_library)
 (wrapped false)
 (modules Direct_consumer)
 (libraries provider_a_library verocaml.ghost)
 %s)
|}
      (ppx route `Retained) (ppx route `Retained) (ppx route `Retained)
      (ppx route `Retained)
  in
  let dune =
    dune
    ^ declared_authority_rule "provider_a_library" "Provider_a"
    ^ declared_authority_rule
        ~dependencies:[ ("provider_a_library", "Provider_a") ]
        "forwarder_library" "Forwarder"
  in
  Fixture.dune_project
    {
      files =
        [
          file "dune-project" "(lang dune 3.17)\n(name broadcast_interfaces)\n";
          file "dune" dune;
          file "provider_a.mli" provider_a_mli;
          file "provider_a.ml" provider_a_ml;
          file "forwarder.mli" forwarder_mli;
          file "forwarder.ml" forwarder_ml;
          file "consumer.ml" consumer_ml;
          file "direct_consumer.ml" direct_consumer_ml;
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units =
        [ "Provider_a"; "Forwarder"; "Consumer"; "Direct_consumer" ];
    }

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let artifact root unit_name extension =
  let expected = String.uncapitalize_ascii unit_name ^ extension in
  match
    files_below (Filename.concat root "_build")
    |> List.filter (fun path ->
           String.equal (Filename.basename path) expected
           && not (contains path "/install/"))
  with
  | [ path ] -> Ok path
  | [] -> mismatch "missing artifact %s%s" unit_name extension
  | _ -> mismatch "ambiguous artifact %s%s" unit_name extension

let optional_artifact root unit_name extension =
  let expected = String.uncapitalize_ascii unit_name ^ extension in
  match
    files_below (Filename.concat root "_build")
    |> List.filter (fun path ->
           String.equal (Filename.basename path) expected
           && not (contains path "/install/"))
  with
  | [ path ] -> Some path
  | [] | _ :: _ :: _ -> None

let installed_artifact root unit_name extension =
  let expected = String.uncapitalize_ascii unit_name ^ extension in
  let matches =
    files_below (Filename.concat root "_build/install/default")
    |> List.filter (fun path -> String.equal (Filename.basename path) expected)
  in
  let matches =
    match List.filter (fun path -> contains path "/.private/") matches with
    | _ :: _ as private_matches -> private_matches
    | [] -> matches
  in
  match matches with
  | [ path ] -> Ok path
  | [] -> mismatch "missing installed artifact %s%s" unit_name extension
  | _ -> mismatch "ambiguous installed artifact %s%s" unit_name extension

let load root unit_name =
  let* cmt = artifact root unit_name ".cmt" in
  let* cmi = artifact root unit_name ".cmi" in
  let cmti = optional_artifact root unit_name ".cmti"
  and vri = optional_artifact root unit_name ".vri" in
  let artifact_directories =
    files_below (Filename.concat root "_build/default")
    |> List.filter (fun filename ->
           List.mem (Filename.extension filename) [ ".cmi"; ".cmti"; ".vri" ])
    |> List.map Filename.dirname |> List.sort_uniq String.compare
  in
  match
    Cmt_input.load_with_interface ~cmt ~cmi ?cmti ?vri ~artifact_directories ()
  with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      mismatch "load %s failed: %s: %s" unit_name diagnostic.code
        diagnostic.message

let verify ~threads ~consumer ~dependencies =
  let* configuration =
    match Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None with
    | Ok value -> Ok value
    | Error error -> mismatch "%s" (Verifier_service.configuration_error_message error)
  in
  match
    Verifier_service.verify
      (Verifier_service.request ~configuration ~consumer ~dependencies)
  with
  | Ok result -> Ok result
  | Error error -> mismatch "%s" (Verifier_service.error_message error)

let imported_authority ~consumer ~dependencies =
  let* policy =
    Solver_policy_private.create_default ~timeout_ms:60_000
    |> Result.map_error (fun error ->
           Failure.make Failure.Expectation_mismatch
             (Solver_policy_private.error_to_string error))
  in
  let* environment, _ =
    Interface_specification_loaded_private.authenticate ~solver_policy:policy
      ~external_targets:[] ~dependencies ~consumer
    |> Result.map_error (fun error ->
           Failure.make Failure.Expectation_mismatch
             (Interface_specification_loaded_private.error_message error))
  in
  Interface_specification_environment_private.imported_environment_authenticated
    environment
  |> Result.map_error (fun error ->
         Failure.make Failure.Expectation_mismatch
           (Interface_specification_environment_private.error_to_string error))

let lifecycle_balanced () =
  let counters = Z3_bridge.counters () in
  counters.contexts_live = 0
  && counters.contexts_created = counters.contexts_cleaned
  && Verification_pipeline.For_testing.scheduler_creations ()
     = Verification_pipeline.For_testing.scheduler_stops ()
  && Function_vc_worker_private.For_testing.cleanup_failures () = 0

let reset_lifecycle () =
  Z3_bridge.reset_counters ();
  Verification_pipeline.For_testing.reset_scheduler_counts ();
  Function_vc_worker_private.For_testing.reset ()

let broadcast_accounting result =
  (Verifier_service.vir result).Vir.functions
  |> List.concat_map (fun execution -> execution.Vir.obligations)
  |> List.filter_map Broadcast_vc_private.report
  |> List.map (fun (report : Broadcast_vc_private.report) ->
         ( report.active_declarations,
           report.trusted_broadcast_declarations,
           report.trusted_broadcast_uses,
           List.fold_left
             (fun (proved, trusted) insertion ->
               if insertion.Broadcast_vc_private.trusted then
                 (proved, trusted + 1)
               else (proved + 1, trusted))
             (0, 0) report.inserted ))
  |> List.sort compare

let valid_broadcast_accounting accounting =
  let active, trusted_declarations, trusted_uses, proved, trusted =
    List.fold_left
      (fun (active, declarations, uses, proved, trusted)
           (report_active, report_declarations, report_uses,
            (report_proved, report_trusted)) ->
        ( active + report_active,
          declarations + report_declarations,
          uses + report_uses,
          proved + report_proved,
          trusted + report_trusted ))
      (0, 0, 0, 0, 0) accounting
  in
  active > 0 && trusted_declarations > 0 && trusted_uses > 0 && proved > 0
  && trusted > 0

let positive_case route name =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider_a" Outcome.Unit_verified
      |> Expectation.require_unit "Forwarder" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_unit "Direct_consumer" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let* outcome = Fixture.run ~environment ~workspace (positive_project route) in
      let root = Filename.concat workspace "project" in
      let* provider = load root "Provider_a" in
      let* forwarder = load root "Forwarder" in
      let* consumer = load root "Consumer" in
      let* direct_consumer = load root "Direct_consumer" in
      let* serial = verify ~threads:1 ~consumer ~dependencies:[ provider; forwarder ] in
      let* threaded = verify ~threads:2 ~consumer ~dependencies:[ provider; forwarder ] in
      let* direct = verify ~threads:1 ~consumer:direct_consumer ~dependencies:[ provider ] in
      let* imported =
        imported_authority ~consumer ~dependencies:[ provider; forwarder ]
      in
      let* () =
        require
          (Verifier_service.status serial = Verifier_service.Verified
          && Verifier_service.status threaded = Verifier_service.Verified
          && Verifier_service.status direct = Verifier_service.Verified)
          "serial/threaded imported broadcast verification diverged"
      in
      let consumer_functions =
        [
          "consume_zero";
          "consume_nested_int";
          "consume_nested_bool";
          "consume_proved";
        ]
      in
      let consumer_facts projection =
        Outcome.named_facts projection
        |> List.filter (fun (_, fact) ->
               match fact with
               | Outcome.Function_exists name -> List.mem name consumer_functions
               | Obligation_kind_exists { function_name; _ } ->
                   List.mem function_name consumer_functions)
        |> List.sort compare
      in
      let retained = Outcome.of_verifier_result serial in
      let* () =
        require
          (Outcome.status outcome = Outcome.Verified
          && Outcome.status retained = Outcome.Verified
          && consumer_facts outcome = consumer_facts retained)
          "source and retained-CMT broadcast consumer outcomes diverged"
      in
      let cli_root = Filename.concat workspace "source-cmt-parity" in
      let cli_root =
        if Filename.is_relative cli_root then Filename.concat (Sys.getcwd ()) cli_root
        else cli_root
      in
      let temp = Filename.concat cli_root "temp" in
      mkdir_p temp;
      write_file (Filename.concat cli_root "consumer.ml") consumer_ml;
      let* provider_cmt = artifact root "Provider_a" ".cmt" in
      let* provider_cmi = artifact root "Provider_a" ".cmi" in
      let* provider_cmti = artifact root "Provider_a" ".cmti" in
      let* provider_vri = artifact root "Provider_a" ".vri" in
      let* forwarder_cmt = artifact root "Forwarder" ".cmt" in
      let* forwarder_cmi = artifact root "Forwarder" ".cmi" in
      let* forwarder_cmti = artifact root "Forwarder" ".cmti" in
      let* forwarder_vri = artifact root "Forwarder" ".vri" in
      let* consumer_cmt = artifact root "Consumer" ".cmt" in
      let* consumer_cmi = artifact root "Consumer" ".cmi" in
      List.iter
        (fun source ->
          copy_file source
            (Filename.concat cli_root (Filename.basename source)))
        [
          provider_cmt;
          provider_cmi;
          provider_cmti;
          provider_vri;
          forwarder_cmt;
          forwarder_cmi;
          forwarder_cmti;
          forwarder_vri;
          consumer_cmt;
          consumer_cmi;
        ];
      let run_cli input threads =
        Process_adapter.run ~cwd:cli_root
          {
            program =
              Filename.concat
                (Project_environment.binary_root environment)
                "verocaml";
            arguments =
              [
                "verify";
                input;
                "--dependency";
                Filename.basename provider_cmt;
                "--dependency";
                Filename.basename forwarder_cmt;
                "--threads";
                string_of_int threads;
                "--timeout-ms";
                "60000";
              ];
            forwarded =
              [
                ("PATH", Project_environment.tool_path environment);
                ("OCAMLPATH", Project_environment.ocaml_path environment);
                ("DUNE_CACHE", "disabled");
                ("HOME", cli_root);
                ("TMPDIR", temp);
                ("OCAML_COLOR", "never");
              ];
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* source_cli = run_cli (Filename.concat cli_root "consumer.ml") 1 in
      let* cmt_cli = run_cli (Filename.basename consumer_cmt) 2 in
      let exited_zero projection =
        Outcome.process_facts projection
        |> List.exists (function
             | Outcome.Exit_class (Outcome.Exited 0) -> true
             | Exit_class (Exited _) | Exit_class Signaled | Exit_class Stopped
             | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false)
      in
      let* () =
        require
          (exited_zero source_cli && exited_zero cmt_cli
          && Array.length (Sys.readdir temp) = 0)
          "source/retained-CMT CLI parity or private source cleanup diverged"
      in
      let serial_accounting = broadcast_accounting serial
      and threaded_accounting = broadcast_accounting threaded
      and direct_accounting = broadcast_accounting direct in
      let* () =
        require
          (valid_broadcast_accounting serial_accounting
          && serial_accounting = threaded_accounting
          && serial_accounting = direct_accounting)
          "proved/trusted insertion or trust accounting diverged across serial/threaded/direct/detached verification"
      in
      let* () =
        let declarations = Imported_callable.broadcast_declarations imported in
        require
          (List.exists
             (fun declaration ->
               declaration.Imported_callable.trust
               = Retained_broadcast_private.Proved)
             declarations
          && List.exists
               (fun declaration ->
                 declaration.Imported_callable.trust
                 = Retained_broadcast_private.Trusted)
               declarations)
          "proved/trusted imported authorities were not classified distinctly"
      in
      let imported_paths =
        provider.interface_broadcasts
        |> List.map (fun member -> member.Retained_broadcast_private.identity.canonical_path)
      in
      let* () =
        require
          (not (List.exists (String.ends_with ~suffix:"private_broadcast") imported_paths))
          "implementation-private broadcast escaped into the retained interface"
      in
      let* configuration =
        Verifier_service.configuration ~threads:1 ~timeout_ms:60_000 ~rlimit:None
        |> Result.map_error (fun error ->
               Failure.make Failure.Expectation_mismatch
                 (Verifier_service.configuration_error_message error))
      in
      Solver_backend_counter_private.reset_solver_creation_count ();
      reset_lifecycle ();
      let missing_dependency =
        Verifier_service.verify
          (Verifier_service.request ~configuration ~consumer
             ~dependencies:[ forwarder ])
      in
      let* () =
        match missing_dependency with
        | Ok _ -> mismatch "missing broadcast dependency was accepted"
        | Error error ->
            require
              (Verifier_service.error_classification error
                = Verifier_service.Dependency_error
              && Solver_backend_counter_private.solver_creation_count () = 0)
              "missing broadcast dependency reached solver/trust admission"
      in
      let* () = require (lifecycle_balanced ()) "verification lifecycle did not balance" in
      Ok outcome)

let installed_provider_project =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name installed_broadcast)\n(package (name installed_broadcast))\n";
          file "dune"
            ({|(library
 (name installed_provider)
 (public_name installed_broadcast.provider)
 (wrapped false)
 (modules Provider_a)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|}
            ^ declared_authority_rule ~package:"installed_broadcast"
                ~destination:"provider/.private" "installed_provider" "Provider_a");
          file "provider_a.mli" provider_a_mli;
          file "provider_a.ml" provider_a_ml;
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@install" ];
      selected_units = [ "Provider_a" ];
    }

let installed_adjacent_cmti_case =
  Suite.case ~name:"installed-provider-adjacent-cmti-load-route"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* outcome =
        Fixture.run ~environment ~workspace installed_provider_project
      in
      let root = Filename.concat workspace "project" in
      let* cmt = installed_artifact root "Provider_a" ".cmt" in
      let* cmi = installed_artifact root "Provider_a" ".cmi" in
      let* vri = installed_artifact root "Provider_a" ".vri" in
      let cmti = Filename.remove_extension cmt ^ ".cmti" in
      let* implementation =
        if not (Sys.file_exists cmti) then
          mismatch "installed retained provider lacks adjacent CMTI"
        else
          match Cmt_input.load_with_interface ~cmt ~cmi ~cmti ~vri () with
          | Ok implementation -> Ok implementation
          | Error diagnostic ->
              mismatch "installed adjacent CMTI load failed: %s"
                diagnostic.code
      in
      let* () =
        require (implementation.interface_broadcasts <> [])
          "installed adjacent CMTI lost retained broadcast authority"
      in
      let split_cmt_directory = Filename.concat workspace "split/cmt"
      and split_interface_directory = Filename.concat workspace "split/interface" in
      mkdir_p split_cmt_directory;
      mkdir_p split_interface_directory;
      let split_cmt = Filename.concat split_cmt_directory "provider_a.cmt"
      and split_cmi = Filename.concat split_interface_directory "provider_a.cmi"
      and split_cmti = Filename.concat split_interface_directory "provider_a.cmti"
      and split_vri = Filename.concat split_interface_directory "provider_a.vri" in
      copy_file cmt split_cmt;
      copy_file cmi split_cmi;
      copy_file cmti split_cmti;
      copy_file vri split_vri;
      let* () =
        match
          Cmt_input.load_with_interface ~cmt:split_cmt ~cmi:split_cmi
            ~cmti:split_cmti ~vri:split_vri ()
        with
        | Ok split ->
            require
              (String.equal
                 (Cmt_input.broadcast_complete_receipt split)
                 (Cmt_input.broadcast_complete_receipt implementation)
              && List.length split.interface_broadcasts
                 = List.length implementation.interface_broadcasts)
              "split CMT and CMI/CMTI layout changed retained identities"
        | Error diagnostic ->
            mismatch "split CMT and CMI/CMTI layout failed: %s" diagnostic.code
      in
      Ok outcome)

let ecosystem_provider_mli =
  {|[%%verocaml.symbolic val observed_zero : int -> bool]

val zero_axiom : int -> unit
[@@verocaml.proof] [@@verocaml.broadcast]
|}

let ecosystem_provider_ml =
  {|[%%verocaml.symbolic val observed_zero : int -> bool]

let zero_axiom (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed_zero value) [@trigger])) || value = 0];
  ()
[@@verocaml.axiom] [@@verocaml.broadcast]
|}

let ecosystem_consumer_ml =
  {|[@@@verocaml.verify]
[@@@verocaml.activate [Arbitrary_provider.zero_axiom]]

let consume (value : int) : unit =
  [%verocaml.requires Arbitrary_provider.observed_zero value];
  [%verocaml.assert value = 0];
  ()
[@@verocaml.proof]
|}

let dune_ecosystem_transport_case =
  Suite.case
    ~name:
      "declared-manifest-real-prefix-cold-consumer-and-verifier-no-write"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let workspace =
        if Filename.is_relative workspace then Filename.concat (Sys.getcwd ()) workspace
        else workspace
      in
      let provider_root = Filename.concat workspace "arbitrary-provider"
      and prefix = Filename.concat workspace "installed-prefix" in
      let library_directory = Filename.concat provider_root "lib" in
      mkdir_p library_directory;
      write_file (Filename.concat provider_root "dune-project")
        "(lang dune 3.17)\n(name arbitrary_bundle)\n(package (name arbitrary_bundle))\n";
      write_file (Filename.concat library_directory "arbitrary_provider.ml")
        ecosystem_provider_ml;
      write_file (Filename.concat library_directory "arbitrary_provider.mli")
        ecosystem_provider_mli;
      let generated_fragment =
        Filename.concat library_directory "arbitrary_provider.verocaml.inc"
      in
      let generator =
        run_captured ~workspace:(Filename.concat workspace "generator")
          ~stdout_path:generated_fragment
          ~program:
            (Filename.concat
               (Project_environment.binary_root environment)
               "verocaml-retained-interface")
          ~arguments:
            [
              "dune-stanza";
              "arbitrary_bundle";
              "api";
              "arbitrary_api";
              "Arbitrary_provider";
            ]
          ~forwarded:
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("OCAML_COLOR", "never");
            ]
          ()
      in
      let* () =
        require (process_exited 0 generator)
          ("retained-interface stanza generator failed: " ^ generator.stderr)
      in
      let fragment = read_file generated_fragment in
      let* () =
        require
          (contains fragment "arbitrary_provider.verocaml-retained-interface"
          && contains fragment "arbitrary_provider.vri"
          && contains fragment ".arbitrary_api.objs/byte/arbitrary_provider.cmt"
          && not (contains fragment "verocaml_authority")
          && not (contains fragment "installed_broadcast"))
          "generated Dune integration omitted declared products or embedded a provider name"
      in
      write_file (Filename.concat library_directory "dune")
        {|(library
 (name arbitrary_api)
 (public_name arbitrary_bundle.api)
 (wrapped false)
 (modules Arbitrary_provider)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(include arbitrary_provider.verocaml.inc)
|};
      let provider_temp = Filename.concat provider_root "temp" in
      mkdir_p provider_temp;
      let provider_environment =
        [
          ( "PATH",
            Project_environment.tool_path environment
            ^ ":" ^ Option.value ~default:"" (Sys.getenv_opt "PATH") );
          ("OCAMLPATH", Project_environment.ocaml_path environment);
          ("DUNE_CACHE", "disabled");
          ("HOME", provider_root);
          ("TMPDIR", provider_temp);
          ("OCAML_COLOR", "never");
        ]
      in
      let provider_build =
        run_captured ~workspace:(Filename.concat workspace "provider-build")
          ~program:(Project_environment.dune_path environment)
          ~arguments:
            [
              "build";
              "--root";
              provider_root;
              "--build-dir";
              Filename.concat provider_root "_build";
              "--profile";
              "release";
              "@install";
            ]
          ~forwarded:provider_environment ()
      in
      let* () =
        require (process_exited 0 provider_build)
          ("arbitrary provider build failed: " ^ provider_build.stderr)
      in
      let build_directory =
        Filename.concat provider_root "_build/default/lib"
      in
      let build_manifest =
        Filename.concat build_directory
          "arbitrary_provider.verocaml-retained-interface"
      and build_vri =
        Filename.concat build_directory "arbitrary_provider.vri"
      in
      let* () =
        require
          (existing_file build_vri && existing_file build_manifest
          && String.equal (read_file build_manifest)
               "verocaml-retained-interface-manifest-v1\n.arbitrary_api.objs/byte/arbitrary_provider.cmt\n.arbitrary_api.objs/byte/arbitrary_provider.cmi\n.arbitrary_api.objs/byte/arbitrary_provider.cmti\narbitrary_provider.vri\n")
          "Dune did not own the declared VRI target and relative build manifest"
      in
      let install =
        run_captured ~workspace:(Filename.concat workspace "provider-install")
          ~program:(Project_environment.dune_path environment)
          ~arguments:
            [
              "install";
              "--root";
              provider_root;
              "--build-dir";
              Filename.concat provider_root "_build";
              "--prefix";
              prefix;
              "arbitrary_bundle";
            ]
          ~forwarded:provider_environment ()
      in
      let* () =
        require (process_exited 0 install)
          ("real-prefix provider install failed: " ^ install.stderr)
      in
      let installed_library =
        Filename.concat prefix "lib/arbitrary_bundle/api"
      in
      let installed_manifest =
        Filename.concat installed_library
          "arbitrary_provider.verocaml-retained-interface"
      and installed_cmt =
        Filename.concat installed_library "arbitrary_provider.cmt"
      and installed_cmi =
        Filename.concat installed_library "arbitrary_provider.cmi"
      and installed_cmti =
        Filename.concat installed_library "arbitrary_provider.cmti"
      and installed_vri =
        Filename.concat installed_library "arbitrary_provider.vri"
      in
      let installed_family =
        [
          installed_manifest;
          installed_cmt;
          installed_cmi;
          installed_cmti;
          installed_vri;
        ]
      in
      let* () =
        require
          (List.for_all existing_file installed_family
          && String.equal (read_file installed_manifest)
               "verocaml-retained-interface-manifest-v1\narbitrary_provider.cmt\narbitrary_provider.cmi\narbitrary_provider.cmti\narbitrary_provider.vri\n")
          "real prefix omitted the relocatable retained-interface family"
      in
      let installed_receipts = List.map Digest.file installed_family in
      let removed_provider = Filename.concat workspace "provider-source-removed" in
      Unix.rename provider_root removed_provider;
      let consumer_root = Filename.concat workspace "cold-consumer" in
      mkdir_p consumer_root;
      write_file (Filename.concat consumer_root "dune-project")
        "(lang dune 3.17)\n(name cold_consumer)\n";
      write_file (Filename.concat consumer_root "dune")
        {|(library
 (name cold_consumer)
 (wrapped false)
 (modules Consumer)
 (libraries arbitrary_bundle.api verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
      write_file (Filename.concat consumer_root "consumer.ml")
        ecosystem_consumer_ml;
      let consumer_temp = Filename.concat consumer_root "temp" in
      mkdir_p consumer_temp;
      let consumer_environment =
        [
          ("PATH", Project_environment.tool_path environment);
          ( "OCAMLPATH",
            Filename.concat prefix "lib"
            ^ ":" ^ Project_environment.ocaml_path environment );
          ("VEROCAML_DUNE", Project_environment.dune_path environment);
          ("DUNE_CACHE", "disabled");
          ("HOME", consumer_root);
          ("TMPDIR", consumer_temp);
          ("OCAML_COLOR", "never");
        ]
      in
      let* () =
        require (not (Sys.file_exists (Filename.concat consumer_root "_build")))
          "cold consumer was built before one-step verification"
      in
      let cold =
        run_captured ~workspace:(Filename.concat workspace "cold-run")
          ~program:
            (Filename.concat
               (Project_environment.binary_root environment)
               "verocaml")
          ~arguments:
            [
              "verify";
              consumer_root;
              "--threads";
              "1";
              "--timeout-ms";
              "60000";
            ]
          ~forwarded:consumer_environment ()
      in
      let* () =
        require
          (process_exited 0 cold
          && contains cold.stdout "result=verified"
          && Sys.file_exists (Filename.concat consumer_root "_build"))
          ("cold installed-provider verification failed: " ^ cold.stderr)
      in
      let* () =
        require
          (installed_receipts = List.map Digest.file installed_family
          && not
               (List.exists
                  (fun path ->
                    String.equal (Filename.basename path)
                      "arbitrary_provider.vri")
                  (files_below (Filename.concat consumer_root "_build"))))
          "directory verification copied or rewrote the installed provider family"
      in
      let withheld_vri = installed_vri ^ ".withheld" in
      Unix.rename installed_vri withheld_vri;
      let withheld_receipt = Digest.file withheld_vri in
      let rejected =
        run_captured ~workspace:(Filename.concat workspace "missing-vri-run")
          ~program:
            (Filename.concat
               (Project_environment.binary_root environment)
               "verocaml")
          ~arguments:
            [
              "verify";
              consumer_root;
              "--threads";
              "1";
              "--timeout-ms";
              "60000";
            ]
          ~forwarded:consumer_environment ()
      in
      let rejected_diagnostic = public_diagnostic rejected in
      let* () =
        require
          (process_exited 2 rejected
          && Option.fold ~none:false
               ~some:(fun diagnostic ->
                 contains diagnostic "VERO_DEPENDENCY"
                 && contains diagnostic "arbitrary_provider"
                 && contains diagnostic "rebuild or reinstall"
                 && not (contains diagnostic installed_library)
                 && not (contains diagnostic "manifest"))
               rejected_diagnostic
          && not (Sys.file_exists installed_vri)
          && Sys.file_exists withheld_vri
          && Digest.file withheld_vri = withheld_receipt
          && not
               (List.exists
                  (fun path ->
                    String.equal (Filename.basename path)
                      "arbitrary_provider.vri")
                  (files_below (Filename.concat consumer_root "_build"))))
          "verifier recreated a missing provider VRI or omitted the actionable rejection"
      in
      Unix.rename withheld_vri installed_vri;
      let withheld_manifest = installed_manifest ^ ".withheld" in
      Unix.rename installed_manifest withheld_manifest;
      let undeclared =
        run_captured ~workspace:(Filename.concat workspace "missing-manifest-run")
          ~program:
            (Filename.concat
               (Project_environment.binary_root environment)
               "verocaml")
          ~arguments:
            [
              "verify";
              consumer_root;
              "--threads";
              "1";
              "--timeout-ms";
              "60000";
            ]
          ~forwarded:consumer_environment ()
      in
      let undeclared_diagnostic = public_diagnostic undeclared in
      let* () =
        require
          (process_exited 2 undeclared
          && Option.fold ~none:false
               ~some:(fun diagnostic ->
                 contains diagnostic "VERO_DEPENDENCY"
                 && contains diagnostic "rebuild"
                 && not (contains diagnostic installed_library)
                 && not (contains diagnostic "manifest"))
               undeclared_diagnostic
          && existing_file installed_vri
          && existing_file withheld_manifest
          && not (existing_file installed_manifest))
          "an undeclared adjacent VRI granted authority or produced an unsafe diagnostic"
      in
      Unix.rename withheld_manifest installed_manifest;
      let declared_manifest = read_file installed_manifest in
      write_file installed_manifest "invalid-declared-artifact-inventory\n";
      let malformed =
        run_captured ~workspace:(Filename.concat workspace "malformed-manifest-run")
          ~program:
            (Filename.concat
               (Project_environment.binary_root environment)
               "verocaml")
          ~arguments:
            [
              "verify";
              consumer_root;
              "--threads";
              "1";
              "--timeout-ms";
              "60000";
            ]
          ~forwarded:consumer_environment ()
      in
      let malformed_diagnostic = public_diagnostic malformed in
      let* () =
        require
          (process_exited 2 malformed
          && Option.fold ~none:false
               ~some:(fun diagnostic ->
                 contains diagnostic "VERO_DEPENDENCY"
                 && contains diagnostic "arbitrary_provider"
                 && contains diagnostic "rebuild or reinstall"
                 && not (contains diagnostic installed_library)
                 && not (contains diagnostic "manifest"))
               malformed_diagnostic
          && existing_file installed_vri)
          "a malformed declared artifact inventory escaped dependency diagnostics"
      in
      write_file installed_manifest declared_manifest;
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let split_inventory_project_case =
  Suite.case
    ~name:"fully-split-transitive-vri-inventory-relative-absolute-parity"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* _ = Fixture.run ~environment ~workspace (positive_project Standalone) in
      let root = Filename.concat workspace "project" in
      let* provider_cmt = artifact root "Provider_a" ".cmt" in
      let* provider_cmi = artifact root "Provider_a" ".cmi" in
      let* provider_cmti = artifact root "Provider_a" ".cmti" in
      let* provider_vri = artifact root "Provider_a" ".vri" in
      let* forwarder_cmt = artifact root "Forwarder" ".cmt" in
      let* forwarder_cmi = artifact root "Forwarder" ".cmi" in
      let* forwarder_cmti = artifact root "Forwarder" ".cmti" in
      let* forwarder_vri = artifact root "Forwarder" ".vri" in
      let* consumer_cmt = artifact root "Consumer" ".cmt" in
      let* consumer_cmi = artifact root "Consumer" ".cmi" in
      let inventory_root = Filename.concat workspace "split-inventory" in
      mkdir_p inventory_root;
      let inventory_root = Unix.realpath inventory_root in
      let place directory source =
        let directory = Filename.concat inventory_root directory in
        mkdir_p directory;
        let destination = Filename.concat directory (Filename.basename source) in
        copy_file source destination;
        destination
      in
      let provider_cmt = place "provider-cmt" provider_cmt
      and provider_cmi = place "provider-cmi" provider_cmi
      and provider_cmti = place "provider-cmti" provider_cmti
      and provider_vri = place "provider-vri" provider_vri
      and forwarder_cmt = place "forwarder-cmt" forwarder_cmt
      and forwarder_cmi = place "forwarder-cmi" forwarder_cmi
      and forwarder_cmti = place "forwarder-cmti" forwarder_cmti
      and forwarder_vri = place "forwarder-vri" forwarder_vri
      and consumer_cmt = place "consumer-cmt" consumer_cmt
      and consumer_cmi = place "consumer-cmi" consumer_cmi in
      Unix.rename (Filename.concat root "_build")
        (Filename.concat workspace "removed-original-build");
      let source = Filename.concat inventory_root "consumer.ml"
      and source_inventory = Filename.concat inventory_root "source.inventory" in
      write_file source consumer_ml;
      write_file source_inventory
        (String.concat ""
           [
             "verocaml-artifact-inventory-v1\n";
             Printf.sprintf "dependency\t%s\t%s\t%s\t%s\n" provider_cmt
               provider_cmi provider_cmti provider_vri;
             Printf.sprintf "dependency\t%s\t%s\t%s\t%s\n" forwarder_cmt
               forwarder_cmi forwarder_cmti forwarder_vri;
           ]);
      let run paths =
        let temp = Filename.concat inventory_root "temp" in
        mkdir_p temp;
        Process_adapter.run ~cwd:inventory_root
          {
            program =
              Filename.concat (Project_environment.binary_root environment)
                "verocaml";
            arguments =
              [
                "verify-project";
                "--dependency";
                paths provider_cmt;
                paths provider_cmi;
                paths provider_cmti;
                paths provider_vri;
                "--dependency";
                paths forwarder_cmt;
                paths forwarder_cmi;
                paths forwarder_cmti;
                paths forwarder_vri;
                "--root";
                paths consumer_cmt;
                paths consumer_cmi;
                "--threads";
                "2";
                "--timeout-ms";
                "60000";
              ];
            forwarded =
              [
                ("PATH", Project_environment.tool_path environment);
                ("OCAMLPATH", Project_environment.ocaml_path environment);
                ("DUNE_CACHE", "disabled");
                ("HOME", inventory_root);
                ("TMPDIR", temp);
                ("OCAML_COLOR", "never");
              ];
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let relative path =
        let prefix = inventory_root ^ Filename.dir_sep in
        String.sub path (String.length prefix)
          (String.length path - String.length prefix)
      in
      let* absolute = run Fun.id in
      let* relative = run relative in
      let run_source threads =
        let temp = Filename.concat inventory_root ("source-temp-" ^ threads) in
        mkdir_p temp;
        Process_adapter.run ~cwd:inventory_root
          {
            program =
              Filename.concat (Project_environment.binary_root environment)
                "verocaml";
            arguments =
              [
                "verify";
                source;
                "--inventory";
                source_inventory;
                "--threads";
                threads;
                "--timeout-ms";
                "60000";
              ];
            forwarded =
              [
                ("PATH", Project_environment.tool_path environment);
                ("OCAMLPATH", Project_environment.ocaml_path environment);
                ("DUNE_CACHE", "disabled");
                ("HOME", inventory_root);
                ("TMPDIR", temp);
                ("OCAML_COLOR", "never");
              ];
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* source_serial = run_source "1" in
      let* source_threaded = run_source "2" in
      let exited_zero projection =
        Outcome.process_facts projection
        |> List.exists (function
             | Outcome.Exit_class (Outcome.Exited 0) -> true
             | Exit_class (Exited _) | Exit_class Signaled | Exit_class Stopped
             | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false)
      in
      let* () =
        let* () =
          require (exited_zero absolute)
            "fully split absolute transitive inventory failed"
        in
        require (exited_zero relative)
          "fully split relative transitive inventory failed"
      in
      let* () =
        require
          (exited_zero source_serial && exited_zero source_threaded)
          "fully split source inventory diverged from project or threaded routes"
      in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

type mutation =
  | Substitution
  | Duplicate
  | Kind_change
  | Unknown
  | Shadow_substitution
  | Shadow_selected
  | Order_only

let mutation_name = function
  | Substitution -> "substitution"
  | Duplicate -> "duplicate"
  | Kind_change -> "kind-change"
  | Unknown -> "unknown"
  | Shadow_substitution -> "open-shadow-substitution"
  | Shadow_selected -> "open-shadow-selected"
  | Order_only -> "order-only"

let simple_a_mli =
  {|[%%verocaml.symbolic val probe_a : int -> bool]
val lemma_a : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
val lemma_b : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
val ordinary : int -> unit [@@verocaml.proof]
[@@@verocaml.broadcast_group (base_group, [lemma_a])]
|}

let simple_a_ml =
  {|[%%verocaml.symbolic val probe_a : int -> bool]
let lemma_a (value : int) : unit =
  [%verocaml.ensures fun _ -> (not ((probe_a value) [@trigger])) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
let lemma_b (value : int) : unit =
  [%verocaml.ensures fun _ -> (not ((probe_a value) [@trigger])) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
let ordinary (value : int) : unit = [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof]
[@@@verocaml.broadcast_group (base_group, [lemma_a])]
|}

let mutation_sources mutation =
  match mutation with
  | Substitution ->
      ( "[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_a])]\n",
        "[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_b])]\n" )
  | Duplicate ->
      ( "module Alias = Provider_a\n[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_a; Alias.lemma_a])]\n",
        "module Alias = Provider_a\n[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_a])]\n" )
  | Kind_change ->
      ( "[@@@verocaml.broadcast_group (selected, [Provider_a.base_group])]\n",
        "[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_a])]\n" )
  | Unknown ->
      ( "[@@@verocaml.broadcast_group (selected, [Provider_a.ordinary])]\n",
        "[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_a])]\n" )
  | Shadow_substitution ->
      ( "open Provider_a\nopen Provider_c\n[@@@verocaml.broadcast_group (selected, [lemma_a])]\n",
        "open Provider_a\nopen Provider_c\n[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_a])]\n" )
  | Shadow_selected ->
      ( "open Provider_a\nopen Provider_c\n[@@@verocaml.broadcast_group (selected, [lemma_a])]\n",
        "open Provider_a\nopen Provider_c\n[@@@verocaml.broadcast_group (selected, [Provider_c.lemma_a])]\n" )
  | Order_only ->
      ( "[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_a; Provider_a.lemma_b])]\n",
        "[@@@verocaml.broadcast_group (selected, [Provider_a.lemma_b; Provider_a.lemma_a])]\n" )

let mutation_project mutation =
  let interface_source, implementation_source = mutation_sources mutation in
  let has_provider_c =
    mutation = Shadow_substitution || mutation = Shadow_selected
  in
  let has_valid_mutation_authority =
    mutation = Order_only || mutation = Shadow_selected
  in
  let dune =
    Printf.sprintf
      {|(library
 (name provider_a_library)
 (wrapped false)
 (modules Provider_a)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
%s
(library
 (name mutation_library)
 (wrapped false)
 (modules Mutation_provider)
 (libraries provider_a_library %s verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name mutation_consumer_library)
 (wrapped false)
 (modules Mutation_consumer)
 (libraries mutation_library provider_a_library %s verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|}
      (if has_provider_c then
         {|(library
 (name provider_c_library)
 (wrapped false)
 (modules Provider_c)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|}
       else "")
      (if has_provider_c then "provider_c_library" else "")
      (if has_provider_c then "provider_c_library" else "")
    ^ declared_authority_rule "provider_a_library" "Provider_a"
    ^ (if has_provider_c then
         declared_authority_rule "provider_c_library" "Provider_c"
       else "")
    ^
    if has_valid_mutation_authority then
      declared_authority_rule
        ~dependencies:
          ([ ("provider_a_library", "Provider_a") ]
          @ if has_provider_c then [ ("provider_c_library", "Provider_c") ]
            else [])
        "mutation_library" "Mutation_provider"
    else ""
  in
  let provider_c_files =
    if has_provider_c then
      [
        file "provider_c.mli" simple_a_mli;
        file "provider_c.ml"
          (String.concat ""
             [
               "[%%verocaml.symbolic val probe_a : int -> bool]\n";
               "let lemma_a (value : int) : unit = [%verocaml.ensures fun _ -> (not ((probe_a value) [@trigger])) || value = value]; () [@@verocaml.proof] [@@verocaml.broadcast]\n";
               "let lemma_b (value : int) : unit = [%verocaml.ensures fun _ -> (not ((probe_a value) [@trigger])) || value = value]; () [@@verocaml.proof] [@@verocaml.broadcast]\n";
               "let ordinary (value : int) : unit = [%verocaml.ensures fun _ -> value = value]; () [@@verocaml.proof]\n";
               "[@@@verocaml.broadcast_group (base_group, [lemma_a])]\n";
             ]);
      ]
    else []
  in
  Fixture.dune_project
    {
      files =
        [
          file "dune-project" "(lang dune 3.17)\n(name broadcast_mutation)\n";
          file "dune" dune;
          file "provider_a.mli" simple_a_mli;
          file "provider_a.ml" simple_a_ml;
          file "mutation_provider.mli" interface_source;
          file "mutation_provider.ml" implementation_source;
          file "mutation_consumer.ml"
            ("open Mutation_provider\nopen Provider_a\n"
            ^ (if has_provider_c then "open Provider_c\n" else "")
            ^ "let touch (value : int) : unit = [%verocaml.ensures fun _ -> value = value]; () [@@verocaml.proof]\n");
        ]
        @ provider_c_files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units =
        [ "Provider_a" ]
        @ (if has_provider_c then [ "Provider_c" ] else [])
        @ [ "Mutation_provider" ]
        @
        if mutation = Order_only || mutation = Shadow_selected then
          [ "Mutation_consumer" ]
        else [];
    }

let mutation_case mutation =
  let accepted = mutation = Order_only || mutation = Shadow_selected in
  Suite.case ~name:("exact-set-" ^ mutation_name mutation)
    ~expectation:
      (Expectation.empty
      |> Expectation.status
           (if accepted then Outcome.Verified else Outcome.Frontend_rejected))
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let* outcome = Fixture.run ~environment ~workspace (mutation_project mutation) in
      let* () = require (lifecycle_balanced ()) "mutation lifecycle did not balance" in
      Ok outcome)

let same_cmi_shadow_project reversed =
  let opens =
    if reversed then "open Provider_c\nopen Provider_a\n"
    else "open Provider_a\nopen Provider_c\n"
  in
  let provider_source =
    {|let lemma (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|}
  and provider_interface =
    "val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]\n"
  in
  Fixture.dune_project
    {
      files =
        [
          file "dune-project" "(lang dune 3.17)\n(name same_cmi_shadow)\n";
          file "dune"
            ({|(library
 (name provider_a_library)
 (wrapped false)
 (modules Provider_a)
 (libraries verocaml.ghost)
 (flags (:standard -w -27-33-39 -ppx "verocaml-ppx --keep-ghost")))
(library
 (name provider_c_library)
 (wrapped false)
 (modules Provider_c)
 (libraries verocaml.ghost)
 (flags (:standard -w -27-33-39 -ppx "verocaml-ppx --keep-ghost")))
(library
 (name mutation_library)
 (wrapped false)
 (modules Mutation_provider)
 (libraries provider_a_library provider_c_library verocaml.ghost)
 (flags (:standard -w -27-33-39 -ppx "verocaml-ppx --keep-ghost")))
|}
            ^ declared_authority_rule "provider_a_library" "Provider_a"
            ^ declared_authority_rule "provider_c_library" "Provider_c"
            ^ declared_authority_rule
                ~dependencies:
                  [
                    ("provider_a_library", "Provider_a");
                    ("provider_c_library", "Provider_c");
                  ]
                "mutation_library"
                "Mutation_provider");
          file "provider_a.mli" provider_interface;
          file "provider_a.ml" provider_source;
          file "provider_c.mli" provider_interface;
          file "provider_c.ml" provider_source;
          file "mutation_provider.mli"
            (opens
            ^ "[@@@verocaml.broadcast_group (selected, [lemma])]\n");
          file "mutation_provider.ml"
            (opens
            ^ "[@@@verocaml.broadcast_group (selected, [lemma])]\n");
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider_a"; "Provider_c"; "Mutation_provider" ];
    }

let stale_same_cmi_cmti_case =
  Suite.case ~name:"stale-same-cmi-different-cmti-exact-member-substitution"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let original_workspace = Filename.concat workspace "original"
      and substituted_workspace = Filename.concat workspace "substituted" in
      let* original =
        Fixture.run ~environment ~workspace:original_workspace
          (same_cmi_shadow_project false)
      in
      let* substituted =
        Fixture.run ~environment ~workspace:substituted_workspace
          (same_cmi_shadow_project true)
      in
      let original_root = Filename.concat original_workspace "project"
      and substituted_root = Filename.concat substituted_workspace "project" in
      let* original_cmt = artifact original_root "Mutation_provider" ".cmt" in
      let* original_cmi = artifact original_root "Mutation_provider" ".cmi" in
      let* original_cmti = artifact original_root "Mutation_provider" ".cmti" in
      let* original_vri = artifact original_root "Mutation_provider" ".vri" in
      let* substituted_cmi =
        artifact substituted_root "Mutation_provider" ".cmi"
      in
      let* substituted_cmt =
        artifact substituted_root "Mutation_provider" ".cmt"
      in
      let* substituted_cmti =
        artifact substituted_root "Mutation_provider" ".cmti"
      in
      let* substituted_vri =
        artifact substituted_root "Mutation_provider" ".vri"
      in
      let* () =
        require
          (Digest.file original_cmi = Digest.file substituted_cmi
          && Digest.file original_cmti <> Digest.file substituted_cmti)
          "shadow substitution fixture did not produce the required same-CMI/different-CMTI boundary"
      in
      let stale_root = Filename.concat workspace "stale-artifacts" in
      mkdir_p stale_root;
      let stale_cmt = Filename.concat stale_root "mutation_provider.cmt"
      and stale_cmi = Filename.concat stale_root "mutation_provider.cmi"
      and stale_cmti = Filename.concat stale_root "mutation_provider.cmti"
      and stale_vri = Filename.concat stale_root "mutation_provider.vri" in
      copy_file original_cmt stale_cmt;
      copy_file original_cmi stale_cmi;
      copy_file substituted_cmti stale_cmti;
      copy_file original_vri stale_vri;
      Solver_backend_counter_private.reset_solver_creation_count ();
      let stale =
        Cmt_input.load_with_interface ~cmt:stale_cmt ~cmi:stale_cmi
          ~cmti:stale_cmti ~vri:stale_vri ()
      in
      let* () =
        match stale with
        | Ok _ -> mismatch "same-CMI substituted typed witness map was accepted"
        | Error diagnostic ->
            if diagnostic.code <> "VERO_DEPENDENCY" then
              mismatch "same-CMI substitution produced %s" diagnostic.code
            else
              require
                (Solver_backend_counter_private.solver_creation_count () = 0)
                "same-CMI substituted typed witness map reached a solver"
      in
      copy_file original_cmti stale_cmti;
      copy_file substituted_vri stale_vri;
      let crossed_sidecar =
        Cmt_input.load_with_interface ~cmt:stale_cmt ~cmi:stale_cmi
          ~cmti:stale_cmti ~vri:stale_vri ()
      in
      let* () =
        match crossed_sidecar with
        | Error { Diagnostic.code = "VERO_DEPENDENCY"; _ } -> Ok ()
        | Error diagnostic ->
            mismatch "crossed sidecar produced %s" diagnostic.code
        | Ok _ -> mismatch "same-CMI substituted sidecar was accepted"
      in
      let decode_vri filename =
        match Retained_interface_authority_private.decode (read_file filename) with
        | Ok authority -> Ok authority
        | Error reason -> mismatch "canonical VRI decode failed: %s" reason
      in
      let* original_authority = decode_vri original_vri in
      let* substituted_authority = decode_vri substituted_vri in
      let forged_authority =
        { original_authority with payloads = substituted_authority.payloads }
      in
      let forged = Filename.concat stale_root "canonical-payload-substitution.vri" in
      write_file forged (Retained_interface_authority_private.encode forged_authority);
      let* round_trip = decode_vri forged in
      let artifact_directories =
        files_below (Filename.concat original_root "_build/default")
        @ files_below (Filename.concat substituted_root "_build/default")
        |> List.filter (fun filename ->
               List.mem (Filename.extension filename)
                 [ ".cmi"; ".cmti"; ".vri" ])
        |> List.map Filename.dirname |> List.sort_uniq String.compare
      in
      Solver_backend_counter_private.reset_solver_creation_count ();
      let direct_payload_substitution =
        Cmt_input.load_with_interface ~cmt:substituted_cmt ~cmi:original_cmi
          ~cmti:original_cmti ~vri:forged ~artifact_directories ()
      in
      let* () =
        require
          (Retained_interface_authority_private.equal forged_authority round_trip
          && String.equal forged_authority.cmi_receipt
               original_authority.cmi_receipt
          && String.equal forged_authority.cmti_receipt
               original_authority.cmti_receipt)
          "payload substitution did not retain exact canonical artifact bindings"
      in
      let* () =
        match direct_payload_substitution with
        | Error { Diagnostic.code = "VERO_DEPENDENCY"; _ } ->
            require
              (Solver_backend_counter_private.solver_creation_count () = 0)
              "canonical direct-provider payload substitution reached a solver"
        | Error diagnostic ->
            mismatch "canonical direct-provider payload substitution produced %s"
              diagnostic.code
        | Ok _ ->
            mismatch
              "canonical direct-provider payload substitution crossed the exact CMTI witness boundary"
      in
      let _ = (original, substituted) in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let replace_last value =
  let length = String.length value in
  if length = 0 then value
  else
    String.sub value 0 (length - 1)
    ^ (if value.[length - 1] = '0' then "1" else "0")

let sidecar_rejection_case =
  Suite.case
    ~name:
      "vri-missing-malformed-stale-substituted-digest-and-dependency-rejections"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* _ = Fixture.run ~environment ~workspace (positive_project Standalone) in
      let root = Filename.concat workspace "project" in
      let* cmt = artifact root "Provider_a" ".cmt" in
      let* cmi = artifact root "Provider_a" ".cmi" in
      let* cmti = artifact root "Provider_a" ".cmti" in
      let* vri = artifact root "Provider_a" ".vri" in
      let scratch = Filename.concat workspace "sidecar-defects" in
      mkdir_p scratch;
      let reject candidate =
        Solver_backend_counter_private.reset_solver_creation_count ();
        match
          Cmt_input.load_with_interface ~cmt ~cmi ~cmti ~vri:candidate ()
        with
        | Error { Diagnostic.code = "VERO_DEPENDENCY"; _ } ->
            require
              (Solver_backend_counter_private.solver_creation_count () = 0)
              "sidecar rejection reached solver creation"
        | Error diagnostic ->
            mismatch "sidecar rejection produced %s" diagnostic.code
        | Ok _ -> mismatch "invalid retained interface sidecar was accepted"
      in
      let missing = Filename.concat scratch "missing.vri" in
      let malformed = Filename.concat scratch "malformed.vri" in
      write_file malformed "not-a-retained-interface";
      let inconsistent = Filename.concat scratch "digest-inconsistent.vri" in
      write_file inconsistent (read_file vri |> replace_last);
      let dependency = Filename.concat scratch "dependency-inconsistent.vri" in
      let* authority =
        match Retained_interface_authority_private.decode (read_file vri) with
        | Ok authority -> Ok authority
        | Error _ -> mismatch "generated sidecar did not decode canonically"
      in
      let encoded = Retained_interface_authority_private.encode authority in
      let malformed_lengths =
        [
          replace_first_frame_length encoded (string_of_int max_int);
          replace_first_frame_length encoded "-1";
          replace_first_frame_length encoded "+";
          String.sub encoded 0 (String.length encoded - 1);
          encoded ^ "trailing";
        ]
      in
      let total_decode_rejection bytes =
        try Result.is_error (Retained_interface_authority_private.decode bytes)
        with _ -> false
      in
      let* () =
        require (List.for_all total_decode_rejection malformed_lengths)
          "malformed VRI frame length escaped the total decoder boundary"
      in
      let reordered =
        {
          authority with
          cmi_imports = List.rev authority.cmi_imports;
          cmti_imports = List.rev authority.cmti_imports;
          dependencies = List.rev authority.dependencies;
          ordinary_cmi_receipts = List.rev authority.ordinary_cmi_receipts;
          payloads =
            List.map
              (function
                | Retained_interface_authority_private.Broadcast_witnesses members ->
                    Retained_interface_authority_private.Broadcast_witnesses
                      (List.rev members
                      |> List.map (fun member ->
                             {
                               member with
                               Retained_broadcast_private.source_members =
                                 List.rev
                                   member.Retained_broadcast_private.source_members;
                             }))
                | Retained_interface_authority_private.Logical_sorts sorts ->
                    Retained_interface_authority_private.Logical_sorts sorts
                | Retained_interface_authority_private.Logical_values values ->
                    Retained_interface_authority_private.Logical_values
                      (List.rev values)
                | (Retained_interface_authority_private.Numeric_claims _
                  | Retained_interface_authority_private.Unknown_optional_section _)
                  as payload ->
                    payload)
              authority.payloads;
        }
      in
      let* () =
        require
          (Retained_interface_authority_private.equal authority reordered
          && String.equal encoded
               (Retained_interface_authority_private.encode reordered)
          && String.equal
               (Retained_interface_authority_private.index authority)
               (Retained_interface_authority_private.index reordered))
          "VRI set-semantic ordering changed equality, encoding, or index"
      in
      let replace_first_byte value byte replacement =
        match String.index_from_opt value 0 byte with
        | None -> value
        | Some index ->
            String.sub value 0 index ^ String.make 1 replacement
            ^ String.sub value (index + 1) (String.length value - index - 1)
      in
      let unknown_version =
        match String.index_opt encoded '\000' with
        | None -> encoded
        | Some magic_end ->
            let version_byte = magic_end + 3 in
            String.sub encoded 0 version_byte ^ "9"
            ^ String.sub encoded (version_byte + 1)
                (String.length encoded - version_byte - 1)
      in
      let unknown_critical = replace_first_byte encoded 'b' 'x' in
      let duplicated_payload =
        Retained_interface_authority_private.encode
          { authority with payloads = authority.payloads @ authority.payloads }
      in
      let duplicated_dependency =
        match authority.dependencies with
        | [] -> encoded
        | first :: _ ->
            Retained_interface_authority_private.encode
              { authority with dependencies = first :: authority.dependencies }
      in
      let* () =
        require
          (List.for_all
             (fun bytes ->
               Result.is_error
                 (Retained_interface_authority_private.decode bytes))
             [
               unknown_version;
               unknown_critical;
               duplicated_payload;
               duplicated_dependency;
               encoded ^ "trailing";
               replace_last encoded;
             ])
          "unknown, duplicate, trailing, or corrupted VRI wire data decoded"
      in
      let dependencies =
        match authority.Retained_interface_authority_private.dependencies with
        | [] -> []
        | first :: rest ->
            {
              first with
              dependency_interface_receipt = String.make 32 '0';
            }
            :: rest
      in
      write_file dependency
        (Retained_interface_authority_private.encode
           { authority with dependencies });
      let substituted_workspace = Filename.concat workspace "substituted" in
      let* _ =
        Fixture.run ~environment ~workspace:substituted_workspace
          (positive_project Ppxlib)
      in
      let* substituted =
        artifact (Filename.concat substituted_workspace "project") "Provider_a"
          ".vri"
      in
      let* () = reject missing in
      let* () = reject malformed in
      let* () = reject inconsistent in
      let* () = reject dependency in
      let* () = reject substituted in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let sidecar_coalescence_case =
  Suite.case ~name:"vri-exact-coalescence-and-conflicting-load-order-permutation"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* _ = Fixture.run ~environment ~workspace (positive_project Standalone) in
      let root = Filename.concat workspace "project" in
      let* cmt = artifact root "Provider_a" ".cmt" in
      let* cmi = artifact root "Provider_a" ".cmi" in
      let* cmti = artifact root "Provider_a" ".cmti" in
      let* vri = artifact root "Provider_a" ".vri" in
      let split = Filename.concat workspace "coalescence" in
      mkdir_p split;
      let split_cmt = Filename.concat split "provider_a.cmt"
      and split_cmi = Filename.concat split "provider_a.cmi"
      and split_cmti = Filename.concat split "provider_a.cmti"
      and split_vri = Filename.concat split "provider_a.vri" in
      List.iter2 copy_file [ cmt; cmi; cmti; vri ]
        [ split_cmt; split_cmi; split_cmti; split_vri ];
      let* () =
        match
          Cmt_input.load_with_interface ~cmt:split_cmt ~cmi:split_cmi
            ~cmti:split_cmti ~vri_candidates:[ split_vri; vri ] ()
        with
        | Ok _ -> Ok ()
        | Error diagnostic ->
            mismatch "exact sidecar candidates did not coalesce: %s" diagnostic.code
      in
      let original = read_file vri in
      let* authority =
        match Retained_interface_authority_private.decode original with
        | Ok authority -> Ok authority
        | Error _ -> mismatch "generated sidecar did not decode canonically"
      in
      let conflicting =
        Retained_interface_authority_private.encode
          {
            authority with
            provider_origin = authority.provider_origin ^ "_substituted";
          }
      in
      let rejected candidates =
        Solver_backend_counter_private.reset_solver_creation_count ();
        match
          Cmt_input.load_with_interface ~cmt:split_cmt ~cmi:split_cmi
            ~cmti:split_cmti ~vri_candidates:candidates ()
        with
        | Error { Diagnostic.code = "VERO_DEPENDENCY"; _ } ->
            Solver_backend_counter_private.solver_creation_count () = 0
        | Error _ | Ok _ -> false
      in
      write_file split_vri conflicting;
      let first_order = rejected [ split_vri; vri ] in
      let second_order = rejected [ vri; split_vri ] in
      write_file split_vri original;
      let* () =
        require (first_order && second_order)
          "conflicting complete sidecars depended on load-path order"
      in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let authenticated_ordinary_view_graph_case =
  Suite.case
    ~name:"authenticated-ordinary-cmi-view-reaches-retained-provider-graph"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* _ = Fixture.run ~environment ~workspace (positive_project Standalone) in
      let root = Filename.concat workspace "project" in
      let* provider_cmt = artifact root "Provider_a" ".cmt" in
      let* provider_cmi = artifact root "Provider_a" ".cmi" in
      let* provider_cmti = artifact root "Provider_a" ".cmti" in
      let* original_vri = artifact root "Provider_a" ".vri" in
      let view_root = Filename.concat workspace "ordinary-view" in
      mkdir_p view_root;
      let view_root = Unix.realpath view_root in
      let ordinary_cmi = Filename.concat view_root "provider_a.cmi"
      and authority_vri = Filename.concat view_root "provider_a.vri" in
      let emitter =
        Filename.concat (Project_environment.binary_root environment)
          "verocaml-retained-interface"
      in
      let forwarded =
        [
          ("PATH", Project_environment.tool_path environment);
          ("OCAMLPATH", Project_environment.ocaml_path environment);
          ("HOME", view_root);
          ("TMPDIR", view_root);
        ]
      in
      let erased =
        run_captured ~workspace:view_root ~program:emitter
          ~arguments:[ "erase-cmi"; provider_cmi; ordinary_cmi ] ~forwarded ()
      in
      let* () = require (process_exited 0 erased) "ordinary CMI erasure failed" in
      let emitted =
        run_captured ~workspace:view_root ~program:emitter
          ~arguments:
            [
              "emit";
              provider_cmt;
              provider_cmi;
              provider_cmti;
              authority_vri;
              "--ordinary-cmi";
              ordinary_cmi;
              "--artifact-directory";
              Filename.dirname provider_cmi;
              "--artifact-directory";
              view_root;
            ]
          ~forwarded ()
      in
      let* () = require (process_exited 0 emitted) "ordinary CMI receipt emission failed" in
      write_file (Filename.concat view_root "dune-project")
        "(lang dune 3.17)\n(name ordinary_view_root)\n";
      write_file (Filename.concat view_root "dune")
        "(rule\n (targets root.cmo root.cmi root.cmt)\n (deps (:compiler %{ocamlc}) (:ppx %{bin:verocaml-ppx}) (:ghost %{lib:verocaml.ghost:vero_ghost.cmi}) root.ml provider_a.cmi)\n (action (run sh -c \"ghost_dir=$(dirname '%{ghost}'); exec '%{compiler}' -bin-annot -I \\\"$ghost_dir\\\" -I . -ppx '%{ppx} --keep-ghost' -c root.ml\")))\n(alias (name all) (deps root.cmt root.cmi))\n";
      write_file (Filename.concat view_root "root.ml")
        "let retain_type (value : int Provider_a.aurora_carrier) : unit =\n  [%verocaml.ensures fun _ -> value = value]; ()\n[@@verocaml.proof]\n";
      let* built =
        Process_adapter.run ~cwd:view_root
          {
            program = Project_environment.dune_path environment;
            arguments =
              [
                "build";
                "--root";
                view_root;
                "--build-dir";
                Filename.concat view_root "_build";
                "--profile";
                "release";
                "@all";
              ];
            forwarded =
              ("DUNE_CACHE", "disabled") :: forwarded;
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* () =
        require
          (List.exists
             (function Outcome.Exit_class (Outcome.Exited 0) -> true | _ -> false)
             (Outcome.process_facts built))
          "ordinary-view consumer did not compile"
      in
      let* provider =
        match
          Cmt_input.load_with_interface ~cmt:provider_cmt ~cmi:provider_cmi
            ~cmti:provider_cmti ~vri:authority_vri
            ~artifact_directories:[ Filename.dirname provider_cmi; view_root ] ()
        with
        | Ok provider -> Ok provider
        | Error diagnostic -> mismatch "ordinary-view provider load produced %s" diagnostic.code
      in
      let* consumer_cmt = artifact view_root "Root" ".cmt" in
      let* consumer_cmi = artifact view_root "Root" ".cmi" in
      let* consumer =
        match Cmt_input.load consumer_cmt with
        | Ok consumer -> Ok consumer
        | Error diagnostic -> mismatch "ordinary-view consumer load produced %s" diagnostic.code
      in
      let* () =
        match Interface_specification_candidate_private.graph_order [ provider ] consumer with
        | Ok [ selected ] when selected == provider -> Ok ()
        | Ok _ -> mismatch "ordinary-view graph selected a different retained provider"
        | Error error -> mismatch "authenticated ordinary-view graph rejected: %s" error.message
      in
      let* provider_without_view =
        match
          Cmt_input.load_with_interface ~cmt:provider_cmt ~cmi:provider_cmi
            ~cmti:provider_cmti ~vri:original_vri
            ~artifact_directories:[ Filename.dirname provider_cmi ] ()
        with
        | Ok provider -> Ok provider
        | Error diagnostic -> mismatch "original provider load produced %s" diagnostic.code
      in
      let* () =
        match
          Interface_specification_candidate_private.graph_order
            [ provider_without_view ] consumer
        with
        | Error _ -> Ok ()
        | Ok _ -> mismatch "ordinary CMI import without an authenticated view receipt was accepted"
      in
      let verified =
        run_captured ~workspace:view_root
          ~program:(Filename.concat (Project_environment.binary_root environment) "verocaml")
          ~arguments:
            [
              "verify-project";
              "--dependency";
              provider_cmt;
              provider_cmi;
              provider_cmti;
              authority_vri;
              "--root";
              consumer_cmt;
              consumer_cmi;
              "--threads";
              "1";
              "--timeout-ms";
              "60000";
            ]
          ~forwarded ()
      in
      let* () = require (process_exited 0 verified) "verify-project rejected authenticated ordinary CMI view" in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let missing_body_project =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project" "(lang dune 3.17)\n(name broadcast_body_boundary)\n";
          file "dune"
            {|(library
 (name body_provider_library)
 (wrapped false)
 (modules Body_provider)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name body_consumer_library)
 (wrapped false)
 (modules Body_consumer)
 (libraries body_provider_library verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          file "body_provider.mli"
            {|[%%verocaml.symbolic val observed : int -> bool]
val promised : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
|};
          file "body_provider.ml"
            {|[%%verocaml.symbolic val observed : int -> bool]
let promised (value : int) : unit =
  [%verocaml.ensures fun _ -> (not ((observed value) [@trigger])) || value = value]; ()
[@@verocaml.proof]
|};
          file "body_consumer.ml"
            {|open Body_provider
let touch (value : int) : unit = [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof]
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Body_provider"; "Body_consumer" ];
    }

let missing_body_case =
  Suite.case ~name:"completed-body-marker-required-before-trust"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_DEPENDENCY")
    (fun ~environment ~workspace ->
      Solver_backend_counter_private.reset_solver_creation_count ();
      Fixture.run ~environment ~workspace missing_body_project)

let carrier_project consumer_source =
  let dune =
    Printf.sprintf
      {|(library
 (name provider_a_library)
 (wrapped false)
 (modules Provider_a)
 (libraries verocaml.ghost)
 %s)
(library
 (name forwarder_library)
 (wrapped false)
 (modules Forwarder)
 (libraries provider_a_library verocaml.ghost)
 %s)
(library
 (name carrier_consumer_library)
 (wrapped false)
 (modules Carrier_consumer)
 (libraries provider_a_library forwarder_library verocaml.ghost)
 %s)
|}
      (ppx Standalone `Retained) (ppx Standalone `Retained)
      (ppx Standalone `Retained)
  in
  ({
      files =
        [
          file "dune-project" "(lang dune 3.17)\n(name carrier_non_authority)\n";
          file "dune" dune;
          file "provider_a.mli" provider_a_mli;
          file "provider_a.ml" provider_a_ml;
          file "forwarder.mli" forwarder_mli;
          file "forwarder.ml" forwarder_ml;
          file "carrier_consumer.ml" consumer_source;
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider_a"; "Forwarder"; "Carrier_consumer" ];
    } : Fixture.dune_project)

let carrier_non_authority_case =
  Suite.case ~name:"synthetic-carrier-call-does-not-activate-authority"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Frontend_rejected)
    (fun ~environment ~workspace ->
      let sources =
        [
          ("call", "let unavailable = Forwarder.alias_zero ()\n");
          ( "alias",
            "module Alias = Forwarder\nlet unavailable = Alias.alias_zero ()\n" );
          ("open", "open Forwarder\nlet unavailable = alias_zero ()\n");
          ( "reexport",
            "module Reexport = struct\n  let unavailable = Forwarder.alias_zero\nend\n" );
        ]
      in
      sources
      |> List.map (fun (name, source) ->
             run_compile_rejection ~environment ~workspace ~name
               (carrier_project source))
      |> List.fold_left
           (fun outcomes result ->
             let* outcomes = outcomes in
             let* outcome = result in
             Ok (outcome :: outcomes))
           (Ok [])
      |> Result.map Outcome.merge)

let private_activation_project : Fixture.dune_project =
    {
      files =
        [
          file "dune-project" "(lang dune 3.17)\n(name private_activation)\n";
          file "dune"
            {|(library
 (name private_provider_library)
 (wrapped false)
 (modules Provider_a)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
(library
 (name private_consumer_library)
 (wrapped false)
 (modules Private_consumer)
 (libraries private_provider_library verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          file "provider_a.mli" provider_a_mli;
          file "provider_a.ml" provider_a_ml;
          file "private_consumer.ml"
            {|[@@@verocaml.activate [Provider_a.private_broadcast]]
let use value = value
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider_a" ];
    }

let private_activation_case =
  Suite.case ~name:"implementation-private-broadcast-consumer-activation-rejected"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Frontend_rejected)
    (fun ~environment ~workspace ->
      run_compile_rejection ~environment ~workspace ~name:"private-activation"
        private_activation_project)

let cycle_project =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project" "(lang dune 3.17)\n(name broadcast_cycle)\n";
          file "dune"
            {|(library
 (name cycle_library)
 (wrapped false)
 (modules Cycle)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          file "cycle.ml"
            {|let observed (_value : int) : bool = true [@@verocaml.spec]
let lemma_cycle (value : int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger]) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
[@@@verocaml.broadcast_group (cycle_left, [cycle_right; lemma_cycle])]
[@@@verocaml.broadcast_group (cycle_right, [cycle_left])]
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Cycle" ];
    }

let cycle_case =
  Suite.case ~name:"broadcast-group-cycle-rejected-pre-solver"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Frontend_rejected)
    (fun ~environment ~workspace ->
      Solver_backend_counter_private.reset_solver_creation_count ();
      let* outcome = Fixture.run ~environment ~workspace cycle_project in
      let* () =
        require
          (Solver_backend_counter_private.solver_creation_count () = 0)
          "broadcast cycle reached solver creation"
      in
      Ok outcome)

let parse_signature source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "broadcast_boundary.mli";
  Parse.interface lexbuf

let retained_rewriter_rejects source =
  try
    let mapper = Vero_ppx_rewriter.make [ "--keep-ghost" ] in
    ignore (mapper.Ast_mapper.signature mapper (parse_signature source));
    false
  with Location.Error _ -> true

let retained_structure_rewriter_rejects source =
  try
    let lexbuf = Lexing.from_string source in
    Location.init lexbuf "broadcast_boundary.ml";
    let mapper = Vero_ppx_rewriter.make [ "--keep-ghost" ] in
    ignore (mapper.Ast_mapper.structure mapper (Parse.implementation lexbuf));
    false
  with Location.Error _ -> true

let raw_marker_collision_case =
  Suite.case ~name:"raw-marker-and-carrier-collision-rejected"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      let raw =
        {|val forged : int
[@@verocaml.internal.broadcast.interface_declaration.v1 "v1"]
|}
      and collision =
        {|val selected : unit -> unit
[@@@verocaml.broadcast_group (selected, [selected])]
|}
      and implementation_collision =
        {|let selected () = ()
[@@@verocaml.broadcast_group (selected, [selected])]
|}
      in
      let* () =
        require
          (retained_rewriter_rejects raw
          && retained_rewriter_rejects collision
          && retained_structure_rewriter_rejects implementation_collision)
          "raw retained marker or synthetic carrier collision was accepted"
      in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let include_value_collision_project =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name include_value_collision)\n";
          file "dune"
            {|(library
 (name provider_a_library)
 (wrapped false)
 (modules Provider_a)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
(library
 (name ordinary_provider_library)
 (wrapped false)
 (modules Ordinary_provider))
(library
 (name collision_library)
 (wrapped false)
 (modules Include_collision)
 (libraries provider_a_library ordinary_provider_library verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          file "provider_a.mli" provider_a_mli;
          file "provider_a.ml" provider_a_ml;
          file "ordinary_provider.mli" "val selected : int\n";
          file "ordinary_provider.ml" "let selected = 1\n";
          file "include_collision.mli"
            {|include module type of Ordinary_provider
[@@@verocaml.broadcast_group (selected, [Provider_a.zero_axiom])]
|};
          file "include_collision.ml"
            {|include Ordinary_provider
[@@@verocaml.broadcast_group (selected, [Provider_a.zero_axiom])]
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Include_collision" ];
    }

let include_value_collision_case =
  Suite.case ~name:"include-derived-value-and-group-carrier-collision-rejected"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_DEPENDENCY")
    (fun ~environment ~workspace ->
      Solver_backend_counter_private.reset_solver_creation_count ();
      let* outcome =
        Fixture.run ~environment ~workspace include_value_collision_project
      in
      let* () =
        require
          (Solver_backend_counter_private.solver_creation_count () = 0)
          "include-derived carrier collision reached solver creation"
      in
      Ok outcome)

let include_origin_project =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project" "(lang dune 3.17)\n(name include_origin)\n";
          file "dune"
            ({|(library
 (name provider_a_library)
 (wrapped false)
 (modules Provider_a)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
(library
 (name include_forwarder_library)
 (wrapped false)
 (modules Include_forwarder)
 (libraries provider_a_library verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|}
            ^ declared_authority_rule "provider_a_library" "Provider_a"
            ^ declared_authority_rule
                ~dependencies:[ ("provider_a_library", "Provider_a") ]
                "include_forwarder_library" "Include_forwarder");
          file "provider_a.mli" provider_a_mli;
          file "provider_a.ml" provider_a_ml;
          file "include_forwarder.mli"
            {|include module type of Provider_a
[@@@verocaml.broadcast_group (included_zero, [zero_axiom])]
|};
          file "include_forwarder.ml"
            {|include Provider_a
[@@@verocaml.broadcast_group (included_zero, [Provider_a.zero_axiom])]
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider_a" ];
    }

let include_origin_case =
  Suite.case ~name:"typed-include-reexport-preserves-original-member-origin"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider_a" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      let* outcome = Fixture.run ~environment ~workspace include_origin_project in
      let root = Filename.concat workspace "project" in
      let* provider = load root "Provider_a" in
      let* forwarder = load root "Include_forwarder" in
      Solver_backend_counter_private.reset_solver_creation_count ();
      let normalization_failure =
        Cmt_input.normalize_value_path provider Location.none Env.empty
          (Path.Pdot
             (Path.Pident (Ident.create_persistent "Missing_broadcast_provider"),
              "zero_axiom"))
      in
      let* () =
        require
          (normalization_failure = None
          && Solver_backend_counter_private.solver_creation_count () = 0)
          "compiler normalization failure retained a raw path or reached a solver"
      in
      let provider_zero =
        provider.interface_broadcasts
        |> List.find_opt (fun member ->
               String.ends_with ~suffix:".zero_axiom"
                 member.Retained_broadcast_private.identity.canonical_path)
      in
      let forwarder_groups =
        forwarder.interface_broadcasts
        |> List.filter (fun member ->
               member.Retained_broadcast_private.identity.kind
               = Retained_broadcast_private.Group)
      in
      let forwarder_declarations =
        forwarder.interface_broadcasts
        |> List.filter (fun member ->
               member.Retained_broadcast_private.identity.kind
               = Retained_broadcast_private.Declaration)
      in
      let* () =
        match (provider_zero, forwarder_groups) with
        | Some provider_zero, [ group ] ->
            require
              (forwarder_declarations = []
              &&
              match group.source_members with
              | [ source ] ->
                  String.equal source.member_compiler_uid
                    provider_zero.identity.compiler_uid
                  && String.equal source.member_canonical_path
                       provider_zero.identity.canonical_path
                  && source.member_kind = Retained_broadcast_private.Declaration
              | [] | _ :: _ :: _ -> false)
              "flattened include minted local broadcast authority or lost original UID"
        | None, _ | _, [] | _, _ :: _ :: _ ->
            mismatch "include origin fixture has the wrong retained authority shape"
      in
      Ok outcome)

let ordinary_project route =
  let dune =
    Printf.sprintf
      {|(library
 (name ordinary_surface)
 (public_name ordinary_surface)
 (wrapped false)
 (modules Surface)
 (libraries verocaml.ghost)
 %s)
(executable
 (name ordinary_main)
 (modules Ordinary_main)
 (libraries ordinary_surface))
|}
      (ppx route `Ordinary)
  in
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name ordinary_surface)\n(package (name ordinary_surface))\n";
          file "dune" dune;
          file "surface.mli"
            {|val genuine : int -> int
val hidden_proof : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
[@@@verocaml.broadcast_group (hidden_group, [hidden_proof])]
|};
          file "surface.ml"
            {|let genuine value = value
let hidden_proof value = [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
[@@@verocaml.broadcast_group (hidden_group, [hidden_proof])]
|};
          file "ordinary_main.ml" "let () = ignore (Surface.genuine 0)\n";
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all"; "@install" ];
      selected_units = [ "Surface" ];
    }

let signature_values cmi =
  let info = Cmi_format.read_cmi_lazy cmi in
  Subst.Lazy.force_signature info.Cmi_format.cmi_sign
  |> List.filter_map (function
       | Types.Sig_value (ident, description, Types.Exported) ->
           Some (Ident.name ident, description.Types.val_attributes)
       | _ -> None)

let file_contains path fragment =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
      let contents = really_input_string channel (in_channel_length channel) in
      contains contents fragment)

let ordinary_erasure_case route name =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Surface" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      let* outcome = Fixture.run ~environment ~workspace (ordinary_project route) in
      let root = Filename.concat workspace "project" in
      let* cmi = artifact root "Surface" ".cmi" in
      let* cmo = artifact root "Surface" ".cmo" in
      let* cmx = artifact root "Surface" ".cmx" in
      let* linked = artifact root "Ordinary_main" ".exe" in
      let values = signature_values cmi in
      let* () = require (List.mem_assoc "genuine" values) "ordinary genuine val was erased" in
      let* () = require (not (List.mem_assoc "hidden_group" values)) "ordinary group carrier survived" in
      let* () = require (not (List.mem_assoc "hidden_proof" values)) "ordinary proof carrier survived" in
      let internal =
        List.exists
          (fun (_, attributes) ->
            List.exists
              (fun attribute ->
                String.starts_with ~prefix:"verocaml.internal.broadcast."
                  attribute.Parsetree.attr_name.txt)
              attributes)
          values
      in
      let* () = require (not internal) "ordinary CMI retained an internal broadcast marker" in
      let* () =
        require
          (not
             (file_contains cmo "verocaml.internal.broadcast."
             || file_contains cmo "verocaml:broadcast:carrier:"
             || file_contains cmx "verocaml.internal.broadcast."
             || file_contains cmx "verocaml:broadcast:carrier:"
             || file_contains linked "verocaml.internal.broadcast."
             || file_contains linked "verocaml:broadcast:carrier:"))
          "ordinary byte/native/linkage artifact retained broadcast metadata"
      in
      let manifests =
        files_below (Filename.concat root "_build/install/default")
        |> List.filter (fun path ->
               List.mem (Filename.basename path)
                 [ "META"; "dune-package"; "ordinary_surface.opam" ])
      in
      let* () =
        require
          (manifests <> []
          && not
               (List.exists
                  (fun path ->
                    file_contains path "verocaml.internal.broadcast."
                    || file_contains path "verocaml:broadcast:carrier:")
                  manifests))
          "ordinary install/package manifest retained broadcast metadata"
      in
      Ok outcome)

let nested_constrained_source =
  {|module Outer = struct
  module Middle = struct
    module Inline : sig
      val genuine : int -> int
      val hidden_proof : int -> unit [@@verocaml.proof]
    end = struct
      let genuine value = value
      let hidden_proof value =
        [%verocaml.ensures fun _ -> value = value]; ()
      [@@verocaml.proof]
    end

    module type API = sig
      val genuine : int -> int
      val hidden_proof : int -> unit [@@verocaml.proof]
    end

    module Named : API = struct
      let genuine value = value + 1
      let hidden_proof value =
        [%verocaml.ensures fun _ -> value = value]; ()
      [@@verocaml.proof]
    end

    module rec Recursive : API = struct
      let genuine value = value + 2
      let hidden_proof value =
        [%verocaml.ensures fun _ -> value = value]; ()
      [@@verocaml.proof]
    end
  end
end
|}

let recursive_signature_values cmi =
  let interface = Cmi_format.read_cmi_lazy cmi in
  let path prefix name = String.concat "." (prefix @ [ name ]) in
  let rec module_type prefix = function
    | Types.Mty_signature signature -> signature_items prefix signature
    | Types.Mty_functor (parameter, result) ->
        let parameter_values =
          match parameter with
          | Types.Unit -> []
          | Types.Named (_, argument) -> module_type prefix argument
        in
        parameter_values @ module_type prefix result
    | Types.Mty_strengthen (nested, _, _) -> module_type prefix nested
    | Types.Mty_ident _ | Types.Mty_alias _ -> []
  and signature_items prefix signature =
    List.concat_map
      (function
        | Types.Sig_value (ident, _, _) -> [ path prefix (Ident.name ident) ]
        | Types.Sig_module (ident, _, declaration, _, _) ->
            let name = Ident.name ident in
            module_type (prefix @ [ name ]) declaration.Types.md_type
        | Types.Sig_modtype (ident, declaration, _) ->
            Option.fold ~none:[]
              ~some:(module_type (prefix @ [ Ident.name ident ]))
              declaration.Types.mtd_type
        | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_class _
        | Types.Sig_class_type _ ->
            [])
      signature
  in
  Subst.Lazy.force_signature interface.Cmi_format.cmi_sign
  |> signature_items [] |> List.sort_uniq String.compare

let nested_constrained_erasure_project route =
  ({
      files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name nested_constrained_erasure)\n";
          file "dune"
            (Printf.sprintf
               {|(library
 (name nested_ordinary)
 (wrapped false)
 (modules Nested_ordinary)
 (libraries verocaml.ghost)
 %s)
(library
 (name nested_retained)
 (wrapped false)
 (modules Nested_retained)
 (libraries verocaml.ghost)
 %s)
(executable
 (name nested_main)
 (modules Nested_main)
 (libraries nested_ordinary))
|}
               (ppx route `Ordinary) (ppx route `Retained));
          file "nested_ordinary.ml" nested_constrained_source;
          file "nested_retained.ml" nested_constrained_source;
          file "nested_main.ml"
            {|let () =
  assert (Nested_ordinary.Outer.Middle.Inline.genuine 3 = 3);
  assert (Nested_ordinary.Outer.Middle.Named.genuine 3 = 4);
  assert (Nested_ordinary.Outer.Middle.Recursive.genuine 3 = 5)
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Nested_ordinary" ];
    } : Fixture.dune_project)

let nested_constrained_erasure_case route name =
  Suite.case ~name
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let (project : Fixture.dune_project) =
        nested_constrained_erasure_project route
      in
      let root = Filename.concat workspace "project" in
      List.iter
        (fun source ->
          write_file (Filename.concat root source.Fixture.path) source.contents)
        project.files;
      let root = Unix.realpath root in
      let build =
        run_captured ~workspace:(Filename.concat workspace "nested-build")
          ~program:(Project_environment.dune_path environment)
          ~arguments:
            [
              "build";
              "--root";
              root;
              "--build-dir";
              Filename.concat root "_build";
              "--profile";
              "release";
              "@all";
            ]
          ~forwarded:
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("DUNE_CACHE", "disabled");
              ("HOME", root);
              ("TMPDIR", root);
              ("OCAML_COLOR", "never");
            ]
          ()
      in
      let* () =
        require (process_exited 0 build)
          ("nested constrained ordinary build failed: " ^ build.stderr)
      in
      let* ordinary_cmi = artifact root "Nested_ordinary" ".cmi" in
      let* retained_cmi = artifact root "Nested_retained" ".cmi" in
      let* executable = artifact root "Nested_main" ".exe" in
      let expected =
        [
          "Outer.Middle.API.genuine";
          "Outer.Middle.Inline.genuine";
        ]
      in
      let ordinary_values = recursive_signature_values ordinary_cmi
      and retained_values = recursive_signature_values retained_cmi in
      let retained_genuine =
        List.filter (fun path -> String.ends_with ~suffix:".genuine" path)
          retained_values
      in
      let runtime =
        run_captured ~workspace:(Filename.concat workspace "nested-runtime")
          ~program:executable ~arguments:[] ~forwarded:[] ()
      in
      let parity =
        ordinary_values = expected && retained_genuine = expected
        && not
             (List.exists
                (String.ends_with ~suffix:".hidden_proof")
                ordinary_values)
        && process_exited 0 runtime
      in
      let* () =
        if parity then Ok ()
        else
          mismatch "nested constrained erasure ordinary=[%s] retained=[%s] runtime=%b"
            (String.concat "," ordinary_values)
            (String.concat "," retained_genuine)
            (process_exited 0 runtime)
      in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let route_neutral_identity_key identity =
  let neutral =
    {
      identity with
      Retained_broadcast_private.interface_digest = String.make 32 '0';
      compiler_uid = "route-neutral-uid";
      dependency_receipt = String.make 32 '0';
      witness_receipt = String.make 32 '0';
      artifact_family = "retained-v1";
      provider_route = "route-neutral";
    }
  in
  Retained_broadcast_private.identity_key neutral

let source_projection source =
  ( Retained_broadcast_private.kind_name
      source.Retained_broadcast_private.member_kind,
    Some source.member_canonical_path )

let route_projection implementation =
  implementation.Cmt_input.interface_broadcasts
  |> List.map (fun member ->
         let identity = member.Retained_broadcast_private.identity in
         ( route_neutral_identity_key identity,
           member.source_members |> List.map source_projection
           |> List.sort_uniq compare ))
  |> List.sort compare

let resolved_group_projection imported =
  Imported_callable.broadcast_groups imported
  |> List.map (fun group ->
         ( route_neutral_identity_key group.Imported_callable.identity,
           group.members |> List.map route_neutral_identity_key
           |> List.sort_uniq String.compare ))
  |> List.sort compare

let route_parity_case =
  Suite.case ~name:"standalone-ppxlib-broadcast-structure-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider_a" Outcome.Unit_verified
      |> Expectation.require_unit "Forwarder" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_unit "Direct_consumer" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      let standalone_workspace = Filename.concat workspace "standalone" in
      let ppxlib_workspace = Filename.concat workspace "ppxlib" in
      let* standalone =
        Fixture.run ~environment ~workspace:standalone_workspace
          (positive_project Standalone)
      in
      let* ppxlib =
        Fixture.run ~environment ~workspace:ppxlib_workspace
          (positive_project Ppxlib)
      in
      let* standalone_provider =
        load (Filename.concat standalone_workspace "project") "Provider_a"
      in
      let* ppxlib_provider =
        load (Filename.concat ppxlib_workspace "project") "Provider_a"
      in
      let* standalone_forwarder =
        load (Filename.concat standalone_workspace "project") "Forwarder"
      in
      let* standalone_consumer =
        load (Filename.concat standalone_workspace "project") "Consumer"
      in
      let* ppxlib_forwarder =
        load (Filename.concat ppxlib_workspace "project") "Forwarder"
      in
      let* ppxlib_consumer =
        load (Filename.concat ppxlib_workspace "project") "Consumer"
      in
      let* standalone_imported =
        imported_authority ~consumer:standalone_consumer
          ~dependencies:[ standalone_provider; standalone_forwarder ]
      in
      let* ppxlib_imported =
        imported_authority ~consumer:ppxlib_consumer
          ~dependencies:[ ppxlib_provider; ppxlib_forwarder ]
      in
      let* () =
        require
          (route_projection standalone_provider = route_projection ppxlib_provider)
          "standalone/Ppxlib retained broadcast structure forked by route"
      in
      let* () =
        require
          (resolved_group_projection standalone_imported
          = resolved_group_projection ppxlib_imported)
          "standalone/Ppxlib resolved full member identities forked by route"
      in
      let* () =
        require
          (standalone_provider.interface_family_issuers = [ "standalone-v1" ]
          && ppxlib_provider.interface_family_issuers = [ "ppxlib-v1" ])
          "route provenance was not retained separately from semantic structure"
      in
      let standalone_root = Filename.concat standalone_workspace "project"
      and ppxlib_root = Filename.concat ppxlib_workspace "project" in
      let* standalone_provider_cmt = artifact standalone_root "Provider_a" ".cmt" in
      let* ppxlib_provider_cmt = artifact ppxlib_root "Provider_a" ".cmt" in
      let* ppxlib_provider_cmi = artifact ppxlib_root "Provider_a" ".cmi" in
      let authority_secrets =
        standalone_provider.interface_broadcasts
        |> List.concat_map
             (fun (member : Retained_broadcast_private.interface_member) ->
               let identity = member.identity in
               [
                 identity.canonical_path;
                 identity.compiler_uid;
                 identity.interface_digest;
                 identity.dependency_receipt;
               ]
               @ List.map
                   (fun source -> source.Retained_broadcast_private.member_canonical_path)
                   member.source_members)
      in
      Solver_backend_counter_private.reset_solver_creation_count ();
      let stale =
        Cmt_input.load_with_interface ~cmt:standalone_provider_cmt
          ~cmi:ppxlib_provider_cmi ()
      in
      let* () =
        match stale with
        | Ok _ -> mismatch "mismatched route receipt was accepted"
        | Error diagnostic -> (
            match diagnostic.Diagnostic.classification with
            | Diagnostic.Invalid_broadcast_dependency _ ->
                let* () =
                  require
                    (Solver_backend_counter_private.solver_creation_count () = 0)
                    "broadcast-bearing receipt mismatch reached solver or trust"
                in
                require (diagnostic_redacts authority_secrets diagnostic)
                  "public broadcast diagnostic exposed retained authority material"
            | _ ->
                mismatch
                  "broadcast-bearing receipt mismatch had diagnostic code %s instead of dependency failure"
                  diagnostic.code)
      in
      let duplicate_root = Filename.concat workspace "duplicate-source-candidate" in
      mkdir_p duplicate_root;
      let duplicate_source = Filename.concat duplicate_root "consumer.ml" in
      write_file duplicate_source direct_consumer_ml;
      Solver_backend_counter_private.reset_solver_creation_count ();
      let* duplicate_process =
        Process_adapter.run ~cwd:duplicate_root
          {
            program =
              Filename.concat (Project_environment.binary_root environment)
                "verocaml";
            arguments =
              [
                "verify";
                duplicate_source;
                "--dependency";
                standalone_provider_cmt;
                "--dependency";
                ppxlib_provider_cmt;
                "--threads";
                "1";
              ];
            forwarded =
              [
                ("PATH", Project_environment.tool_path environment);
                ("OCAMLPATH", Project_environment.ocaml_path environment);
                ("DUNE_CACHE", "disabled");
                ("HOME", duplicate_root);
                ("TMPDIR", duplicate_root);
                ("OCAML_COLOR", "never");
              ];
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* () =
        require
          (List.exists
             (function
               | Outcome.Exit_class (Outcome.Exited 2) -> true
               | Exit_class (Exited _) | Exit_class Signaled | Exit_class Stopped
               | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false)
             (Outcome.process_facts duplicate_process)
          && Solver_backend_counter_private.solver_creation_count () = 0)
          "conflicting complete duplicate source candidates were accepted or reached a solver"
      in
      Ok (Outcome.merge [ standalone; ppxlib ]))

let dune_directory_case =
  Suite.case ~name:"dune-directory-one-step-generates-retained-authority"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let root = Filename.concat workspace "dune-directory" in
      [
        file "dune-project" "(lang dune 3.17)\n(name directory_authority)\n";
        file "dune"
          ({|(library
 (name provider)
 (wrapped false)
 (modules Provider)
 (libraries verocaml.ghost)
 (flags (:standard -w -27-39))
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
(library
 (name consumer)
 (wrapped false)
 (modules Consumer)
 (libraries provider verocaml.ghost)
 (flags (:standard -w -27))
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
|}
          ^ declared_authority_rule "provider" "Provider");
        file "provider.mli"
          "val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]\n[%%verocaml.symbolic val probe : int -> bool]\n";
        file "provider.ml"
          {|[@@@verocaml.verify]
[%%verocaml.symbolic val probe : int -> bool]
let lemma (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((probe value) [@trigger])) || value = value];
  let _ = value in ()
[@@verocaml.proof] [@@verocaml.broadcast]
|};
        file "consumer.ml"
          {|[@@@verocaml.verify]
[@@@verocaml.activate [Provider.lemma]]
let check (value : int) : unit =
  [%verocaml.assert value = value];
  let _ = value in ()
[@@verocaml.proof]
|};
      ]
      |> List.iter (fun source ->
             write_file (Filename.concat root source.Fixture.path) source.contents);
      let root = Unix.realpath root in
      let* process =
        Process_adapter.run ~cwd:root
          {
            program =
              Filename.concat (Project_environment.binary_root environment)
                "verocaml";
            arguments =
              [ "verify"; "."; "--threads"; "1"; "--timeout-ms"; "60000" ];
            forwarded =
              [
                ("PATH", Project_environment.tool_path environment);
                ("OCAMLPATH", Project_environment.ocaml_path environment);
                ("VEROCAML_DUNE", Project_environment.dune_path environment);
                ("DUNE_CACHE", "disabled");
                ("HOME", root);
                ("TMPDIR", root);
                ("OCAML_COLOR", "never");
              ];
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let exited_zero =
        Outcome.process_facts process
        |> List.exists (function
             | Outcome.Exit_class (Outcome.Exited 0) -> true
             | Exit_class (Exited _) | Exit_class Signaled | Exit_class Stopped
             | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false)
      in
      let generated =
        files_below (Filename.concat root "_build")
        |> List.exists (fun path -> Filename.check_suffix path "provider.vri")
      in
      let* () =
        require exited_zero "one-step Dune directory verification failed"
      in
      let* () =
        require generated
          "one-step Dune directory verification omitted retained authority"
      in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let dune_directory_malformed_vri_case =
  Suite.case ~name:"dune-directory-oversized-vri-is-an-artifact-diagnostic"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let root = Filename.concat workspace "dune-directory-malformed" in
      let helper =
        if Filename.is_relative Sys.executable_name then
          Filename.concat (Sys.getcwd ()) Sys.executable_name
        else Sys.executable_name
      in
      let dune =
        Printf.sprintf
          {|(library
 (name provider)
 (wrapped false)
 (modules Provider)
 (libraries verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
(rule
 (targets provider.vri provider.verocaml-retained-interface)
 (deps
  (sandbox always)
  (:emitter %%{bin:verocaml-retained-interface})
  (:cmt .provider.objs/byte/provider.cmt)
  (:cmi .provider.objs/byte/provider.cmi)
  (:cmti .provider.objs/byte/provider.cmti))
 (action
  (progn
   (run %%{emitter} emit %%{cmt} %%{cmi} %%{cmti} provider.vri
    --artifact-directory .
    --artifact-directory .provider.objs/byte)
   (run %S --oversize-vri provider.vri)
   (run %%{emitter} manifest provider.verocaml-retained-interface
    %%{cmt} %%{cmi} %%{cmti} provider.vri))))
(alias (name all) (deps provider.vri provider.verocaml-retained-interface))
|}
          helper
      in
      [
        file "dune-project"
          "(lang dune 3.17)\n(name directory_malformed_authority)\n";
        file "dune" dune;
        file "provider.mli"
          "val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]\n";
        file "provider.ml"
          {|[@@@verocaml.verify]
let lemma (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|};
      ]
      |> List.iter (fun source ->
             write_file (Filename.concat root source.Fixture.path) source.contents);
      let root = Unix.realpath root in
      let process =
        run_captured ~workspace:root
          ~program:
            (Filename.concat (Project_environment.binary_root environment)
               "verocaml")
          ~arguments:[ "verify"; root; "--threads"; "1" ]
          ~forwarded:
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("VEROCAML_DUNE", Project_environment.dune_path environment);
              ("DUNE_CACHE", "disabled");
              ("HOME", root);
              ("TMPDIR", root);
              ("OCAML_COLOR", "never");
            ]
          ()
      in
      let forbidden =
        [
          "Invalid_argument";
          "compiler_uid";
          "receipt";
          "canonical_path";
          "verocaml.internal";
          "broadcast-artifact-receipt";
          string_of_int max_int;
        ]
      in
      let diagnostic = public_diagnostic process in
      let* () =
        require
          (process_exited 2 process
          && Option.fold ~none:false
               ~some:(fun diagnostic ->
                 contains diagnostic "VERO_DEPENDENCY"
                 && contains diagnostic "declared specification-library artifacts"
                 && contains diagnostic "rebuild or reinstall"
                 && List.for_all
                      (fun value -> not (contains diagnostic value))
                      forbidden)
               diagnostic)
          "Dune directory route did not render a safe actionable artifact diagnostic"
      in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let dune_directory_conflicting_manifests_case =
  Suite.case ~name:"dune-directory-conflicting-manifests-are-dependency-errors"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let root = Filename.concat workspace "dune-directory-conflicting" in
      [
        file "dune-project"
          "(lang dune 3.17)\n(name directory_conflicting_authority)\n";
        file "dune"
          {|(library
 (name providers)
 (wrapped false)
 (modules Provider Other)
 (libraries verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
(rule
 (targets provider.vri provider.verocaml-retained-interface)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .providers.objs/byte/provider.cmt)
  (:cmi .providers.objs/byte/provider.cmi)
  (:cmti .providers.objs/byte/provider.cmti))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} provider.vri
    --artifact-directory .providers.objs/byte)
   (run %{emitter} manifest provider.verocaml-retained-interface
    %{cmt} %{cmi} %{cmti} provider.vri))))
(rule
 (targets other.vri other.verocaml-retained-interface)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:provider_cmt .providers.objs/byte/provider.cmt)
  (:other_cmt .providers.objs/byte/other.cmt)
  (:other_cmi .providers.objs/byte/other.cmi)
  (:other_cmti .providers.objs/byte/other.cmti))
 (action
  (progn
   (run %{emitter} emit %{other_cmt} %{other_cmi} %{other_cmti} other.vri
    --artifact-directory .providers.objs/byte)
   (run %{emitter} manifest other.verocaml-retained-interface
    %{provider_cmt} %{other_cmi} %{other_cmti} other.vri))))
(alias
 (name all)
 (deps
  provider.vri
  provider.verocaml-retained-interface
  other.vri
  other.verocaml-retained-interface))
|};
        file "provider.mli"
          "val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]\n";
        file "provider.ml"
          {|[@@@verocaml.verify]
let lemma (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|};
        file "other.mli"
          "val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]\n";
        file "other.ml"
          {|[@@@verocaml.verify]
let lemma (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|};
      ]
      |> List.iter (fun source ->
             write_file (Filename.concat root source.Fixture.path) source.contents);
      let root = Unix.realpath root in
      let process =
        run_captured ~workspace:root
          ~program:
            (Filename.concat (Project_environment.binary_root environment)
               "verocaml")
          ~arguments:[ "verify"; root; "--threads"; "1" ]
          ~forwarded:
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("VEROCAML_DUNE", Project_environment.dune_path environment);
              ("DUNE_CACHE", "disabled");
              ("HOME", root);
              ("TMPDIR", root);
              ("OCAML_COLOR", "never");
            ]
          ()
      in
      let diagnostic = public_diagnostic process in
      let* () =
        require
          (process_exited 2 process
          && Option.fold ~none:false
               ~some:(fun diagnostic ->
                 contains diagnostic "VERO_DEPENDENCY"
                 && contains diagnostic "declared specification-library artifacts"
                 && contains diagnostic "rebuild or reinstall"
                 && not (contains diagnostic root)
                 && not (contains diagnostic "manifest"))
               diagnostic)
          "conflicting declared artifact families escaped safe dependency diagnostics"
      in
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project))

let static_module_metadata_case =
  Suite.case
    ~name:"wrapped-dune-nested-module-type-and-multihop-alias-authority"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let root = Filename.concat workspace "project" in
      let generated_fragment =
        Filename.concat workspace "authority_routes.verocaml.inc"
      in
      let generator =
        run_captured ~workspace:(Filename.concat workspace "wrapped-generator")
          ~stdout_path:generated_fragment
          ~program:
            (Filename.concat
               (Project_environment.binary_root environment)
               "verocaml-retained-interface")
          ~arguments:
            [
              "dune-stanza";
              "static_module_metadata";
              ".";
              "authority_routes";
              "Authority_routes__Provider";
              "--transport-unit";
              "Authority_routes";
            ]
          ~forwarded:
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("OCAML_COLOR", "never");
            ]
          ()
      in
      let* () =
        require (process_exited 0 generator)
          ("wrapped retained-interface stanza generator failed: "
          ^ generator.stderr)
      in
      let retained_fragment = read_file generated_fragment in
      let project =
        ({
            files =
              [
        file "dune-project"
          "(lang dune 3.17)\n(name static_module_metadata)\n(package (name static_module_metadata))\n";
        file "dune"
          ({|(library
 (name authority_routes)
 (modules Provider)
 (libraries verocaml.ghost)
 (flags (:standard -w -27-39))
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
(library
 (name static_consumer)
 (wrapped false)
 (modules Consumer)
 (libraries authority_routes verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
|}
          ^ retained_fragment);
        file "provider.mli"
          {|[%%verocaml.symbolic val reflexive_probe : 'a -> bool]
module type PROOFS = sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
module Base : PROOFS
module Alias = Base
module Alias2 = Alias
module Strengthened : module type of Base
module type POLY = sig
  type t
  val lemma : t -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
module type INT_PROOFS = POLY with type t := int
module Substituted : INT_PROOFS
module Qualified : sig
  module type INNER = PROOFS
  module Impl : INNER
end
[@@@verocaml.broadcast_group
  (selected, [Alias2.lemma; Strengthened.lemma; Substituted.lemma;
              Qualified.Impl.lemma])]
|};
        file "provider.ml"
          {|[%%verocaml.symbolic val reflexive_probe : 'a -> bool]
module type PROOFS = sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
module Base : PROOFS = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((reflexive_probe value) [@trigger])) || value = value];
    ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Alias = Base
module Alias2 = Alias
module Strengthened : module type of Base = Base
module type POLY = sig
  type t
  val lemma : t -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
module type INT_PROOFS = POLY with type t := int
module Substituted : INT_PROOFS = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((reflexive_probe value) [@trigger])) || value = value];
    ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Qualified = struct
  module type INNER = PROOFS
  module Impl : INNER = struct
    let lemma (value : int) : unit =
      [%verocaml.ensures fun _ ->
        (not ((reflexive_probe value) [@trigger])) || value = value];
      ()
    [@@verocaml.proof] [@@verocaml.broadcast]
  end
end
[@@@verocaml.broadcast_group
  (selected, [Alias2.lemma; Strengthened.lemma; Substituted.lemma;
              Qualified.Impl.lemma])]
|};
        file "consumer.ml"
          {|[@@@verocaml.verify]
[@@@verocaml.activate [Authority_routes.Provider.selected]]
let consume (value : int) : unit =
  [%verocaml.assert value = value];
  ()
[@@verocaml.proof]
|};
              ];
            libraries = [ "verocaml.ghost" ];
            targets = [ "@all" ];
            selected_units =
              [ "Authority_routes"; "Authority_routes__Provider"; "Consumer" ];
          } : Fixture.dune_project)
      in
      List.iter
        (fun source ->
          write_file (Filename.concat root source.Fixture.path) source.contents)
        project.files;
      let root = Unix.realpath root in
      let* activated =
        Process_adapter.run ~cwd:root
          {
            program = Project_environment.dune_path environment;
            arguments =
              [
                "build";
                "--root";
                root;
                "--build-dir";
                Filename.concat root "_build";
                "--profile";
                "release";
                "@all";
              ];
            forwarded =
              [
                ("PATH", Project_environment.tool_path environment);
                ("OCAMLPATH", Project_environment.ocaml_path environment);
                ("DUNE_CACHE", "disabled");
                ("HOME", root);
                ("TMPDIR", root);
                ("OCAML_COLOR", "never");
              ];
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* () =
        require
          (Outcome.process_facts activated
          |> List.exists (function
               | Outcome.Exit_class (Outcome.Exited 0) -> true
               | Exit_class (Exited _) | Exit_class Signaled | Exit_class Stopped
               | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false))
          "wrapped Dune directory verification rejected exact declared authority"
      in
      let* provider = load root "Authority_routes__Provider" in
      let* consumer = load root "Consumer" in
      let* () =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[ provider ]
            ~consumer
        with
        | Ok () -> Ok ()
        | Error error ->
            mismatch "wrapped authority preflight rejected: %s"
              (Interface_specification_environment_private.error_to_string error)
      in
      let retained = Option.to_list provider.retained_authority_filename in
      let* authority =
        match retained with
        | [ vri ] -> (
            match Retained_interface_authority_private.decode (read_file vri) with
            | Ok authority -> Ok authority
            | Error reason ->
                mismatch "wrapped Dune authority did not decode: %s" reason)
        | [] -> mismatch "wrapped Dune library omitted provider authority"
        | _ :: _ :: _ -> mismatch "wrapped Dune provider authority was ambiguous"
      in
      let members =
        Retained_interface_authority_private.broadcast_witnesses authority
      in
      let declarations =
        members
        |> List.filter (fun member ->
               member.Retained_broadcast_private.identity.kind
               = Retained_broadcast_private.Declaration)
        |> List.map (fun (member : Retained_broadcast_private.interface_member) ->
               member.identity.canonical_path)
      and selected_sources =
        members
        |> List.find_map (fun (member : Retained_broadcast_private.interface_member) ->
               if
                 member.identity.kind = Retained_broadcast_private.Group
                 && String.ends_with ~suffix:".selected"
                      member.identity.canonical_path
               then
                 Some
                   (List.map
                      (fun source -> source.Retained_broadcast_private.member_canonical_path)
                      member.source_members)
               else None)
      in
      let has_suffix suffix paths =
        List.exists (String.ends_with ~suffix) paths
      in
      let* () =
        require
          (has_suffix ".Base.lemma" declarations
          && has_suffix ".Qualified.Impl.lemma" declarations
          &&
          match selected_sources with
          | Some sources ->
              List.length sources = 4
              && has_suffix ".Base.lemma" sources
              && has_suffix ".Strengthened.lemma" sources
              && has_suffix ".Substituted.lemma" sources
              && has_suffix ".Qualified.Impl.lemma" sources
          | None -> false)
          "static module metadata did not preserve exact canonical authority"
      in
      Ok activated)

let ordinary_wrapped_transport_case =
  Suite.case ~name:"ordinary-wrapped-component-is-transport-only"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 0)))
    (fun ~environment ~workspace ->
      let root = Filename.concat workspace "ordinary-wrapper" in
      let fragment = Filename.concat workspace "retained.verocaml.inc" in
      let generator =
        run_captured ~workspace:(Filename.concat workspace "generator")
          ~stdout_path:fragment
          ~program:
            (Filename.concat
               (Project_environment.binary_root environment)
               "verocaml-retained-interface")
          ~arguments:
            [
              "dune-stanza";
              "ordinary_wrapper";
              ".";
              "arbitrary_transport";
              "Arbitrary_transport__Member";
              "--transport-unit";
              "Arbitrary_transport";
            ]
          ~forwarded:
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("OCAML_COLOR", "never");
            ]
          ()
      in
      let* () =
        require (process_exited 0 generator)
          ("ordinary wrapped transport stanza generation failed: "
          ^ generator.stderr)
      in
      let files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name ordinary_wrapper)\n(package (name ordinary_wrapper))\n";
          file "dune"
            ({|(library
 (name arbitrary_transport)
 (modules Member)
 (libraries verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
(library
 (name consumer)
 (wrapped false)
 (modules Consumer)
 (libraries arbitrary_transport verocaml.ghost)
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
|}
            ^ read_file fragment);
          file "member.mli"
            {|[%%verocaml.symbolic val observed : int -> bool]
val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
|};
          file "member.ml"
            {|[%%verocaml.symbolic val observed : int -> bool]
let lemma (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]
|};
          file "consumer.ml"
            {|[@@@verocaml.verify]
[@@@verocaml.activate [Arbitrary_transport.Member.lemma]]
let consume (value : int) : unit =
  [%verocaml.assert value = value];
  ()
[@@verocaml.proof]
|};
        ]
      in
      List.iter
        (fun source ->
          write_file (Filename.concat root source.Fixture.path) source.contents)
        files;
      let root = Unix.realpath root in
      Process_adapter.run ~cwd:root
        {
          program =
            Filename.concat
              (Project_environment.binary_root environment)
              "verocaml";
          arguments =
            [ "verify"; "."; "--threads"; "2"; "--timeout-ms"; "60000" ];
          forwarded =
            [
              ("PATH", Project_environment.tool_path environment);
              ("OCAMLPATH", Project_environment.ocaml_path environment);
              ("VEROCAML_DUNE", Project_environment.dune_path environment);
              ("DUNE_CACHE", "disabled");
              ("HOME", root);
              ("TMPDIR", root);
              ("OCAML_COLOR", "never");
            ];
          cleanup_paths = [];
          adjacency = [];
        })

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      positive_case Standalone "standalone-direct-forwarded-polymorphic-trust";
      positive_case Ppxlib "ppxlib-direct-forwarded-polymorphic-trust";
      installed_adjacent_cmti_case;
      dune_ecosystem_transport_case;
      split_inventory_project_case;
      route_parity_case;
      dune_directory_case;
      dune_directory_malformed_vri_case;
      dune_directory_conflicting_manifests_case;
      static_module_metadata_case;
      ordinary_wrapped_transport_case;
      ordinary_erasure_case Standalone
        "standalone-ordinary-interface-runtime-linkage-erasure";
      ordinary_erasure_case Ppxlib
        "ppxlib-ordinary-interface-runtime-linkage-erasure";
      nested_constrained_erasure_case Standalone
        "standalone-two-level-constrained-recursive-ordinary-erasure";
      nested_constrained_erasure_case Ppxlib
        "ppxlib-two-level-constrained-recursive-ordinary-erasure";
      missing_body_case;
      carrier_non_authority_case;
      private_activation_case;
      cycle_case;
      raw_marker_collision_case;
      include_value_collision_case;
      include_origin_case;
      stale_same_cmi_cmti_case;
      sidecar_rejection_case;
      sidecar_coalescence_case;
      authenticated_ordinary_view_graph_case;
      mutation_case Order_only;
      mutation_case Substitution;
      mutation_case Duplicate;
      mutation_case Kind_change;
      mutation_case Unknown;
      mutation_case Shadow_substitution;
      mutation_case Shadow_selected;
    ]
