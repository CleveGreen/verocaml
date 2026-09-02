open Outcome_test_support

let suite_path = "test/usability_p0_project/outcome_cases.ml"
let ( let* ) = Result.bind

let provider_interface =
  {|
type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) result
[@@verocaml.external_type_specification]

type record_result = { value : int; unstated : int }
type 'a box = { box_value : 'a }
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

val id : 'a -> 'a
val copy_option : 'a option -> 'a option
val copy_result : ('a, 'b) result -> ('a, 'b) result
val length : 'a list -> int
val append : 'a list -> 'a list -> 'a list
val tree_size : 'a tree -> int
val make_record : int -> record_result
val make_box : 'a -> 'a box
val pass_box : 'a box -> 'a box
val make_tree : 'a -> 'a tree
val pass_tree : 'a tree -> 'a tree
val make_unique_record : int -> record_result @ unique
|}

let provider_source =
  {|
[@@@verocaml.verify]

type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) result
[@@verocaml.external_type_specification]

type record_result = { value : int; unstated : int }
type 'a box = { box_value : 'a }
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let id value =
  [%verocaml.ensures fun result -> result = value];
  value

let copy_option (value : 'a option) =
  match value with None -> None | Some payload -> Some payload

let copy_result (value : ('a, 'b) result) =
  match value with Ok payload -> Ok payload | Error error -> Error error

let safe_succ value =
  [%verocaml.requires value >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  if value = 4611686018427387903 then value else value + 1

let safe_add left right =
  [%verocaml.requires left >= 0];
  [%verocaml.requires right >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  if left > 4611686018427387903 - right then 4611686018427387903
  else left + right

let rec length (values : 'a list) =
  [%verocaml.ensures fun result -> result >= 0];
  [%verocaml.decreases values];
  match values with [] -> 0 | _ :: rest -> safe_succ (length rest)

let rec append (left : 'a list) (right : 'a list) =
  [%verocaml.decreases left];
  match left with [] -> right | value :: rest -> value :: append rest right

let rec tree_size (tree : 'a tree) =
  [%verocaml.ensures fun result -> result >= 0];
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> 0
  | Node (_, left, right) ->
      safe_succ (safe_add (tree_size left) (tree_size right))

let make_record value =
  [%verocaml.ensures fun result -> result.value = value];
  { value; unstated = 41 }

let make_box box_value = { box_value }
let pass_box box = box
let make_tree value = Node (value, Leaf, Leaf)
let pass_tree tree = tree

let make_unique_record value : record_result @ unique =
  [%verocaml.ensures fun result -> result.value = value];
  { value; unstated = 42 }
|}

let consumer_interface =
  {|
val evaluate : int -> int
val option_payload : int option -> int
val result_payload : (int, int) result -> int
val list_size : 'a list -> int
val tree_size : 'a Provider.tree -> int
val factory_values : int -> int
|}

let consumer_source =
  {|
[@@@verocaml.verify]

let option_payload value =
  match Provider.copy_option value with None -> 0 | Some payload -> payload

let result_payload value =
  match Provider.copy_result value with
  | Ok payload -> payload
  | Error payload -> payload

let list_size values = Provider.length (Provider.append values [])
let tree_size tree = Provider.tree_size (Provider.pass_tree tree)

let factory_values value =
  let record = Provider.make_record value in
  let unique = Provider.make_unique_record value in
  let box = Provider.pass_box (Provider.make_box value) in
  let _unique_value = unique.Provider.value in
  let _box_value = box.Provider.box_value in
  record.Provider.value

let evaluate value =
  [%verocaml.requires value >= 0];
  let tree =
    Provider.Node
      ( value,
        Provider.make_tree value,
        Provider.Node (value, Provider.Leaf, Provider.Leaf) )
  in
  let copied = Provider.id value in
  let _option = option_payload (Some copied) in
  let _result = result_payload (Ok copied) in
  let _list = list_size [ copied; copied ] in
  tree_size tree
|}

let legacy_interface =
  {|
val promised : int -> int
val select : first:'a -> ?value:'a -> unit -> 'a
|}

let legacy_source =
  {|
let promised value =
  try value + 1 with _ -> value

let select ~first ?(value = first) () = value

module Unsupported_target_body = struct
  class ['a] state (initial : 'a) = object
    val mutable value = initial
    method get = value
  end
end
|}

let external_client_interface =
  {|
val logical_marker : bool -> bool
[@@verocaml.spec]
val promised : int -> int
val reordered : bool -> bool
val omitted : int -> int
val supplied : int -> int
val forwarded : int -> int Provider.option_specification -> int
|}

let external_client_source =
  {|
[@@@verocaml.verify]

let logical_marker (value : bool) = value [@@verocaml.spec]

let promised_specification (value : int) =
  [%verocaml.requires value < 100];
  [%verocaml.ensures fun result -> result = value + 1];
  Legacy.promised value
[@@verocaml.external_specification]

let select_specification ~(first : 'a) ?value () =
  Legacy.select ~first ?value ()
[@@verocaml.external_specification]

let promised value =
  [%verocaml.requires value < 100];
  [%verocaml.ensures fun result -> result = value + 1];
  Legacy.promised value

let reordered (value : bool) = Legacy.select () ~first:value ~value:false
let omitted (value : int) = Legacy.select ~first:value ()
let supplied (value : int) = Legacy.select ~value ~first:value ()

let forwarded
    (value : int)
    (option_value : int Provider.option_specification) =
  Legacy.select ~first:value ?value:option_value ()
|}

let marked_legacy_source =
  {|
[@@@verocaml.verify]

let promised value =
  try value + 1 with _ -> value

module Unsupported = struct
  class ['a] state (initial : 'a) = object
    val mutable value = initial
    method get = value
  end
end

exception Legacy of int
|}

let dune_project_contents name =
  Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name

let library_contents ~name modules =
  Printf.sprintf
    "(library\n (name %s)\n (wrapped false)\n (modules %s)\n (libraries \
     verocaml.ghost)\n (flags (:standard -ppx \"verocaml-ppx \
     --keep-ghost\")))\n"
    name (String.concat " " modules)

let mixed_library_contents =
  {|
(library
 (name legacy_fixture)
 (wrapped false)
 (modules Legacy)
 (flags (:standard -ppx "verocaml-ppx")))

(library
 (name mixed_project_parity)
 (wrapped false)
 (modules Provider Consumer External_client)
 (libraries verocaml.ghost legacy_fixture)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|}

let common_files =
  [
    { Fixture.path = "provider.mli"; contents = provider_interface };
    { path = "provider.ml"; contents = provider_source };
    { path = "consumer.mli"; contents = consumer_interface };
    { path = "consumer.ml"; contents = consumer_source };
  ]

let consumer_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = dune_project_contents "consumer_parity";
          };
          {
            path = "dune";
            contents =
              library_contents ~name:"consumer_parity" [ "Provider"; "Consumer" ];
          };
        ]
        @ common_files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider" ];
    }

let mixed_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = dune_project_contents "mixed_project_parity";
          };
          {
            path = "dune";
            contents = mixed_library_contents;
          };
          { path = "legacy.mli"; contents = legacy_interface };
          { path = "legacy.ml"; contents = legacy_source };
          {
            path = "external_client.mli";
            contents = external_client_interface;
          };
          { path = "external_client.ml"; contents = external_client_source };
        ]
        @ common_files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider" ];
    }

let marked_legacy_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = dune_project_contents "marked_legacy_rejection";
          };
          {
            path = "dune";
            contents =
              library_contents ~name:"marked_legacy_rejection"
                [ "Marked_legacy" ];
          };
          { path = "marked_legacy.ml"; contents = marked_legacy_source };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Marked_legacy" ];
    }

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let check expectation outcome =
  match Expectation.check expectation outcome with
  | Ok () -> Ok ()
  | Error message -> mismatch "%s" message

let provider_bootstrap_expectation =
  Expectation.empty |> Expectation.status Outcome.Verified
  |> Expectation.require_unit "Provider" Outcome.Unit_verified

let prepare_project input ~environment ~workspace =
  let fixture_workspace = Filename.concat workspace "compiled-source" in
  let* outcome = Fixture.run ~environment ~workspace:fixture_workspace input in
  let* () = check provider_bootstrap_expectation outcome in
  Ok (Filename.concat fixture_workspace "project")

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

type artifact = {
  unit_name : string;
  cmt : string;
  cmi : string;
  implementation : Cmt_input.implementation;
}

let load_implementation ~unit_name ~cmt ~cmi =
  match Cmt_input.load_with_interface ~cmt ~cmi () with
  | Ok implementation -> Ok { unit_name; cmt; cmi; implementation }
  | Error diagnostic ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (Printf.sprintf "%s [%s]: %s" unit_name diagnostic.Diagnostic.code
              diagnostic.message))

let discover_artifact project_root unit_name =
  let basename = String.uncapitalize_ascii unit_name ^ ".cmt" in
  let matches =
    files_below (Filename.concat project_root "_build")
    |> List.filter (fun path -> String.equal (Filename.basename path) basename)
  in
  match matches with
  | [ cmt ] ->
      let cmi = Filename.remove_extension cmt ^ ".cmi" in
      if Sys.file_exists cmi then load_implementation ~unit_name ~cmt ~cmi
      else
        Error
          (Failure.make Failure.Selected_cmt_discovery
             ("selected CMI is absent for unit " ^ unit_name))
  | [] ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("no selected CMT for unit " ^ unit_name))
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("ambiguous selected CMT for unit " ^ unit_name))

let load_artifacts project_root names =
  names
  |> List.fold_left
       (fun result name ->
         let* artifacts = result in
         let* artifact = discover_artifact project_root name in
         Ok ((name, artifact) :: artifacts))
       (Ok [])

let artifact name artifacts = List.assoc name artifacts

let configuration threads =
  match
    Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None
  with
  | Ok configuration -> Ok configuration
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Verifier_service.configuration_error_message error))

let inventory_entry role artifact =
  (role, artifact.cmt, artifact.cmi, artifact.implementation)

let run_scope ~threads inventory =
  let* configuration = configuration threads in
  match Verifier_service.scoped_request ~configuration ~inventory with
  | Error error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.scoped_plan_error_message error))
  | Ok request -> Ok (Verifier_service.verify_scope request)

let find_row unit_name scoped =
  match
    Verifier_service.scoped_rows scoped
    |> List.filter (fun row ->
           String.equal (Verifier_service.scoped_row_unit_name row) unit_name)
  with
  | [ row ] -> Ok row
  | [] -> mismatch "scope omitted keyed unit %s" unit_name
  | _ -> mismatch "scope repeated keyed unit %s" unit_name

let root_result unit_name scoped =
  let* row = find_row unit_name scoped in
  match
    ( Verifier_service.scoped_row_classification row,
      Verifier_service.scoped_row_outcome row )
  with
  | Verifier_service.Scoped_verified,
    Verifier_service.Scoped_verification result ->
      Ok result
  | Verifier_service.Scoped_verified, Verifier_service.Scoped_rejection error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))
  | _ -> mismatch "unit %s was not a project root" unit_name

let require_dependency unit_name scoped =
  let* row = find_row unit_name scoped in
  match
    ( Verifier_service.scoped_row_classification row,
      Verifier_service.scoped_row_outcome row )
  with
  | Verifier_service.Scoped_verified_dependency,
    Verifier_service.Scoped_dependency_success ->
      Ok ()
  | Verifier_service.Scoped_verified_dependency,
    Verifier_service.Scoped_rejection error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))
  | _ -> mismatch "unit %s was not a verified dependency" unit_name

let require_skipped unit_name scoped =
  let* row = find_row unit_name scoped in
  match
    ( Verifier_service.scoped_row_classification row,
      Verifier_service.scoped_row_outcome row )
  with
  | Verifier_service.Scoped_skipped, Verifier_service.Scoped_skip -> Ok ()
  | _ -> mismatch "unit %s was not skipped" unit_name

let disposition outcome =
  match Outcome.status outcome with
  | Outcome.Verified -> Outcome.Unit_verified
  | Outcome.Counterexample -> Outcome.Unit_counterexample
  | Outcome.Inconclusive -> Outcome.Unit_inconclusive
  | Outcome.Incomplete_source -> Outcome.Unit_incomplete_source
  | Outcome.Frontend_rejected -> Outcome.Unit_frontend_rejected

let mode_fact key value = (key, Outcome.Function_exists value)

let project_function result key function_name =
  let fact = ("function:" ^ function_name, Outcome.Function_exists function_name) in
  if List.mem fact (Outcome.named_facts (Outcome.of_verifier_result result)) then
    [ (key, Outcome.Function_exists function_name) ]
  else []

let trusted_functions =
  [ "promised"; "reordered"; "omitted"; "supplied"; "forwarded" ]

let trusted_facts result =
  (Verifier_service.vir result).Vir.functions
  |> List.filter_map (fun (execution : Vir.function_execution) ->
         let function_name = execution.function_ref.function_name in
         if
           List.mem function_name trusted_functions
           && List.exists
                (function
                  | Vir.Trusted_external_target_specification_use use ->
                      String.equal use.target_unit "Legacy"
                  | Vir.Trusted_external_specification_use _
                  | Vir.Trusted_external_body_use _ ->
                      false)
                execution.trusted_summary_uses
         then
           Some
             ( "trusted-external:External_client." ^ function_name,
               Outcome.Function_exists function_name )
         else None)

let combined_status results =
  results |> List.map Outcome.of_verifier_result |> Outcome.merge |> Outcome.status

let mixed_projection ~mode_facts scoped =
  let* consumer = root_result "Consumer" scoped in
  let* external_client = root_result "External_client" scoped in
  let* () = require_dependency "Provider" scoped in
  let* () = require_skipped "Legacy" scoped in
  let consumer_outcome = Outcome.of_verifier_result consumer in
  let external_outcome = Outcome.of_verifier_result external_client in
  Ok
    (Outcome.observation
       ~status:(combined_status [ consumer; external_client ])
       ~units:
         [
           ("Consumer", disposition consumer_outcome);
           ("External_client", disposition external_outcome);
           ("Provider", Outcome.Unit_dependency_success);
           ("Legacy", Outcome.Unit_skipped);
         ]
       ~named_facts:(trusted_facts external_client @ mode_facts) ()
    |> Outcome.project)

let consumer_projection ~mode_facts scoped =
  let* consumer = root_result "Consumer" scoped in
  let* () = require_dependency "Provider" scoped in
  let consumer_outcome = Outcome.of_verifier_result consumer in
  Ok
    (Outcome.observation ~status:(Outcome.status consumer_outcome)
       ~units:
         [
           ("Consumer", disposition consumer_outcome);
           ("Provider", Outcome.Unit_dependency_success);
         ]
       ~named_facts:
         (project_function consumer "function:evaluate" "evaluate" @ mode_facts)
       ()
    |> Outcome.project)

let serial_inventory artifacts =
  [
    inventory_entry Verifier_service.Scope_root (artifact "Consumer" artifacts);
    inventory_entry Verifier_service.Scope_dependency
      (artifact "Provider" artifacts);
    inventory_entry Verifier_service.Scope_root
      (artifact "External_client" artifacts);
    inventory_entry Verifier_service.Scope_dependency (artifact "Legacy" artifacts);
  ]

let legacy_first_inventory artifacts =
  [
    inventory_entry Verifier_service.Scope_dependency (artifact "Legacy" artifacts);
    inventory_entry Verifier_service.Scope_root
      (artifact "External_client" artifacts);
    inventory_entry Verifier_service.Scope_dependency
      (artifact "Provider" artifacts);
    inventory_entry Verifier_service.Scope_root (artifact "Consumer" artifacts);
  ]

let consumer_inventory artifacts =
  [
    inventory_entry Verifier_service.Scope_root (artifact "Consumer" artifacts);
    inventory_entry Verifier_service.Scope_dependency
      (artifact "Provider" artifacts);
  ]

let require_parity ~except left right =
  match Outcome.semantic_parity ~except left right with
  | Ok () -> Ok ()
  | Error message -> mismatch "%s" message

let mixed_base_expectation expectation =
  expectation |> Expectation.status Outcome.Verified
  |> Expectation.require_unit "Consumer" Outcome.Unit_verified
  |> Expectation.require_unit "External_client" Outcome.Unit_verified
  |> Expectation.require_unit "Provider" Outcome.Unit_dependency_success
  |> Expectation.require_unit "Legacy" Outcome.Unit_skipped
  |> Expectation.require_named_fact
       "trusted-external:External_client.promised"
       (Outcome.Function_exists "promised")
  |> Expectation.require_named_fact
       "trusted-external:External_client.reordered"
       (Outcome.Function_exists "reordered")
  |> Expectation.require_named_fact
       "trusted-external:External_client.omitted"
       (Outcome.Function_exists "omitted")
  |> Expectation.require_named_fact
       "trusted-external:External_client.supplied"
       (Outcome.Function_exists "supplied")
  |> Expectation.require_named_fact
       "trusted-external:External_client.forwarded"
       (Outcome.Function_exists "forwarded")

let consumer_base_expectation expectation =
  expectation |> Expectation.status Outcome.Verified
  |> Expectation.require_unit "Consumer" Outcome.Unit_verified
  |> Expectation.require_unit "Provider" Outcome.Unit_dependency_success
  |> Expectation.require_named_fact "function:evaluate"
       (Outcome.Function_exists "evaluate")

let mixed_serial_threaded_expectation =
  Expectation.empty |> mixed_base_expectation
  |> Expectation.require_named_fact "execution-mode"
       (Outcome.Function_exists "threads-1")
  |> Expectation.require_named_fact "inventory-order"
       (Outcome.Function_exists "consumer-first")
  |> Expectation.require_named_fact "execution-mode"
       (Outcome.Function_exists "threads-2")
  |> Expectation.require_named_fact "inventory-order"
       (Outcome.Function_exists "legacy-first")

let consumer_source_cmt_expectation =
  Expectation.empty |> consumer_base_expectation
  |> Expectation.require_named_fact "input-mode"
       (Outcome.Function_exists "source")
  |> Expectation.require_named_fact "execution-mode"
       (Outcome.Function_exists "threads-1")
  |> Expectation.require_named_fact "input-mode"
       (Outcome.Function_exists "prepared-cmt")
  |> Expectation.require_named_fact "execution-mode"
       (Outcome.Function_exists "threads-2")

let mixed_copied_expectation =
  Expectation.empty |> mixed_base_expectation
  |> Expectation.require_named_fact "artifact-location"
       (Outcome.Function_exists "original")
  |> Expectation.require_named_fact "artifact-location"
       (Outcome.Function_exists "copied")

let mixed_serial_threaded_parity ~environment ~workspace =
  let* project_root = prepare_project mixed_project ~environment ~workspace in
  let* artifacts =
    load_artifacts project_root
      [ "Consumer"; "External_client"; "Provider"; "Legacy" ]
  in
  let* serial_scope = run_scope ~threads:1 (serial_inventory artifacts) in
  let* serial =
    mixed_projection
      ~mode_facts:
        [
          mode_fact "execution-mode" "threads-1";
          mode_fact "inventory-order" "consumer-first";
        ]
      serial_scope
  in
  let* threaded_artifacts =
    load_artifacts project_root
      [ "Consumer"; "External_client"; "Provider"; "Legacy" ]
  in
  let* threaded_scope =
    run_scope ~threads:2 (legacy_first_inventory threaded_artifacts)
  in
  let* threaded =
    mixed_projection
      ~mode_facts:
        [
          mode_fact "execution-mode" "threads-2";
          mode_fact "inventory-order" "legacy-first";
        ]
      threaded_scope
  in
  let* () =
    require_parity
      ~except:[ "execution-mode"; "inventory-order" ]
      serial threaded
  in
  Ok (Outcome.merge [ serial; threaded ])

let copy_file source destination =
  let input_channel = open_in_bin source in
  let output_channel = open_out_bin destination in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr input_channel;
      close_out_noerr output_channel)
    (fun () ->
      let buffer = Bytes.create 65_536 in
      let rec copy () =
        match input input_channel buffer 0 (Bytes.length buffer) with
        | 0 -> ()
        | count ->
            output output_channel buffer 0 count;
            copy ()
      in
      copy ())

let copy_artifacts ~destination artifacts =
  if not (Sys.file_exists destination) then Unix.mkdir destination 0o755;
  artifacts
  |> List.fold_left
       (fun result (name, artifact) ->
         let* copied = result in
         let cmt = Filename.concat destination (Filename.basename artifact.cmt) in
         let cmi = Filename.concat destination (Filename.basename artifact.cmi) in
         copy_file artifact.cmt cmt;
         copy_file artifact.cmi cmi;
         let* artifact = load_implementation ~unit_name:name ~cmt ~cmi in
         Ok ((name, artifact) :: copied))
       (Ok [])

let declare_prepared artifact =
  match
    Fixture.prepared_cmt ~declared_dependencies:[ artifact.cmt ] artifact.cmt
  with
  | Ok _ -> Ok ()
  | Error message -> mismatch "prepared CMT %s: %s" artifact.unit_name message

let consumer_source_cmt_parity ~environment ~workspace =
  let* project_root = prepare_project consumer_project ~environment ~workspace in
  let* source_artifacts =
    load_artifacts project_root [ "Consumer"; "Provider" ]
  in
  let* source_scope = run_scope ~threads:1 (consumer_inventory source_artifacts) in
  let* source =
    consumer_projection
      ~mode_facts:
        [
          mode_fact "input-mode" "source";
          mode_fact "execution-mode" "threads-1";
        ]
      source_scope
  in
  let* prepared_artifacts =
    copy_artifacts ~destination:(Filename.concat workspace "prepared-cmt")
      source_artifacts
  in
  let* () = declare_prepared (artifact "Consumer" prepared_artifacts) in
  let* () = declare_prepared (artifact "Provider" prepared_artifacts) in
  let* prepared_scope =
    run_scope ~threads:2 (consumer_inventory prepared_artifacts)
  in
  let* prepared =
    consumer_projection
      ~mode_facts:
        [
          mode_fact "input-mode" "prepared-cmt";
          mode_fact "execution-mode" "threads-2";
        ]
      prepared_scope
  in
  let* () =
    require_parity
      ~except:[ "input-mode"; "execution-mode" ]
      source prepared
  in
  Ok (Outcome.merge [ source; prepared ])

let mixed_project_copied_artifact_parity ~environment ~workspace =
  let* project_root = prepare_project mixed_project ~environment ~workspace in
  let* original_artifacts =
    load_artifacts project_root
      [ "Consumer"; "External_client"; "Provider"; "Legacy" ]
  in
  let* original_scope = run_scope ~threads:1 (serial_inventory original_artifacts) in
  let* original =
    mixed_projection
      ~mode_facts:[ mode_fact "artifact-location" "original" ]
      original_scope
  in
  let* copied_artifacts =
    copy_artifacts ~destination:(Filename.concat workspace "copied")
      original_artifacts
  in
  let* copied_scope = run_scope ~threads:1 (serial_inventory copied_artifacts) in
  let* copied =
    mixed_projection ~mode_facts:[ mode_fact "artifact-location" "copied" ]
      copied_scope
  in
  let* () =
    require_parity ~except:[ "artifact-location" ] original copied
  in
  Ok (Outcome.merge [ original; copied ])

let mixed_serial_threaded_case =
  Suite.case ~name:"mixed-project-serial-threaded-parity"
    ~expectation:mixed_serial_threaded_expectation mixed_serial_threaded_parity

let marked_legacy_case =
  Suite.case ~name:"marked-legacy-frontend-rejection"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_STRUCTURE_ITEM"
      |> Expectation.require_unit "Marked_legacy"
           Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace marked_legacy_project)

let consumer_source_cmt_case =
  Suite.case ~name:"consumer-source-cmt-parity"
    ~expectation:consumer_source_cmt_expectation consumer_source_cmt_parity

let mixed_copied_case =
  Suite.case ~name:"mixed-project-copied-artifact-parity"
    ~expectation:mixed_copied_expectation
    mixed_project_copied_artifact_parity

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      mixed_serial_threaded_case;
      marked_legacy_case;
      consumer_source_cmt_case;
      mixed_copied_case;
    ]
