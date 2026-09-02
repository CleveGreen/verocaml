open Outcome_test_support

let ( let* ) = Result.bind
let suite_path = "test/broadcast_authority_soundness/outcome_cases.ml"

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let require condition message = if condition then Ok () else mismatch "%s" message
let file path contents = { Fixture.path; contents }

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

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let contains text fragment =
  let text_length = String.length text
  and fragment_length = String.length fragment in
  let rec search index =
    index + fragment_length <= text_length
    &&
    (String.sub text index fragment_length = fragment || search (index + 1))
  in
  fragment_length = 0 || search 0

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
  | _ :: _ :: _ -> mismatch "ambiguous artifact %s%s" unit_name extension

let optional_artifact root unit_name extension =
  match artifact root unit_name extension with Ok path -> Some path | Error _ -> None

let artifact_directories root =
  files_below (Filename.concat root "_build")
  |> List.filter (fun path ->
         List.mem (Filename.extension path) [ ".cmi"; ".cmti"; ".vri" ])
  |> List.map Filename.dirname |> List.sort_uniq String.compare

let load root unit_name =
  let* cmt = artifact root unit_name ".cmt" in
  let* cmi = artifact root unit_name ".cmi" in
  match
    Cmt_input.load_with_interface ~cmt ~cmi
      ?cmti:(optional_artifact root unit_name ".cmti")
      ?vri:(optional_artifact root unit_name ".vri")
      ~artifact_directories:(artifact_directories root) ()
  with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      mismatch "failed to load %s: %s" unit_name diagnostic.Diagnostic.code

let broadcast_scan implementation =
  match
    Broadcast_declaration_private.authenticate_typedtree
      ~source_file:implementation.Cmt_input.source_file
      ~imports:implementation.imports
      ~artifact:
        (Some
           (Typedtree_adapter_private.Public.proof_capture_artifact
              implementation))
      implementation.structure
  with
  | Ok scan -> Ok scan
  | Error diagnostic ->
      mismatch "broadcast scan rejected: %s" diagnostic.Diagnostic.code

let run_dune ?(cleanup_paths = []) ~environment ~workspace files =
  let workspace =
    if Filename.is_relative workspace then Filename.concat (Sys.getcwd ()) workspace
    else workspace
  in
  let root = Filename.concat workspace "project" in
  List.iter
    (fun source ->
      write_file (Filename.concat root source.Fixture.path) source.contents)
    files;
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
        cleanup_paths;
        adjacency = [];
      }
  in
  Ok (root, process)

let exited code projection =
  List.exists
    (function
      | Outcome.Exit_class (Outcome.Exited actual) -> actual = code
      | Exit_class Signaled | Exit_class Stopped | Stable_code _ | Forwarded _
      | Cleaned _ | Adjacent _ ->
          false)
    (Outcome.process_facts projection)

let rejected projection =
  List.exists
    (function
      | Outcome.Exit_class (Outcome.Exited code) -> code <> 0
      | Exit_class Signaled | Exit_class Stopped -> true
      | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false)
    (Outcome.process_facts projection)

let frontend_projection process =
  let frontend_codes =
    Outcome.process_facts process
    |> List.filter_map (function
         | Outcome.Stable_code code -> Some code
         | Exit_class _ | Forwarded _ | Cleaned _ | Adjacent _ -> None)
  in
  Outcome.observation ~status:Outcome.Frontend_rejected
    ~frontend_codes
    ~process_facts:(Outcome.process_facts process) ()
  |> Outcome.project

type route = Standalone | Ppxlib

let retained_preprocessing = function
  | Standalone ->
      {|(flags
 (:standard -w -27-32-39-60 -ppx "verocaml-ppx --keep-ghost"))|}
  | Ppxlib ->
      {|(flags (:standard -w -27-32-39-60))
 (preprocess (pps verocaml.ppx -- --verocaml-retained))|}

let authority_rule library unit_name =
  let stem = String.uncapitalize_ascii unit_name in
  let object_path = "." ^ library ^ ".objs/byte/" ^ stem in
  String.concat ""
    [
      "\n(rule\n (targets ";
      stem;
      ".vri)\n (deps\n  (sandbox always)\n  (:emitter %{bin:verocaml-retained-interface})\n  (:cmt ";
      object_path;
      ".cmt)\n  (:cmi ";
      object_path;
      ".cmi)\n  (:cmti ";
      object_path;
      ".cmti))\n (action\n  (run %{emitter} emit %{cmt} %{cmi} %{cmti} ";
      stem;
      ".vri\n   --artifact-directory .\n   --artifact-directory .";
      library;
      ".objs/byte)))\n(alias (name all) (deps ";
      stem;
      ".vri))\n";
    ]

let declared_authority_rule library unit_name =
  let stem = String.uncapitalize_ascii unit_name in
  let object_path = "." ^ library ^ ".objs/byte/" ^ stem in
  Printf.sprintf
    {|
(rule
 (targets %s.vri %s.verocaml-retained-interface)
 (deps
  (sandbox always)
  (:emitter %%{bin:verocaml-retained-interface})
  (:cmt %s.cmt)
  (:cmi %s.cmi)
  (:cmti %s.cmti))
 (action
  (progn
   (run %%{emitter} emit %%{cmt} %%{cmi} %%{cmti} %s.vri
    --artifact-directory .
    --artifact-directory .%s.objs/byte)
   (run %%{emitter} manifest %s.verocaml-retained-interface
    %s.cmt %s.cmi %s.cmti %s.vri))))
(alias (name all) (deps %s.vri %s.verocaml-retained-interface))
|}
    stem stem object_path object_path object_path stem library stem object_path
    object_path object_path stem stem stem

let provider_project_description ~route ~name ~provider_mli ~provider_ml
    ?consumer_ml () =
  let consumer_stanza =
    match consumer_ml with
    | None -> ""
    | Some _ ->
        String.concat "\n"
          [
            "(library";
            " (name consumer_library)";
            " (wrapped false)";
            " (modules Consumer)";
            " (libraries provider_library verocaml.ghost)";
            " ";
            retained_preprocessing route;
            ")";
          ]
  in
  let dune =
    String.concat "\n"
      [
        "(library";
        " (name provider_library)";
        " (wrapped false)";
        " (modules Provider)";
        " (libraries verocaml.ghost)";
        " ";
        retained_preprocessing route;
        ")";
        consumer_stanza;
      ]
    ^ authority_rule "provider_library" "Provider"
  in
  ({
      files =
        [
          file "dune-project"
            ("(lang dune 3.17)\n(name " ^ name ^ ")\n");
          file "dune" dune;
          file "provider.mli" provider_mli;
          file "provider.ml" provider_ml;
        ]
        @ (match consumer_ml with
          | None -> []
          | Some source -> [ file "consumer.ml" source ]);
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units =
        "Provider" :: (match consumer_ml with None -> [] | Some _ -> [ "Consumer" ]);
    } : Fixture.dune_project)

let provider_project ~route ~name ~provider_mli ~provider_ml ?consumer_ml () =
  provider_project_description ~route ~name ~provider_mli ~provider_ml
    ?consumer_ml ()
  |> Fixture.dune_project

let simple_provider_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
val lemma_a : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
val lemma_b : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
[@@@verocaml.broadcast_group (selected, [lemma_a; lemma_b])]
|}

let simple_provider_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
let lemma_a (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]

let lemma_b (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]

[@@@verocaml.broadcast_group (selected, [lemma_b; lemma_a])]
|}

let simple_consumer_ml =
  {|[@@@verocaml.activate [Provider.selected]]
let use (value : int) : unit =
  [%verocaml.assert value = value]; ()
[@@verocaml.proof]
|}

let kind_name identity =
  Retained_broadcast_private.kind_name
    identity.Retained_broadcast_private.kind

let authority_projection implementation =
  implementation.Cmt_input.interface_broadcasts
  |> List.map (fun (member : Retained_broadcast_private.interface_member) ->
         ( member.identity.canonical_path,
           kind_name member.identity,
           member.source_members
           |> List.map (fun source ->
                  ( source.Retained_broadcast_private.member_canonical_path,
                    Retained_broadcast_private.kind_name source.member_kind ))
           |> List.sort compare ))
  |> List.sort compare

let positive_group_case =
  Suite.case ~name:"positive-group-exact-self-crc-identity-and-activation"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      let project =
        provider_project ~route:Standalone ~name:"positive_group"
          ~provider_mli:simple_provider_mli ~provider_ml:simple_provider_ml
          ~consumer_ml:simple_consumer_ml ()
      in
      let* outcome = Fixture.run ~environment ~workspace project in
      let root = Filename.concat workspace "project" in
      let* provider = load root "Provider" in
      let declarations, groups =
        List.partition
          (fun member ->
            member.Retained_broadcast_private.identity.kind
            = Retained_broadcast_private.Declaration)
          provider.interface_broadcasts
      in
      let* group =
        match groups with
        | [ group ] -> Ok group
        | [] | _ :: _ :: _ -> mismatch "positive group authority is not unique"
      in
      let source_matches_identity source =
        List.exists
          (fun (declaration : Retained_broadcast_private.interface_member) ->
            let identity = declaration.identity in
            String.equal source.Retained_broadcast_private.member_provider_origin
              identity.provider_origin
            && String.equal source.member_interface_receipt
                 identity.interface_digest
            && String.equal source.member_dependency_receipt
                 identity.dependency_receipt
            && String.equal source.member_compiler_uid identity.compiler_uid
            && String.equal source.member_canonical_path identity.canonical_path
            && source.member_kind = identity.kind)
          declarations
      in
      let* () =
        require
          (List.length declarations = 2
          && List.length group.source_members = 2
          && List.for_all source_matches_identity group.source_members)
          "group source identity did not use the declaration's exact CMI self-CRC representation"
      in
      let* () =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[] ~consumer:provider
        with
        | Ok () -> Ok ()
        | Error error ->
            mismatch "positive preflight rejected: %s"
              (Interface_specification_environment_private.error_to_string error)
      in
      Ok outcome)

let nested_provider_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
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
module Equal : POLY with type t = int
module Removed : POLY with type t := int
module Named : sig
  module type INNER = PROOFS
  module Impl : INNER
end
[@@@verocaml.broadcast_group
  (selected,
   [Alias2.lemma; Strengthened.lemma; Equal.lemma; Removed.lemma;
    Named.Impl.lemma])]
|}

let nested_provider_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module type PROOFS = sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
module Base : PROOFS = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Alias = Base
module Alias2 = Alias
module Strengthened : module type of Base = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module type POLY = sig
  type t
  val lemma : t -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
module Equal : POLY with type t = int = struct
  type t = int
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Removed : POLY with type t := int = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Named = struct
  module type INNER = PROOFS
  module Impl : INNER = struct
    let lemma (value : int) : unit =
      [%verocaml.ensures fun _ ->
        (not ((observed value) [@trigger])) || value = value]; ()
    [@@verocaml.proof] [@@verocaml.broadcast]
  end
end
[@@@verocaml.broadcast_group
  (selected,
   [Named.Impl.lemma; Removed.lemma; Equal.lemma; Strengthened.lemma;
    Alias2.lemma])]
|}

let nested_consumer_ml =
  {|[@@@verocaml.activate [Provider.selected]]
let use (value : int) : unit =
  [%verocaml.assert value = value]; ()
[@@verocaml.proof]
|}

let nested_project_description route =
  provider_project_description ~route ~name:"nested_authority"
    ~provider_mli:nested_provider_mli ~provider_ml:nested_provider_ml
    ~consumer_ml:nested_consumer_ml ()

let nested_route_parity_case =
  Suite.case
    ~name:
      "nested-named-alias-strengthening-with-equal-and-destructive-parity-activation"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let standalone_workspace = Filename.concat workspace "standalone"
      and ppxlib_workspace = Filename.concat workspace "ppxlib" in
      let standalone_project = nested_project_description Standalone
      and ppxlib_project = nested_project_description Ppxlib in
      let* standalone_root, standalone =
        run_dune ~environment ~workspace:standalone_workspace
          standalone_project.files
      in
      let* ppxlib_root, ppxlib =
        run_dune ~environment ~workspace:ppxlib_workspace ppxlib_project.files
      in
      let* () =
        require (exited 0 standalone && exited 0 ppxlib)
          "nested retained projects did not compile and emit authority"
      in
      let* standalone_provider =
        load standalone_root "Provider"
      in
      let* ppxlib_provider =
        load ppxlib_root "Provider"
      in
      let* standalone_consumer = load standalone_root "Consumer" in
      let* ppxlib_consumer = load ppxlib_root "Consumer" in
      let preflight route provider consumer =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[ provider ]
            ~consumer
        with
        | Ok () -> Ok ()
        | Error error ->
            let diagnostic =
              Interface_specification_environment_private.error_diagnostic error
              |> Option.map (fun diagnostic ->
                     match diagnostic.Diagnostic.classification with
                     | Diagnostic.Invalid_broadcast detail -> detail
                     | _ -> diagnostic.message)
              |> Option.value ~default:"no diagnostic"
            in
            mismatch "%s nested preflight/activation rejected: %s (%s)" route
              (Interface_specification_environment_private.error_to_string error)
              diagnostic
      in
      let* () =
        preflight "standalone" standalone_provider standalone_consumer
      in
      let* () = preflight "ppxlib" ppxlib_provider ppxlib_consumer in
      let expected_paths =
        [
          "Provider.Base.lemma";
          "Provider.Equal.lemma";
          "Provider.Named.Impl.lemma";
          "Provider.Removed.lemma";
          "Provider.Strengthened.lemma";
        ]
      in
      let declarations_and_sources implementation =
        List.fold_left
          (fun (declarations, sources) member ->
            match member.Retained_broadcast_private.identity.kind with
            | Retained_broadcast_private.Declaration ->
                (member.identity.canonical_path :: declarations, sources)
            | Retained_broadcast_private.Group ->
                ( declarations,
                  List.rev_append
                    (List.map
                       (fun source ->
                         source.Retained_broadcast_private.member_canonical_path)
                       member.source_members)
                    sources ))
          ([], []) implementation.Cmt_input.interface_broadcasts
      in
      let standalone_declarations, standalone_sources =
        declarations_and_sources standalone_provider
      and ppxlib_declarations, ppxlib_sources =
        declarations_and_sources ppxlib_provider
      in
      let canonical values = List.sort_uniq String.compare values in
      let* () =
        require
          (canonical standalone_declarations = expected_paths
          && canonical standalone_sources = expected_paths
          && canonical ppxlib_declarations = expected_paths
          && canonical ppxlib_sources = expected_paths)
          "nested alias/strengthening/with-type authority did not preserve exact compiler paths"
      in
      let* () =
        require
          (authority_projection standalone_provider
          = authority_projection ppxlib_provider)
          "standalone and Ppxlib nested authority projections diverged"
      in
      let* () =
        require
          (standalone_provider.interface_family_markers = [ "retained-v1" ]
          && ppxlib_provider.interface_family_markers = [ "retained-v1" ]
          && standalone_provider.interface_family_issuers = [ "standalone-v1" ]
          && ppxlib_provider.interface_family_issuers = [ "ppxlib-v1" ])
          "nested retained family receipt or route provenance was not authenticated"
      in
      Ok (Outcome.merge [ standalone; ppxlib ]))

let nested_only_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Outer : sig
  module Inner : sig
    val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
  end
end
|}

let nested_only_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Outer = struct
  module Inner = struct
    let lemma (value : int) : unit =
      [%verocaml.ensures fun _ ->
        (not ((observed value) [@trigger])) || value = value]; ()
    [@@verocaml.proof] [@@verocaml.broadcast]
  end
end
|}

let nested_only_family_case =
  Suite.case ~name:"nested-only-declaration-authenticates-family-receipt"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let project =
        provider_project_description ~route:Ppxlib ~name:"nested_only_family"
          ~provider_mli:nested_only_mli ~provider_ml:nested_only_ml ()
      in
      let* root, outcome = run_dune ~environment ~workspace project.files in
      let* () = require (exited 0 outcome) "nested-only provider did not emit" in
      let* provider = load root "Provider" in
      let* () =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[] ~consumer:provider
        with
        | Ok () -> Ok ()
        | Error error ->
            mismatch "nested-only provider preflight rejected: %s"
              (Interface_specification_environment_private.error_to_string error)
      in
      let* () =
        require
          (provider.interface_family_markers = [ "retained-v1" ]
          && provider.interface_family_issuers = [ "ppxlib-v1" ]
          &&
          match provider.interface_broadcasts with
          | [ member ] ->
              member.identity.kind = Retained_broadcast_private.Declaration
              && String.equal member.identity.canonical_path
                   "Provider.Outer.Inner.lemma"
          | [] | _ :: _ :: _ -> false)
          "nested-only retained declaration lost its authenticated family receipt"
      in
      Ok outcome)

let missing_completion_mli =
  {|val promised : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
|}

let missing_completion_ml =
  {|let promised (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof]
|}

let substitution_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
val lemma_a : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
[@@@verocaml.broadcast_group (selected, [lemma_a])]
|}

let substitution_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
let lemma_a (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]

let lemma_b (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]

[@@@verocaml.broadcast_group (selected, [lemma_b])]
|}

let preflight_negative_case ~name ~provider_mli ~provider_ml =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_DEPENDENCY")
    (fun ~environment ~workspace ->
      Solver_backend_counter_private.reset_solver_creation_count ();
      Interface_specification_loaded_private.For_testing
      .reset_provider_verification_entries ();
      let project =
        provider_project ~route:Standalone ~name ~provider_mli ~provider_ml ()
      in
      let* outcome = Fixture.run ~environment ~workspace project in
      let* () =
        require
          (Solver_backend_counter_private.solver_creation_count () = 0
          &&
          Interface_specification_loaded_private.For_testing
          .provider_verification_entries ()
          = 0)
          "invalid implementation authority reached provider verification or solver work"
      in
      let* provider = load (Filename.concat workspace "project") "Provider" in
      Solver_backend_counter_private.reset_solver_creation_count ();
      Interface_specification_loaded_private.For_testing
      .reset_provider_verification_entries ();
      let* diagnostic =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[] ~consumer:provider
        with
        | Ok () -> mismatch "invalid implementation authority passed direct preflight"
        | Error error -> (
            match
              Interface_specification_environment_private.error_diagnostic error
            with
            | Some diagnostic
              when not
                     (Interface_specification_environment_private
                      .error_is_internal error) ->
                Ok diagnostic
            | Some _ | None ->
                mismatch
                  "invalid implementation authority lacked a public dependency diagnostic")
      in
      let* () =
        require
          (String.equal diagnostic.Diagnostic.code "VERO_DEPENDENCY"
          && Diagnostic.failure_class diagnostic.classification
             = Diagnostic.Artifact_failure
          && Solver_backend_counter_private.solver_creation_count () = 0
          &&
          Interface_specification_loaded_private.For_testing
          .provider_verification_entries ()
          = 0)
          "preflight rejected at the wrong boundary or after solver/provider work"
      in
      Ok outcome)

let missing_completion_case =
  preflight_negative_case
    ~name:"missing-broadcast-completion-rejects-before-provider-and-solver"
    ~provider_mli:missing_completion_mli ~provider_ml:missing_completion_ml

let substitution_case =
  preflight_negative_case
    ~name:"group-member-substitution-rejects-for-implementation-member-before-solver"
    ~provider_mli:substitution_mli ~provider_ml:substitution_ml

let shadowed_projection_mli =
  {|module Public : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
|}

let shadowed_projection_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Target = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Target = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ -> value = value]; ()
  [@@verocaml.proof]
end
module Public : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end = Target
|}

let shadowed_projection_case =
  Suite.case
    ~name:"shadowed-same-name-module-cannot-supply-projected-broadcast-body"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Frontend_rejected)
    (fun ~environment ~workspace ->
      let project =
        provider_project_description ~route:Standalone ~name:"shadowed_projection"
          ~provider_mli:shadowed_projection_mli
          ~provider_ml:shadowed_projection_ml ()
      in
      let* _, process = run_dune ~environment ~workspace project.files in
      let* () =
        require (rejected process)
          "same-spelling shadowed module supplied broadcast authority"
      in
      Ok (frontend_projection process))

let two_hop_projection_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
module B : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
|}

let two_hop_projection_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Base = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module A : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end = Base
module B : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end = A
|}

let two_hop_projection_case =
  Suite.case ~name:"two-hop-constrained-projection-preserves-one-body"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let project =
        provider_project_description ~route:Standalone
          ~name:"two_hop_projection" ~provider_mli:two_hop_projection_mli
          ~provider_ml:two_hop_projection_ml ()
      in
      let* root, process = run_dune ~environment ~workspace project.files in
      let* () = require (exited 0 process) "two-hop projection did not compile" in
      let* provider = load root "Provider" in
      let* () =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[] ~consumer:provider
        with
        | Ok () -> Ok ()
        | Error error ->
            mismatch "two-hop projection preflight rejected: %s"
              (Interface_specification_environment_private.error_to_string error)
      in
      Ok process)

let local_shadow_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
val use : int -> unit [@@verocaml.proof]
|}

let local_shadow_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
let lemma (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
let lemma (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof]
[@@@verocaml.activate [lemma]]
let use (value : int) : unit =
  [%verocaml.assert value = value]; ()
[@@verocaml.proof]
|}

let local_shadow_case =
  Suite.case ~name:"shadowed-local-value-uid-cannot-gain-broadcast-authority"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Frontend_rejected)
    (fun ~environment ~workspace ->
      let files =
        [
          file "dune-project" "(lang dune 3.17)\n(name local_shadow)\n";
          file "dune"
            (String.concat "\n"
               [
                 "(library";
                 " (name provider_library)";
                 " (wrapped false)";
                 " (modules Provider)";
                 " (libraries verocaml.ghost)";
                 retained_preprocessing Standalone;
                 ")";
               ]);
          file "provider.mli" local_shadow_mli;
          file "provider.ml" local_shadow_ml;
        ]
      in
      let* root, process = run_dune ~environment ~workspace files in
      let* () = require (exited 0 process) "local shadow fixture did not compile" in
      let* provider = load root "Provider" in
      Solver_backend_counter_private.reset_solver_creation_count ();
      let* () =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[] ~consumer:provider
        with
        | Ok () -> mismatch "shadowed local value gained broadcast authority"
        | Error _ ->
            require
              (Solver_backend_counter_private.solver_creation_count () = 0)
              "shadowed local rejection occurred after solver creation"
      in
      Ok (frontend_projection process))

let constrained_subset_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Public : sig
  val visible : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
|}

let constrained_subset_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Base = struct
  let visible (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
  let hidden (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Public : sig
  val visible : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end = Base
|}

let constrained_subset_case =
  Suite.case ~name:"constraint-hidden-broadcast-member-has-no-projected-authority"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let project =
        provider_project_description ~route:Standalone ~name:"constrained_subset"
          ~provider_mli:constrained_subset_mli
          ~provider_ml:constrained_subset_ml ()
      in
      let* root, process = run_dune ~environment ~workspace project.files in
      let* () = require (exited 0 process) "constrained subset did not compile" in
      let* provider = load root "Provider" in
      let* scan = broadcast_scan provider in
      let* () =
        require
          (Option.is_some
             (Typedtree_broadcast_private.declaration_identity scan
                "broadcast:Public.visible")
          && Option.is_none
               (Typedtree_broadcast_private.declaration_identity scan
                  "broadcast:Public.hidden"))
          "constraint did not preserve the exact public broadcast subset"
      in
      Ok process)

let alias_projection_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Base = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Alias = Base
module Public : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end = Alias
|}

let alias_projection_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Public : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
|}

let alias_projection_case =
  Suite.case ~name:"transparent-alias-then-constrained-projection-is-exact"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let project =
        provider_project_description ~route:Standalone ~name:"alias_projection"
          ~provider_mli:alias_projection_mli
          ~provider_ml:alias_projection_ml ()
      in
      let* root, process = run_dune ~environment ~workspace project.files in
      let* () = require (exited 0 process) "alias projection did not compile" in
      let* provider = load root "Provider" in
      let* () =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[] ~consumer:provider
        with
        | Ok () -> Ok ()
        | Error error ->
            mismatch "alias projection preflight rejected: %s"
              (Interface_specification_environment_private.error_to_string error)
      in
      Ok process)

let outer_module_type_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
module type OUTER = sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
module Nested : sig
  module type ALIAS = OUTER
  module Impl : ALIAS
end
|}

let outer_module_type_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module type OUTER = sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
module Nested = struct
  module type ALIAS = OUTER
  module Impl : ALIAS = struct
    let lemma (value : int) : unit =
      [%verocaml.ensures fun _ ->
        (not ((observed value) [@trigger])) || value = value]; ()
    [@@verocaml.proof] [@@verocaml.broadcast]
  end
end
|}

let outer_module_type_case =
  Suite.case ~name:"nested-outer-module-type-alias-keeps-exact-binding-uid"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let project =
        provider_project_description ~route:Standalone ~name:"outer_module_type"
          ~provider_mli:outer_module_type_mli
          ~provider_ml:outer_module_type_ml ()
      in
      let* root, process = run_dune ~environment ~workspace project.files in
      let* () = require (exited 0 process) "outer module-type alias did not compile" in
      let* provider = load root "Provider" in
      let* () =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[] ~consumer:provider
        with
        | Ok () -> Ok ()
        | Error error ->
            mismatch "outer module-type alias preflight rejected: %s"
              (Interface_specification_environment_private.error_to_string error)
      in
      Ok process)

let ambiguous_nested_mli =
  {|module Outer : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
|}

let ambiguous_nested_ml =
  {|module Outer = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ -> (value = value) [@trigger]]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
|}

let ambiguous_nested_case =
  Suite.case ~name:"ambiguous-nested-interface-binding-rejects-before-solver"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Frontend_rejected)
    (fun ~environment ~workspace ->
      Solver_backend_counter_private.reset_solver_creation_count ();
      let project =
        provider_project_description ~route:Standalone ~name:"ambiguous_nested"
          ~provider_mli:ambiguous_nested_mli ~provider_ml:ambiguous_nested_ml ()
      in
      let* _, process = run_dune ~environment ~workspace project.files in
      let* () =
        require
          (rejected process
          && Solver_backend_counter_private.solver_creation_count () = 0)
          "ambiguous nested binding was accepted or reached a solver"
      in
      Ok (frontend_projection process))

let colliding_projection_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module Base = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
module Public : sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end = Base
|}

let colliding_projection_case =
  Suite.case ~name:"colliding-projection-definitions-reject-before-solver"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Frontend_rejected)
    (fun ~environment ~workspace ->
      let project =
        provider_project_description ~route:Standalone
          ~name:"colliding_projection" ~provider_mli:alias_projection_mli
          ~provider_ml:colliding_projection_ml ()
      in
      let* root, process = run_dune ~environment ~workspace project.files in
      let* () = require (exited 0 process) "collision fixture did not compile" in
      let* provider = load root "Provider" in
      Solver_backend_counter_private.reset_solver_creation_count ();
      let* () =
        match
          Interface_specification_environment_private
          .preflight_broadcast_implementations ~dependencies:[] ~consumer:provider
        with
        | Ok () -> mismatch "colliding projection definitions were accepted"
        | Error _ ->
            require
              (Solver_backend_counter_private.solver_creation_count () = 0)
              "colliding projection rejection occurred after solver creation"
      in
      Ok (frontend_projection process))

let cycle_source =
  {|let lemma (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
[@@@verocaml.broadcast_group (left, [right; lemma])]
[@@@verocaml.broadcast_group (right, [left])]
|}

let cycle_case =
  Suite.case ~name:"broadcast-group-cycle-rejects-before-solver"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_INVALID_BROADCAST")
    (fun ~environment ~workspace ->
      Solver_backend_counter_private.reset_solver_creation_count ();
      Interface_specification_loaded_private.For_testing
      .reset_provider_verification_entries ();
      let* outcome =
        Fixture.run ~environment ~workspace
          (Fixture.single_source ~module_name:"Cycle" ~source:cycle_source
             ~libraries:[ "verocaml.ghost" ])
      in
      let* () =
        require
          (Solver_backend_counter_private.solver_creation_count () = 0
          &&
          Interface_specification_loaded_private.For_testing
          .provider_verification_entries ()
          = 0)
          "broadcast cycle reached provider verification or a solver"
      in
      Ok outcome)

let functor_mli =
  {|[%%verocaml.symbolic val observed : int -> bool]
module F : functor () -> sig
  val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
end
|}

let functor_ml =
  {|[%%verocaml.symbolic val observed : int -> bool]
module F () = struct
  let lemma (value : int) : unit =
    [%verocaml.ensures fun _ ->
      (not ((observed value) [@trigger])) || value = value]; ()
  [@@verocaml.proof] [@@verocaml.broadcast]
end
|}

let functor_case =
  Suite.case ~name:"retained-functor-authority-rejects-at-emission"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 1))
      |> Expectation.require_process_fact
           (Outcome.Cleaned "_build/default/functor_provider.vri"))
    (fun ~environment ~workspace ->
      Solver_backend_counter_private.reset_solver_creation_count ();
      let dune =
        String.concat "\n"
          [
            "(library";
            " (name functor_provider)";
            " (wrapped false)";
            " (modules Functor_provider)";
            " (libraries verocaml.ghost)";
            " ";
            retained_preprocessing Standalone;
            ")";
          ]
        ^ authority_rule "functor_provider" "Functor_provider"
      in
      let files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name functor_authority_rejection)\n";
          file "dune" dune;
          file "functor_provider.mli" functor_mli;
          file "functor_provider.ml" functor_ml;
        ]
      in
      let* _, process =
        run_dune ~cleanup_paths:[ "_build/default/functor_provider.vri" ]
          ~environment ~workspace files
      in
      let process_facts = Outcome.process_facts process in
      let* () =
        require
          (List.mem (Outcome.Exit_class (Outcome.Exited 1)) process_facts
          && List.mem
               (Outcome.Cleaned "_build/default/functor_provider.vri")
               process_facts
          && Solver_backend_counter_private.solver_creation_count () = 0)
          "retained functor authority was accepted or reached a solver"
      in
      Ok (frontend_projection process))

let erasure_mli =
  {|module Source : sig
  type 'a box = 'a option
  val genuine : int -> int [@@deprecated "ordinary-user-attribute"]
  [%%verocaml.symbolic val hidden_symbolic : int -> bool]
  val hidden_proof : int -> unit
  [@@verocaml.proof] [@@verocaml.broadcast]
  val hidden_spec : int -> int [@@verocaml.spec]
  [@@@verocaml.broadcast_group (hidden_group, [hidden_proof])]
end

include module type of Source
module Alias = Source

module Nested : sig
  module Deep : sig
    type ordinary = int
    val genuine : int -> int
    [%%verocaml.symbolic val hidden_symbolic : int -> bool]
    val hidden_proof : int -> unit
    [@@verocaml.proof] [@@verocaml.broadcast]
    [@@@verocaml.broadcast_group (hidden_group, [hidden_proof])]
  end
end

val root_genuine : int -> int [@@deprecated "ordinary-root-attribute"]
|}

type surface_kind = Surface_value | Surface_type | Surface_module | Surface_modtype

type surface_entry = {
  surface_kind : surface_kind;
  surface_path : string;
  surface_attributes : Parsetree.attributes;
}

let ordinary_surface cmi =
  let interface = Cmi_format.read_cmi_lazy cmi in
  let path prefix name = String.concat "." (prefix @ [ name ]) in
  let rec module_type prefix = function
    | Types.Mty_signature signature -> signature_items prefix signature
    | Types.Mty_functor (parameter, result) ->
        let parameter_entries =
          match parameter with
          | Types.Unit -> []
          | Types.Named (_, argument) -> module_type prefix argument
        in
        parameter_entries @ module_type prefix result
    | Types.Mty_strengthen (nested, _, _) -> module_type prefix nested
    | Types.Mty_ident _ | Types.Mty_alias _ -> []
  and signature_items prefix signature =
    List.concat_map
      (function
        | Types.Sig_value (ident, description, _) ->
            [
              {
                surface_kind = Surface_value;
                surface_path = path prefix (Ident.name ident);
                surface_attributes = description.Types.val_attributes;
              };
            ]
        | Types.Sig_type (ident, declaration, _, _) ->
            [
              {
                surface_kind = Surface_type;
                surface_path = path prefix (Ident.name ident);
                surface_attributes = declaration.Types.type_attributes;
              };
            ]
        | Types.Sig_module (ident, _, declaration, _, _) ->
            let name = Ident.name ident in
            {
              surface_kind = Surface_module;
              surface_path = path prefix name;
              surface_attributes = declaration.Types.md_attributes;
            }
            :: module_type (prefix @ [ name ]) declaration.Types.md_type
        | Types.Sig_modtype (ident, declaration, _) ->
            let name = Ident.name ident in
            {
              surface_kind = Surface_modtype;
              surface_path = path prefix name;
              surface_attributes = declaration.Types.mtd_attributes;
            }
            :: Option.fold ~none:[]
                 ~some:(module_type (prefix @ [ name ]))
                 declaration.Types.mtd_type
        | Types.Sig_typext _ | Types.Sig_class _ | Types.Sig_class_type _ ->
            [])
      signature
  in
  Subst.Lazy.force_signature interface.Cmi_format.cmi_sign
  |> signature_items []

let recursive_erasure_case =
  Suite.case ~name:"recursive-ordinary-include-reexport-authority-erasure"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let dune =
        String.concat "\n"
          [
            "(library";
            " (name retained_surface)";
            " (wrapped false)";
            " (modules Surface)";
            " (modules_without_implementation Surface)";
            " (libraries verocaml.ghost)";
            " ";
            retained_preprocessing Ppxlib;
            ")";
            "(rule";
            " (targets surface.ordinary.cmi)";
            " (deps";
            "  (sandbox always)";
            "  (:eraser %{bin:verocaml-retained-interface})";
            "  (:retained .retained_surface.objs/byte/surface.cmi))";
            " (action (run %{eraser} erase-cmi %{retained} surface.ordinary.cmi)))";
            "(alias (name all) (deps surface.ordinary.cmi))";
          ]
      in
      let files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name recursive_ordinary_erasure)\n";
          file "dune" dune;
          file "surface.mli" erasure_mli;
        ]
      in
      let* root, process = run_dune ~environment ~workspace files in
      let* () = require (exited 0 process) "ordinary CMI erasure build failed" in
      let ordinary =
        files_below (Filename.concat root "_build")
        |> List.filter (fun path ->
               String.equal (Filename.basename path) "surface.ordinary.cmi")
      in
      let* ordinary =
        match ordinary with
        | [ ordinary ] -> Ok ordinary
        | [] | _ :: _ :: _ -> mismatch "ordinary erased CMI is not unique"
      in
      let entries = ordinary_surface ordinary in
      let values =
        entries
        |> List.filter_map (fun entry ->
               if entry.surface_kind = Surface_value then
                 Some entry.surface_path
               else None)
      and types =
        entries
        |> List.filter_map (fun entry ->
               if entry.surface_kind = Surface_type then Some entry.surface_path
               else None)
      and modules =
        entries
        |> List.filter_map (fun entry ->
               if entry.surface_kind = Surface_module then Some entry.surface_path
               else None)
      in
      let* () =
        require
          (List.for_all
             (fun path -> List.mem path values)
             [
               "Source.genuine";
               "genuine";
               "Nested.Deep.genuine";
               "root_genuine";
             ]
          && List.mem "Source.box" types && List.mem "box" types
          && List.mem "Source" modules && List.mem "Nested" modules
          && List.mem "Nested.Deep" modules)
          "recursive erasure removed a genuine direct, nested, or included declaration"
      in
      let* () =
        require (not (List.mem "Alias" modules))
          "ordinary module alias reexport retained an uninspectable authority route"
      in
      let hidden path =
        List.exists (contains path)
          [ "hidden_symbolic"; "hidden_proof"; "hidden_spec"; "hidden_group" ]
      in
      let* () =
        require
          (not (List.exists hidden (values @ types @ modules)))
          "recursive ordinary CMI retained proof/spec/broadcast/symbolic surface"
      in
      let attributes = List.concat_map (fun entry -> entry.surface_attributes) entries in
      let* () =
        require
          (not
             (List.exists
                (fun attribute ->
                  String.starts_with ~prefix:"verocaml."
                    attribute.Parsetree.attr_name.txt)
                attributes))
          "recursive ordinary CMI retained a VeroCaml authority attribute"
      in
      let* () =
        require
          (List.exists
             (fun entry ->
               String.equal entry.surface_path "root_genuine"
               && List.exists
                    (fun attribute ->
                      String.equal attribute.Parsetree.attr_name.txt "deprecated")
                    entry.surface_attributes)
             entries)
          "recursive erasure removed a genuine ordinary user attribute"
      in
      Ok process)

let symbolic_import_identity_case =
  Suite.case ~name:"symbolic-type-uid-uses-exact-import-crc-not-ambient-order"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let exact_files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name exact_symbolic_identity)\n";
          file "dune"
            (String.concat "\n"
               [
                 "(library";
                 " (name identity)";
                 " (wrapped false)";
                 " (modules Identity))";
                 "(library";
                 " (name symbolic_provider)";
                 " (wrapped false)";
                 " (modules Symbolic_provider)";
                 " (libraries identity verocaml.ghost)";
                 " ";
                 retained_preprocessing Ppxlib;
                 ")";
               ]);
          file "identity.ml" "type t = Exact of int\n";
          file "symbolic_provider.mli"
            "[%%verocaml.symbolic val inspect : Identity.t -> bool]\n";
          file "symbolic_provider.ml"
            "[%%verocaml.symbolic val inspect : Identity.t -> bool]\n";
        ]
      and ambient_files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name ambient_symbolic_identity)\n";
          file "dune"
            "(library\n (name identity)\n (wrapped false)\n (modules Identity))\n";
          file "identity.ml" "type t = Ambient of bool\n";
        ]
      in
      let* exact_root, exact_build =
        run_dune ~environment ~workspace:(Filename.concat workspace "exact")
          exact_files
      in
      let* ambient_root, ambient_build =
        run_dune ~environment ~workspace:(Filename.concat workspace "ambient")
          ambient_files
      in
      let* () =
        require (exited 0 exact_build && exited 0 ambient_build)
          "same-name CMI identity projects did not compile"
      in
      let* cmt = artifact exact_root "Symbolic_provider" ".cmt" in
      let* cmi = artifact exact_root "Symbolic_provider" ".cmi" in
      let* exact_identity = artifact exact_root "Identity" ".cmi" in
      let* ambient_identity = artifact ambient_root "Identity" ".cmi" in
      let exact_directory = Filename.dirname exact_identity
      and ambient_directory = Filename.dirname ambient_identity in
      let load_with directories =
        match
          Cmt_input.load_with_interface ~cmt ~cmi
            ?cmti:(optional_artifact exact_root "Symbolic_provider" ".cmti")
            ~artifact_directories:directories ()
        with
        | Ok implementation -> Ok implementation
        | Error diagnostic ->
            mismatch "exact symbolic import identity rejected: %s"
              diagnostic.Diagnostic.code
      in
      let* ambient_first = load_with [ ambient_directory; exact_directory ] in
      let* exact_first = load_with [ exact_directory; ambient_directory ] in
      let projection implementation =
        implementation.Cmt_input.interface_symbolic_declarations
        |> List.map (fun declaration ->
               ( declaration.Cmt_input.symbolic_path,
                 declaration.symbolic_typed_abi ))
      in
      let* () =
        require
          (projection ambient_first <> []
          && projection ambient_first = projection exact_first)
          "same-name different-CRC ambient CMI changed symbolic ABI identity"
      in
      Ok (Outcome.merge [ exact_build; ambient_build ]))

let exact_transitive_manifest_case =
  Suite.case
    ~name:"exact-transitive-manifest-ignores-unrelated-malformed-colocated-file"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let root = Filename.concat workspace "manifest-project" in
      let provider_mli =
        {|[%%verocaml.symbolic val probe : int -> bool]
val lemma : int -> unit [@@verocaml.proof] [@@verocaml.broadcast]
|}
      and provider_ml =
        {|[%%verocaml.symbolic val probe : int -> bool]
let lemma (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((probe value) [@trigger])) || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|}
      in
      let dune =
        String.concat "\n"
          [
            "(library";
            " (name provider_library)";
            " (wrapped false)";
            " (modules Provider)";
            " (libraries verocaml.ghost)";
            " ";
            retained_preprocessing Ppxlib;
            ")";
            "(library";
            " (name bridge_library)";
            " (wrapped false)";
            " (modules Bridge)";
            " (libraries provider_library))";
            "(library";
            " (name consumer_library)";
            " (wrapped false)";
            " (modules Consumer)";
            " (libraries bridge_library verocaml.ghost)";
            " ";
            retained_preprocessing Ppxlib;
            ")";
          ]
        ^ declared_authority_rule "provider_library" "Provider"
      in
      [
        file "dune-project"
          "(lang dune 3.17)\n(name exact_transitive_manifest)\n";
        file "dune" dune;
        file "provider.mli" provider_mli;
        file "provider.ml" provider_ml;
        file "bridge.ml" "let use = Provider.lemma\n";
        file "consumer.ml"
          {|[@@@verocaml.verify]
[@@@verocaml.activate [Provider.lemma]]
let check (value : int) : unit =
  [%verocaml.assert value = value]; ()
[@@verocaml.proof]
|};
        file "obsolete.verocaml-retained-interface"
          "not-a-retained-interface-manifest\n";
      ]
      |> List.iter (fun source ->
             write_file (Filename.concat root source.Fixture.path) source.contents);
      let root = Unix.realpath root in
      let* verification =
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
      let* () =
        require (exited 0 verification)
          "exact transitive manifest was rejected by an unrelated co-located manifest"
      in
      Ok verification)

let ambient_wrapped_cmt_case =
  Suite.case ~name:"ambient-wrapped-cmt-does-not-gain-catalog-authority"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name ambient_wrapped_catalog)\n";
          file "dune"
            (String.concat "\n"
               [
                 "(library";
                 " (name domain)";
                 " (wrapped false)";
                 " (modules Domain))";
                 "(library";
                 " (name ambient_routes)";
                 " (modules Catalog Transport)";
                 " (libraries domain verocaml.ghost)";
                 " ";
                 retained_preprocessing Ppxlib;
                 ")";
                 "(library";
                 " (name ambient_consumer)";
                 " (wrapped false)";
                 " (modules Ambient_consumer)";
                 " (libraries domain ambient_routes verocaml.ghost)";
                 " ";
                 retained_preprocessing Ppxlib;
                 ")";
               ]);
          file "domain.ml" "type 'a carrier = Carrier of 'a\n";
          file "catalog.ml"
            {|type 'a carrier_specification = 'a Domain.carrier
[@@verocaml.external_type_specification]
|};
          file "transport.ml" "let ready = true\n";
          file "ambient_consumer.ml"
            {|let check (value : int Domain.carrier) =
  [%verocaml.requires Ambient_routes.Transport.ready];
  [%verocaml.ensures fun result -> result = value];
  value
|};
        ]
      in
      let* root, build = run_dune ~environment ~workspace files in
      let* () = require (exited 0 build) "ambient wrapped catalog did not compile" in
      let* consumer = artifact root "Ambient_consumer" ".cmt" in
      let* wrapper_cmt = artifact root "Ambient_routes" ".cmt" in
      let* member_cmt = artifact root "Ambient_routes__Catalog" ".cmt" in
      let load_cmt filename =
        match Cmt_input.load filename with
        | Ok implementation -> Ok implementation
        | Error diagnostic ->
            mismatch "ambient authority fixture CMT rejected: %s"
              diagnostic.Diagnostic.code
      in
      let* consumer_identity = load_cmt consumer in
      let* wrapper_identity = load_cmt wrapper_cmt in
      let* member_identity = load_cmt member_cmt in
      let imports implementation imported crc =
        Array.exists
          (fun (edge : Cmt_input.import) ->
            String.equal edge.unit_name imported && crc edge.crc)
          implementation.Cmt_input.imports
      in
      let* () =
        require
          (imports consumer_identity wrapper_identity.unit_name Option.is_some)
          "ambient consumer did not retain an exact wrapper import"
      in
      let* () =
        require
          (not (imports consumer_identity member_identity.unit_name Option.is_some))
          "ambient consumer unexpectedly retained an exact member import"
      in
      let* () =
        require
          (imports wrapper_identity member_identity.unit_name Option.is_none)
          "ambient wrapper did not retain a CRC-less member edge"
      in
      let* verification =
        Process_adapter.run ~cwd:root
          {
            program =
              Filename.concat (Project_environment.binary_root environment)
                "verocaml";
            arguments = [ "verify"; consumer; "--threads"; "1" ];
            forwarded =
              [
                ("PATH", Project_environment.tool_path environment);
                ("OCAMLPATH", Project_environment.ocaml_path environment);
                ("HOME", root);
                ("TMPDIR", root);
                ("OCAML_COLOR", "never");
              ];
            cleanup_paths = [];
            adjacency = [];
          }
      in
      let* () =
        require (rejected verification)
          "ambient wrapped member CMT gained external-type catalog authority"
      in
      Ok (Outcome.merge [ build; verification ]))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      positive_group_case;
      missing_completion_case;
      substitution_case;
      shadowed_projection_case;
      two_hop_projection_case;
      local_shadow_case;
      constrained_subset_case;
      alias_projection_case;
      outer_module_type_case;
      ambiguous_nested_case;
      colliding_projection_case;
      nested_route_parity_case;
      nested_only_family_case;
      cycle_case;
      functor_case;
      recursive_erasure_case;
      symbolic_import_identity_case;
      exact_transitive_manifest_case;
      ambient_wrapped_cmt_case;
    ]
