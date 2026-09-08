open Outcome_test_support

let require condition message = if not condition then failwith message
let ok = function Ok value -> value | Error reason -> failwith reason
let loaded = function Ok value -> value | Error diagnostic -> failwith diagnostic.Diagnostic.message

let cases ~write_file ~run_process =
  let case ~name check = Suite.case ~name
      ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace ->
        try
          let directory = Filename.dirname Sys.executable_name in
          let ppx = Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe")
          and ghost = Unix.realpath (Filename.concat directory "../../runtime/.vero_ghost.objs/byte")
          and compiler = Filename.concat Config.bindir "ocamlc" in
          let compile stem suffix = run_process compiler
              ["-w"; "-A"; "-c"; "-bin-annot"; "-I"; workspace; "-I"; ghost;
               "-ppx"; ppx ^ " --keep-ghost"; "-o";
               stem ^ (if suffix = ".mli" then ".cmi" else ".cmo"); stem ^ suffix] in
          let load stem = loaded (Cmt_input.load_with_interface
              ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti") ~artifact_directories:[workspace] ()) in
          let emit_load stem =
            loaded (Cmt_input.emit_retained_interface_authority
              ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
              ~output:(stem ^ ".vri") ~artifact_directories:[workspace] ());
            load stem in
          let build_provider unit_name ~changed views =
            let stem = Filename.concat workspace unit_name in
            if not changed then begin
              write_file (stem ^ ".mli")
                ("module Arithmetic : sig\ntype t = int [@@verocaml.logical_sort]\n"
                 ^ "val parse : string -> t [@@verocaml.integer_literal]\nend\nval runtime_identity : int -> int\n"
                 ^ "type carrier = int\n[@@verocaml.numeric_carrier {profile = \"requested-profile\"; representation = \"immediate\"; compatibility = []}]\n"
                 ^ String.concat "" (List.map (fun (name, _, _) ->
                     "val law_" ^ name ^ " : carrier -> unit [@@verocaml.proof]\n"
                     ^ "val " ^ name ^ " : carrier -> int [@@verocaml.spec]\n"
                     ^ "[@@verocaml.numeric_role {carrier = carrier; role_schema = \"numeric-role.v1\"; role = \"unsigned-view\"; semantics = law_"
                     ^ name ^ "; visibility = \"opaque\"; reveal = false; inline = false}]\n") views));
              compile stem ".mli"
            end;
            write_file (stem ^ ".ml")
              ("[@@@verocaml.verify]\nmodule Arithmetic = struct\n"
               ^ "type t = int [@@verocaml.logical_sort]\nlet parse value = int_of_string value [@@verocaml.integer_literal]\nend\n"
               ^ "let runtime_identity (x : int) = x\ntype carrier = int\n"
               ^ String.concat "" (List.map (fun (name, width, trusted) ->
                   "let " ^ name ^ " (_x : int) = " ^ (if changed then "let zero = 0 in zero" else "0") ^ " [@@verocaml.spec]\n"
                   ^ "let law_" ^ name ^ " (x : int) =\n[%verocaml.ensures fun _ -> 0 <= " ^ name ^ " x && " ^ name
                   ^ " x < " ^ (if width = 32 then "65536 * 65536" else "65536 * 65536 * 65536 * 65536")
                   ^ "]; () [@@verocaml." ^ (if trusted then "axiom" else "proof") ^ "]\n") views));
            compile stem ".ml";
            emit_load stem in
          let admit provider =
            let policy = match Solver_policy_private.create_default ~timeout_ms:5000 with
              | Ok policy -> policy | Error _ -> failwith "invalid solver policy" in
            let report = match Verification_driver_private.run_with_policy ~solver_policy:policy
                ~allow_imported_opens:true ~allow_public_parametric_signatures:true provider with
              | Ok report -> report
              | Error (Verification_driver_private.Frontend_error diagnostic) -> failwith diagnostic.Diagnostic.message
              | Error (Validation_error error) -> failwith (Sst_validation.error_to_string error)
              | Error (Provider_surface_error message | Internal_error message) -> failwith message
              | Error _ -> failwith "numeric registry provider did not verify" in
            let completion = match Verification_driver_private.verified_completion report with
              | Some value -> value | None -> failwith "no completed numeric registry provider" in
            let validated = Verification_driver_private.validated report in
            let declarations = ok (Numeric_semantics_binding_private.complete_local ~completion ~implementation:provider ~validated) in
            let logical_sort = match provider.Cmt_input.interface_logical_sorts with [sort] -> sort | _ -> failwith "missing base Int" in
            let targets = ok (Build_target_profile_private.authenticate_instances (Build_target_profile_private.capability ())) in
            List.concat_map (fun declaration -> List.filter_map (fun target ->
                match Numeric_ghost_law_private.admit_unsigned_range ~completion ~implementation:provider ~validated
                    ~base_provider:provider ~logical_sort ~target declaration with
                | Ok law -> Some law
                | Error Statement_mismatch -> None
                | Error _ -> failwith "completed registry provider law was not admitted") targets) declarations in
          let wrapper ?(expose = true) unit_name provider_unit =
            let stem = Filename.concat workspace unit_name in
            let body = "module Original = " ^ provider_unit ^ "\n" in
            write_file (stem ^ ".mli") (if expose then body else "val keep : int -> int\n");
            write_file (stem ^ ".ml") (body ^ (if expose then "" else "let keep (x : int) = Original.runtime_identity x\n"));
            List.iter (compile stem) [".mli"; ".ml"];
            emit_load stem in
          let consumer unit_name imports =
            let stem = Filename.concat workspace unit_name in
            write_file (stem ^ ".ml")
              ("[@@@verocaml.verify]\n" ^ String.concat "" (List.mapi (fun index name ->
                 "module Imported" ^ string_of_int index ^ " = " ^ name ^ "\n") imports)
               ^ "let identity (x : int) = x\n");
            compile stem ".ml";
            loaded (Cmt_input.load (stem ^ ".cmt")) in
          let import consumer dependencies laws =
            let before = (Z3_bridge.counters ()).contexts_created in
            let result = Numeric_ghost_registry_private.import_laws ~consumer ~dependencies laws in
            require ((Z3_bridge.counters ()).contexts_created = before) "numeric registry import started a solver";
            result in
          let reload unit_name = load (Filename.concat workspace unit_name) in
          check build_provider admit wrapper consumer import reload;
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message -> Error (Failure.make Failure.Runner_internal message)) in
  let imported = function Ok registry -> registry | Error _ -> failwith "valid numeric law registry was rejected" in
  let single_view = ["project", 32, false] in
  [case ~name:"numeric-registry-repeated-laws-preserve-origin" (fun build admit _ consumer import _ ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let user = consumer "use_digits" ["Digits"] in
       let registry = imported (import user [provider] (laws @ laws)) in
       require (Numeric_ghost_registry_private.laws registry = laws) "duplicate import minted or repeated a numeric law");
   case ~name:"numeric-registry-independent-provider-order-is-canonical" (fun build admit _ consumer import _ ->
       let first = build "digits" ~changed:false single_view in
       let first_laws = admit first in
       let second = build "other_words" ~changed:false single_view in
       let second_laws = admit second in
       let user = consumer "use_both" ["Digits"; "Other_words"] in
       let left = imported (import user [first; second] (first_laws @ second_laws))
       and right = imported (import user [second; first] (second_laws @ first_laws)) in
       require (Numeric_ghost_registry_private.equal left right
         && List.length (Numeric_ghost_registry_private.laws left) = 4)
         "disjoint carriers depended on dependency discovery order");
   case ~name:"numeric-registry-diamond-reexports-keep-original-law" (fun build admit wrapper consumer import _ ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let left = wrapper "left" "Digits" and right = wrapper "right" "Digits" in
       let user = consumer "use_diamond" ["Left"; "Right"] in
       let first = imported (import user [left; provider; right] (laws @ laws))
       and second = imported (import user [right; left; provider] laws) in
       require (Numeric_ghost_registry_private.equal first second) "diamond order changed numeric scope identity";
       require (List.for_all2 (==) (Numeric_ghost_registry_private.laws first) laws)
         "a reexport minted replacement numeric authority");
   case ~name:"numeric-registry-unrelated-candidate-grants-no-authority" (fun build admit _ consumer import _ ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let user = consumer "unrelated" [] in
       require (match import user [provider] laws with Error (Unreachable_law _) -> true | _ -> false)
         "an ambient candidate made its numeric law visible");
   case ~name:"numeric-registry-follows-genuinely-indirect-provider-edge" (fun build admit wrapper consumer import _ ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let bridge = wrapper ~expose:false "opaque_bridge" "Digits" in
       let user = consumer "indirect_user" ["Opaque_bridge"] in
       require (not (Cmt_input.exact_imports user provider)
         && Cmt_input.exact_imports user bridge && Cmt_input.exact_imports bridge provider)
         "fixture did not establish an exclusively indirect original-provider edge";
       let registry = imported (import user [provider; bridge] laws) in
       require (List.for_all2 (==) (Numeric_ghost_registry_private.laws registry) laws)
         "a private transitive dependency lost its original numeric authority");
   case ~name:"numeric-registry-absent-original-provider-rejects" (fun build admit wrapper consumer import _ ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let bridge = wrapper "bridge" "Digits" in
       let user = consumer "use_bridge" ["Bridge"] in
       require (match import user [bridge] laws with Error (Unreachable_law _) -> true | _ -> false)
         "a missing original provider was replaced by a reexport");
   case ~name:"numeric-registry-reuses-invalid-inventory-rejection" (fun build admit _ consumer import _ ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let user = consumer "use_digits" ["Digits"] in
       require (match import user [provider; provider] laws with Error (Invalid_dependency_graph _) -> true | _ -> false)
         "numeric import bypassed the dependency graph's duplicate candidate rejection");
   case ~name:"numeric-registry-reloaded-exact-artifact-is-equivalent" (fun build admit _ consumer import reload ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let user = consumer "use_digits" ["Digits"] in
       let first = imported (import user [provider] laws)
       and second = imported (import user [reload "digits"] laws) in
       require (Numeric_ghost_registry_private.equal first second) "reloading an exact artifact changed import authority");
   case ~name:"numeric-registry-same-cmi-new-body-cannot-inherit-law" (fun build admit _ consumer import _ ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let user = consumer "use_digits" ["Digits"] in
       let changed = build "digits" ~changed:true single_view in
       require (provider.Cmt_input.interface_digest = changed.interface_digest) "fixture changed its interface instead of its body";
       require (match import user [changed] laws with Error (Artifact_mismatch _) -> true | _ -> false)
         "a changed implementation inherited the old numeric proof");
   case ~name:"numeric-registry-conflicting-views-are-order-independent" (fun build admit _ consumer import _ ->
       let provider = build "digits" ~changed:false ["first", 32, false; "second", 32, false] in
       let laws = admit provider in
       let user = consumer "use_digits" ["Digits"] in
       match import user [provider] laws, import user [provider] (List.rev laws) with
       | Error (Conflicting_laws first), Error (Conflicting_laws second) ->
           require (first = second && List.length first = 2) "conflict identities depend on discovery order"
       | _ -> failwith "overlapping unsigned views silently selected one authority");
   case ~name:"numeric-registry-proof-axiom-overlap-cannot-change-trust" (fun build admit _ consumer import _ ->
       let provider = build "digits" ~changed:false ["checked", 32, false; "assumed", 32, true] in
       let laws = admit provider in
       let user = consumer "use_digits" ["Digits"] in
       require (match import user [provider] laws with Error (Conflicting_laws _) -> true | _ -> false)
         "overlapping checked and trusted laws silently changed the TCB");
   case ~name:"numeric-registry-distinct-concrete-target-laws-coexist" (fun build admit _ consumer import _ ->
       let provider = build "digits" ~changed:false ["narrow", 32, false; "wide", 64, false] in
       let laws = admit provider in
       let user = consumer "use_digits" ["Digits"] in
       let registry = imported (import user [provider] laws) in
       let laws = Numeric_ghost_registry_private.laws registry in
       let widths = List.map (fun law -> law.Numeric_ghost_law_private.target.target_claim.width)
           laws |> List.sort Int.compare
       and semantic_widths = List.map (fun law -> law.Numeric_ghost_law_private.semantic_width.width)
           laws |> List.sort Int.compare in
       require (widths = [32; 32; 64; 64]
         && semantic_widths = [32; 32; 64; 64])
         "semantic and physical target dimensions were collapsed");
   case ~name:"numeric-registry-consumers-have-distinct-scope-identities" (fun build admit _ consumer import _ ->
       let provider = build "digits" ~changed:false single_view in
       let laws = admit provider in
       let first = imported (import (consumer "use_first" ["Digits"]) [provider] laws)
       and second = imported (import (consumer "use_second" ["Digits"]) [provider] laws) in
       require (not (Numeric_ghost_registry_private.equal first second)) "a registry was detached from its consumer artifact";
       require (Numeric_ghost_registry_private.laws first = Numeric_ghost_registry_private.laws second)
         "consumer scope changed original law identity")]
