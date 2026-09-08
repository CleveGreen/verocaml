open Outcome_test_support

let require condition message = if not condition then failwith message
let ok = function Ok value -> value | Error reason -> failwith reason
let loaded = function Ok value -> value | Error diagnostic -> failwith diagnostic.Diagnostic.message

let cases ~write_file ~run_process =
  let case ~name ?(width = 32) ?semantic_width ?relation_width
      ?(unsigned_math = false) ?(signed_math = false)
      ?(unsigned_axiom = false) ?(signed_axiom = false) ?(precondition = false)
      ?(prerequisite = `Original) formula expected =
    Suite.case ~name ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace ->
        try
          let directory = Filename.dirname Sys.executable_name in
          let ppx = Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe")
          and ghost = Unix.realpath (Filename.concat directory "../../runtime/.vero_ghost.objs/byte")
          and compiler = Filename.concat Config.bindir "ocamlc" in
          let stem = Filename.concat workspace "signed_provider" in
          let compile stem suffix = run_process compiler
              ["-w"; "-A"; "-c"; "-bin-annot"; "-I"; workspace; "-I"; ghost;
               "-ppx"; ppx ^ " --keep-ghost"; "-o";
               stem ^ (if suffix = ".mli" then ".cmi" else ".cmo"); stem ^ suffix] in
          let carrier_marker = "[@@verocaml.numeric_carrier {profile = \"request\"; representation = \"immediate\"; compatibility = []}]\n" in
          let base_signature name = "module " ^ name ^ " : sig\ntype t = int [@@verocaml.logical_sort]\nval parse : string -> t [@@verocaml.integer_literal]\nend\n" in
          let base_implementation name = "module " ^ name ^ " = struct\ntype t = int [@@verocaml.logical_sort]\nlet parse text = int_of_string text [@@verocaml.integer_literal]\nend\n" in
          let signature_role carrier view law role =
            "val " ^ law ^ " : " ^ carrier ^ " -> unit [@@verocaml.proof]\nval " ^ view ^ " : " ^ carrier ^ " -> int [@@verocaml.spec]\n"
            ^ "[@@verocaml.numeric_role {carrier = " ^ carrier ^ "; role_schema = \"numeric-role.v1\"; role = \"" ^ role
            ^ "\"; semantics = " ^ law ^ "; visibility = \"opaque\"; reveal = false; inline = false}]\n" in
          write_file (stem ^ ".mli")
            (base_signature "Arithmetic" ^ base_signature "Alternative"
             ^ "type carrier = int\n" ^ carrier_marker ^ "type other = int\n" ^ carrier_marker
             ^ signature_role "carrier" "u" "u_law" "unsigned-view"
             ^ signature_role "other" "other_u" "other_law" "unsigned-view"
             ^ signature_role "carrier" "competing_u" "competing_law" "unsigned-view"
             ^ signature_role "carrier" "s" "s_law" "signed-view");
          compile stem ".mli";
          let semantic_width = Option.value ~default:width semantic_width in
          let relation_width = Option.value ~default:semantic_width relation_width in
          let uwidth = semantic_width in
          let power_of_two width =
            if width = 64 then "(4294967296 * 4294967296)"
            else if width = 63 then "(4294967296 * 2147483648)"
            else Z.(shift_left one width |> to_string)
          in
          let modulus width = power_of_two width in
          let half width = power_of_two (width - 1) in
          let source ~changed =
            let ubody = if unsigned_math then "0 + 0" else "0" in
            let unsigned view law = "let " ^ view ^ " (_x : int) = " ^ ubody ^ " [@@verocaml.spec]\n"
              ^ "let " ^ law ^ " (x : int) = [%verocaml.ensures fun _ -> 0 <= " ^ view ^ " x && " ^ view ^ " x < " ^ modulus uwidth
              ^ "]; () [@@verocaml." ^ (if unsigned_axiom && view = "u" then "axiom" else "proof") ^ "]\n" in
            "[@@@verocaml.verify]\n" ^ base_implementation "Arithmetic" ^ base_implementation "Alternative"
            ^ "type carrier = int\ntype other = int\n"
            ^ unsigned "u" "u_law" ^ unsigned "other_u" "other_law" ^ unsigned "competing_u" "competing_law"
            ^ "let foreign_u (_x : int) = 0 [@@verocaml.spec]\nlet foreign_s (_x : int) = 0 [@@verocaml.spec]\n"
            ^ "let s (_x : int) = " ^ (if changed then "let zero = 0 in " else "")
            ^ (if signed_math then "0 + 0" else if changed then "zero" else "0") ^ " [@@verocaml.spec]\n"
            ^ "let s_law (x : int) = " ^ (if precondition then "[%verocaml.requires x >= 0];\n" else "")
            ^ "[%verocaml.ensures fun _ -> " ^ formula (half relation_width) (modulus relation_width) ^ "]; () [@@verocaml."
            ^ (if signed_axiom || expected = Some Numeric_ghost_law_private.Statement_mismatch then "axiom" else "proof") ^ "]\n" in
          let build changed =
            write_file (stem ^ ".ml") (source ~changed);
            compile stem ".ml";
            loaded (Cmt_input.emit_retained_interface_authority ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
              ~output:(stem ^ ".vri") ~artifact_directories:[workspace] ());
            loaded (Cmt_input.load_with_interface ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti") ~artifact_directories:[workspace] ()) in
          let policy = match Solver_policy_private.create_default ~timeout_ms:5000 with Ok policy -> policy | Error _ -> failwith "invalid solver policy" in
          let targets = ok (Build_target_profile_private.authenticate_instances (Build_target_profile_private.capability ())) in
          let target width = List.find (fun target -> target.Build_target_profile_private.target_claim.width = width) targets in
          let context threads provider =
            let report = match Verification_driver_private.run_with_policy_and_threads ~threads ~solver_policy:policy
                ~allow_imported_opens:true ~allow_public_parametric_signatures:true provider with
              | Ok report -> report
              | Error (Verification_driver_private.Frontend_error diagnostic) -> failwith diagnostic.Diagnostic.message
              | Error (Validation_error error) -> failwith (Sst_validation.error_to_string error)
              | Error (Provider_surface_error message | Internal_error message) -> failwith message
              | Error _ -> failwith "signed provider failed before admission" in
            let completion = match Verification_driver_private.verified_completion report with Some completion -> completion | None -> failwith "signed provider did not verify" in
            let validated = Verification_driver_private.validated report in
            let declarations = ok (Numeric_semantics_binding_private.complete_local ~completion ~implementation:provider ~validated) in
            let declaration name = List.find (fun declaration -> declaration.Numeric_semantics_binding_private.definition.function_id.function_name = name) declarations in
            let sort name = List.find (fun sort -> sort.Logical_sort_private.type_path = "Signed_provider." ^ name ^ ".t") provider.Cmt_input.interface_logical_sorts in
            let unsigned_target =
              if prerequisite = `Other_target then
                target (if width = 32 then 64 else 32)
              else target width
            in
            let unsigned name = match Numeric_ghost_law_private.admit_unsigned_range ~completion ~implementation:provider ~validated
                ~base_provider:provider ~logical_sort:(sort "Arithmetic") ~target:unsigned_target (declaration name) with
              | Ok law -> law | Error _ -> failwith "unsigned prerequisite was not admitted" in
            report, completion, validated, declaration, sort, unsigned in
          let original = build false in
          let provider, old_law = if prerequisite <> `Other_artifact then original, None else
              let _, _, _, _, _, unsigned = context 1 original in
              let old_law = unsigned "u_law" in
              build true, Some old_law in
          let check threads =
            let report, completion, validated, declaration, sort, unsigned = context threads provider in
            let prior = match old_law with Some law -> law | None -> unsigned (if prerequisite = `Other_carrier then "other_law" else "u_law") in
            let before = (Z3_bridge.counters ()).contexts_created in
            let admitted = Numeric_ghost_law_private.admit_signed_view ~completion ~implementation:provider ~validated
                ~base_provider:provider ~logical_sort:(sort (if prerequisite = `Other_base then "Alternative" else "Arithmetic"))
                ~target:(target width) ~unsigned:prior (declaration "s_law") in
            require ((Z3_bridge.counters ()).contexts_created = before) "signed admission started a solver";
            match expected, admitted with
            | Some expected, Error actual -> require (expected = actual) "signed law rejected at wrong boundary"; None
            | Some _, Ok _ -> failwith "inexact signed relation or prerequisite acquired semantic authority"
            | None, Error _ -> failwith ("valid signed relation was not admitted\n" ^ Verification_driver_private.semantic_sst report)
            | None, Ok law ->
                require (law.meaning = Signed_view && law.authority = (if signed_axiom then Explicit_axiom else Checked_proof)) "signed law meaning/trust changed";
                require (match law.prerequisites with [actual] -> actual == prior | _ -> false) "signed law lost original unsigned prerequisite";
                require (law.result_interpretation = (if signed_math then Mathematical_result else Lifted_runtime_result)
                  && prior.result_interpretation = (if unsigned_math then Mathematical_result else Lifted_runtime_result)) "signed relation erased an explicit result lift";
                require (List.length (List.filter (fun dependency -> dependency.Numeric_ghost_law_private.trusted) law.dependencies)
                  = (if unsigned_axiom then 1 else 0) + (if signed_axiom then 1 else 0)) "signed law lost prerequisite or own axiom trust";
                require (match Numeric_ghost_law_private.admit_signed_view ~completion ~implementation:provider ~validated
                  ~base_provider:provider ~logical_sort:(sort "Arithmetic") ~target:(target width) ~unsigned:law (declaration "s_law") with
                  | Error Prerequisite_mismatch -> true | _ -> false) "signed law admitted a circular prerequisite";
                let consumer_stem = Filename.concat workspace "signed_consumer" in
                write_file (consumer_stem ^ ".ml") "[@@@verocaml.verify]\nmodule Numbers = Signed_provider\nlet identity (x : int) = x\n";
                compile consumer_stem ".ml";
                let consumer = loaded (Cmt_input.load (consumer_stem ^ ".cmt")) in
                let registry = match Numeric_ghost_registry_private.import_laws ~consumer ~dependencies:[provider] [law] with
                  | Ok registry -> registry | Error error -> failwith (Numeric_ghost_registry_private.error_message error) in
                let imported = Numeric_ghost_registry_private.laws registry in
                require (List.length imported = 2 && List.exists ((==) law) imported && List.exists ((==) prior) imported)
                  "signed import dropped or replaced its unsigned prerequisite";
                let competing = unsigned "competing_law" in
                (match Numeric_ghost_registry_private.import_laws ~consumer ~dependencies:[provider] [law; competing],
                       Numeric_ghost_registry_private.import_laws ~consumer ~dependencies:[provider] [competing; law] with
                | Error (Conflicting_laws first), Error (Conflicting_laws second) ->
                    require (first = second && List.length first = 1
                      && List.for_all (fun conflict -> conflict.Numeric_ghost_registry_private.meaning = Unsigned_range) first)
                      "hidden prerequisite conflict was not canonical"
                | _ -> failwith "a signed law hid its conflicting unsigned prerequisite");
                require ((Z3_bridge.counters ()).contexts_created = before) "prerequisite import/readmission started a solver";
                Some law.full_key in
          require (check 1 = check 2) "signed law full keys depend on worker count";
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message -> Error (Failure.make Failure.Runner_internal message)) in
  let conditional half modulus = "(if u x < " ^ half ^ " then u x else u x - (" ^ modulus ^ "))" in
  let relation half modulus = "s x = " ^ conditional half modulus in
  let mismatch = Some Numeric_ghost_law_private.Statement_mismatch and wrong_prior = Some Numeric_ghost_law_private.Prerequisite_mismatch in
  [case ~name:"signed-view-exact-conditional-relation" relation None;
   case ~name:"signed-view-reversed-equality" (fun half modulus -> conditional half modulus ^ " = s x") None;
   case ~name:"signed-view-reversed-threshold-comparison" (fun half modulus -> "s x = (if " ^ half ^ " > u x then u x else u x - (" ^ modulus ^ "))") None;
   case ~name:"signed-view-64-bit-relation" ~width:64 ~signed_math:true relation None;
   case ~name:"signed-view-semantic16-on-machine32" ~width:32
     ~semantic_width:16 relation None;
   case ~name:"signed-view-semantic16-on-machine64" ~width:64
     ~semantic_width:16 relation None;
   case ~name:"signed-view-rejects-semantic8-for-unsigned16-same-target"
     ~width:32 ~semantic_width:16 ~relation_width:8 relation mismatch;
   case ~name:"signed-view-direct-mathematical-results" ~unsigned_math:true ~signed_math:true relation None;
   case ~name:"signed-view-mixed-result-lifts" ~unsigned_math:true relation None;
   case ~name:"signed-view-keeps-trusted-unsigned-prerequisite" ~unsigned_axiom:true relation None;
   case ~name:"signed-view-keeps-own-and-prerequisite-trust" ~unsigned_axiom:true ~signed_axiom:true relation None;
   case ~name:"signed-view-rejects-wrong-threshold" (fun _ modulus -> "s x = (if u x < 1 then u x else u x - (" ^ modulus ^ "))") mismatch;
   case ~name:"signed-view-rejects-inclusive-threshold" (fun half modulus -> "s x = (if u x <= " ^ half ^ " then u x else u x - (" ^ modulus ^ "))") mismatch;
   case ~name:"signed-view-rejects-wrong-modulus" (fun half _ -> "s x = (if u x < " ^ half ^ " then u x else u x - 1)") mismatch;
   case ~name:"signed-view-rejects-reversed-subtraction" (fun half modulus -> "s x = (if u x < " ^ half ^ " then u x else (" ^ modulus ^ ") - u x)") mismatch;
   case ~name:"signed-view-rejects-swapped-branches" (fun half modulus -> "s x = (if u x < " ^ half ^ " then u x - (" ^ modulus ^ ") else u x)") mismatch;
   case ~name:"signed-view-rejects-different-unsigned-callable" (fun half modulus -> "s x = (if u x < " ^ half ^ " then foreign_u x else u x - (" ^ modulus ^ "))") mismatch;
   case ~name:"signed-view-rejects-different-argument" (fun half modulus -> "s x = (if u x < " ^ half ^ " then u x else u 0 - (" ^ modulus ^ "))") mismatch;
   case ~name:"signed-view-rejects-different-signed-callable" (fun half modulus -> "foreign_s x = " ^ conditional half modulus) mismatch;
   case ~name:"signed-view-range-only-is-not-twos-complement" (fun half _ -> "0 <= s x && s x < " ^ half) mismatch;
   case ~name:"signed-view-does-not-drop-preconditions" ~precondition:true relation mismatch;
   case ~name:"signed-view-rejects-other-carrier-prerequisite" ~prerequisite:`Other_carrier relation wrong_prior;
   case ~name:"signed-view-rejects-other-base-prerequisite" ~prerequisite:`Other_base relation wrong_prior;
   case ~name:"signed-view-rejects-other-target-prerequisite" ~prerequisite:`Other_target relation wrong_prior;
   case ~name:"signed-view-rejects-stale-artifact-prerequisite" ~prerequisite:`Other_artifact relation wrong_prior]
