open Outcome_test_support

let require condition message = if not condition then failwith message
let ok = function Ok value -> value | Error reason -> failwith reason
let loaded = function Ok value -> value | Error diagnostic -> failwith diagnostic.Diagnostic.message

let cases ~write_file ~run_process =
  let case ~name ?(stem_name = "small_words") ?(nested = false) ?(domain = `Carrier)
      ?(base = `Local) ?(kind = `Proof) ?(role = "unsigned-view")
      ?(width = 32) ?(semantic_width = 32) expected =
    Suite.case ~name
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
          let emit_load stem =
            loaded (Cmt_input.emit_retained_interface_authority
              ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
              ~output:(stem ^ ".vri") ~artifact_directories:[workspace] ());
            loaded (Cmt_input.load_with_interface
              ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
              ~artifact_directories:[workspace] ()) in
          let base_signature =
            "type t = int [@@verocaml.logical_sort]\nval parse : string -> t [@@verocaml.integer_literal]\n"
          and base_implementation =
            "type t = int [@@verocaml.logical_sort]\nlet parse text = int_of_string text [@@verocaml.integer_literal]\n" in
          let external_base = if base = `Local then None else
              let stem = Filename.concat workspace "external_numbers" in
              write_file (stem ^ ".mli") base_signature;
              write_file (stem ^ ".ml") base_implementation;
              List.iter (compile stem) [".mli"; ".ml"];
              Some (emit_load stem) in
          let argument, implementation_argument = match domain with
            | `Carrier -> "carrier", "int"
            | `Bool -> "bool", "bool"
            | `Other -> "other", "int" in
          let numeric_signature =
            "type carrier = int\n"
            ^ "[@@verocaml.numeric_carrier {profile = \"requested-profile\"; representation = \"immediate\"; compatibility = []}]\n"
            ^ "type other = int\nval law : " ^ argument ^ " -> "
            ^ (if kind = `Spec then "bool [@@verocaml.spec]\n" else "unit [@@verocaml.proof]\n")
            ^ "val project : " ^ argument ^ " -> int [@@verocaml.spec]\n"
            ^ "[@@verocaml.numeric_role {carrier = carrier; role_schema = \"numeric-role.v1\"; role = \"" ^ role
            ^ "\"; semantics = law; visibility = \"opaque\"; reveal = false; inline = false}]\n" in
          let modulus = Z.(shift_left one semantic_width |> to_string) in
          let postcondition =
            "[%verocaml.ensures fun _ -> 0 <= project x && project x < "
            ^ modulus ^ "];\n"
          in
          let numeric_implementation =
            "type carrier = int\ntype other = int\nlet project (_x : " ^ implementation_argument ^ ") = "
            ^ (if kind = `Axiom then "-1" else "0") ^ " [@@verocaml.spec]\n"
            ^ (if kind = `Transitive then "let foundation (x : int) =\n" ^ postcondition ^ "() [@@verocaml.axiom]\n" else "")
            ^ "let law (x : " ^ implementation_argument ^ ") =\n"
            ^ (if kind = `Spec then "true [@@verocaml.spec]\n" else
                postcondition ^ (if kind = `Transitive then "foundation x" else "()")
                ^ " [@@verocaml." ^ (if kind = `Axiom then "axiom" else "proof") ^ "]\n") in
          let wrap signature contents = if not nested then contents else
            "module View " ^ (if signature then ": sig\n" else "= struct\n") ^ contents ^ "end\n" in
          let stem = Filename.concat workspace stem_name in
          write_file (stem ^ ".mli")
            ((if base = `Imported then "module Arithmetic = External_numbers\n"
              else "module Arithmetic : sig\n" ^ base_signature ^ "end\n") ^ wrap true numeric_signature);
          write_file (stem ^ ".ml")
            ("[@@@verocaml.verify]\n"
             ^ (if base = `Imported then "module Arithmetic = External_numbers\n"
                else "module Arithmetic = struct\n" ^ base_implementation ^ "end\n") ^ wrap false numeric_implementation);
          List.iter (compile stem) [".mli"; ".ml"];
          let provider = emit_load stem in
          let base_provider = Option.value ~default:provider external_base in
          let logical_sort = match base_provider.interface_logical_sorts with
            | [sort] -> sort | _ -> failwith "fixture has no unique base mathematical Int receipt" in
          let policy = match Solver_policy_private.create_default ~timeout_ms:5000 with
            | Ok policy -> policy | Error _ -> failwith "invalid fixture solver policy" in
          let target = ok (Build_target_profile_private.authenticate_instances (Build_target_profile_private.capability ()))
            |> List.find (fun target -> target.Build_target_profile_private.target_claim.width = width) in
          let check threads =
            let report = if base = `Imported then
                match Interface_specification_loaded_private.verify ~threads ~solver_policy:policy
                  ~external_specifications:None ~external_targets:[] ~consumer:provider ~dependencies:[base_provider] with
                | Ok verified -> Interface_specification_loaded_private.driver verified
                | Error error -> failwith (Interface_specification_loaded_private.error_message error)
              else match Verification_driver_private.run_with_policy_and_threads ~threads ~solver_policy:policy
                  ~allow_imported_opens:true ~allow_public_parametric_signatures:true provider with
              | Ok report -> report
              | Error (Verification_driver_private.Frontend_error diagnostic) -> failwith diagnostic.Diagnostic.message
              | Error (Validation_error error) -> failwith (Sst_validation.error_to_string error)
              | Error (Provider_surface_error message | Internal_error message) -> failwith message
              | Error _ -> failwith "ghost law fixture failed before admission" in
            let completion = match Verification_driver_private.verified_completion report with
              | Some completion -> completion | None -> failwith "ordinary ghost law fixture did not verify" in
            let validated = Verification_driver_private.validated report in
            let declarations = ok (Numeric_semantics_binding_private.complete_local ~completion ~implementation:provider ~validated) in
            let declaration = match declarations with [declaration] -> declaration | _ -> failwith "no unique numeric role" in
            let before = (Z3_bridge.counters ()).contexts_created in
            let admitted = Numeric_ghost_law_private.admit_unsigned_range ~completion ~implementation:provider
                ~validated ~base_provider ~logical_sort ~target declaration in
            require ((Z3_bridge.counters ()).contexts_created = before) "ghost semantic admission started a solver";
            match expected, admitted with
            | Some expected, Error actual ->
                require (actual = expected) "ghost law rejection occurred at the wrong boundary"; None
            | Some _, Ok _ -> failwith "unsupported ghost law acquired semantic authority"
            | None, Error _ -> failwith ("valid ghost law was not admitted\n" ^ Verification_driver_private.semantic_sst report)
            | None, Ok law ->
                require (law.authority = (if kind = `Axiom then Numeric_ghost_law_private.Explicit_axiom else Checked_proof))
                  "ghost semantic admission lost proof versus axiom provenance";
                require
                  (law.semantic_width.width = semantic_width
                  && law.semantic_width.target_full_key = target.full_key)
                  "ghost semantic receipt lost its evidence-derived width or exact target";
                require (List.length (List.filter (fun dependency -> dependency.Numeric_ghost_law_private.trusted) law.dependencies)
                  = (if kind = `Axiom || kind = `Transitive then 1 else 0))
                  "ghost semantic admission lost transitive trust";
                require (law.base_int.base_int_full_key = Logical_sort_private.canonical_material logical_sort
                  && law.carrier.carrier_claim.carrier_uid = declaration.role.numeric_role_carrier_uid
                  && law.role.callable_uid = declaration.role.numeric_role_callable_uid)
                  "ghost semantic receipt crossed base/carrier/callable identities";
                let again = match Numeric_ghost_law_private.admit_unsigned_range ~completion ~implementation:provider
                    ~validated ~base_provider ~logical_sort ~target declaration with
                  | Ok law -> law | Error _ -> failwith "repeated ghost admission was unavailable" in
                require (Numeric_ghost_law_private.equal law again && law.full_key = again.full_key)
                  "repeated admission changed full semantic identity";
                let independently_loaded = loaded (Cmt_input.load_with_interface
                  ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti") ~artifact_directories:[workspace] ()) in
                require (match Numeric_ghost_law_private.admit_unsigned_range ~completion ~implementation:independently_loaded
                    ~validated ~base_provider ~logical_sort ~target declaration with
                  | Error Completion_mismatch -> true | _ -> false)
                  "a completion crossed into another loaded provider";
                Some law.full_key in
          require (check 1 = check 2) "ghost semantic identities differ between serial and threaded completion";
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message -> Error (Failure.make Failure.Runner_internal message)) in
  [case ~name:"ghost-unsigned-law-composes-local-authorities" None;
   case ~name:"ghost-unsigned-law-unrelated-nested-provider" ~stem_name:"other_arithmetic" ~nested:true None;
   case ~name:"ghost-unsigned-law-explicit-false-axiom-keeps-trust" ~kind:`Axiom None;
   case ~name:"ghost-unsigned-law-checked-proof-keeps-transitive-trust" ~kind:`Transitive None;
   case ~name:"ghost-unsigned-law-authenticated-imported-base" ~base:`Imported None;
   case ~name:"ghost-unsigned-law-rejects-unimported-base" ~base:`Unimported (Some Numeric_ghost_law_private.Base_not_imported);
   case ~name:"ghost-unsigned-law-rejects-wrong-carrier-domain" ~domain:`Bool (Some Carrier_domain_unavailable);
   case ~name:"ghost-unsigned-law-does-not-infer-nominal-domain-from-layout" ~domain:`Other (Some Carrier_domain_unavailable);
   case ~name:"ghost-unsigned-law-does-not-promote-other-role" ~role:"operation" (Some Unsupported_role);
   case ~name:"ghost-unsigned-law-ordinary-spec-is-not-proof" ~kind:`Spec (Some Statement_mismatch);
   case ~name:"ghost-unsigned-law-semantic32-on-machine64" ~width:64 None;
   case ~name:"ghost-unsigned-law-semantic16-on-machine32" ~width:32
     ~semantic_width:16 None;
   case ~name:"ghost-unsigned-law-semantic16-on-machine64" ~width:64
     ~semantic_width:16 None]
