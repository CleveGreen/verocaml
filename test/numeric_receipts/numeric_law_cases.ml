open Outcome_test_support

let require condition message = if not condition then failwith message
let ok label = function Ok value -> value | Error _ -> failwith label

let cases ~write_file ~run_process =
  let extended_case ~remote ~helpers ~law_body ~extra_dependencies ~expected_trust ~expected_broadcast ~unavailable
      ~name ~view_name ~logical ~axiom ~requires ~ensures ~width =
    Suite.case ~name
      ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace ->
        try
          let directory = Filename.dirname Sys.executable_name in
          let ppx = Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe")
          and ghost = Unix.realpath (Filename.concat directory "../../runtime/.vero_ghost.objs/byte")
          and compiler = Filename.concat Config.bindir "ocamlc" in
          let compile stem suffix =
            run_process compiler
              [ "-w"; "-A"; "-c"; "-bin-annot"; "-I"; workspace; "-I"; ghost;
                "-ppx"; ppx ^ " --keep-ghost"; "-o";
                stem ^ (if suffix = ".mli" then ".cmi" else ".cmo"); stem ^ suffix ]
          in
          let emit stem =
            match Cmt_input.emit_retained_interface_authority ~cmt:(stem ^ ".cmt")
              ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti") ~output:(stem ^ ".vri")
              ~artifact_directories:[workspace] () with
            | Ok () -> () | Error diagnostic -> failwith diagnostic.Diagnostic.message
          in
          let imported =
            if remote = `None then [] else
              let remote_stem = Filename.concat workspace "remote" in
              write_file (remote_stem ^ ".mli")
                "val bridge : int -> unit [@@verocaml.proof]\nval projection : int -> int [@@verocaml.spec]\n";
              write_file (remote_stem ^ ".ml")
                "let bridge (x : int) = [%verocaml.ensures fun _ -> x = x]; () [@@verocaml.proof]\nlet projection (_x : int) = 0 [@@verocaml.spec]\n";
              List.iter (compile remote_stem) [".mli"; ".ml"];
              emit remote_stem;
              [ok "remote artifact load failed" (Cmt_input.load_with_interface
                ~cmt:(remote_stem ^ ".cmt") ~cmi:(remote_stem ^ ".cmi") ~cmti:(remote_stem ^ ".cmti")
                ~artifact_directories:[workspace] ())]
          in
          let stem = Filename.concat workspace "law_provider" in
          let signature =
            {|type carrier = int
[@@verocaml.numeric_carrier
  { profile = "example-profile"; representation = "immediate"; compatibility = [] }]
val law : carrier -> unit [@@verocaml.proof]
|} ^ "val " ^ view_name ^ " : carrier -> int [@@verocaml.spec]\n"
            ^ {|[@@verocaml.numeric_role
  { carrier = carrier; role_schema = "numeric-role.v1"; role = "unsigned-view";
    semantics = law; visibility = "opaque"; reveal = false; inline = false }]
|}
          in
          let implementation =
            "[@@@verocaml.verify]\ntype carrier = int\nlet " ^ view_name
            ^ " (_x : int) = " ^ (if remote = `Spec then "Remote.projection _x" else if logical then "0 + 0" else "0")
            ^ " [@@verocaml.spec]\n"
            ^ "let different (_x : int) = 0 [@@verocaml.spec]\n"
            ^ helpers view_name
            ^ "let law (x : int) =\n" ^ requires ^ ensures view_name
            ^ law_body ^ "\n[@@verocaml." ^ (if axiom then "axiom" else "proof") ^ "]\n"
          in
          write_file (stem ^ ".mli") signature;
          write_file (stem ^ ".ml") implementation;
          List.iter (compile stem) [".mli"; ".ml"];
          emit stem;
          let load_provider () = ok "law provider load failed"
              (Cmt_input.load_with_interface ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi")
                ~cmti:(stem ^ ".cmti") ~artifact_directories:[workspace] ()) in
          let provider = load_provider () in
          let policy = ok "policy" (Solver_policy_private.create_default ~timeout_ms:5000) in
          let check threads =
          let report =
            if remote <> `None then
              (match Interface_specification_loaded_private.verify ~threads ~solver_policy:policy
                ~external_specifications:None ~external_targets:[] ~consumer:provider ~dependencies:imported with
              | Ok result -> Interface_specification_loaded_private.driver result
              | Error error -> failwith (Interface_specification_loaded_private.error_message error))
            else
            match Verification_driver_private.run_with_policy_and_threads ~threads ~solver_policy:policy
              ~allow_imported_opens:true ~allow_public_parametric_signatures:true provider with
            | Ok report -> report
            | Error (Verification_driver_private.Frontend_error diagnostic) -> failwith diagnostic.Diagnostic.message
            | Error (Validation_error error) -> failwith (Sst_validation.error_to_string error)
            | Error (Invariant_error error) -> failwith (Type_invariant.error_to_string error)
            | Error (Internal_error message | Provider_surface_error message) -> failwith message
            | Error _ -> failwith "law provider verification failed"
          in
          let completion = match Verification_driver_private.verified_completion report with
            | Some completion -> completion
            | None -> failwith "range fixture did not verify before statement matching" in
          let declarations = ok "law declaration binding failed"
              (Numeric_semantics_binding_private.complete_local ~completion ~implementation:provider
                ~validated:(Verification_driver_private.validated report)) in
          let declaration = match declarations with
            | [declaration] -> declaration | _ -> failwith "expected one law declaration" in
          let targets = ok "build target authentication failed"
              (Build_target_profile_private.authenticate_instances (Build_target_profile_private.capability ())) in
          let before = (Z3_bridge.counters ()).contexts_created in
          let summaries = List.map (fun target ->
              let matching = Numeric_law_match_private.unsigned_range ~target declaration in
              require (Option.is_some matching = Option.is_some width)
                ("incorrect unsigned-range match for target " ^ string_of_int target.target_claim.width
                  ^ "\n" ^ Verification_driver_private.semantic_sst report);
              Option.map (fun (matched : Numeric_law_match_private.unsigned_range) ->
                  require (Some matched.semantic_width = width)
                    "range match derived the wrong semantic width";
                  require (matched.declaration == declaration && matched.target == target)
                    "range match replaced declaration or build target";
                  require (matched.result_interpretation =
                    (if logical then Numeric_law_match_private.Mathematical_result else Lifted_runtime_result))
                    "range matching erased the view result's logical lift boundary";
                  require (matched.declaration.kind =
                    (if axiom then Numeric_semantics_binding_private.Explicit_axiom_declaration
                     else Completed_proof_declaration)) "statement matching changed proof/axiom provenance";
                  require (List.exists (fun clause -> clause == matched.lower_bound) declaration.definition.contracts.ensures
                    && List.exists (fun clause -> clause == matched.upper_bound) declaration.definition.contracts.ensures)
                    "range match detached its actual contract clauses";
                  let admitted = Numeric_proof_dependencies_private.complete_unsigned_range
                      ~completion ~implementation:provider ~validated:(Verification_driver_private.validated report) matched in
                  match admitted, unavailable with
                  | Error Numeric_proof_dependencies_private.Logical_constant_context, `Constant -> []
                  | Error (Missing_local_identity _), `Import -> []
                  | Error (Unsupported_dependency _), `Unsupported -> []
                  | Error _, _ -> failwith "local numeric law dependency evidence unexpectedly unavailable"
                  | Ok _, (`Constant | `Import | `Unsupported) -> failwith "numeric law was admitted without complete dependency support"
                  | Ok evidence, `No ->
                      let names = List.map (fun dependency -> dependency.Numeric_proof_dependencies_private.definition.function_id.function_name)
                          evidence.dependencies |> List.sort String.compare in
                      require (names = List.sort String.compare ("law" :: view_name :: extra_dependencies))
                        ("numeric law dependency closure has the wrong members: " ^ String.concat "," names);
                      require (match Numeric_proof_dependencies_private.complete_unsigned_range ~completion
                        ~implementation:(load_provider ()) ~validated:(Verification_driver_private.validated report) matched with
                        | Error Completion_mismatch -> true | _ -> false)
                        "numeric theorem completion was transferred to a separately loaded artifact";
                      let trust = List.map (fun dependency -> dependency.Numeric_proof_dependencies_private.definition.function_id.function_name)
                          evidence.trusted_dependencies |> List.sort String.compare in
                      require (trust = List.sort String.compare (if axiom then "law" :: expected_trust else expected_trust))
                        "numeric law dependency closure lost or invented trusted axioms";
                      let broadcasts = List.concat_map (fun dependency -> dependency.Numeric_proof_dependencies_private.broadcasts) evidence.dependencies in
                      require ((broadcasts <> []) = expected_broadcast)
                        "numeric law dependency closure omitted a broadcast insertion";
                      List.map (fun dependency -> dependency.Numeric_proof_dependencies_private.definition.function_id.function_name,
                          dependency.compiler_uid, dependency.trusted) evidence.dependencies) matching)
            targets
          in
          require ((Z3_bridge.counters ()).contexts_created = before)
            "numeric statement/dependency admission started a solver";
          summaries
          in
          require (check 1 = check 2) "numeric proof dependency evidence differs between serial and threaded verification";
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message -> Error (Failure.make Failure.Runner_internal message))
  in
  let case = extended_case ~remote:`None ~helpers:(fun _ -> "") ~law_body:"()"
      ~extra_dependencies:[] ~expected_trust:[] ~expected_broadcast:false ~unavailable:`No in
  let clause formula = "[%verocaml.ensures fun _ -> " ^ formula ^ "];\n" in
  let range view = clause ("0 <= " ^ view ^ " x && " ^ view ^ " x < 65536 * 65536") in
  let dependency_case ~name ~helpers ~body ~dependencies ~trust ~broadcast ~unavailable =
    extended_case ~remote:`None ~name ~helpers ~law_body:body ~expected_trust:trust
      ~extra_dependencies:dependencies
      ~expected_broadcast:broadcast ~unavailable ~view_name:"observe" ~logical:false
      ~axiom:false ~requires:"" ~ensures:range ~width:(Some 32)
  in
  let imported_case name remote body =
    extended_case ~remote ~name ~helpers:(fun _ -> "") ~law_body:body ~expected_trust:[]
      ~extra_dependencies:[] ~expected_broadcast:false ~unavailable:`Import ~view_name:"observe"
      ~logical:false ~axiom:false ~requires:"" ~ensures:range ~width:(Some 32)
  in
  let foundation ~trusted view =
    "let foundation (x : int) =\n" ^ range view ^ "()\n[@@verocaml."
    ^ (if trusted then "axiom" else "proof") ^ "]\n"
    ^ "let bridge (x : int) =\n" ^ range view ^ "foundation x\n[@@verocaml.proof]\n"
  in
  let broadcasts ~proved _ =
    "[%%verocaml.symbolic val probe : int -> bool]\n"
    ^ "let foundation (x : int) = [%verocaml.ensures fun _ -> (probe x)[@trigger]]; ()\n"
    ^ "[@@verocaml.axiom]" ^ (if proved then "\n" else " [@@verocaml.broadcast]\n")
    ^ (if proved then
         "let propagated (x : int) = [%verocaml.ensures fun _ -> (probe x)[@trigger]]; foundation x\n"
         ^ "[@@verocaml.proof] [@@verocaml.broadcast]\n[@@@verocaml.activate [propagated]]\n"
       else "[@@@verocaml.activate [foundation]]\n")
  in
  [ case ~name:"proved-unsigned-range-exact-target" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:range ~width:(Some 32);
    case ~name:"unsigned-range-direct-mathematical-view" ~view_name:"wide" ~logical:true ~axiom:false
      ~requires:"" ~ensures:range ~width:(Some 32);
    case ~name:"trusted-unsigned-range-keeps-axiom-provenance" ~view_name:"project" ~logical:false ~axiom:true
      ~requires:"" ~ensures:range ~width:(Some 32);
    case ~name:"unsigned-range-split-reversed-bounds" ~view_name:"unsigned" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun view -> clause (view ^ " x >= 0") ^ clause ("65536 * 65536 > " ^ view ^ " x")) ~width:(Some 32);
    case ~name:"unsigned-range-64-bit-constant-evaluation" ~view_name:"measure" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun view -> clause ("0 <= " ^ view ^ " x && " ^ view ^ " x < 65536 * 65536 * 65536 * 65536")) ~width:(Some 64);
    case ~name:"unsigned-role-label-cannot-authorize-unrelated-theorem" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun _ -> clause "x = x") ~width:None;
    case ~name:"unsigned-range-rejects-different-callable" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun _ -> range "different") ~width:None;
    case ~name:"unsigned-range-rejects-inclusive-upper-bound" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun view -> clause ("0 <= " ^ view ^ " x && " ^ view ^ " x <= 65536 * 65536")) ~width:None;
    case ~name:"unsigned-range-rejects-non-power-of-two-upper-bound" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun view -> clause ("0 <= " ^ view ^ " x && " ^ view ^ " x < 65535")) ~width:None;
    case ~name:"unsigned-range-rejects-zero-semantic-width" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun view -> clause ("0 <= " ^ view ^ " x && " ^ view ^ " x < 1")) ~width:None;
    case ~name:"unsigned-range-rejects-ambiguous-semantic-width" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun view ->
        clause ("0 <= " ^ view ^ " x && " ^ view ^ " x < 256")
        ^ clause (view ^ " x < 65536")) ~width:None;
    case ~name:"unsigned-range-does-not-drop-preconditions" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"[%verocaml.requires x >= 0];\n" ~ensures:range ~width:None;
    case ~name:"unsigned-range-does-not-flatten-disjunction" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun view -> clause ("0 <= " ^ view ^ " x || " ^ view ^ " x < 65536 * 65536")) ~width:None;
    case ~name:"unsigned-range-rejects-different-argument" ~view_name:"observe" ~logical:false ~axiom:false
      ~requires:"" ~ensures:(fun view -> clause ("0 <= " ^ view ^ " 0 && " ^ view ^ " 0 < 65536 * 65536")) ~width:None;
    dependency_case ~name:"numeric-law-local-transitive-proof-closure"
      ~helpers:(foundation ~trusted:false) ~body:"bridge x" ~dependencies:["foundation"; "bridge"] ~trust:[] ~broadcast:false ~unavailable:`No;
    dependency_case ~name:"numeric-law-transitive-trusted-axiom-visible"
      ~helpers:(foundation ~trusted:true) ~body:"bridge x" ~dependencies:["foundation"; "bridge"] ~trust:["foundation"] ~broadcast:false ~unavailable:`No;
    dependency_case ~name:"numeric-law-unused-axiom-not-a-dependency"
      ~helpers:(foundation ~trusted:true) ~body:"()" ~dependencies:[] ~trust:[] ~broadcast:false ~unavailable:`No;
    dependency_case ~name:"numeric-law-trusted-broadcast-dependency"
      ~helpers:(broadcasts ~proved:false) ~body:"assert (probe x)" ~dependencies:["foundation"; "probe"] ~trust:["foundation"] ~broadcast:true ~unavailable:`No;
    dependency_case ~name:"numeric-law-proved-broadcast-transitive-trust"
      ~helpers:(broadcasts ~proved:true) ~body:"assert (probe x)" ~dependencies:["foundation"; "propagated"; "probe"] ~trust:["foundation"] ~broadcast:true ~unavailable:`No;
    dependency_case ~name:"numeric-law-unavailable-constant-context-keeps-ordinary-proof"
      ~helpers:(fun _ -> "let truth : bool = true [@@verocaml.spec]\n")
      ~body:"assert truth" ~dependencies:[] ~trust:[] ~broadcast:false ~unavailable:`Constant;
    imported_case "numeric-law-imported-proof-cannot-disappear" `Proof "Remote.bridge x";
    imported_case "numeric-law-imported-spec-cannot-disappear" `Spec "()";
    dependency_case ~name:"numeric-law-intermediate-aggregate-context-unavailable"
      ~helpers:(fun _ -> "") ~body:"assert ((x, x) = (x, x))"
      ~dependencies:[] ~trust:[] ~broadcast:false ~unavailable:`Unsupported;
    dependency_case ~name:"numeric-law-special-specification-call-context-unavailable"
      ~helpers:(fun _ -> "let factory (value : int) : int -> int = fun _ -> value [@@verocaml.spec]\n")
      ~body:"assert (factory x 0 = x)" ~dependencies:[] ~trust:[] ~broadcast:false ~unavailable:`Unsupported ]
