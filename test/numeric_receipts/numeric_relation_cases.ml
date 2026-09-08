open Outcome_test_support
let require condition message = if not condition then failwith message
let ok = function Ok value -> value | Error message -> failwith message
let loaded = function Ok value -> value | Error diagnostic -> failwith diagnostic.Diagnostic.message
let cases ~write_file ~run_process =
  let modulus = "65536 * 65536" in
  let range name = "0 <= " ^ name ^ " && " ^ name ^ " < (" ^ modulus ^ ")" in
  let test ~name ~role ~signature ~callable ~formula ?(requires="") ?(axiom=true)
      ?(expected=true) ?(width=32) ?(view_width=32) ?(runtime=false)
      ?(trusted_runtime=false) ?(call_site=false) ?(guarded_occurrence=false)
      ?(counterexample=false) ?(threads=1) ?(helper="") ?(law_body="()")
      ?(consumer="") ?(successful_root="u_law") ?(failed_root="law")
      ?(authorized=[]) ?(excluded=[]) ?(evidence_members=[]) () =
    Suite.case ~name ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace -> try
        let directory = Filename.dirname Sys.executable_name in
        let ppx = Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe")
        and ghost = Unix.realpath (Filename.concat directory "../../runtime/.vero_ghost.objs/byte") in
        let stem = Filename.concat workspace "relation_provider" in
        let marker carrier role semantics =
          "[@@verocaml.numeric_role {carrier = " ^ carrier ^ "; role_schema = \"numeric-role.v1\"; role = \"" ^ role
          ^ "\"; semantics = " ^ semantics ^ "; visibility = \"opaque\"; reveal = false; inline = false}]\n" in
        let argument = if role = "operation" || runtime then "carrier" else "Arithmetic.t" in
        let view_modulus = Z.(shift_left one view_width |> to_string) in
        let view_range name =
          "0 <= " ^ name ^ " && " ^ name ^ " < (" ^ view_modulus ^ ")"
        in
        write_file (stem ^ ".mli")
          ("module Arithmetic : sig\ntype t = int [@@verocaml.logical_sort]\nval parse : string -> t [@@verocaml.integer_literal]\nend\n"
           ^ "type carrier = int [@@verocaml.numeric_carrier {base = Arithmetic.t; profile = \"request\"; representation = \"immediate\"; compatibility = []}]\n"
           ^ "type other = int\nval u_law : carrier -> unit [@@verocaml.proof]\nval u : carrier -> Arithmetic.t [@@verocaml.spec]\n"
           ^ marker "carrier" "unsigned-view" "u_law"
           ^ (if runtime then "" else "val law : " ^ argument ^ " -> unit [@@verocaml.proof]\n")
           ^ "val subject : " ^ signature ^ (if runtime then "\n" else " [@@verocaml.spec]\n")
           ^ marker "carrier" role (if runtime then "u" else "law"));
        write_file (stem ^ ".ml")
          ("[@@@verocaml.verify]\nmodule Arithmetic = struct\ntype t = int [@@verocaml.logical_sort]\nlet parse text = int_of_string text [@@verocaml.integer_literal]\nend\n"
           ^ "type carrier = int\ntype other = int\nlet u (_x : carrier) : Arithmetic.t = 0 + 0 [@@verocaml.spec]\n"
           ^ "let u_law (x : carrier) = [%verocaml.ensures fun _ -> " ^ view_range "(u x)" ^ "]; () [@@verocaml.proof]\n"
           ^ callable ^ "\n"
           ^ helper
           ^ (if runtime then "" else "let law (x : " ^ argument ^ ") = " ^ requires
             ^ "[%verocaml.ensures fun _ -> " ^ formula ^ "]; " ^ law_body
             ^ " [@@verocaml." ^ (if axiom then "axiom" else "proof") ^ "]\n")
           ^ consumer
           ^ (if call_site then "let call_subject (x : carrier) = subject x\n" else "")
           ^ (if guarded_occurrence then "let ghost_call (x : Arithmetic.t) = assert (u (subject x) = u (subject x)) [@@verocaml.proof]\n" else ""));
        List.iter (fun suffix -> run_process (Filename.concat Config.bindir "ocamlc")
          ["-w";"-A";"-c";"-bin-annot";"-I";workspace;"-I";ghost;"-ppx";ppx ^ " --keep-ghost";
           "-o";stem ^ (if suffix=".mli" then ".cmi" else ".cmo");stem ^ suffix]) [".mli";".ml"];
        loaded (Cmt_input.emit_retained_interface_authority ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi")
          ~cmti:(stem ^ ".cmti") ~output:(stem ^ ".vri") ~artifact_directories:[workspace] ());
        let implementation = loaded (Cmt_input.load_with_interface ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi")
          ~cmti:(stem ^ ".cmti") ~artifact_directories:[workspace] ()) in
        let policy = match Solver_policy_private.create_default ~timeout_ms:5000 with
          | Ok policy -> policy | Error _ -> failwith "invalid numeric test policy" in
        let report = match Verification_driver_private.run_with_policy_and_threads ~threads ~solver_policy:policy
          ~allow_imported_opens:true ~allow_public_parametric_signatures:true implementation with
          | Ok report -> report | Error (Frontend_error diagnostic) -> failwith (Printf.sprintf "%s at %d:%d" diagnostic.Diagnostic.message diagnostic.span.start_pos.line diagnostic.span.start_pos.column)
          | Error (Validation_error error) -> failwith (Sst_validation.error_to_string error)
          | Error _ -> failwith "numeric relation did not reach verification" in
        if counterexample then (
          require (Verification_driver_private.status report=Counterexample) "false numeric proof did not produce a counterexample";
          require (Option.is_none (Verification_driver_private.verified_completion report)) "false numeric proof acquired admission completion";
          let coordinator = match Verification_driver_private.prior_law_evidence report with
            | Some coordinator -> coordinator
            | None -> failwith "verification did not return scoped prior-law evidence" in
          let program = Sst_validation.program (Verification_driver_private.validated report) in
          let definition name = List.find (fun candidate ->
              candidate.Sst.function_id.function_name=name) program.functions in
          let closure =
            match
              Numeric_prior_law_closure_private.For_pipeline.complete
                coordinator ~root:(definition successful_root)
            with
            | Ok closure -> closure
            | Error reason ->
                failwith
                  ("a successful preceding law lost its scoped completion evidence: "
                  ^ reason)
          in
          List.iter
            (fun name ->
              let member = definition name in
              require
                (Numeric_prior_law_closure_private.authorizes_definition
                   closure member)
                ("scoped closure omitted required dependency " ^ name))
            authorized;
          List.iter
            (fun name ->
              let nonmember = definition name in
              require
                (not
                   (Numeric_prior_law_closure_private.authorizes_definition
                      closure nonmember))
                ("scoped closure admitted excluded function " ^ name))
            excluded;
          List.iter
            (fun name ->
              require
                (Option.is_some
                   (Numeric_prior_law_closure_private.function_evidence closure
                      (definition name)))
                ("scoped closure omitted successful evidence for " ^ name))
            evidence_members;
          require
            (Result.is_error
               (Numeric_prior_law_closure_private.For_pipeline.complete
                  coordinator ~root:(definition failed_root)))
            "a failed consumer acquired scoped completion evidence"
        ) else (
        let completion = match Verification_driver_private.verified_completion report with
          | Some completion -> completion | None -> failwith "relation fixture did not verify" in
        let validated = Verification_driver_private.validated report in
        let declarations = ok (Numeric_semantics_binding_private.complete_local ~completion ~implementation ~validated) in
        let contexts = (Z3_bridge.counters ()).contexts_created in
        let provider = ok (Numeric_provider_private.complete ~completion ~implementation ~validated ~dependencies:[] ~declarations) in
        require ((Z3_bridge.counters ()).contexts_created=contexts) "numeric admission created solver context";
        if guarded_occurrence then (
          let descriptor=List.find (fun (descriptor : Numeric_admitted_descriptor_private.t) -> descriptor.law.meaning=Relation Checked) provider.descriptors in
          let caller=List.find (fun definition -> definition.Sst.function_id.function_name="ghost_call") (Sst_validation.program validated).functions in
          let subject=List.find (fun definition -> definition.Sst.function_id.function_name="subject") (Sst_validation.program validated).functions in
          let rec flatten acc expression=List.fold_left flatten (expression::acc) (Sst_callback_private.expression_children expression) in
          let expressions=Numeric_proof_dependencies_private.definition_expressions caller |> List.fold_left flatten [] in
          let occurrence=List.find (fun expression -> match expression.Sst.expression_desc with
            | Direct_call {callee;_} -> callee=subject.function_id
            | _ -> false) expressions in
          require (Result.is_error (Numeric_admitted_descriptor_private.authorize_occurrence ~completion ~implementation ~validated ~descriptor ~caller ~occurrence))
            "a checked conversion acquired unconditional authority outside its domain";
          require ((Z3_bridge.counters ()).contexts_created=contexts) "guard checking created solver contexts");
        if call_site then (
          let descriptor=List.find (fun (descriptor : Numeric_admitted_descriptor_private.t) ->
            descriptor.law.meaning=Unsigned_range && descriptor.law.target.target_claim.width=width) provider.descriptors in
          let caller=List.find (fun definition -> definition.Sst.function_id.function_name="call_subject") (Sst_validation.program validated).functions in
          let occurrence=match caller.body with Checked_exec {body;_} -> body.expression | _ -> failwith "missing actual runtime call" in
          let authorize occurrence = Numeric_admitted_descriptor_private.authorize_occurrence ~completion ~implementation ~validated ~descriptor ~caller ~occurrence in
          require ((Result.is_ok (authorize occurrence))=expected) "runtime occurrence gate ignored exact implementation authority";
          let copied={occurrence with Sst.span=occurrence.span} in
          require (Result.is_error (authorize copied)) "a reconstructed expression acquired source occurrence authority";
          require ((Z3_bridge.counters ()).contexts_created=contexts) "occurrence admission created solver context");
        if runtime then (
          let refinements = List.filter (fun (refinement : Numeric_runtime_refinement_private.t) -> refinement.law.target.target_claim.width=width) provider.refinements in
          require ((refinements<>[]) = expected) "runtime refinement admission disagreed with the actual contract";
          List.iter (fun (refinement : Numeric_runtime_refinement_private.t) ->
            require ((refinement.authority=Explicit_external_body)=trusted_runtime) "runtime trust was relabelled as proof") refinements
        ) else (
          let admitted = List.filter (fun (law : Numeric_ghost_law_private.t) ->
            law.target.target_claim.width=width && Numeric_ghost_law_private.meaning_name law.meaning=role) provider.laws in
          require ((admitted<>[]) = expected)
            ("unexpected " ^ role ^ " admission: " ^ String.concat "; " (List.map (fun (failure : Numeric_provider_private.unavailable) -> failure.reason) provider.unavailable)
             ^ "\n" ^ Verification_driver_private.semantic_sst report);
          List.iter (fun (law : Numeric_ghost_law_private.t) -> require ((law.authority=Explicit_axiom)=axiom) "semantic trust changed") admitted
        ));
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message)) in
  let bounds ?requires ?expected ?width ?axiom ?counterexample name formula = test ~name ~role:"bounds"
    ~signature:"Arithmetic.t -> bool" ~callable:("let subject (x : Arithmetic.t) = " ^ range "x" ^ " [@@verocaml.spec]")
    ~formula ?requires ?expected ?width ?axiom ?counterexample () in
  let conversion ?requires ?expected role name callable formula = test ~name ~role
    ~signature:"Arithmetic.t -> carrier" ~callable ~formula ?requires ?expected () in
  let plain = "let subject (_x : Arithmetic.t) : carrier = 0 [@@verocaml.spec]" in
  let checked = plain in
  let checked_guard = "[%verocaml.requires " ^ range "x" ^ "];" in
  let operation ?requires ?expected ?axiom ?counterexample ?threads name formula = test ~name ~role:"operation" ~signature:"carrier -> carrier"
    ~callable:"let subject (_x : carrier) : carrier = 0 [@@verocaml.spec]" ~formula ?requires ?expected ?axiom ?counterexample ?threads () in
  [bounds "relation-bounds-exact" ("subject x = (" ^ range "x" ^ ")");
   bounds ~axiom:false "relation-bounds-genuinely-proved" ("subject x = (" ^ range "x" ^ ")");
   bounds ~axiom:false ~counterexample:true "relation-bounds-false-proof-cannot-complete" ("subject x = not (" ^ range "x" ^ ")");
   bounds "relation-bounds-reversed-comparisons" ("subject x = (x >= 0 && (" ^ modulus ^ ") > x)");
   bounds ~width:64 "relation-bounds-semantic32-on-machine64" ("subject x = (" ^ range "x" ^ ")");
   bounds ~expected:false "relation-bounds-one-way-is-not-equivalence" ("not (subject x) || (" ^ range "x" ^ ")");
   bounds ~expected:false "relation-bounds-inclusive-upper" ("subject x = (0 <= x && x <= (" ^ modulus ^ "))");
   bounds ~expected:false ~requires:"[%verocaml.requires false];" "relation-bounds-vacuous-guard" ("subject x = (" ^ range "x" ^ ")");
   conversion ~requires:checked_guard "checked-conversion" "relation-checked-exact-domain" checked "u (subject x) = x";
   test ~name:"relation-checked-domain-is-not-unconditional-ghost-authority" ~role:"checked-conversion"
     ~signature:"Arithmetic.t -> carrier" ~callable:checked ~requires:checked_guard ~formula:"u (subject x) = x" ~guarded_occurrence:true ();
   conversion ~expected:false "checked-conversion" "relation-checked-missing-law-domain" checked "u (subject x) = x";
   conversion ~requires:checked_guard "checked-conversion" "relation-checked-total-specification" plain "u (subject x) = x";
   conversion ~requires:"[%verocaml.requires x = 0];" ~expected:false "checked-conversion" "relation-checked-stronger-law-domain" checked "u (subject x) = x";
   conversion "partial-conversion" "relation-partial-domain-implication" plain ("not (" ^ range "x" ^ ") || u (subject x) = x");
   conversion ~expected:false "partial-conversion" "relation-partial-reversed-implication" plain ("not (u (subject x) = x) || (" ^ range "x" ^ ")");
   conversion ~requires:checked_guard ~expected:false "partial-conversion" "relation-partial-guarded-law-is-not-unconditional" checked ("not (" ^ range "x" ^ ") || u (subject x) = x");
   conversion "modular-conversion" "relation-modular-exact-quotient" plain ("exists (fun (q : Arithmetic.t) -> x = u (subject x) + (" ^ modulus ^ ") * q)");
   conversion ~expected:false "modular-conversion" "relation-modular-wrong-modulus" plain "exists (fun (q : Arithmetic.t) -> x = u (subject x) + 17 * q)";
   test ~name:"relation-modular-rejects-mod2-32-for-semantic16"
     ~role:"modular-conversion" ~view_width:16 ~expected:false
     ~signature:"Arithmetic.t -> carrier" ~callable:plain
     ~formula:("exists (fun (q : Arithmetic.t) -> x = u (subject x) + (" ^ modulus ^ ") * q)") ();
   conversion ~expected:false "modular-conversion" "relation-modular-does-not-claim-exact-conversion" plain "x = u (subject x)";
   operation "relation-operation-addition" "u (subject x) = u x + 1";
   operation ~axiom:false "relation-operation-genuinely-proved" "u (subject x) = u x";
   operation ~axiom:false ~counterexample:true "relation-operation-false-proof-cannot-complete" "u (subject x) = u x + 1";
   operation ~axiom:false ~counterexample:true ~threads:2
     "relation-operation-threaded-false-proof-cannot-complete"
     "u (subject x) = u x + 1";
   test ~name:"relation-failed-helper-does-not-authorize-dependent-law"
     ~role:"operation" ~signature:"carrier -> carrier"
     ~callable:"let subject (_x : carrier) : carrier = 0 [@@verocaml.spec]"
     ~formula:"u (subject x) = u x + 1" ~axiom:false ~counterexample:true
     ~helper:"let checked_helper (_x : carrier) = [%verocaml.ensures fun _ -> false]; () [@@verocaml.proof]\n"
     ~law_body:"checked_helper x" ~excluded:["checked_helper"; "law"] ();
   test ~name:"relation-successful-helper-closes-law-before-failed-consumer"
     ~role:"operation" ~signature:"carrier -> carrier"
     ~callable:"let subject (_x : carrier) : carrier = 0 [@@verocaml.spec]"
     ~formula:"u (subject x) = u x" ~axiom:false ~counterexample:true
     ~helper:"let checked_helper (x : carrier) = [%verocaml.ensures fun _ -> x = x]; () [@@verocaml.proof]\n"
     ~law_body:"checked_helper x"
     ~consumer:"let consumer (_x : carrier) = [%verocaml.ensures fun _ -> false]; () [@@verocaml.proof]\n"
     ~successful_root:"law" ~failed_root:"consumer"
     ~authorized:["checked_helper"; "law"] ~excluded:["consumer"]
     ~evidence_members:["checked_helper"; "law"] ();
   operation "relation-operation-multiplication" "u (subject x) = u x * u x";
   operation "relation-operation-conditional" "u (subject x) = (if u x < 8 then u x + 1 else u x - 8)";
   operation ~expected:false ~requires:"[%verocaml.requires false];" "relation-operation-vacuous-proof-not-unconditional" "u (subject x) = u x + 1";
   test ~name:"runtime-view-checked-result-equivalence" ~role:"runtime-view" ~runtime:true ~call_site:true ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) = [%verocaml.ensures fun result -> result = u x]; 0" ~formula:"" ();
   test ~name:"runtime-view-explicit-trust-is-distinct" ~role:"trusted-runtime-view" ~runtime:true ~trusted_runtime:true ~call_site:true ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) = [%verocaml.ensures fun result -> result = u x]; 1 [@@verocaml.external_body]" ~formula:"" ();
   test ~name:"runtime-view-wrong-result-is-not-refinement" ~role:"runtime-view" ~runtime:true ~expected:false ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) = [%verocaml.ensures fun _ -> u x = 0]; 1" ~formula:"" ();
   test ~name:"runtime-view-external-body-alone-is-not-full-equivalence" ~role:"runtime-view" ~runtime:true ~expected:false ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) : int = [%verocaml.ensures fun result -> result = u x]; failwith \"boom\" [@@verocaml.external_body]" ~formula:"" ();
   test ~name:"runtime-view-missing-result-equation" ~role:"runtime-view" ~runtime:true ~call_site:true ~expected:false ~signature:"carrier -> int"
     ~callable:"let subject (_x : carrier) = 0" ~formula:"" ();
   test ~name:"runtime-view-offset-is-not-exact-equivalence" ~role:"runtime-view" ~runtime:true ~expected:false ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) = [%verocaml.ensures fun result -> result + 1 = u x]; -1" ~formula:"" ();
   test ~name:"runtime-view-reversed-result-equation" ~role:"runtime-view" ~runtime:true ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) = [%verocaml.ensures fun result -> u x = result]; 0" ~formula:"" ();
   test ~name:"runtime-view-semantic32-on-machine64" ~role:"runtime-view" ~runtime:true ~width:64 ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) = [%verocaml.ensures fun result -> result = u x]; 0" ~formula:"" ();
   test ~name:"runtime-view-false-precondition-is-vacuous" ~role:"runtime-view" ~runtime:true ~expected:false ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) = [%verocaml.requires false]; [%verocaml.ensures fun result -> result = u x]; 0" ~formula:"" ();
   test ~name:"runtime-view-extra-trivial-ensures" ~role:"runtime-view" ~runtime:true ~signature:"carrier -> int"
     ~callable:"let subject (x : carrier) = [%verocaml.ensures fun result -> result = u x]; [%verocaml.ensures fun _ -> true]; 0" ~formula:"" ();
   test ~name:"runtime-view-wrong-nominal-carrier" ~role:"runtime-view" ~runtime:true ~expected:false ~signature:"other -> int"
     ~callable:"let subject (x : other) = [%verocaml.ensures fun result -> result = u x]; 0" ~formula:"" ();
   test ~name:"runtime-view-erased-argument-does-not-establish-runtime-abi" ~role:"runtime-view" ~runtime:true ~expected:false ~signature:"(carrier [@ghost]) -> int"
     ~callable:"let subject (x : carrier [@ghost]) : int = [%verocaml.ensures fun result -> result = u x]; 0" ~formula:"" ();
   test ~name:"runtime-view-trusted-executable-dependency-does-not-prove-totality" ~role:"runtime-view" ~runtime:true ~expected:false ~signature:"carrier -> int"
     ~callable:"let leaf (x : carrier) : int = [%verocaml.ensures fun result -> result = u x]; failwith \"boom\" [@@verocaml.external_body]\nlet subject (x : carrier) = [%verocaml.ensures fun result -> result = u x]; leaf x" ~formula:"" ()]
