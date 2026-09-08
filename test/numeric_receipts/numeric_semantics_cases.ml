open Outcome_test_support

let require condition message = if not condition then failwith message
let ok label = function Ok value -> value | Error _ -> failwith label

let cases ~write_file ~run_process =
  let case ~name ~nested ~kind ~extra ~succeeds =
    Suite.case ~name
      ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace ->
        try
          let directory = Filename.dirname Sys.executable_name in
          let ppx = Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe")
          and ghost = Unix.realpath (Filename.concat directory "../../runtime/.vero_ghost.objs/byte")
          and compiler = Filename.concat Config.bindir "ocamlc" in
          let compile suffix stem =
            run_process compiler
              [ "-c"; "-bin-annot"; "-I"; workspace; "-I"; ghost;
                "-ppx"; ppx ^ " --keep-ghost"; "-o";
                stem ^ (if suffix = ".mli" then ".cmi" else ".cmo"); stem ^ suffix ]
          in
          let stem = Filename.concat workspace "semantics_provider" in
          let marker, result_type, body, expected_kind =
            match kind with
            | `Proof -> "proof", "unit",
                "let law (x : int) = [%verocaml.ensures fun _ -> x = x]; () [@@verocaml.proof]\n",
                Some Numeric_semantics_binding_private.Completed_proof_declaration
            | `Axiom -> "proof", "unit",
                "let law (x : int) = [%verocaml.ensures fun _ -> x = 0]; () [@@verocaml.axiom]\n",
                Some Explicit_axiom_declaration
            | `Spec -> "spec", "int",
                "let law (x : int) = x [@@verocaml.spec]\n",
                Some Ordinary_specification_declaration
            | `Failed -> "proof", "unit",
                "let law (x : int) = [%verocaml.ensures fun _ -> x = 0]; () [@@verocaml.proof]\n",
                None
          in
          let signature =
            {|type carrier = int
[@@verocaml.numeric_carrier
  { profile = "example-profile"; representation = "immediate"; compatibility = [] }]
|} ^ "val law : carrier -> " ^ result_type ^ " [@@verocaml." ^ marker ^ "]\n"
            ^ {|val operation : carrier -> carrier
[@@verocaml.numeric_role
  { carrier = carrier; role_schema = "numeric-role.v1"; role = "operation";
    semantics = law; visibility = "opaque"; reveal = false; inline = false }]
|}
          in
          let implementation = "type carrier = int\n" ^ body ^ "let operation (x : int) = x\n" in
          let nest contents =
            if nested then "module Nested " ^ contents else contents
          in
          write_file (stem ^ ".mli")
            (if nested then nest (": sig\n" ^ signature ^ "end\n") else signature);
          write_file (stem ^ ".ml")
            ("[@@@verocaml.verify]\n"
             ^ (if nested then nest ("= struct\n" ^ implementation ^ "end\n") else implementation)
             ^ extra);
          compile ".mli" stem;
          compile ".ml" stem;
          (match Cmt_input.emit_retained_interface_authority ~cmt:(stem ^ ".cmt")
              ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti") ~output:(stem ^ ".vri")
              ~artifact_directories:[workspace] () with
          | Ok () -> ()
          | Error diagnostic -> failwith diagnostic.Diagnostic.message);
          let load () =
            ok "provider artifact load failed"
              (Cmt_input.load_with_interface ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi")
                ~cmti:(stem ^ ".cmti") ~artifact_directories:[workspace] ())
          in
          let provider = load () in
          let policy = ok "solver policy failed" (Solver_policy_private.create_default ~timeout_ms:5000) in
          let report = ok "provider verification failed"
              (Verification_driver_private.run_with_policy ~solver_policy:policy
                ~allow_imported_opens:true ~allow_public_parametric_signatures:true provider) in
          (match Verification_driver_private.verified_completion report, expected_kind with
          | None, None ->
              require (Verification_driver_private.status report = Verification_pipeline.Counterexample)
                "false numeric proof did not produce a counterexample"
          | Some completion, Some expected_kind ->
              let validated = Verification_driver_private.validated report in
              let bindings = ok "completed semantics correlation failed"
                  (Numeric_semantics_binding_private.complete_local ~completion
                    ~implementation:provider ~validated) in
              let binding = match bindings with [binding] -> binding | _ -> failwith "expected one semantics declaration" in
              require (binding.kind = expected_kind) "numeric declaration trust was promoted or lost";
              require
                (Numeric_semantics_binding_private.selects_explicit_axiom bindings binding.definition = (kind = `Axiom))
                "numeric axiom export selection lost exact declaration kind";
              require (Result.is_error
                (Numeric_semantics_binding_private.complete_local ~completion
                  ~implementation:(load ()) ~validated))
                "completion was transferred to a separately loaded artifact"
          | _ -> failwith "numeric proof completion disagrees with verification outcome");
          let consumer_stem = Filename.concat workspace "semantics_consumer" in
          let path = "Semantics_provider." ^ (if nested then "Nested." else "") in
          write_file (consumer_stem ^ ".ml")
            ("[@@@verocaml.verify]\nmodule Provider = Semantics_provider\nlet check (x : int) =\n"
             ^ (if kind = `Axiom then "[%verocaml.ensures fun _ -> x = 0];\n" else "")
             ^ (if kind = `Spec then "()" else path ^ "law x") ^ "\n[@@verocaml.proof]\n");
          compile ".ml" consumer_stem;
          let consumer = ok "consumer artifact load failed"
              (Cmt_input.load (consumer_stem ^ ".cmt")) in
          let result = Interface_specification_loaded_private.verify ~threads:1
              ~solver_policy:policy ~external_specifications:None ~external_targets:[]
              ~consumer ~dependencies:[provider] in
          (match succeeds, result with
          | true, Ok result ->
              let driver = Interface_specification_loaded_private.driver result in
              require (Verification_driver_private.status driver = Verification_pipeline.Verified)
                "consumer failed to use completed numeric provider";
              let vir = Verification_driver_private.vir driver in
              let trusted =
                List.exists (fun function_ ->
                    List.exists (function Vir.Trusted_external_body_use _ -> true | _ -> false)
                      function_.Vir.trusted_summary_uses)
                  vir.functions
              in
              require (trusted = (kind = `Axiom))
                "numeric axiom trust was hidden or invented in consumer output"
          | false, Error error ->
              require (not (Interface_specification_loaded_private.error_is_internal error))
                "numeric provider rejection was misclassified as an internal failure"
          | true, Error error -> failwith (Interface_specification_loaded_private.error_message error)
          | false, Ok _ -> failwith "unselected trust or failed numeric proof was admitted");
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message -> Error (Failure.make Failure.Runner_internal message))
  in
  [ case ~name:"completed-numeric-proof-is-not-an-axiom" ~nested:false ~kind:`Proof ~extra:"" ~succeeds:true;
    case ~name:"selected-numeric-axiom-remains-trusted" ~nested:false ~kind:`Axiom ~extra:"" ~succeeds:true;
    case ~name:"nested-selected-numeric-axiom" ~nested:true ~kind:`Axiom ~extra:"" ~succeeds:true;
    case ~name:"ordinary-numeric-spec-is-not-law-authority" ~nested:false ~kind:`Spec ~extra:"" ~succeeds:true;
    case ~name:"failed-numeric-proof-has-no-completion" ~nested:false ~kind:`Failed ~extra:"" ~succeeds:false;
    case ~name:"unselected-axiom-is-not-exported-by-numeric-metadata" ~nested:false ~kind:`Proof
      ~extra:"let unrelated () = [%verocaml.ensures fun _ -> false]; () [@@verocaml.axiom]\n" ~succeeds:false ]
