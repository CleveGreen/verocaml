open Typedtree
let fail fmt = Printf.ksprintf failwith fmt
let span n =
  let p c = Diagnostic.{line=n; column=c} in
  Diagnostic.{file="proof.ml"; start_pos=p 0; end_pos=p 1}
let expr n typ expression_desc = Sst.{typ; expression_desc; span=span n}
let id i name = Sst.{function_index=i; function_name=name}
let check label condition = if not condition then fail "%s" label
let proof ?(recursive=false) ?(parameters=[]) ?(contracts=Sst.empty_contracts)
    ?(provenance=Sst.Raw_semantic_body (span 0)) i name body =
  Sst.{ function_id=id i name; type_binders=[]; mode=Proof; recursive; parameters; contracts;
    body=Proof_body {body={stage=Proof_stage; expression=body}; provenance};
    policy=Default_linear_z3; result_type=Unit; returns_unique_parameter=None; span=span i }
let proof_call ?(recursive=false) n target =
  expr n Sst.Unit
    (Sst.Direct_call
       {call_form=Proof_call; callee=target; type_arguments=[]; arguments=[]; recursive})
let decrease n expression =
  Sst.{clause_index=0; predicate={stage=Logical; expression}; span=span n}
let structural () =
  let a=id 0 "a" and b=id 1 "b" in
  let cyclic = Sst.{policy=Default_linear_z3; parametric_adts=[]; types=[]; functions=[proof 0 "a" (proof_call 1 b); proof 1 "b" (proof_call 2 a)]} in
  (match Sst_validation.validate cyclic with Error _ -> print_endline "rejected: cyclic proof graph" | Ok _ -> fail "cycle accepted");
  let self = id 3 "self" in
  let wrong_marker =
    proof ~recursive:true
      ~provenance:
        (Sst.Authenticated_typedtree
           {source_file="proof.ml"; declaration_span=span 3})
      ~contracts:{Sst.empty_contracts with decreases=[decrease 4 (expr 4 Sst.Int (Sst.Int_constant Z.zero))]}
      3 "self" (proof_call ~recursive:false 5 self)
  in
  (match Sst_validation.validate Sst.{policy=Default_linear_z3;parametric_adts=[];types=[];functions=[wrong_marker]} with
  | Error {kind=Invalid_recursive_marker _;_} ->
      print_endline "rejected: counterfeit recursive proof marker"
  | Error error -> fail "wrong marker produced %s" (Sst_validation.error_to_string error)
  | Ok _ -> fail "wrong recursive proof marker accepted");
  let proof_id = id 4 "proof_side" and spec_id = id 5 "spec_side" in
  let proof_side =
    proof ~recursive:true 4 "proof_side"
      (expr 6 Sst.Unit
         (Sst.Direct_call
            {call_form=Specification_call; callee=spec_id; arguments=[];
             recursive=false; type_arguments=[]}))
  in
  let spec_side =
    Sst.{function_id=spec_id; type_binders=[]; mode=Spec; recursive=true; parameters=[];
      contracts=empty_contracts;
      body=Spec_definition
        {stage=Logical;
         expression=expr 7 Sst.Unit
           (Sst.Direct_call
              {call_form=Proof_call; callee=proof_id; arguments=[];
               recursive=false; type_arguments=[]})};
      policy=Default_linear_z3; result_type=Unit;
      returns_unique_parameter=None; span=span 5}
  in
  let cross_mode =
    Sst.{policy=Default_linear_z3;parametric_adts=[];types=[];functions=[proof_side;spec_side]}
  in
  let analysis = Termination.analyze_raw cross_mode in
  (match Termination.precheck analysis with
  | Error {kind=Unsupported_mutual_scc members;_}
    when members=[proof_id;spec_id] ->
      print_endline "rejected: cross-mode recursive SCC"
  | Error error -> fail "cross-mode SCC produced %s" (Termination.error_to_string error)
  | Ok () -> fail "cross-mode SCC accepted");
  let region=expr 3 Sst.Unit (Sst.Proof_region (expr 3 Sst.Unit Sst.Unit_constant)) in
  let exec=Sst_normalize.checked_exec_raw ~function_id:(id 2 "bad_region") ~recursive:false ~parameters:[] ~contracts:Sst.empty_contracts ~body:region ~result_type:Sst.Unit ~returns_unique_parameter:None ~span:(span 3) in
  (match Sst_validation.validate Sst.{policy=Default_linear_z3;parametric_adts=[];types=[];functions=[exec]} with Error _ -> print_endline "rejected: proof region outside statement position" | Ok _ -> fail "region accepted");
  print_endline "raw proof graph, marker, stage, and statement-position checks passed"
let load filename = match Typedtree_lowering.lower_file filename with Ok s -> s | Error d -> fail "%s" d.Diagnostic.message
let lower filename = let s=load filename in match Symbolic_executor.lower_program s with Ok v -> s,v | Error e -> fail "%s" (Symbolic_executor.error_to_string e)
let inspect mode filename =
  let impl = match Cmt_input.load filename with Ok x->x | Error d->fail "%s" d.Diagnostic.message in
  let declarations=ref 0 and regions=ref 0 and decreases=ref 0
      and raw=ref 0 and nonghost=ref 0 in
  let default=Tast_iterator.default_iterator in
  let it={default with
    value_binding=(fun self b -> List.iter (fun a->if String.equal a.Parsetree.attr_name.txt "verocaml.proof" then incr raw) b.vb_attributes; default.value_binding self b);
    expr=(fun self e -> (match e.exp_desc with
      | Texp_apply ({exp_desc=Texp_ident(path,_,_,_,_);_},_,_,_,_)
        when String.equal (Path.name path) "Vero_ghost.proof_definition" ->
          incr declarations; if not e.exp_loc.loc_ghost then incr nonghost
      | Texp_apply ({exp_desc=Texp_ident(path,_,_,_,_);_},_,_,_,_)
        when String.equal (Path.name path) "Vero_ghost.proof_region" ->
          incr regions; if not e.exp_loc.loc_ghost then incr nonghost
      | Texp_apply ({exp_desc=Texp_ident(path,_,_,_,_);_},_,_,_,_)
        when String.equal (Path.name path) "Vero_ghost.decreases" ->
          incr decreases; if not e.exp_loc.loc_ghost then incr nonghost
      | _->()); default.expr self e)} in
  it.structure it impl.structure;
  match mode with
  | "erased" when !declarations=0 && !regions=0 && !raw=0 -> print_endline "ordinary CMT erased proof declarations and regions"
  | "retained" when !declarations=2 && !regions=1 && !decreases=0
      && !raw=0 && !nonghost=0 ->
      print_endline "retained CMT authenticated two proof declarations and one region"
  | "recursive" when !declarations=3 && !regions=1 && !decreases=1
      && !raw=0 && !nonghost=1 ->
      print_endline "retained CMT authenticated recursive proof and decreases carriers"
  | _ -> fail "carrier mismatch d=%d r=%d dec=%d raw=%d nonghost=%d"
      !declarations !regions !decreases !raw !nonghost
let reject f = match Typedtree_lowering.lower_file f with Error d->Printf.printf "adapter rejected: %s\n" d.Diagnostic.code | Ok s -> (match Symbolic_executor.lower_program s with Error {unsupported=Malformed_sst _;_}->print_endline "semantic validation rejected before VIR" | Error e->fail "%s" (Symbolic_executor.error_to_string e) | Ok _->fail "accepted")
let reject_totality f =
  match Typedtree_lowering.lower_file f with
  | Error diagnostic->fail "%s" diagnostic.Diagnostic.message
  | Ok program->
      (match Symbolic_executor.lower_program program with
      | Error error->
          Printf.printf "termination rejected before solver: %s\n"
            (Symbolic_executor.error_to_string error)
      | Ok _->fail "accepted")
let solve f = let _,v=lower f in let c=match Solver_backend.config ~timeout_ms:5000 with Ok c->c|Error e->fail "%s" (Solver_backend.error_to_string e) in List.iter (fun x->match Solver_backend.solve_in_order c x.Vir.obligations with Ok rs when List.for_all (fun r->match r.Solver_backend.outcome with Verified->true|_->false) rs->Printf.printf "%s: verified (%d obligations)\n" x.function_ref.function_name (List.length x.obligations)|Ok _->fail "not verified"|Error e->fail "%s" (Solver_backend.error_to_string e)) v.functions
let termination f =
  let program=load f in
  let validated=match Sst_validation.validate program with
    | Ok validated->validated
    | Error error->fail "%s" (Sst_validation.error_to_string error)
  in
  let analysis=Termination.analyze validated in
  let plan=match Termination.prepare analysis with
    | Ok plan->plan
    | Error error->fail "%s" (Termination.error_to_string error)
  in
  match Termination.pending_summaries plan with
  | [pending] ->
      let callable=Termination.pending_callable pending in
      let group=Termination.pending_group pending in
      check "pending callable is not proof"
        (Termination.callable_mode callable=Sst.Proof);
      check "pending group is not direct self"
        (Termination.scc_recursion_kind group=Some Termination.Direct_self);
      check "pending domain is not integer"
        (Termination.pending_domain pending=Termination.Integer_height);
      check "recursive proof did not retain one ordered edge"
        (List.length (Termination.scc_edges group)=1);
      let function_id=Termination.callable_id callable in
      check "entry intent missing"
        (Option.is_some (Termination.find_entry_intent plan function_id));
      let edge=List.hd (Termination.scc_edges group) in
      check "edge intent missing"
        (Option.is_some
           (Termination.find_edge_intent plan ~caller:function_id
              ~callee:function_id ~span:(Termination.call_edge_span edge)));
      print_endline
        "recursive proof uses one opaque pending summary and ordered integer entry/edge intents"
  | pending -> fail "expected one pending recursive proof summary, got %d"
      (List.length pending)
let kind_name (obligation:Vir.obligation) =
  match obligation.kind with
  | Entry_measure_nonnegative _ -> "entry-measure-nonnegative"
  | Recursive_call_measure_nonnegative _ ->
      "recursive-call-measure-nonnegative"
  | Recursive_call_strict_descent _ -> "recursive-call-strict-descent"
  | Arithmetic_safety _ -> "arithmetic-safety"
  | Call_precondition _ -> "call-precondition"
  | Callback_precondition _ -> "callback-precondition"
  | Invariant_validity _ -> "invariant-validity"
  | Assertion _ -> "assertion"
  | Local_assertion _ -> "local-assertion"
  | Postcondition _ -> "postcondition"
let outcomes f =
  let _,program=lower f in
  let config=match Solver_backend.config ~timeout_ms:5000 with
    | Ok config->config
    | Error error->fail "%s" (Solver_backend.error_to_string error)
  in
  List.iter
    (fun execution->
      match Solver_backend.solve_in_order config execution.Vir.obligations with
      | Error error->fail "%s" (Solver_backend.error_to_string error)
      | Ok results->
          List.iter
            (fun result->
              match result.Solver_backend.outcome with
              | Verified->()
              | Counterexample _->
                  Printf.printf "%s: counterexample %s\n"
                    execution.function_ref.function_name
                    (kind_name result.obligation)
              | Inconclusive _->
                  Printf.printf "%s: inconclusive %s\n"
                    execution.function_ref.function_name
                    (kind_name result.obligation))
            results)
    program.functions
let () = match Array.to_list Sys.argv with
| [_;"structural"]->structural()
| [_;"inspect";m;f]->inspect m f
| [_;"sst";f]->let s,_=lower f in print_string(Sst.to_string s)
| [_;"vir";f]->let _,v=lower f in print_string(Vir.to_string v)
| [_;"solve";f]->solve f
| [_;"termination";f]->termination f
| [_;"outcomes";f]->outcomes f
| [_;"reject-totality";f]->reject_totality f
| [_;"reject";f]->reject f
| _->fail "usage"
