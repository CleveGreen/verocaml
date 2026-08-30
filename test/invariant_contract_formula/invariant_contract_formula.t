The tutorial-shaped model, invariant, and contracts use complete strict permits.
All source/CMT x threads 1/6 x repeats 1/2 SST and VIR members are compared,
and every member has the exact 13-VC structure with one goal per VC.

  $ mkdir artifacts
  $ export DELATOR_LOG='Logical_spec_capability_private=info,Logical_spec_admission_private=info,warn'
  $ export DELATOR_FORMAT=flat
  $ export DELATOR_COLOR=never
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/path_product.cmo fixtures/path_product.ml
  $ for route in source cmt; do for threads in 1 6; do for repeat in 1 2; do if test "$route" = source; then input=fixtures/path_product.ml; else input=artifacts/path_product.cmt; fi; VEROCAML_TEST_FORMULA_TRACE=1 VEROCAML_TEST_FORMULA_CAPABILITY_CONTROLS=1 OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads "$threads" --timeout-ms 5000 --dump-sst "artifacts/$route.$threads.$repeat.sst" --dump-vir "artifacts/$route.$threads.$repeat.vir" > "artifacts/$route.$threads.$repeat.out" 2>&1; done; done; done
  $ for route in source cmt; do for threads in 1 6; do for repeat in 1 2; do base="artifacts/$route.$threads.$repeat"; test "$(grep -c '^verocaml: verified .*functions=4 obligations=13$' "$base.out")" -eq 1; test "$(grep -c '^INFO Logical_spec_capability_private: formula profile .* outcome=permit$' "$base.out")" -eq 5; test "$(grep -c '^INFO Logical_spec_capability_private: formula evaluation .* authority_unchanged=true$' "$base.out")" -eq 11; test "$(grep -c '^INFO Logical_spec_capability_private: capability negative control .* rejected=true$' "$base.out")" -eq 10; test "$(./invariant_contract_formula_tool.exe boundary-check "$base.vir")" = "boundaries=13 unique-keys=13 unique-function-indices=13"; printf 'matrix route=%s threads=%s repeat=%s obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13\n' "$route" "$threads" "$repeat"; done; done; done
  matrix route=source threads=1 repeat=1 obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13
  matrix route=source threads=1 repeat=2 obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13
  matrix route=source threads=6 repeat=1 obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13
  matrix route=source threads=6 repeat=2 obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13
  matrix route=cmt threads=1 repeat=1 obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13
  matrix route=cmt threads=1 repeat=2 obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13
  matrix route=cmt threads=6 repeat=1 obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13
  matrix route=cmt threads=6 repeat=2 obligations=13 headers=13 goals=13 duplicates=0 permit=5 evaluation-zero=11 capability-negative=10 boundaries=13
  $ for route in source cmt; do for threads in 1 6; do for repeat in 1 2; do test "$threads.$repeat" = 1.1 || { cmp "artifacts/$route.1.1.sst" "artifacts/$route.$threads.$repeat.sst"; cmp "artifacts/$route.1.1.vir" "artifacts/$route.$threads.$repeat.vir"; }; done; done; done
  $ for suffix in sst vir; do ./invariant_contract_formula_tool.exe normalize "artifacts/source.1.1.$suffix" "artifacts/source.$suffix.normalized"; ./invariant_contract_formula_tool.exe normalize "artifacts/cmt.1.1.$suffix" "artifacts/cmt.$suffix.normalized"; cmp "artifacts/source.$suffix.normalized" "artifacts/cmt.$suffix.normalized"; done
  $ echo source-cmt-normalization=only-input-path-checked-body-authority-source-snapshot-provenance; echo matrix-comparisons=all-eight-sst-and-vir-members
  source-cmt-normalization=only-input-path-checked-body-authority-source-snapshot-provenance
  matrix-comparisons=all-eight-sst-and-vir-members

The production VIR observer reports all 13 boundaries by function, VC kind,
and actual path. The Node transition precedes its return invariant/postcondition.

  $ ./invariant_contract_formula_tool.exe vir artifacts/source.1.1.vir
  headers=13 goals=13 normalized-duplicates=0
  Stack.singleton#3=3
  Stack.zero_head#4=5
  singleton_then_zero#5=5
  boundaries=13 unique-keys=13 unique-function-indices=13

The complete capability is non-transferable on source and retained-CMT routes.
These are production permit checks; the tool only retains their observations.

  $ for route in source cmt; do grep '^INFO Logical_spec_capability_private: capability negative control ' "artifacts/$route.1.1.out" | sed -E "s/^INFO Logical_spec_capability_private: /route=$route /"; done
  route=source capability negative control control=equal-distinct-root rejected=true
  route=source capability negative control control=requires-ensures-token-swap rejected=true
  route=source capability negative control control=cross-program-root rejected=true
  route=source capability negative control control=sibling-callable rejected=true
  route=source capability negative control control=wrong-model-result rejected=true
  route=source capability negative control control=wrong-aggregate-descriptor rejected=true
  route=source capability negative control control=ordinary-spec-mutable-field rejected=true
  route=source capability negative control control=sibling-model rejected=true
  route=source capability negative control control=non-representation-mutable-field rejected=true
  route=source capability negative control control=wrong-field-descriptor rejected=true
  route=cmt capability negative control control=equal-distinct-root rejected=true
  route=cmt capability negative control control=requires-ensures-token-swap rejected=true
  route=cmt capability negative control control=cross-program-root rejected=true
  route=cmt capability negative control control=sibling-callable rejected=true
  route=cmt capability negative control control=wrong-model-result rejected=true
  route=cmt capability negative control control=wrong-aggregate-descriptor rejected=true
  route=cmt capability negative control control=ordinary-spec-mutable-field rejected=true
  route=cmt capability negative control control=sibling-model rejected=true
  route=cmt capability negative control control=non-representation-mutable-field rejected=true
  route=cmt capability negative control control=wrong-field-descriptor rejected=true
  $ grep -E 'formula profile |formula registry constructed |formula authority observed ' artifacts/source.1.1.out | sed -E 's/\$[0-9]+/\$N/g'
  INFO Logical_spec_capability_private: formula profile root=invariant:invariant:Stack.t:2:Stack.invariant:1:Stack.invariant#1:de78f24b5501df43c294a2f99e8f9ac1 outcome=permit
  INFO Logical_spec_capability_private: formula profile root=contract:Stack.singleton#3:ensures:0 outcome=permit
  INFO Logical_spec_capability_private: formula profile root=contract:Stack.zero_head#4:requires:0 outcome=permit
  INFO Logical_spec_capability_private: formula profile root=contract:Stack.zero_head#4:ensures:0 outcome=permit
  INFO Logical_spec_capability_private: formula profile root=contract:singleton_then_zero#5:ensures:0 outcome=permit
  INFO Logical_spec_capability_private: formula registry constructed authority_unchanged=true entries=5
  INFO Logical_spec_capability_private: formula authority observed function_name=Stack.singleton registry_unchanged=true
  INFO Logical_spec_capability_private: formula authority observed function_name=Stack.zero_head registry_unchanged=true
  INFO Logical_spec_capability_private: formula authority observed function_name=singleton_then_zero registry_unchanged=true
  INFO Logical_spec_capability_private: formula authority observed function_name=Stack.length registry_unchanged=true
  $ ./invariant_contract_formula_tool.exe boundaries artifacts/source.1.1.vir
  obligation-index=0 function=Stack.singleton#3 kind=invariant-validity span=path_product.ml:39:4-39:54 path=""
  obligation-index=1 function=Stack.singleton#3 kind=invariant-validity span=path_product.ml:33:2-39:54 path=""
  obligation-index=2 function=Stack.singleton#3 kind=postcondition span=path_product.ml:34:4-38:27 path=""
  obligation-index=0 function=Stack.zero_head#4 kind=invariant-validity span=path_product.ml:53:8-53:25 path="(not (verocaml_owned_root_scalar_v1.f0:top/tag0:Empty#768433844 stack$N)) && (verocaml_owned_root_scalar_v1.f0:top/tag1:Node#729897716 stack$N)"
  obligation-index=1 function=Stack.zero_head#4 kind=invariant-validity span=path_product.ml:41:2-54:13 path="(verocaml_owned_root_scalar_v1.f0:top/tag0:Empty#768433844 stack$N)"
  obligation-index=2 function=Stack.zero_head#4 kind=postcondition span=path_product.ml:45:4-49:27 path="(verocaml_owned_root_scalar_v1.f0:top/tag0:Empty#768433844 stack$N)"
  obligation-index=3 function=Stack.zero_head#4 kind=invariant-validity span=path_product.ml:41:2-54:13 path="(not (verocaml_owned_root_scalar_v1.f0:top/tag0:Empty#768433844 stack$N)) && (verocaml_owned_root_scalar_v1.f0:top/tag1:Node#729897716 stack$N)"
  obligation-index=4 function=Stack.zero_head#4 kind=postcondition span=path_product.ml:45:4-49:27 path="(not (verocaml_owned_root_scalar_v1.f0:top/tag0:Empty#768433844 stack$N)) && (verocaml_owned_root_scalar_v1.f0:top/tag1:Node#729897716 stack$N)"
  obligation-index=0 function=singleton_then_zero#5 kind=invariant-validity span=path_product.ml:63:14-63:35 path=""
  obligation-index=1 function=singleton_then_zero#5 kind=call-precondition span=path_product.ml:64:2-64:23 path=""
  obligation-index=2 function=singleton_then_zero#5 kind=invariant-validity span=path_product.ml:64:2-64:23 path=""
  obligation-index=3 function=singleton_then_zero#5 kind=invariant-validity span=path_product.ml:57:0-64:23 path=""
  obligation-index=4 function=singleton_then_zero#5 kind=postcondition span=path_product.ml:58:2-62:25 path=""
  boundaries=13 unique-keys=13 unique-function-indices=13

Every deliberate false claim is admitted, reaches the backend, and is an exact
counterexample on source and retained CMT. Diagnostics retain the source span,
function, VC kind, and required actual-path boundary.

  $ cp fixtures/false_claims.ml artifacts/node_true.ml
  $ sed 's/view.head = value (\* WRONG_SINGLETON_HEAD \*)/view.head = 0 (\* WRONG_SINGLETON_HEAD \*)/' fixtures/false_claims.ml > artifacts/wrong_singleton.ml
  $ sed 's/record.value <- 0; (\* OMIT_UPDATE \*)/record.value <- 1; (\* OMIT_UPDATE \*)/' fixtures/false_claims.ml > artifacts/omitted_update.ml
  $ sed 's/view.length = 1 (\* WEAK_PRECONDITION \*)/view.length >= 0 (\* WEAK_PRECONDITION \*)/' fixtures/false_claims.ml > artifacts/weakened_precondition.ml
  $ sed 's/Stack.zero_head stack (\* SKIP_TRANSITION \*)/let other = Stack.singleton value in let _ = Stack.zero_head other in stack (\* SKIP_TRANSITION \*)/' fixtures/false_claims.ml > artifacts/skipped_transition.ml
  $ sed 's/view.head = 0 (\* NODE_REQUIRED \*)/view.head = 1 (\* NODE_REQUIRED \*)/' fixtures/false_claims.ml > artifacts/node_false.ml
  $ for name in wrong_singleton omitted_update weakened_precondition skipped_transition node_false node_true; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "artifacts/$name.ml"; done
  $ for name in wrong_singleton omitted_update weakened_precondition skipped_transition node_false; do for route in source cmt; do if test "$route" = source; then input="artifacts/$name.ml"; else input="artifacts/$name.cmt"; fi; set +e; VEROCAML_TEST_FORMULA_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 5000 --dump-vir "artifacts/$name.$route.vir" > "artifacts/$name.$route.out" 2>&1; rc=$?; set -e; test "$rc" -eq 1; test "$(grep -c '^INFO Logical_spec_capability_private: formula profile .* outcome=permit$' "artifacts/$name.$route.out")" -eq 5; grep -q '^verocaml: counterexample ' "artifacts/$name.$route.out"; test "$(grep -c '^  vc .*postcondition' "artifacts/$name.$route.vir")" -gt 0; printf 'route=%s diagnostic control=%s outcome=counterexample production-output-and-boundary=retained\n' "$route" "$name"; done; done
  route=source diagnostic control=wrong_singleton outcome=counterexample production-output-and-boundary=retained
  route=cmt diagnostic control=wrong_singleton outcome=counterexample production-output-and-boundary=retained
  route=source diagnostic control=omitted_update outcome=counterexample production-output-and-boundary=retained
  route=cmt diagnostic control=omitted_update outcome=counterexample production-output-and-boundary=retained
  route=source diagnostic control=weakened_precondition outcome=counterexample production-output-and-boundary=retained
  route=cmt diagnostic control=weakened_precondition outcome=counterexample production-output-and-boundary=retained
  route=source diagnostic control=skipped_transition outcome=counterexample production-output-and-boundary=retained
  route=cmt diagnostic control=skipped_transition outcome=counterexample production-output-and-boundary=retained
  route=source diagnostic control=node_false outcome=counterexample production-output-and-boundary=retained
  route=cmt diagnostic control=node_false outcome=counterexample production-output-and-boundary=retained
  $ for route in source cmt; do if test "$route" = source; then input=artifacts/node_true.ml; else input=artifacts/node_true.cmt; fi; OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 5000 | sed -E "s#file=[^ ]+#route=$route#"; done
  verocaml: verified route=source functions=4 obligations=13
  verocaml: verified route=cmt functions=4 obligations=13

Real Exec and Proof `if`/`match` bodies retain four functions with two exact
actual exits each. Their contract roots remain eligible on source and CMT.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/exec_proof_paths.cmo fixtures/exec_proof_paths.ml
  $ for route in source cmt; do if test "$route" = source; then input=fixtures/exec_proof_paths.ml; else input=artifacts/exec_proof_paths.cmt; fi; VEROCAML_TEST_FORMULA_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 5000 --dump-vir "artifacts/exec.$route.vir" > "artifacts/exec.$route.out" 2>&1; grep '^verocaml: verified' "artifacts/exec.$route.out" | sed -E "s#file=[^ ]+#route=$route#"; test "$(grep -c 'formula profile root=contract:.* outcome=permit' "artifacts/exec.$route.out")" -eq 4; done
  verocaml: verified route=source functions=4 obligations=12
  verocaml: verified route=cmt functions=4 obligations=12
  $ ./invariant_contract_formula_tool.exe normalize artifacts/exec.source.vir artifacts/exec.source.normalized.vir
  $ ./invariant_contract_formula_tool.exe normalize artifacts/exec.cmt.vir artifacts/exec.cmt.normalized.vir
  $ cmp artifacts/exec.source.normalized.vir artifacts/exec.cmt.normalized.vir
  $ ./invariant_contract_formula_tool.exe boundaries artifacts/exec.source.vir | grep -E 'function=exec_.*kind=postcondition|function=proof_.*kind=local-assertion'
  obligation-index=0 function=exec_if#0 kind=postcondition span=exec_proof_paths.ml:5:2-5:49 path="flag$N"
  obligation-index=1 function=exec_if#0 kind=postcondition span=exec_proof_paths.ml:5:2-5:49 path="(not flag$N)"
  obligation-index=0 function=exec_match#1 kind=postcondition span=exec_proof_paths.ml:8:2-8:51 path="(and true (= (tag.option_specification<int>#0 value$N) 0))"
  obligation-index=1 function=exec_match#1 kind=postcondition span=exec_proof_paths.ml:8:2-8:51 path="(not (and true (= (tag.option_specification<int>#0 value$N) 0))) && (and (and true (= (tag.option_specification<int>#0 value$N) 1)) true)"
  obligation-index=0 function=proof_if#2 kind=local-assertion span=exec_proof_paths.ml:15:15-15:38 path="flag$N"
  obligation-index=1 function=proof_if#2 kind=local-assertion span=exec_proof_paths.ml:15:44-15:71 path="(not flag$N)"
  obligation-index=0 function=proof_match#3 kind=local-assertion span=exec_proof_paths.ml:22:12-22:35 path="(and true (= (tag.option_specification<int>#0 value$N) 0))"
  obligation-index=1 function=proof_match#3 kind=local-assertion span=exec_proof_paths.ml:23:17-23:47 path="(not (and true (= (tag.option_specification<int>#0 value$N) 0))) && (and (and true (= (tag.option_specification<int>#0 value$N) 1)) true)"
  $ test "$(./invariant_contract_formula_tool.exe boundary-check artifacts/exec.source.vir)" = "boundaries=12 unique-keys=12 unique-function-indices=12"; echo exec-proof-paths=four-functions-two-actual-exits-each
  exec-proof-paths=four-functions-two-actual-exits-each

The fixed abstention manifest classifies exactly 39 current families. Registry
rows retain phase-local construction authority, direct unchanged fallback, and
the real later counter rendering; validator rows make no fallback or session-
authority claim. Every non-static row records source/CMT evidence, the full CLI
outcome, headers/goals, and the complete ordered rendered-obligation key set.

  $ cat > artifacts/abstention-manifest <<'EOF'
  > imported-identity|validator-retained|validator-rejected|unsupported-or-stale-callable|carrier=abstention_imported_identity node=exact-direct-call transformation=classifier-only:unsupported-callable|abstention_imported_identity|0|1
  > external-identity|validator-retained|profile-preempted|external-or-trusted-root|carrier=abstention_external_identity node=actual-contract-profile phase=pre-admit transformation=none|abstention_external_identity|0|0
  > trusted-identity|validator-retained|profile-preempted|external-or-trusted-root|carrier=abstention_trusted_identity node=actual-contract-profile phase=pre-admit transformation=none|abstention_trusted_identity|2|none
  > symbolic-only-identity|validator-retained|validator-rejected|symbolic-only-callable|carrier=abstention node=exact-callback-call transformation=retarget-actual-symbolic-declaration|abstention|0|7
  > stale-callable-model|validator-retained|validator-rejected|stale-callable-descriptor+stale-model-identity|callable-carrier=abstention_imported_identity node=exact-direct-call transformation=stale-descriptor-identity;model-carrier=abstention_frozen_model fixture-sha256=a82f209c6535ebee30d8bb35fde96df5f3ee7dc60fb5124571143a3ce0389461 node=exact-captured-permit transformation=model_callable.function_name+"-stale"|abstention_imported_identity,abstention_frozen_model|0,0|1,7
  > recursion-cycle|registry-fallback|registry-fallback|recursive-callable|carrier=abstention_recursion_cycle node=actual-ensures-root transformation=none|abstention_recursion_cycle|2|none
  > exec-call|validator-retained|validator-rejected|exec-or-proof-callable|carrier=abstention_exec_call node=exact-exec-direct-call transformation=call-form-only:specification|abstention_exec_call|1|4
  > proof-call|validator-retained|validator-rejected|exec-or-proof-callable|carrier=abstention_proof_call node=exact-proof-direct-call transformation=call-form-only:specification|abstention_proof_call|0|1
  > callback-parameter-call|registry-fallback|registry-fallback|callback+callback-parameter|fallback-carrier=abstention node=actual-requires-and-ensures-callback-calls transformation=none;validator-carrier=abstention_exec_call node=exact-callback-node transformation=value-only-argument-projection|abstention,abstention_exec_call|0,1|7,4
  > lambda-higher-order|registry-fallback|registry-fallback|lambda-or-higher-order-application|carrier=abstention_lambda_higher_order node=actual-ensures-root transformation=none|abstention_lambda_higher_order|0|1
  > symbolic-application|registry-fallback|registry-fallback|symbolic-application|carrier=abstention node=actual-proof_region-requires-root transformation=none|abstention|0|7
  > quantifier-trigger|registry-fallback|registry-fallback|quantifier-or-trigger|carrier=abstention node=actual-quantified-ensures-root transformation=none|abstention|0|7
  > mutable-operation|validator-retained|validator-rejected|mutable-operation|evidence-carrier=abstention_imported_identity node=exact-variable transformation=Mutable_read;outcome-carrier=abstention_mutable_read|abstention_mutable_read|2|none
  > mutable-field-read|registry-fallback|registry-fallback|mutable-field-read-outside-captured-model|carrier=abstention node=actual-old_value-ensures-root transformation=none|abstention|0|7
  > field-write|validator-retained|validator-rejected|write-or-rebase|carrier=abstention_field_write node=exact-source-node transformation=none|abstention_field_write|0|0
  > shared-scalar-write|validator-retained|validator-rejected|write-or-rebase|carrier=abstention_shared_scalar_write node=exact-source-node transformation=none|abstention_shared_scalar_write|0|0
  > owned-tree-write|validator-retained|validator-rejected|write-or-rebase|carrier=abstention_owned_tree_write node=exact-source-node transformation=none|abstention_owned_tree_write|0|10
  > owned-tree-rebase|validator-retained|validator-rejected|write-or-rebase|carrier=abstention_owned_tree_rebase node=exact-source-node transformation=none|abstention_owned_tree_rebase|0|10
  > owned-cursor|validator-retained|validator-rejected|unsupported-match-pattern|carrier=abstention_owned_cursor node=exact-match transformation=scrutinee-only:owned-cursor|abstention_owned_cursor|0|11
  > frozen-model|validator-retained|validator-rejected|unsupported-or-stale-callable|carrier=abstention_frozen_model node=exact-invariant-model-call transformation=caller-supplied-exclusion|abstention_frozen_model|0|7
  > owned-model|validator-retained|validator-rejected|unsupported-or-stale-callable|carrier=abstention_owned_model node=exact-direct-call transformation=caller-supplied-exclusion|abstention_owned_model|2|none
  > shared-model|registry-fallback|registry-fallback|shared-invariant|carrier=abstention_shared_model node=actual-invariant-root transformation=none|abstention_shared_model|0|11
  > session-model|validator-retained|validator-rejected|unsupported-or-stale-callable|evidence-carrier=abstention_frozen_model node=exact-invariant-model-call transformation=generic-caller-supplied-exclusion;outcome-carrier=abstention_session_model|abstention_session_model|2|none
  > receipt-model|validator-retained|validator-rejected|unsupported-or-stale-callable|carrier=abstention_receipt_model node=exact-invariant-model-call transformation=caller-supplied-exclusion|abstention_receipt_model|2|none
  > reveal-fuel|validator-retained|validator-rejected|reveal-or-fuel|carrier=abstention_reveal_fuel node=exact-source-sequence transformation=none|abstention_reveal_fuel|0|0
  > invariant-use|validator-retained|validator-rejected|invariant-use|carrier=abstention_invariant_use node=exact-source-node transformation=none|abstention_invariant_use|0|4
  > assertion|validator-retained|validator-rejected|assertion|carrier=abstention_assertion node=exact-source-node transformation=none|abstention_assertion|0|1
  > proof-region|validator-retained|validator-rejected|proof-region|carrier=abstention_proof_region node=exact-source-node transformation=none|abstention_proof_region|0|1
  > old|registry-fallback|registry-fallback|old|carrier=abstention node=actual-old_integer-ensures-root transformation=none|abstention|0|7
  > finite-operation|validator-retained|validator-rejected|unsupported-or-stale-callable|carrier=abstention_finite_operation node=exact-recursive-spec-call transformation=caller-supplied-exclusion|abstention_finite_operation|2|none
  > receipt-operation|validator-retained|validator-rejected|unsupported-or-stale-callable|carrier=abstention_receipt_operation node=exact-invariant-model-call transformation=caller-supplied-exclusion|abstention_receipt_operation|2|none
  > transition-operation|validator-retained|validator-rejected|unsupported-or-stale-callable|evidence-carrier=abstention_frozen_model node=exact-invariant-model-call transformation=excluded-operation-validation;outcome-carrier=abstention_transition_operation|abstention_transition_operation|1|2
  > boundary-operation|validator-retained|validator-rejected|unsupported-or-stale-callable|carrier=abstention_boundary_operation node=exact-invariant-model-call transformation=caller-supplied-exclusion|abstention_boundary_operation|1|5
  > missing-aggregate|validator-retained|validator-rejected|missing-aggregate-descriptor|carrier=abstention_missing_aggregate_descriptor node=exact-record transformation=remove-aggregate-descriptor-list|abstention_missing_aggregate_descriptor|0|1
  > missing-option|validator-retained|validator-rejected|missing-option-descriptor|carrier=abstention_missing_option_descriptor node=exact-optional-forward transformation=result-type-to-non-option|abstention_missing_option_descriptor|0|0
  > missing-field|validator-retained|validator-rejected|missing-field-descriptor|carrier=abstention_missing_field_descriptor node=exact-field-read transformation=remove-field-descriptor-list|abstention_missing_field_descriptor|0|1
  > partial-if|validator-retained|validator-rejected|partial-if|carrier=abstention_partial_if node=exact-source-node transformation=none|abstention_partial_if|0|0
  > empty-match|validator-retained|validator-rejected|empty-match|evidence-carrier=abstention_owned_cursor node=exact-match transformation=remove-cases;outcome-carrier=abstention_non_total_match|abstention_non_total_match|2|none
  > malformed-sequence|validator-retained|validator-rejected|non-unit-sequence|evidence-carrier=abstention_imported_identity node=exact-non-unit-variable transformation=wrap-as-sequence;outcome-carrier=abstention_malformed_form|abstention_malformed_form|2|none
  > EOF
  $ accepted='imported-identity external-identity trusted-identity symbolic-only-identity stale-callable-model recursion-cycle exec-call proof-call callback-parameter-call lambda-higher-order symbolic-application quantifier-trigger mutable-operation mutable-field-read field-write shared-scalar-write owned-tree-write owned-tree-rebase owned-cursor frozen-model owned-model shared-model session-model receipt-model reveal-fuel invariant-use assertion proof-region old finite-operation receipt-operation transition-operation boundary-operation missing-aggregate missing-option missing-field partial-if empty-match malformed-sequence'; printf '%s\n' $accepted | sort > artifacts/accepted-families; cut -d '|' -f 1 artifacts/abstention-manifest | sort > artifacts/manifest-families; cmp artifacts/accepted-families artifacts/manifest-families; test "$(wc -l < artifacts/manifest-families)" -eq 39; test "$(sort -u artifacts/manifest-families | wc -l)" -eq 39; test "$(awk -F '|' '$2 == "registry-fallback" { count++ } END { print count+0 }' artifacts/abstention-manifest)" -eq 8; test "$(awk -F '|' '$2 == "validator-retained" { count++ } END { print count+0 }' artifacts/abstention-manifest)" -eq 31; test "$(awk -F '|' '$3 == "profile-preempted" { count++ } END { print count+0 }' artifacts/abstention-manifest)" -eq 2; test "$(awk -F '|' '$3 == "validator-rejected" { count++ } END { print count+0 }' artifacts/abstention-manifest)" -eq 29; echo abstention-manifest current=39 registry-fallback=8 validator-retained=31 profile-preempted=2 validator-rejected=29 static=1 total=40
  abstention-manifest current=39 registry-fallback=8 validator-retained=31 profile-preempted=2 validator-rejected=29 static=1 total=40

  $ carriers="abstention $(find fixtures -maxdepth 1 -name 'abstention_*.ml' -printf '%f\n' | sed 's/\.ml$//' | sort)"; test "$(printf '%s\n' $carriers | wc -l)" -eq 33
  $ for carrier in $carriers; do sha256sum "fixtures/$carrier.ml" | sed "s#  fixtures/# fixture=$carrier #"; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$carrier.cmo" "fixtures/$carrier.ml"; done > artifacts/carrier-hashes; test "$(grep -c '^a82f209c6535ebee30d8bb35fde96df5f3ee7dc60fb5124571143a3ce0389461 fixture=abstention_frozen_model ' artifacts/carrier-hashes)" -eq 1; echo carriers=33 retained-cmt=independently-compiled frozen-model-sha256=a82f209c6535ebee30d8bb35fde96df5f3ee7dc60fb5124571143a3ce0389461
  carriers=33 retained-cmt=independently-compiled frozen-model-sha256=a82f209c6535ebee30d8bb35fde96df5f3ee7dc60fb5124571143a3ce0389461
  $ for carrier in $carriers; do for route in source cmt; do if test "$route" = source; then input="fixtures/$carrier.ml"; else input="artifacts/$carrier.cmt"; fi; set +e; VEROCAML_TEST_FORMULA_TRACE=1 VEROCAML_TEST_FORMULA_VALIDATOR_MATRIX=1 OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 5000 --dump-sst "artifacts/$carrier.$route.sst" --dump-vir "artifacts/$carrier.$route.vir" > "artifacts/$carrier.$route.out" 2>&1; rc=$?; set -e; printf '%s\n' "$rc" > "artifacts/$carrier.$route.rc"; done; done

  $ validator_evidence () { family=$1; route=$2; suffix=".$route.out"; case "$family" in imported-identity) grep -m1 'candidate=unsupported-callable reason=unsupported-or-stale-callable' "artifacts/abstention_imported_identity$suffix";; external-identity) grep -m1 'formula profile .*outcome=abstain:external-or-trusted-root' "artifacts/abstention_external_identity$suffix";; trusted-identity) grep -m1 'formula profile .*outcome=abstain:external-or-trusted-root' "artifacts/abstention_trusted_identity$suffix";; symbolic-only-identity) grep -m1 'candidate=symbolic-callable reason=symbolic-only-callable' "artifacts/abstention$suffix";; stale-callable-model) { grep -m1 'candidate=stale-callable reason=stale-callable-descriptor' "artifacts/abstention_imported_identity$suffix"; grep -m1 'candidate=stale-model reason=stale-model-identity' "artifacts/abstention_frozen_model$suffix"; };; exec-call) grep -m1 'candidate=descriptor-call reason=exec-or-proof-callable' "artifacts/abstention_exec_call$suffix";; proof-call) grep -m1 'candidate=descriptor-call reason=exec-or-proof-callable' "artifacts/abstention_proof_call$suffix";; mutable-operation) grep -m1 'candidate=mutable-read reason=mutable-operation' "artifacts/abstention_imported_identity$suffix";; field-write) grep -m1 'expression_tag=field-write candidate=source reason=write-or-rebase' "artifacts/abstention_field_write$suffix";; shared-scalar-write) grep -m1 'expression_tag=shared-scalar-write candidate=source reason=write-or-rebase' "artifacts/abstention_shared_scalar_write$suffix";; owned-tree-write) grep -m1 'expression_tag=owned-tree-write candidate=source reason=write-or-rebase' "artifacts/abstention_owned_tree_write$suffix";; owned-tree-rebase) grep -m1 'expression_tag=owned-tree-rebase candidate=source reason=write-or-rebase' "artifacts/abstention_owned_tree_rebase$suffix";; owned-cursor) grep -m1 'candidate=owned-cursor-pattern reason=unsupported-match-pattern' "artifacts/abstention_owned_cursor$suffix";; frozen-model|session-model|transition-operation) grep -m1 'candidate=unsupported-callable reason=unsupported-or-stale-callable' "artifacts/abstention_frozen_model$suffix";; owned-model) grep -m1 'candidate=unsupported-callable reason=unsupported-or-stale-callable' "artifacts/abstention_owned_model$suffix";; receipt-model) grep -m1 'candidate=unsupported-callable reason=unsupported-or-stale-callable' "artifacts/abstention_receipt_model$suffix";; reveal-fuel) grep -m1 'reason=reveal-or-fuel' "artifacts/abstention_reveal_fuel$suffix";; invariant-use) grep -m1 'reason=invariant-use' "artifacts/abstention_invariant_use$suffix";; assertion) grep -m1 'reason=assertion' "artifacts/abstention_assertion$suffix";; proof-region) grep -m1 'reason=proof-region' "artifacts/abstention_proof_region$suffix";; finite-operation) grep -m1 'candidate=unsupported-callable reason=unsupported-or-stale-callable' "artifacts/abstention_finite_operation$suffix";; receipt-operation) grep -m1 'candidate=unsupported-callable reason=unsupported-or-stale-callable' "artifacts/abstention_receipt_operation$suffix";; boundary-operation) grep -m1 'candidate=unsupported-callable reason=unsupported-or-stale-callable' "artifacts/abstention_boundary_operation$suffix";; missing-aggregate) grep -m1 'candidate=missing-aggregate reason=missing-aggregate-descriptor' "artifacts/abstention_missing_aggregate_descriptor$suffix";; missing-option) grep -m1 'candidate=missing-option reason=missing-option-descriptor' "artifacts/abstention_missing_option_descriptor$suffix";; missing-field) grep -m1 'candidate=missing-field reason=missing-field-descriptor' "artifacts/abstention_missing_field_descriptor$suffix";; partial-if) grep -m1 'reason=partial-if' "artifacts/abstention_partial_if$suffix";; empty-match) grep -m1 'candidate=empty-match reason=empty-match' "artifacts/abstention_owned_cursor$suffix";; malformed-sequence) grep -m1 'candidate=non-unit-sequence reason=non-unit-sequence' "artifacts/abstention_imported_identity$suffix";; *) return 1;; esac; }
  $ registry_carrier () { case "$1" in recursion-cycle) echo abstention_recursion_cycle;; callback-parameter-call|symbolic-application|quantifier-trigger|mutable-field-read|old) echo abstention;; lambda-higher-order) echo abstention_lambda_higher_order;; shared-model) echo abstention_shared_model;; *) return 1;; esac; }; registry_reason () { case "$1" in recursion-cycle) echo recursive-callable;; callback-parameter-call) echo callback;; lambda-higher-order) echo lambda-or-higher-order-application;; symbolic-application) echo symbolic-application;; quantifier-trigger) echo quantifier-or-trigger;; mutable-field-read) echo mutable-field-read-outside-captured-model;; shared-model) echo shared-invariant;; old) echo old;; *) return 1;; esac; }
  $ channel_record () { carrier=$1; route=$2; expected_rc=$3; expected_boundaries=$4; actual_rc=$(cat "artifacts/$carrier.$route.rc"); test "$actual_rc" -eq "$expected_rc"; if test "$expected_boundaries" = none; then code=$(grep -Eo 'VERO_[A-Z_]+' "artifacts/$carrier.$route.out" | head -1); test -n "$code"; cli="diagnostic[$code]"; else cli=$(grep -m1 '^verocaml: ' "artifacts/$carrier.$route.out"); test -n "$cli"; fi; if test "$expected_boundaries" = none; then test ! -e "artifacts/$carrier.$route.sst"; test ! -e "artifacts/$carrier.$route.vir"; printf 'carrier=%s rc=%s cli={%s} sst=jointly-absent vir=jointly-absent boundary=none' "$carrier" "$actual_rc" "$cli"; else test -s "artifacts/$carrier.$route.sst"; test -e "artifacts/$carrier.$route.vir"; summary=$(./invariant_contract_formula_tool.exe vir "artifacts/$carrier.$route.vir" | paste -sd ';' -); boundaries=$(./invariant_contract_formula_tool.exe boundaries "artifacts/$carrier.$route.vir" | paste -sd ';' -); test "$(printf '%s\n' "$boundaries" | sed -E 's/.*boundaries=([0-9]+) unique-keys=.*/\1/')" -eq "$expected_boundaries"; printf 'carrier=%s rc=%s cli={%s} sst=present vir=present vir-summary={%s} boundary-keys={%s}' "$carrier" "$actual_rc" "$cli" "$summary" "$boundaries"; fi; }
  $ make_record () { family=$1; class=$2; mode=$3; reason=$4; candidate=$5; outcome_carriers=$6; expected_rcs=$7; expected_boundaries=$8; route=$9; if test "$class" = registry-fallback; then carrier=$(registry_carrier "$family"); registry_reason=$(registry_reason "$family"); test "$registry_reason" = "${reason%%+*}"; registry=$(grep -m1 'formula registry constructed authority_unchanged=true entries=' "artifacts/$carrier.$route.out"); profile=$(grep -E "formula profile .*outcome=abstain:$registry_reason$" "artifacts/$carrier.$route.out" | paste -sd ';' -); fallback=$(grep -E "formula abstention .*reason=$registry_reason " "artifacts/$carrier.$route.out" | sed -E 's/ authority_before=.* authority_before_fallback=/ authority_before_fallback=/; s/ authority_before_fallback=.* authority_equal=/ authority_equal=/; s/ authority_equal=(true|false)//' | paste -sd ';' -); test -n "$registry"; test -n "$profile"; test -n "$fallback"; printf '%s\n' "$fallback" | grep -q 'formula_events_before=0 formula_events_before_fallback=0 .*fallback_unchanged=true'; test "$(printf '%s\n' "$fallback" | grep -c 'authority_equal=\|authority_before=\|authority_before_fallback=')" -eq 0; evidence="registered={$profile} phase-local={$registry} direct-fallback={$fallback}"; if test "$family" = callback-parameter-call; then sub=$(grep -m1 'candidate=callback-parameter reason=callback-parameter' "artifacts/abstention_exec_call.$route.out"); test -n "$sub"; evidence="$evidence paired-validator-subobservation={$sub}"; fi; else evidence=$(validator_evidence "$family" "$route" | paste -sd ';' -); test -n "$evidence"; test "$(printf '%s\n' "$evidence" | grep -c 'formula abstention\|formula registry\|formula authority')" -eq 0; fi; carrier_count=$(printf '%s' "$outcome_carriers" | awk -F, '{print NF}'); index=1; channels=; while test "$index" -le "$carrier_count"; do carrier=$(printf '%s' "$outcome_carriers" | cut -d, -f "$index"); expected_rc=$(printf '%s' "$expected_rcs" | cut -d, -f "$index"); expected_boundary=$(printf '%s' "$expected_boundaries" | cut -d, -f "$index"); channel=$(channel_record "$carrier" "$route" "$expected_rc" "$expected_boundary"); if test -z "$channels"; then channels=$channel; else channels="$channels || $channel"; fi; index=$((index + 1)); done; printf 'family=%s route=<input> class=%s mode=%s reason=%s selected-candidate={%s} evidence={%s} outcome={%s}\n' "$family" "$class" "$mode" "$reason" "$candidate" "$evidence" "$channels"; }
  $ while IFS='|' read -r family class mode reason candidate outcome_carriers expected_rcs expected_boundaries; do for route in source cmt; do make_record "$family" "$class" "$mode" "$reason" "$candidate" "$outcome_carriers" "$expected_rcs" "$expected_boundaries" "$route" > "artifacts/$family.$route.record"; done; sed -E 's/file=[^ }]+/file=<input>/g; s/unit [^:}]+:/unit <input>:/g' "artifacts/$family.source.record" > "artifacts/$family.source.normalized"; sed -E 's/file=[^ }]+/file=<input>/g; s/unit [^:}]+:/unit <input>:/g' "artifacts/$family.cmt.record" > "artifacts/$family.cmt.normalized"; cmp "artifacts/$family.source.normalized" "artifacts/$family.cmt.normalized"; done < artifacts/abstention-manifest; echo 'abstention-records=39 routes=source+cmt normalized=equal'
  abstention-records=39 routes=source+cmt normalized=equal

  $ checked=0; for file in $(find artifacts -name '*.vir' -type f | sort); do ./invariant_contract_formula_tool.exe boundary-check "$file" >/dev/null; checked=$((checked + 1)); done; test "$checked" -gt 0; echo rendered-production-vir-boundary-files=$checked all-keys-unique global-scan=supplemental
  rendered-production-vir-boundary-files=68 all-keys-unique global-scan=supplemental
