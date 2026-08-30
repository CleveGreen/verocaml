Finite immutable results persist through exact lets and aliases, repeated uses,
copied constructions and results, projections, all-finite joins, two normal
exits, and ordinary completed direct-candidate summaries.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ for name in push_front_inductive_eq push_front_explicit_let push_front_summary_demand caller_transfer alias_and_two_calls two_finite_exits branch_all_paths direct_nested_construction_cycle direct_record_nested_cycle direct_structural_self_composition direct_self_branch_partial direct_self_double_use direct_self_dropped_result direct_self_projection_only result_copy result_rebind failed_obligation_no_publication direct_self_nondecreasing_actual direct_self_missing_base arbitrary_unreceipted result_widen direct_structural_nonchild direct_self_mutable_wrapper direct_self_shared_wrapper mutable_result_route shared_result_route trusted_result_route external_result_route partial_result_route higher_order_result_route direct_result_cycle indirect_result_cycle mutual_nested_construction_barrier; do retained "$name"; done
  $ for name in push_front_inductive_eq push_front_explicit_let push_front_summary_demand caller_transfer alias_and_two_calls two_finite_exits branch_all_paths direct_nested_construction_cycle direct_record_nested_cycle direct_structural_self_composition direct_self_branch_partial direct_self_double_use direct_self_dropped_result direct_self_projection_only result_copy result_rebind; do ./finite_result_promotion_tool.exe "artifacts/$name.cmt"; done
  status=verified functions=4 obligations=40 promotion/path=5/1 lifecycle=1/1/1 consumption=3/3
  status=verified functions=4 obligations=40 promotion/path=5/1 lifecycle=1/1/1 consumption=3/3
  status=verified functions=3 obligations=15 promotion/path=5/1 lifecycle=1/1/1 consumption=3/3
  status=verified functions=3 obligations=5 promotion/path=1/1 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=4 promotion/path=1/1 lifecycle=1/1/1 consumption=2/2
  status=verified functions=3 obligations=4 promotion/path=2/2 lifecycle=1/1/1 consumption=1/1
  status=verified functions=4 obligations=0 promotion/path=1/1 lifecycle=1/1/1 consumption=2/2
  status=verified functions=3 obligations=11 promotion/path=2/2 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=11 promotion/path=2/2 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=7 promotion/path=2/2 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=7 promotion/path=3/3 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=7 promotion/path=2/2 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=7 promotion/path=2/2 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=7 promotion/path=3/3 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=0 promotion/path=1/1 lifecycle=1/1/1 consumption=1/1
  status=verified functions=3 obligations=0 promotion/path=1/1 lifecycle=1/1/1 consumption=1/1

A failed source obligation and a non-strict descent record exits and authorize a
singular candidate, but neither completes, publishes, attempts consumption, nor
mints caller evidence.

  $ ./finite_result_promotion_tool.exe artifacts/failed_obligation_no_publication.cmt
  status=inconclusive functions=2 obligations=11 promotion/path=2/2 lifecycle=1/0/0 consumption=0/0
  $ ./finite_result_promotion_tool.exe artifacts/direct_self_nondecreasing_actual.cmt
  status=counterexample functions=2 obligations=4 promotion/path=2/2 lifecycle=1/0/0 consumption=0/0

A direct recursive nonfinite exit, an unreceipted actual, a transformed actual,
and a non-child structural descent reject before publication and ordinary
consumption.

  $ for item in missing-base:direct_self_missing_base unreceipted:arbitrary_unreceipted transformed:result_widen nonchild:direct_structural_nonchild; do kind=${item%%:*}; name=${item#*:}; ./finite_result_promotion_tool.exe "artifacts/$name.cmt" > "$kind.err" 2>&1; test $? = 3; echo "$kind=production-rejected"; done
  missing-base=production-rejected
  unreceipted=production-rejected
  transformed=production-rejected
  nonchild=production-rejected
  $ grep -F "finite-expression feasible result branch rejected" missing-base.err >/dev/null
  $ grep -F "exact finite receipt is unavailable for a required call actual" unreceipted.err >/dev/null
  $ grep -F "exact finite receipt is unavailable for a required call actual" transformed.err >/dev/null
  $ grep -F "structural recursion must select an authenticated immediate child" nonchild.err >/dev/null

Exact caller, path, call-site, and result evidence mismatches remain rejected,
while a distinct path may consume fresh evidence.

  $ ./finite_result_promotion_tool.exe matrix
  finite-result-call-instance=distinct-path copied-symbol=true
  finite-result-call-instance=same-path-replay result=rejected
  finite-result-call-instance=same-path-double-consume result=rejected
  finite-result-call-instance=sibling-path-substitution result=rejected
  finite-result-call-instance=wrong-caller result=rejected
  finite-result-call-instance=wrong-call-site result=rejected
  finite-result-call-instance=wrong-result result=rejected
  finite-result-call-instance=rejected-downstream-work value=0

Mutable and shared children/results and trusted results cannot manufacture
finite authority.

  $ for item in mutable-child:direct_self_mutable_wrapper shared-child:direct_self_shared_wrapper mutable-result:mutable_result_route shared-result:shared_result_route trusted-result:trusted_result_route; do kind=${item%%:*}; name=${item#*:}; ./finite_result_promotion_tool.exe "artifacts/$name.cmt" > "$kind.err" 2>&1; test $? = 3; echo "$kind=production-rejected"; done
  mutable-child=production-rejected
  shared-child=production-rejected
  mutable-result=production-rejected
  shared-result=production-rejected
  trusted-result=production-rejected

External, partial, higher-order, runtime-cycle, and mutual-recursive sources
stop at their canonical frontend barriers, before finite induction or
publication. Mutual recursion remains an explicit production negative.

  $ for item in external:external_result_route partial:partial_result_route higher-order:higher_order_result_route runtime-cycle:direct_result_cycle indirect-cycle:indirect_result_cycle mutual:mutual_nested_construction_barrier; do kind=${item%%:*}; name=${item#*:}; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --timeout-ms 5000 > "$kind.frontend" 2>&1; test $? = 2; echo "$kind=frontend-rejected"; done
  external=frontend-rejected
  partial=frontend-rejected
  higher-order=frontend-rejected
  runtime-cycle=frontend-rejected
  indirect-cycle=frontend-rejected
  mutual=frontend-rejected
  $ grep -F "VERO_MALFORMED_GHOST_CALL" external.frontend >/dev/null
  $ grep -F "VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION" partial.frontend >/dev/null
  $ grep -F "VERO_CALLBACK_CONTRACT" higher-order.frontend >/dev/null
  $ grep -F "VERO_UNSUPPORTED_TOP_LEVEL_BINDING" runtime-cycle.frontend >/dev/null
  $ grep -F "VERO_UNSUPPORTED_MUTUAL_RECURSION" indirect-cycle.frontend >/dev/null
  $ grep -F "VERO_UNSUPPORTED_MUTUAL_RECURSION" mutual.frontend >/dev/null

Raw and retained/imported sources remain outside publication authority.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/raw_result_route.cmo fixtures/raw_result_route.ml > raw.err 2>&1; test $? = 2; grep -F "requires at least one ensures clause" raw.err >/dev/null; echo raw=ppx-rejected
  raw=ppx-rejected
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained_result_provider.cmi fixtures/retained_result_provider.mli
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained_result_provider.cmo fixtures/retained_result_provider.ml
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained_result_consumer.cmo fixtures/retained_result_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained_result_consumer.cmt --timeout-ms 5000 > retained.err 2>&1; test $? = 2; grep -F "VERO_UNSUPPORTED_TYPE" retained.err >/dev/null; echo retained-imported=frontend-rejected
  retained-imported=frontend-rejected
