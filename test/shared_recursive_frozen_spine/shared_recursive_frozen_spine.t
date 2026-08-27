The exact two-node frozen-spine program verifies from retained CMT.  It uses
the recursive/direct-Z3 route for model totality and the ordinary shared heap
for the payload update; generic finite-formal issuance stays absent.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained exact_pfc
  $ ./shared_recursive_frozen_spine_tool.exe verify artifacts/exact_pfc.cmt
  status=verified functions=4 obligations=20 shared=2/2/2/2 invariant=2/2/2/1/2 recursive-results=13/0 generic-finite=0/0/0 frozen-template=1/1 frozen-conditional=3/3 frozen-instance=0/0 frozen-discharge=0/0/0 frozen-witness=0/0/0 frozen-observation=0/0
  $ ./shared_recursive_frozen_spine_tool.exe route artifacts/exact_pfc.cmt
  ordinary=3 direct-z3=20 recursive=14

The authenticated SST names the complete frozen family.  Mutating its edge
identity fails independent validation.

  $ ./shared_recursive_frozen_spine_tool.exe dump-sst artifacts/exact_pfc.cmt > artifacts/exact.sst
  $ grep 'frozen-spine root=' artifacts/exact.sst | sed -E 's/ @ .*//'
      frozen-spine root=List.node#1 link=List.link#2 payload=type-List.node#1.value#0 edge=type-List.node#1.next#1 helper=List.contents_node#1 model=List.contents#2 result=seq#0 invariant=List.invariant#3 constructor=List.make_two#4 mutator=List.set_head#5 terminal=List.head#6
  $ ./shared_recursive_frozen_spine_tool.exe forge artifacts/exact_pfc.cmt
  forged-descriptor: rejected
  $ ./shared_recursive_frozen_spine_tool.exe raw-constructor artifacts/exact_pfc.cmt
  raw-constructor: rejected

The exact recursive call must use the selected direct child.  A nondecreasing
replay and invariant-only laundering reject before verification.

  $ sed 's/contents_node next/contents_node node/' fixtures/exact_pfc.ml > artifacts/nondecreasing.ml
  $ ../../src/verocaml.exe verify artifacts/nondecreasing.ml > artifacts/nondecreasing.out 2>&1; test $? -eq 2
  $ grep -o 'frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar' artifacts/nondecreasing.out
  frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nondecreasing.cmo artifacts/nondecreasing.ml
  $ ./shared_recursive_frozen_spine_tool.exe reject-boundary artifacts/nondecreasing.cmt
  rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  $ sed 's/match contents node with End -> false | More _ -> true/true/' fixtures/exact_pfc.ml > artifacts/invariant_launder.ml
  $ ../../src/verocaml.exe verify artifacts/invariant_launder.ml > artifacts/invariant.out 2>&1; test $? -eq 2
  $ grep -o 'frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar' artifacts/invariant.out
  frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/invariant_launder.cmo artifacts/invariant_launder.ml
  $ ./shared_recursive_frozen_spine_tool.exe reject-boundary artifacts/invariant_launder.cmt
  rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0

Wrong writes, deleted writes, and wrong recursive tails are concrete
postcondition failures.  Suppressing the private child/root witness also
prevents tail preservation.

  $ sed 's/node.value <- value$/node.value <- value + 1/' fixtures/exact_pfc.ml > artifacts/wrong_write.ml
  $ ../../src/verocaml.exe verify artifacts/wrong_write.ml > artifacts/wrong_write.out 2>&1; test $? -eq 1
  $ grep -o 'counterexample function=List.set_head#[0-9]* vc=arithmetic-safety-upper' artifacts/wrong_write.out
  counterexample function=List.set_head#5 vc=arithmetic-safety-upper
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/wrong_write.cmo artifacts/wrong_write.ml
  $ ./shared_recursive_frozen_spine_tool.exe verify artifacts/wrong_write.cmt
  status=counterexample functions=3 obligations=20 shared=1/1/1/1 invariant=1/1/0/0/0 recursive-results=9/0 generic-finite=0/0/0 frozen-template=1/1 frozen-conditional=2/2 frozen-instance=0/0 frozen-discharge=0/0/0 frozen-witness=0/0/0 frozen-observation=0/0
  $ sed 's/node.value <- value$/let _ = value in ()/' fixtures/exact_pfc.ml > artifacts/delete_write.ml
  $ ../../src/verocaml.exe verify artifacts/delete_write.ml > artifacts/delete_write.out 2>&1; test $? -eq 2
  $ grep -o 'polymorphic functions are not supported' artifacts/delete_write.out
  polymorphic functions are not supported
  $ cp fixtures/exact_pfc.ml artifacts/wrong_tail.ml
  $ sed -i '47c\\      contents node = More (value, End)];' artifacts/wrong_tail.ml
  $ ../../src/verocaml.exe verify artifacts/wrong_tail.ml > artifacts/wrong_tail.out 2>&1; test $? -eq 1
  $ grep -o 'counterexample function=List.set_head#[0-9]* vc=postcondition\[[0-9]*\]' artifacts/wrong_tail.out
  counterexample function=List.set_head#5 vc=postcondition[0]
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/wrong_tail.cmo artifacts/wrong_tail.ml
  $ ./shared_recursive_frozen_spine_tool.exe verify artifacts/wrong_tail.cmt
  status=counterexample functions=3 obligations=18 shared=1/1/1/1 invariant=1/1/0/0/0 recursive-results=8/0 generic-finite=0/0/0 frozen-template=1/1 frozen-conditional=2/2 frozen-instance=0/0 frozen-discharge=0/0/0 frozen-witness=0/0/0 frozen-observation=0/0
  $ ./shared_recursive_frozen_spine_tool.exe dump-vir artifacts/wrong_tail.cmt > artifacts/wrong_tail.vir
  $ ! grep '^function replace_head_and_read#' artifacts/wrong_tail.vir
  $ ./shared_recursive_frozen_spine_tool.exe suppress-witness artifacts/exact_pfc.cmt
  witness-suppressed=counterexample

Independent finite aliased formals retain possible equality.  Mutating one and
claiming the other's complete recursive model is unchanged is therefore a
concrete counterexample from both source and retained CMT; no local alias is
treated as a disequality.

  $ cat fixtures/exact_pfc.ml > artifacts/invalid_alias.ml
  $ printf '%s\n' '' 'let invalid_independent_alias' '    (x : (List.t [@finite]) @ aliased)' '    (y : (List.t [@finite]) @ aliased) value : unit =' '  [%verocaml.requires value <> List.head x];' '  [%verocaml.ensures fun _ ->' '    List.contents y = [%verocaml.old (List.contents y)]];' '  List.set_head x value' >> artifacts/invalid_alias.ml
  $ printf '%s\n' '' 'let invoke_same_actual value : unit =' '  [%verocaml.requires value <> 1];' '  let xs = List.make_two 1 2 in' '  invalid_independent_alias xs xs value' >> artifacts/invalid_alias.ml
  $ ../../src/verocaml.exe verify artifacts/invalid_alias.ml > artifacts/invalid_alias.source.out 2>&1; test $? -eq 1
  $ grep -o 'counterexample function=invalid_independent_alias#[0-9]* vc=postcondition\[[0-9]*\]' artifacts/invalid_alias.source.out
  counterexample function=invalid_independent_alias#8 vc=postcondition[0]
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/invalid_alias.cmo artifacts/invalid_alias.ml
  $ ./shared_recursive_frozen_spine_tool.exe verify artifacts/invalid_alias.cmt
  status=counterexample functions=5 obligations=21 shared=3/3/3/3 invariant=3/3/3/2/3 recursive-results=17/0 generic-finite=0/0/0 frozen-template=1/1 frozen-conditional=5/5 frozen-instance=0/0 frozen-discharge=0/0/0 frozen-witness=0/0/0 frozen-observation=0/0

The conditional proof scope seals the exact validated observation call chain
and symbolic path without becoming actual value authority.  Changing either
binding, or replaying one sealed permit, rejects identically from source and
retained CMT while concrete instance/discharge/observation/descent counters
stay zero.

  $ attack () { route=$1; input=$2; kind=$3; out="artifacts/observation-$route-$kind.out"; VEROCAML_TEST_FROZEN_OBSERVATION_ATTACK="$kind" VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --timeout-ms 10000 >"$out" 2>&1; test $? -eq 2; printf '%s-%s: ' "$route" "$kind"; grep -o 'frozen-spine observation permit call/path mismatch\|frozen-spine observation permit was replayed' "$out"; grep 'private-receipt destroy' "$out" | sed -E 's#.*frozen-template=([0-9]+/[0-9]+) frozen-conditional=([0-9]+/[0-9]+) frozen-instance=([0-9]+/[0-9]+) frozen-discharge=([0-9]+/[0-9]+/[0-9]+) frozen-witness=([0-9]+/[0-9]+/[0-9]+) frozen-observation=([0-9]+/[0-9]+).*#  authority template=\1 conditional=\2 instance=\3 discharge=\4 witness=\5 observation=\6#'; }
  $ for kind in call path replay; do attack source fixtures/exact_pfc.ml "$kind"; done
  source-call: frozen-spine observation permit call/path mismatch
    authority template=1/1 conditional=2/2 instance=0/0 discharge=0/0/0 witness=0/0/0 observation=0/0
  source-path: frozen-spine observation permit call/path mismatch
    authority template=1/1 conditional=2/2 instance=0/0 discharge=0/0/0 witness=0/0/0 observation=0/0
  source-replay: frozen-spine observation permit was replayed
    authority template=1/1 conditional=2/2 instance=0/0 discharge=0/0/0 witness=0/0/0 observation=0/0
  $ for kind in call path replay; do attack cmt artifacts/exact_pfc.cmt "$kind"; done
  cmt-call: frozen-spine observation permit call/path mismatch
    authority template=1/1 conditional=2/2 instance=0/0 discharge=0/0/0 witness=0/0/0 observation=0/0
  cmt-path: frozen-spine observation permit call/path mismatch
    authority template=1/1 conditional=2/2 instance=0/0 discharge=0/0/0 witness=0/0/0 observation=0/0
  cmt-replay: frozen-spine observation permit was replayed
    authority template=1/1 conditional=2/2 instance=0/0 discharge=0/0/0 witness=0/0/0 observation=0/0

The constructor proof is a template, not value authority.  The bare-formal
PFC above has zero concrete instances and discharges.  A closed client creates
one exact result instance and consumes one replace-call discharge for the same
root through an exact alias chain, from both source and retained CMT.

  $ cat fixtures/exact_pfc.ml > artifacts/closed_origin.ml
  $ printf '%s\n' '' 'let closed_origin value : int =' '  [%verocaml.requires value <> 1];' '  [%verocaml.ensures fun result -> result = value];' '  let xs = List.make_two 1 2 in' '  let alias = xs in' '  let peer = alias in' '  replace_head_and_read peer value' >> artifacts/closed_origin.ml
  $ VEROCAML_TEST_FROZEN_ORIGIN_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/closed_origin.ml --timeout-ms 10000 > artifacts/closed_origin.source.out 2>&1
  $ grep '^frozen-origin instance caller=closed_origin' artifacts/closed_origin.source.out | sed -E 's/#[0-9]+/#N/'
  frozen-origin instance caller=closed_origin callee=List.make_two root=List.make_two.result#N epoch=0
  $ grep '^frozen-origin discharge caller=closed_origin callee=replace_head_and_read' artifacts/closed_origin.source.out | sed -E 's/#[0-9]+/#N/' | sort -u
  frozen-origin discharge caller=closed_origin callee=replace_head_and_read formal=xs ordinal=0 root=List.make_two.result#N epoch=0
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/closed_origin.cmo artifacts/closed_origin.ml
  $ VEROCAML_TEST_FROZEN_ORIGIN_TRACE=1 ./shared_recursive_frozen_spine_tool.exe verify artifacts/closed_origin.cmt > artifacts/closed_origin.cmt.out 2>&1
  $ grep '^status=' artifacts/closed_origin.cmt.out
  status=verified functions=5 obligations=24 shared=2/2/2/2 invariant=2/2/2/1/3 recursive-results=16/0 generic-finite=0/0/0 frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=8/8/8 frozen-witness=3/3/3 frozen-observation=3/3
  $ grep '^frozen-origin discharge caller=closed_origin callee=replace_head_and_read' artifacts/closed_origin.cmt.out | sed -E 's/#[0-9]+/#N/' | sort -u
  frozen-origin discharge caller=closed_origin callee=replace_head_and_read formal=xs ordinal=0 root=List.make_two.result#N epoch=0

One persistent structural instance survives supported scalar writes.  The two
direct mutator calls consume distinct current-epoch discharges while the exact
alias and terminal read retain the same aggregate root.

  $ cat fixtures/exact_pfc.ml > artifacts/closed_sequential.ml
  $ printf '%s\n' '' 'let closed_sequential first second : int =' '  [%verocaml.requires first <> 1 && second <> first];' '  [%verocaml.ensures fun result -> result = second];' '  let xs = List.make_two 1 2 in' '  let alias = xs in' '  List.set_head alias first;' '  List.set_head xs second;' '  List.head alias' >> artifacts/closed_sequential.ml
  $ VEROCAML_TEST_FROZEN_ORIGIN_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/closed_sequential.ml --timeout-ms 10000 > artifacts/closed_sequential.source.out 2>&1
  $ test "$(grep -c '^frozen-origin instance caller=closed_sequential' artifacts/closed_sequential.source.out)" -eq 1; echo source-result-instances=1
  source-result-instances=1
  $ grep '^frozen-origin discharge caller=closed_sequential callee=List.set_head' artifacts/closed_sequential.source.out | sed -E 's/#[0-9]+/#N/' | sort -u
  frozen-origin discharge caller=closed_sequential callee=List.set_head formal=node ordinal=0 root=List.make_two.result#N epoch=0
  frozen-origin discharge caller=closed_sequential callee=List.set_head formal=node ordinal=0 root=List.make_two.result#N epoch=1
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/closed_sequential.cmo artifacts/closed_sequential.ml
  $ VEROCAML_TEST_FROZEN_ORIGIN_TRACE=1 ./shared_recursive_frozen_spine_tool.exe verify artifacts/closed_sequential.cmt > artifacts/closed_sequential.cmt.out 2>&1
  $ grep '^status=' artifacts/closed_sequential.cmt.out
  status=verified functions=5 obligations=22 shared=3/4/4/3 invariant=4/4/4/3/3 recursive-results=18/0 generic-finite=0/0/0 frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=13/13/13 frozen-witness=5/5/5 frozen-observation=5/5
  $ test "$(grep -c '^frozen-origin instance caller=closed_sequential' artifacts/closed_sequential.cmt.out)" -eq 1; echo cmt-result-instances=1
  cmt-result-instances=1
  $ grep '^frozen-origin discharge caller=closed_sequential callee=List.set_head' artifacts/closed_sequential.cmt.out | sed -E 's/#[0-9]+/#N/' | sort -u
  frozen-origin discharge caller=closed_sequential callee=List.set_head formal=node ordinal=0 root=List.make_two.result#N epoch=0
  frozen-origin discharge caller=closed_sequential callee=List.set_head formal=node ordinal=0 root=List.make_two.result#N epoch=1

Two constructor calls issue distinct root instances.  Only the selected first
root discharges the mutator call.

  $ cat fixtures/exact_pfc.ml > artifacts/distinct_results.ml
  $ printf '%s\n' '' 'let select_first value : unit =' '  let first = List.make_two 1 2 in' '  let _second = List.make_two 3 4 in' '  List.set_head first value' >> artifacts/distinct_results.ml
  $ VEROCAML_TEST_FROZEN_ORIGIN_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/distinct_results.ml --timeout-ms 10000 > artifacts/distinct_results.source.out 2>&1
  $ grep '^frozen-origin instance caller=select_first' artifacts/distinct_results.source.out | sed -E 's/#[0-9]+/#N/'
  frozen-origin instance caller=select_first callee=List.make_two root=List.make_two.result#N epoch=0
  frozen-origin instance caller=select_first callee=List.make_two root=List.make_two.result#N epoch=0
  $ test "$(grep '^frozen-origin instance caller=select_first' artifacts/distinct_results.source.out | sed -E 's/.*result#([0-9]+).*/\1/' | sort -u | wc -l)" -eq 2; echo distinct-source-roots=2
  distinct-source-roots=2
  $ grep '^frozen-origin discharge caller=select_first callee=List.set_head' artifacts/distinct_results.source.out | sed -E 's/#[0-9]+/#N/' | sort -u
  frozen-origin discharge caller=select_first callee=List.set_head formal=node ordinal=0 root=List.make_two.result#N epoch=0
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/distinct_results.cmo artifacts/distinct_results.ml
  $ VEROCAML_TEST_FROZEN_ORIGIN_TRACE=1 ./shared_recursive_frozen_spine_tool.exe verify artifacts/distinct_results.cmt > artifacts/distinct_results.cmt.out 2>&1
  $ grep '^status=' artifacts/distinct_results.cmt.out
  status=verified functions=5 obligations=22 shared=3/3/3/3 invariant=3/3/3/2/2 recursive-results=17/0 generic-finite=0/0/0 frozen-template=1/1 frozen-conditional=3/3 frozen-instance=2/2 frozen-discharge=9/9/9 frozen-witness=4/4/4 frozen-observation=4/4
  $ test "$(grep '^frozen-origin instance caller=select_first' artifacts/distinct_results.cmt.out | sed -E 's/.*result#([0-9]+).*/\1/' | sort -u | wc -l)" -eq 2; echo distinct-cmt-roots=2
  distinct-cmt-roots=2

An unannotated, unconnected aggregate formal cannot complete the concrete
replace invocation.  It creates no actual instance, discharge, observation,
or descent authority on either production route.

  $ cat fixtures/exact_pfc.ml > artifacts/unconnected.ml
  $ printf '%s\n' '' 'let invoke_unconnected (xs : List.t @ aliased) value : int =' '  [%verocaml.requires value <> List.head xs];' '  replace_head_and_read xs value' >> artifacts/unconnected.ml
  $ authority () { grep 'private-receipt destroy' "$1" | grep -oE 'frozen-(template|conditional|instance|discharge|witness|observation)=[0-9/]+' | paste -sd' ' -; }
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/unconnected.ml --timeout-ms 10000 > artifacts/unconnected.source.out 2>&1; test $? -eq 2
  $ grep -o 'frozen-spine call has no exact constructor-result instance' artifacts/unconnected.source.out
  frozen-spine call has no exact constructor-result instance
  $ authority artifacts/unconnected.source.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=0/0 frozen-discharge=0/0/0 frozen-witness=0/0/0 frozen-observation=0/0
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/unconnected.cmo artifacts/unconnected.ml
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/unconnected.cmt --timeout-ms 10000 > artifacts/unconnected.cmt.out 2>&1; test $? -eq 2
  $ grep -o 'frozen-spine call has no exact constructor-result instance' artifacts/unconnected.cmt.out
  frozen-spine call has no exact constructor-result instance
  $ authority artifacts/unconnected.cmt.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=0/0 frozen-discharge=0/0/0 frozen-witness=0/0/0 frozen-observation=0/0

The call-discharge authenticator rejects every exact identity mutation,
same-type/wrong-root origin, copied or stale instance, replayed instance,
ambiguous origin, and copied/replayed permit from source and retained CMT.
Pre-discharge attacks add no discharge/effect/backend/solver work beyond the
constructor-call baseline; permit copy/replay adds no downstream work.

  $ origin_attack () { route=$1; input=$2; kind=$3; discharge=$4; out="artifacts/origin-$route-$kind.out"; VEROCAML_TEST_FROZEN_ORIGIN_ATTACK="$kind" VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --timeout-ms 10000 >"$out" 2>&1; test $? -eq 2; grep -q 'frozen-spine' "$out"; grep 'private-receipt destroy' "$out" | grep -q "dependent-lowerings=1 dependent-backends=0 dependent-solver-attempts=0.*shared-heap=2/2/18/2/2.*frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=$discharge frozen-witness=1/1/1 frozen-observation=1/1"; }
  $ field_attacks='actual formal ordinal caller callee call span path path-condition epoch body cmt session wrong-root unrelated-producer wrong-constructor instance-copy instance-stale'
  $ for kind in $field_attacks instance-replay ambiguous-origin; do origin_attack source artifacts/closed_origin.ml "$kind" 2/2/2; done; echo source-pre-discharge-attacks=20
  source-pre-discharge-attacks=20
  $ authority artifacts/origin-source-actual.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=2/2/2 frozen-witness=1/1/1 frozen-observation=1/1
  $ for kind in $field_attacks instance-replay ambiguous-origin; do origin_attack cmt artifacts/closed_origin.cmt "$kind" 2/2/2; done; echo cmt-pre-discharge-attacks=20
  cmt-pre-discharge-attacks=20
  $ authority artifacts/origin-cmt-actual.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=2/2/2 frozen-witness=1/1/1 frozen-observation=1/1
  $ origin_attack source artifacts/closed_origin.ml permit-copy 3/2/3; authority artifacts/origin-source-permit-copy.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=3/2/3 frozen-witness=1/1/1 frozen-observation=1/1
  $ origin_attack source artifacts/closed_origin.ml permit-replay 3/3/3; authority artifacts/origin-source-permit-replay.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=3/3/3 frozen-witness=1/1/1 frozen-observation=1/1
  $ origin_attack cmt artifacts/closed_origin.cmt permit-copy 3/2/3; authority artifacts/origin-cmt-permit-copy.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=3/2/3 frozen-witness=1/1/1 frozen-observation=1/1
  $ origin_attack cmt artifacts/closed_origin.cmt permit-replay 3/3/3; authority artifacts/origin-cmt-permit-replay.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=3/3/3 frozen-witness=1/1/1 frozen-observation=1/1

Forwarded, returned, stored, external/trusted, and cross-CMT results cannot
carry the private constructor instance.  The rejection occurs before the
consumer call issues a discharge or applies its effect, from source and
retained CMT.

  $ cat fixtures/exact_pfc.ml > artifacts/forwarded.ml
  $ printf '%s\n' '' 'let forward_make a b = List.make_two a b' '' 'let consume_forwarded value : int =' '  [%verocaml.requires value <> 1];' '  let xs = forward_make 1 2 in' '  replace_head_and_read xs value' >> artifacts/forwarded.ml
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/forwarded.ml --timeout-ms 10000 > artifacts/forwarded.source.out 2>&1; test $? -eq 2
  $ grep -o 'frozen-spine call has no exact constructor-result instance' artifacts/forwarded.source.out
  frozen-spine call has no exact constructor-result instance
  $ authority artifacts/forwarded.source.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=2/2/2 frozen-witness=1/1/1 frozen-observation=1/1
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/forwarded.cmo artifacts/forwarded.ml
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/forwarded.cmt --timeout-ms 10000 > artifacts/forwarded.cmt.out 2>&1; test $? -eq 2
  $ grep -o 'frozen-spine call has no exact constructor-result instance' artifacts/forwarded.cmt.out
  frozen-spine call has no exact constructor-result instance
  $ authority artifacts/forwarded.cmt.out
  frozen-template=1/1 frozen-conditional=3/3 frozen-instance=1/1 frozen-discharge=2/2/2 frozen-witness=1/1/1 frozen-observation=1/1

  $ cat fixtures/exact_pfc.ml > artifacts/stored.ml
  $ printf '%s\n' '' 'type holder = { root : List.t }' '' 'let consume_stored value : int =' '  [%verocaml.requires value <> 1];' '  let box = { root = List.make_two 1 2 } in' '  replace_head_and_read box.root value' >> artifacts/stored.ml
  $ for route in source cmt; do if test "$route" = cmt; then ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/stored.cmo artifacts/stored.ml; input=artifacts/stored.cmt; else input=artifacts/stored.ml; fi; VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --timeout-ms 10000 > "artifacts/stored.$route.out" 2>&1; test $? -eq 2; grep -q 'frozen-spine call has no exact constructor-result instance' "artifacts/stored.$route.out"; done; echo stored-source-cmt=rejected
  stored-source-cmt=rejected

  $ cat fixtures/exact_pfc.ml > artifacts/external_producer.ml
  $ printf '%s\n' '' 'let external_make a b : List.t @ unique =' '  [%verocaml.ensures fun result ->' '    List.contents result = More (a, More (b, End))];' '  List.make_two a b' '[@@verocaml.external_body]' '' 'let consume_external value : int =' '  [%verocaml.requires value <> 1];' '  let xs = external_make 1 2 in' '  replace_head_and_read xs value' >> artifacts/external_producer.ml
  $ ../../src/verocaml.exe verify artifacts/external_producer.ml --timeout-ms 10000 > artifacts/external.source.out 2>&1; test $? -eq 2
  $ grep -o 'imported, external, or trusted result cannot be a receipt dependency' artifacts/external.source.out
  imported, external, or trusted result cannot be a receipt dependency
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/external_producer.cmo artifacts/external_producer.ml
  $ ../../src/verocaml.exe verify artifacts/external_producer.cmt --timeout-ms 10000 > artifacts/external.cmt.out 2>&1; test $? -eq 2
  $ grep -o 'imported, external, or trusted result cannot be a receipt dependency' artifacts/external.cmt.out
  imported, external, or trusted result cannot be a receipt dependency

  $ mkdir artifacts/import
  $ cp fixtures/exact_pfc.ml artifacts/import/provider.ml
  $ printf '%s\n' 'let consume value : int =' '  [%verocaml.requires value <> 1];' '  let xs = Provider.List.make_two 1 2 in' '  Provider.replace_head_and_read xs value' > artifacts/import/consumer.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/import/provider.cmo artifacts/import/provider.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/import -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/import/consumer.cmo artifacts/import/consumer.ml
  $ ../../src/verocaml.exe verify artifacts/import/consumer.ml --dependency artifacts/import/provider.cmt --timeout-ms 10000 > artifacts/import/source.out 2>&1; test $? -eq 2
  $ grep -o 'embedded public type List.t has no exact verified descriptor' artifacts/import/source.out
  embedded public type List.t has no exact verified descriptor
  $ ../../src/verocaml.exe verify artifacts/import/consumer.cmt --dependency artifacts/import/provider.cmt --timeout-ms 10000 > artifacts/import/cmt.out 2>&1; test $? -eq 2
  $ grep -o 'embedded public type List.t has no exact verified descriptor' artifacts/import/cmt.out
  embedded public type List.t has no exact verified descriptor

Constructor, model-effect, and postcondition failures do not finalize a
constructor template, a successful operation close/effect, or dependent caller
VIR.

Removing only the required exact constructor contents postcondition rejects
from source and retained CMT before any frozen/shared/invariant/VIR/backend
work.

  $ sed '41,42d' fixtures/exact_pfc.ml > artifacts/no_constructor_postcondition.ml
  $ sha256sum artifacts/no_constructor_postcondition.ml | cut -d' ' -f1
  4c319c08c7c7d15acafc2b8fffacc1a53eeab50da2ef9431f6e2e24b38ebbcd0
  $ ../../src/verocaml.exe verify artifacts/no_constructor_postcondition.ml > artifacts/no_constructor_postcondition.source.out 2>&1; test $? -eq 2
  $ grep -o 'frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar' artifacts/no_constructor_postcondition.source.out
  frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/no_constructor_postcondition.cmo artifacts/no_constructor_postcondition.ml
  $ ./shared_recursive_frozen_spine_tool.exe reject-boundary artifacts/no_constructor_postcondition.cmt
  rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0

  $ sed '42c\\      contents result = End];' fixtures/exact_pfc.ml > artifacts/failed_constructor.ml
  $ ../../src/verocaml.exe verify artifacts/failed_constructor.ml > artifacts/failed_constructor.source.out 2>&1; test $? -eq 2
  $ grep -o 'frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar' artifacts/failed_constructor.source.out
  frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/failed_constructor.cmo artifacts/failed_constructor.ml
  $ ./shared_recursive_frozen_spine_tool.exe reject-boundary artifacts/failed_constructor.cmt
  rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
Compiler-accepted unsupported shapes reject identically after retained-CMT
lowering and validation, before frozen/shared/invariant/VIR/backend work.

  $ cat fixtures/exact_pfc.ml > artifacts/mutable_edge.ml
  $ sed -i 's/mutable value : int; next : link/mutable value : int; mutable next : link/' artifacts/mutable_edge.ml
  $ cat fixtures/exact_pfc.ml > artifacts/singleton.ml
  $ sed -i 's/{ value = a; next = Next { value = b; next = Nil } }/{ value = a; next = Nil }/' artifacts/singleton.ml
  $ cat fixtures/exact_pfc.ml > artifacts/wrong_model.ml
  $ sed -i 's/    contents_node node$/    More (node.value, End)/' artifacts/wrong_model.ml
  $ cat fixtures/exact_pfc.ml > artifacts/revealed.ml
  $ sed -i '30c\\  [@@verocaml.revealed]' artifacts/revealed.ml
  $ sed '48c\\    match node.next with Nil -> () | Next child -> child.value <- value' fixtures/exact_pfc.ml > artifacts/deeper_write.ml
  $ sed -e '17s/mutable value : int; next : link/mutable value : int; mutable other : int; next : link/' -e '43s/{ value = a; next = Next { value = b; next = Nil } }/{ value = a; other = 0; next = Next { value = b; other = 0; next = Nil } }/' fixtures/exact_pfc.ml > artifacts/second_mutable.ml
  $ sed '49i\\  [@@verocaml.external_body]' fixtures/exact_pfc.ml > artifacts/trusted.ml
  $ for n in mutable_edge singleton wrong_model revealed deeper_write second_mutable trusted; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "artifacts/$n.ml"; done
  $ for n in mutable_edge singleton wrong_model revealed deeper_write second_mutable trusted; do ../../src/verocaml.exe verify "artifacts/$n.ml" > "artifacts/$n.source.out" 2>&1; test $? -eq 2; printf '%s-source: rejected\n' "$n"; out=$(./shared_recursive_frozen_spine_tool.exe reject-boundary "artifacts/$n.cmt"); printf '%s-cmt: %s\n' "$n" "$out"; done
  mutable_edge-source: rejected
  mutable_edge-cmt: rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  singleton-source: rejected
  singleton-cmt: rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  wrong_model-source: rejected
  wrong_model-cmt: rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  revealed-source: rejected
  revealed-cmt: rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  deeper_write-source: rejected
  deeper_write-cmt: rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  second_mutable-source: rejected
  second_mutable-cmt: rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  trusted-source: rejected
  trusted-cmt: rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0

Two sequential calls start the second operation from the first successor epoch;
the original local alias sees the final head while the caller-entry tail stays
exact.

  $ cat fixtures/exact_pfc.ml > artifacts/two_calls.ml
  $ printf '%s\n' '' 'let replace_twice (xs : (List.t [@finite]) @ aliased) first second : int =' '  [%verocaml.requires first <> List.head xs && second <> first];' '  [%verocaml.ensures fun result ->' '    result = second && List.contents xs = More (second, tail ([%verocaml.old (List.contents xs)]))];' '  let peer = xs in' '  List.set_head peer first;' '  List.set_head xs second;' '  List.head peer' >> artifacts/two_calls.ml
  $ TMPDIR="$PWD/artifacts" ../../src/verocaml.exe verify artifacts/two_calls.ml --timeout-ms 10000
  verocaml: verified file=artifacts/two_calls.ml functions=5 obligations=24

SST and VIR output are deterministic, source and retained-CMT verification
agree, and runtime erasure contains no verifier authority.

  $ ./shared_recursive_frozen_spine_tool.exe dump-sst artifacts/exact_pfc.cmt > artifacts/exact-again.sst
  $ cmp artifacts/exact.sst artifacts/exact-again.sst
  $ ./shared_recursive_frozen_spine_tool.exe dump-vir artifacts/exact_pfc.cmt > artifacts/exact.vir
  $ ./shared_recursive_frozen_spine_tool.exe dump-vir artifacts/exact_pfc.cmt > artifacts/exact-again.vir
  $ cmp artifacts/exact.vir artifacts/exact-again.vir
  $ TMPDIR="$PWD/artifacts" ../../src/verocaml.exe verify fixtures/exact_pfc.ml
  verocaml: verified file=fixtures/exact_pfc.ml functions=4 obligations=20
  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -o artifacts/runtime.exe fixtures/runtime_erasure.ml
  $ artifacts/runtime.exe
  42
  $ ! strings artifacts/runtime.exe | grep -E 'Vero_ghost|frozen-spine|recursive-spec'
