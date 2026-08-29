The exact same-CMT invariant-cell PFC verifies. Its hidden write log retains the
intermediate -1, closes the named invariant on the final current epoch, and its
caller applies the exact body-derived effect before the current terminal read.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ for n in exact_pfc two_sequential_calls two_formal_sequential_alias local_alias post_call_invariant constructor_result mixed_unique_shared overflow_rhs two_formal_counterexample wrong_plus_two delete_second_write bad_constructor reject_branch reject_open_invariant_use reject_third_write reject_two_cell_formals review_branch_client stale_constructor_result reject_callback_client reject_reentrant_client reject_raw_cell reject_revealed_cell reject_ghost_operation reject_spec_operation; do retained "$n"; done
  $ retained import_provider
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/reject_cross_cmt_consumer.cmo fixtures/reject_cross_cmt_consumer.ml
  $ ./shared_invariant_cell_tool.exe verify artifacts/exact_pfc.cmt
  status=verified functions=4 obligations=11 cell=3/0/3/2/4/2/1/1/3 heap=2/4/12/4/2
  $ ./shared_invariant_cell_tool.exe dump-sst artifacts/exact_pfc.cmt > artifacts/exact.sst
  $ grep -E 'role=(current-model|shared-invariant-transition|current-terminal-read)' artifacts/exact.sst | sed -E 's/ @ .*//'
      public Cell.model#1 role=current-model
      public Cell.increment#3 role=shared-invariant-transition
      public Cell.get#4 role=current-terminal-read
  $ ./shared_invariant_cell_tool.exe dump-vir artifacts/exact_pfc.cmt > artifacts/exact.vir
  $ grep -F 'value=-1' artifacts/exact.vir | head -1 | sed -E 's/^  //'
  shared-heap-write policy=bounded-shared-scalar-heap-v1 path=0 field=Cell.t#1.value#0 predecessor-epoch=0 successor-epoch=1 location=cell$0 value=-1
  $ grep -F 'boundary=shared-invariant-close:epoch=0->2' artifacts/exact.vir | sed -E 's/^  //; s/ @ .*//'
  vc 2 invariant-validity boundary=shared-invariant-close:epoch=0->2 invariant=invariant:Cell.t:1:Cell.invariant:2 type=Cell.t#1 model=Cell.model#1 predicate=Cell.invariant#2 operation=Cell.increment#3
  $ awk '/^function bump_and_get#/ { seen=1 } seen && /shared-heap-write/ { count++ } END { print "caller-effect-writes=" count }' artifacts/exact.vir
  caller-effect-writes=2
  $ awk '/^function bump_and_get#/ { seen=1 } seen && /shared-heap-read/ && /epoch=2 view=current/ { found=1 } END { print "caller-current-terminal=" found }' artifacts/exact.vir
  caller-current-terminal=1
  $ ! grep -E 'forall|exists' artifacts/exact.vir

Operation-entry old is checkpointed independently from caller-entry old. A
second call begins at the first call's successor epoch, and an exact local alias
observes the peer's update. Closed invariant use is available only after call
close.

  $ ./shared_invariant_cell_tool.exe verify artifacts/two_sequential_calls.cmt
  status=verified functions=4 obligations=14 cell=3/0/3/3/6/3/2/1/3 heap=2/6/16/6/2
  $ ./shared_invariant_cell_tool.exe dump-vir artifacts/two_sequential_calls.cmt > artifacts/two.vir
  $ grep 'shared-heap-write' artifacts/two.vir | awk '/^  shared/ { print $0 }' | grep -E 'predecessor-epoch=(2|3)' | sed -E 's/.*predecessor-epoch=([0-9]+) successor-epoch=([0-9]+).*/epoch=\1->\2/' | sort -u
  epoch=2->3
  epoch=3->4
  $ ./shared_invariant_cell_tool.exe verify artifacts/two_formal_sequential_alias.cmt
  status=verified functions=4 obligations=12 cell=4/0/3/3/6/3/2/0/3 heap=2/6/13/6/2
  $ ./shared_invariant_cell_tool.exe verify artifacts/local_alias.cmt
  status=verified functions=4 obligations=10 cell=3/0/3/2/4/2/1/1/3 heap=2/4/11/4/2
  $ ./shared_invariant_cell_tool.exe verify artifacts/post_call_invariant.cmt
  status=verified functions=4 obligations=9 cell=3/0/3/2/4/2/1/1/3 heap=2/4/11/4/2
  $ ./shared_invariant_cell_tool.exe verify artifacts/constructor_result.cmt
  status=verified functions=4 obligations=8 cell=2/1/3/1/2/1/0/1/3 heap=1/2/6/2/1

Deleting the restoring write fails the named close VC, rather than passing from
a trusted postcondition. The caller +2 mutant and possible-alias frame claim
are concrete postcondition counterexamples. Constructor establishment and
checked RHS overflow remain independent obligations.

  $ ../../src/verocaml.exe verify fixtures/delete_second_write.ml > artifacts/delete.out 2>&1; test $? -eq 1
  $ grep -o 'counterexample function=Cell.increment#[0-9]* vc=invariant-cell-close\[[^]]*\]' artifacts/delete.out
  counterexample function=Cell.increment#3 vc=invariant-cell-close[invariant:Cell.t:1:Cell.invariant:2]
  $ ./shared_invariant_cell_tool.exe verify artifacts/delete_second_write.cmt
  status=counterexample functions=3 obligations=4 cell=2/0/2/1/1/0/0/0/2 heap=1/1/6/1/1
  $ ./shared_invariant_cell_tool.exe dump-vir artifacts/delete_second_write.cmt > artifacts/delete.vir
  $ ! grep -E '^function bump_and_get|caller-effect' artifacts/delete.vir
  $ for n in wrong_plus_two two_formal_counterexample overflow_rhs bad_constructor; do printf "$n: "; ./shared_invariant_cell_tool.exe verify "artifacts/$n.cmt" | sed -E 's/ functions=.*//'; done
  wrong_plus_two: status=counterexample
  two_formal_counterexample: status=counterexample
  overflow_rhs: status=counterexample
  bad_constructor: status=counterexample
  $ ./shared_invariant_cell_tool.exe dump-vir artifacts/overflow_rhs.cmt | grep -E 'arithmetic-(lower|upper)' | sed -E 's/.*(arithmetic-(lower|upper)).*/\1/' | sort -u
  arithmetic-lower
  arithmetic-upper
  $ ../../src/verocaml.exe verify fixtures/bad_constructor.ml > artifacts/constructor.out 2>&1; test $? -eq 1
  $ grep -o 'vc=invariant-constructor-establishment\[[^]]*\]' artifacts/constructor.out | head -1
  vc=invariant-constructor-establishment[invariant:Cell.t:1:Cell.invariant:2]

Unique mutation and plain shared mutation coexist in the same CMT
without invariant-cell issuance on those functions.

  $ ./shared_invariant_cell_tool.exe verify artifacts/mixed_unique_shared.cmt
  status=verified functions=5 obligations=6 cell=2/0/2/1/2/1/0/0/2 heap=2/3/6/3/2
  $ ./shared_invariant_cell_tool.exe dump-sst artifacts/mixed_unique_shared.cmt | grep -E '^function (update_unique|update_shared)|mutation-policy bounded-shared|shared-invariant-transition' | sed -E 's/ @ .*//'
      public Cell.increment#3 role=shared-invariant-transition
    mutation-policy bounded-shared-scalar-heap-v1 path=0 record=Cell.t#1 field=type-Cell.t#1.value#0 formals=cell#0:Cell.t#1 writes=2 entry-epoch=0 final-epoch=2
  function update_unique#5 mode=exec recursive=false result=unique_box#2 policy=default-linear/default-z3
  function update_shared#6 mode=exec recursive=false result=unit policy=default-linear/default-z3
    mutation-policy bounded-shared-scalar-heap-v1 path=0 record=shared_box#3 field=type-shared_box#3.shared_value#0 formals=cell#0:shared_box#3 writes=1 entry-epoch=0 final-epoch=1

Unsupported body shapes and an attempted invariant use while open reject before
invariant-cell, heap, VIR, backend, direct-Z3, or recursive-query work. A read
formal cannot be forged into the aliased source boundary.

  $ reject () { n=$1; printf "$n: "; ./shared_invariant_cell_tool.exe reject-boundary "artifacts/$n.cmt"; }
  $ for n in reject_branch reject_open_invariant_use reject_third_write reject_two_cell_formals; do reject "$n"; done
  reject_branch: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_open_invariant_use: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_third_write: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_two_cell_formals: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  $ if retained reject_wrong_formal > artifacts/wrong-formal.out 2>&1; then false; else echo 'reject_wrong_formal: source-compiler-before-session cell=0/0/0/0/0/0/0/0/0'; fi
  reject_wrong_formal: source-compiler-before-session cell=0/0/0/0/0/0/0/0/0

The independent-review branch PFC and a constructor-result join reject during
source/CMT validation, before constructor or entry eligibility can be issued.
Raw/revealed, Ghost/Spec, cross-CMT, callback, and reentrant source shapes have
the same exact pre-authority boundary.

  $ for n in review_branch_client stale_constructor_result reject_raw_cell reject_revealed_cell reject_ghost_operation reject_spec_operation reject_cross_cmt_consumer reject_callback_client reject_reentrant_client; do reject "$n"; done
  review_branch_client: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  stale_constructor_result: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_raw_cell: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_revealed_cell: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_ghost_operation: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_spec_operation: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_cross_cmt_consumer: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_callback_client: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_reentrant_client: rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  $ for n in review_branch_client stale_constructor_result; do OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" > "artifacts/$n.out" 2>&1; test $? -eq 2; printf "$n: "; grep -o 'shared invariant-cell clients require one straight-line path' "artifacts/$n.out"; done
  review_branch_client: shared invariant-cell clients require one straight-line path
  stale_constructor_result: shared invariant-cell clients require one straight-line path

Public SST role/effect metadata cannot recreate private authority. Exact role,
model, invariant, callable, path, epoch, type, field, and formal mutations all
fail independent validation. Affine lifecycle attacks fail without solver help.

  $ ./shared_invariant_cell_tool.exe forgery-matrix artifacts/exact_pfc.cmt
  forged-role: rejected
  forged-model: rejected
  forged-invariant: rejected
  forged-callable: rejected
  forged-path: rejected
  forged-epoch: rejected
  forged-type: rejected
  forged-field: rejected
  forged-formal: rejected
  $ ./shared_invariant_cell_tool.exe lifecycle
  duplicate-open: rejected
  close-without-update: rejected
  close-without-open: rejected
  open-after-close: rejected
  duplicate-close: rejected
  stale-constructor-epoch: rejected
  copied-eligibility: rejected
  inactive-session: rejected
  open-after-destroy: rejected

SST/VIR dumps are deterministic, source and retained-CMT semantics agree after
normalizing only presentation authority, and the route remains ordinary QF
UF/LIA with no direct Z3 or recursive query.

  $ ./shared_invariant_cell_tool.exe dump-sst artifacts/exact_pfc.cmt > artifacts/exact-again.sst
  $ cmp artifacts/exact.sst artifacts/exact-again.sst
  $ ./shared_invariant_cell_tool.exe dump-vir artifacts/exact_pfc.cmt > artifacts/exact-again.vir
  $ cmp artifacts/exact.vir artifacts/exact-again.vir
  $ mkdir artifacts/temp
  $ TMPDIR="$PWD/artifacts/temp" ../../src/verocaml.exe verify fixtures/exact_pfc.ml --dump-sst artifacts/source.sst --dump-vir artifacts/source.vir
  verocaml: verified file=fixtures/exact_pfc.ml functions=4 obligations=11
  $ ../../src/verocaml.exe verify artifacts/exact_pfc.cmt --dump-sst artifacts/cmt.sst --dump-vir artifacts/cmt.vir
  verocaml: verified file=artifacts/exact_pfc.cmt functions=4 obligations=11
  $ sed -E '/^authority=retained /d; s/snapshot=[0-9a-f]+/snapshot=<digest>/g' artifacts/source.sst > artifacts/source.semantic.sst
  $ sed -E '/^authority=retained /d; s/snapshot=[0-9a-f]+/snapshot=<digest>/g' artifacts/cmt.sst > artifacts/cmt.semantic.sst
  $ cmp artifacts/source.semantic.sst artifacts/cmt.semantic.sst
  $ cmp artifacts/source.vir artifacts/cmt.vir
  $ ./shared_invariant_cell_tool.exe route artifacts/exact_pfc.cmt
  ordinary=11 direct-z3=11 recursive=0

Ordinary runtime erasure retains no verifier, heap, or invariant-cell token.

  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -o artifacts/runtime.exe fixtures/runtime_erasure.ml
  $ artifacts/runtime.exe
  42
  $ ! strings artifacts/runtime.exe | grep -E 'Vero_ghost|shared-invariant|bounded-shared'
