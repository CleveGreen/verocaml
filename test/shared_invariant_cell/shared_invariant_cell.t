Ordinary verification, counterexample, and project/prepared-CMT parity behavior
is covered by outcome_cases.ml.  The retained transcript is an explicit
specialist exception: baseline lines 12-26, 35-38, and 77-82 inspect private
SST/VIR epoch and lowering architecture; 88-114 guards zero-work validation
boundaries that VERO-113 cannot project; 120-139 guards process-local forgery
and affine lifecycle authority; 158-159 pins backend routing; and 163-166
guards runtime erasure.  None participates in shared-invariant-cell-outcome-check.

The exact same-CMT invariant-cell PFC verifies. Its hidden write log retains the
intermediate -1, closes the named invariant on the final current epoch, and its
caller applies the exact body-derived effect before the current terminal read.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ for n in exact_pfc two_sequential_calls two_formal_sequential_alias local_alias post_call_invariant constructor_result mixed_unique_shared overflow_rhs two_formal_counterexample wrong_plus_two delete_second_write bad_constructor reject_branch reject_open_invariant_use reject_third_write reject_two_cell_formals review_branch_client stale_constructor_result reject_callback_client reject_reentrant_client reject_raw_cell reject_revealed_cell reject_ghost_operation reject_spec_operation; do retained "$n"; done
  $ retained import_provider
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/reject_cross_cmt_consumer.cmo fixtures/reject_cross_cmt_consumer.ml
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

  $ ./shared_invariant_cell_tool.exe dump-vir artifacts/two_sequential_calls.cmt > artifacts/two.vir
  $ grep 'shared-heap-write' artifacts/two.vir | awk '/^  shared/ { print $0 }' | grep -E 'predecessor-epoch=(2|3)' | sed -E 's/.*predecessor-epoch=([0-9]+) successor-epoch=([0-9]+).*/epoch=\1->\2/' | sort -u
  epoch=2->3
  epoch=3->4

Unique mutation and plain shared mutation coexist in the same CMT
without invariant-cell issuance on those functions.

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

The retained route assertion is an architecture exception.

  $ ./shared_invariant_cell_tool.exe route artifacts/exact_pfc.cmt
  ordinary=11 direct-z3=11 recursive=0

Ordinary runtime erasure retains no verifier, heap, or invariant-cell token.

  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -o artifacts/runtime.exe fixtures/runtime_erasure.ml
  $ artifacts/runtime.exe
  42
  $ ! strings artifacts/runtime.exe | grep -E 'Vero_ghost|shared-invariant|bounded-shared'
