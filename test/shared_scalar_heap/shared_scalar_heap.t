Ordinary verification, counterexample, trusted-body, and project/prepared-CMT
parity behavior is covered by outcome_cases.ml.  The retained transcript is an
explicit specialist exception: baseline lines 21-45 pin private heap/SST/VIR
and backend architecture, 71-78 separates shared and unique lowering, 83-105
asserts zero-work rejection before the VERO-113 projection exists, 118-131
guards process-local forgery/lifecycle authority, and 149-152 guards runtime
erasure.  None participates in shared-scalar-heap-outcome-check.

The bounded shared-scalar slice is authenticated from retained source/CMT
input. Its positive one- and two-write examples verify, while all lifecycle
counters balance per symbolic path.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ for n in increment_through_alias two_write_exact_alias old_entry_views scalar_result_and_assert mixed_unique_shared overflow_rhs two_formal_counterexample two_formal_false_old wrong_final_constant remove_first_write remove_second_write local_no_disequality forgery_seed; do retained "$n"; done

The two-write semantic SST names two ordered epochs. VIR exposes the exact
newest-first nested integer conditional, entry/current views, and remains
deterministic and quantifier-free.

  $ ./shared_scalar_heap_tool.exe dump-sst artifacts/two_write_exact_alias.cmt > artifacts/two-write.sst
  $ grep -F 'mutation-policy bounded-shared-scalar-heap-v1' artifacts/two-write.sst
    mutation-policy bounded-shared-scalar-heap-v1 path=0 record=box#0 field=type-box#0.value#0 formals=cell#0:box#0 writes=2 entry-epoch=0 final-epoch=2
  $ grep 'shared-scalar-field-write' artifacts/two-write.sst | sed -E 's/ : unit @ .*/ : unit/'
            shared-scalar-field-write type-box#0.value#0 root=cell#0:box#0 pattern=aliased use=aliased local=true public=true mutable=true policy=bounded-shared-scalar-heap-v1 function=two_write_exact_alias#0 path=0 record=box#0 canonical-root=cell#0:box#0 target=cell#0:box#0 aliases=cell#0:box#0 pre-epoch=0 successor-epoch=1 : unit
            shared-scalar-field-write type-box#0.value#0 root=peer#2:box#0 pattern=aliased use=aliased local=true public=true mutable=true policy=bounded-shared-scalar-heap-v1 function=two_write_exact_alias#0 path=0 record=box#0 canonical-root=cell#0:box#0 target=peer#2:box#0 aliases=cell#0:box#0->peer#2:box#0 pre-epoch=1 successor-epoch=2 : unit
  $ ./shared_scalar_heap_tool.exe dump-sst artifacts/two_write_exact_alias.cmt > artifacts/two-write-again.sst
  $ cmp artifacts/two-write.sst artifacts/two-write-again.sst
  $ ./shared_scalar_heap_tool.exe dump-vir artifacts/two_write_exact_alias.cmt > artifacts/two-write.vir
  $ ./shared_scalar_heap_tool.exe dump-vir artifacts/two_write_exact_alias.cmt > artifacts/two-write-again.vir
  $ cmp artifacts/two-write.vir artifacts/two-write-again.vir
  $ grep -F 'shared-heap-read policy=bounded-shared-scalar-heap-v1' artifacts/two-write.vir | grep -F 'epoch=2 view=current' | head -1 | grep -F 'term=(ite (= cell$0 cell$0)' >/dev/null
  $ grep -F 'shared-heap-read policy=bounded-shared-scalar-heap-v1' artifacts/two-write.vir | grep -F 'epoch=0 view=current' | head -1 | grep -F 'term=(t0_box_record.value#0 cell$0)' >/dev/null
  $ grep -F 'shared-heap-read policy=bounded-shared-scalar-heap-v1' artifacts/two-write.vir | grep -F 'epoch=0 view=entry' | head -1
    shared-heap-read policy=bounded-shared-scalar-heap-v1 path=0 field=box#0.value#0 epoch=0 view=entry location=cell$0 term=(t0_box_record.value#0 cell$0)
  $ ! grep -E 'forall|exists' artifacts/two-write.vir

The ordinary QF-UFLIA backend owns the conditional. Neither direct Z3 nor a
recursive query is used, and no finite, unique-transition, or recursive
authority is issued.

  $ ./shared_scalar_heap_tool.exe backend-conditional
  (bool.ite (int.eq f0_s0 f0_s1) (int.add (int.mul 2 0) 1) 0)
  $ ./shared_scalar_heap_tool.exe route artifacts/two_write_exact_alias.cmt
  ordinary=10 direct-z3=10 recursive=0 finite=0 invariant=0 unique=0 recursive-authority=0

A mixed source unit keeps the existing unique functional transition separate.
Only the shared function has heap audit records.

  $ ./shared_scalar_heap_tool.exe dump-sst artifacts/mixed_unique_shared.cmt > artifacts/mixed.sst
  $ grep -E '^function |mutation-policy bounded|shared-scalar-field-write|      field-write' artifacts/mixed.sst | sed -E 's/ @ .*$//'
  function update_shared#0 mode=exec recursive=false result=unit policy=default-linear/default-z3
    mutation-policy bounded-shared-scalar-heap-v1 path=0 record=shared_box#0 field=type-shared_box#0.shared_value#0 formals=cell#0:shared_box#0 writes=1 entry-epoch=0 final-epoch=1
      shared-scalar-field-write type-shared_box#0.shared_value#0 root=cell#0:shared_box#0 pattern=aliased use=aliased local=true public=true mutable=true policy=bounded-shared-scalar-heap-v1 function=update_shared#0 path=0 record=shared_box#0 canonical-root=cell#0:shared_box#0 target=cell#0:shared_box#0 aliases=cell#0:shared_box#0 pre-epoch=0 successor-epoch=1 : unit
  function update_unique#1 mode=exec recursive=false result=unique_box#1 policy=default-linear/default-z3
        field-write type-unique_box#1.unique_value#0 root=cell#0:unique_box#1 uniqueness=unique pattern=unique use=aliased local=true public=true mutable=true : unit
  $ ./shared_scalar_heap_tool.exe dump-vir artifacts/mixed_unique_shared.cmt | awk '/^function / { f=$2 } /shared-heap-/ { print f }' | sort -u
  update_shared#0

All deferred source forms reject before heap authority. The imported type is
compiled separately so its source record is not local to the consumer CMT.

  $ for n in reject_default_mode reject_branch reject_match reject_loop reject_call reject_mutator_call reject_capture reject_return reject_construction reject_store reject_generic reject_second_mutable reject_recursive_record reject_finite reject_invariant reject_shadowing reject_third_write reject_trusted_body; do retained "$n"; done
  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/imported_box.cmo fixtures/imported_box.ml
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -c -o artifacts/reject_imported.cmo fixtures/reject_imported.ml
  $ reject () { n=$1; printf "$n: "; ./shared_scalar_heap_tool.exe reject-boundary "artifacts/$n.cmt"; }
  $ for n in reject_default_mode reject_branch reject_match reject_loop reject_call reject_mutator_call reject_capture reject_return reject_construction reject_store reject_generic reject_second_mutable reject_recursive_record reject_finite reject_invariant reject_shadowing reject_third_write reject_imported; do reject "$n"; done
  reject_default_mode: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_branch: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_match: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_loop: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_call: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_mutator_call: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_capture: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_return: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_construction: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_store: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_generic: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_second_mutable: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_recursive_record: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_finite: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_invariant: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_shadowing: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_third_write: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0
  reject_imported: rejected heap=0/0/0/0/0 vir=0 ordinary=0 direct-z3=0 recursive=0

Copied/raw descriptor policy plus forged path, epoch, type and field identities
cannot recreate the process-local issuance. The capability rejects duplicate
and copied consumption, stale epochs, explicit teardown, and inactive sessions.
A user-constructed VIR conditional with a same-sort location equality but no
private issuance is rejected.

  $ ./shared_scalar_heap_tool.exe forgery-matrix artifacts/forgery_seed.cmt
  forged-raw-policy: rejected
  forged-path: rejected
  forged-epoch: rejected
  forged-type: rejected
  forged-field: rejected
  $ ./shared_scalar_heap_tool.exe lifecycle
  duplicate-consume: rejected
  copied-capability: rejected
  stale-epoch: rejected
  destroyed-session: rejected
  inactive-session: rejected
  $ ./shared_scalar_heap_tool.exe forged-vir-conditional
  forged-vir-conditional: rejected

The ordinary erased executable has no verification runtime dependency and the
same-actual 42 instance evaluates to 86.

  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -o artifacts/same-actual.exe fixtures/same_actual_42_runtime.ml
  $ artifacts/same-actual.exe
  86
  $ ! strings artifacts/same-actual.exe | grep -E 'Vero_ghost|bounded-shared-scalar-heap'
