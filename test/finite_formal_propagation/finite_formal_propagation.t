Finite-formal authority uses the production registry implementation.  The
post-entry baseline contains two assumptions; calls add no assumptions and
commit only complete atomic batches.

  $ ./finite_formal_counter_tool.exe
  formal-entry assumptions=2
  formal-one-path-two-formal delta=+0/+1/+2/+1
  formal-fresh-second-call delta=+0/+1/+2/+1
  formal-missing-actual result=rejected delta=+0/+0/+0/+0
  formal-replay result=rejected delta=+0/+0/+0/+0
  formal-two-path-one-formal result=accepted delta=+0/+2/+2/+2
  formal-attack wrong-callee=rejected delta=+0/+0/+0/+0
  formal-attack wrong-mode=rejected delta=+0/+0/+0/+0
  formal-attack wrong-type=rejected delta=+0/+0/+0/+0
  formal-attack wrong-rank=rejected delta=+0/+0/+0/+0
  formal-attack wrong-profile=rejected delta=+0/+0/+0/+0
  formal-attack wrong-same-name-profile=rejected delta=+0/+0/+0/+0
  formal-attack wrong-same-name-uid=rejected delta=+0/+0/+0/+0
  formal-attack wrong-session=rejected delta=+0/+0/+0/+0
  formal-attack wrong-ordinal-binding=rejected delta=+0/+0/+0/+0

Each named retained-ABI fixture drives the production imported-call validator
inside a fresh production verification session.  The pinned diagnostic is the
earliest retained-boundary failure; all four formal counters and all
call-scoped downstream counters remain unchanged.

  $ for fixture in retained_wrong_label retained_reordered_same_type retained_binding_id_collision retained_omitted_argument retained_default_argument retained_optional_argument retained_destructured_formal retained_wildcard_formal retained_invalid_result_binder retained_partial_application retained_ambiguous_alias retained_ambiguous_open retained_functor_path public_executor_only_provider; do ./finite_formal_counter_tool.exe "fixtures/$fixture.abi"; done
  retained_wrong_label diagnostic=retained parametric signature label/default vector mismatch delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_reordered_same_type diagnostic=retained parametric signature label/default vector mismatch delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_binding_id_collision diagnostic=[VERO_DEPENDENCY] retained callable ABI binding identity collision delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_omitted_argument diagnostic=[VERO_DEPENDENCY] retained callable ABI requires saturated application delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_default_argument diagnostic=retained parametric signature label/default vector mismatch delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_optional_argument diagnostic=retained parametric signature label/default vector mismatch delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_destructured_formal diagnostic=[VERO_DEPENDENCY] retained callable ABI requires simple variable formals delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_wildcard_formal diagnostic=[VERO_DEPENDENCY] retained callable ABI requires simple variable formals delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_invalid_result_binder diagnostic=[VERO_DEPENDENCY] retained parametric signature snapshot mismatch delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_partial_application diagnostic=[VERO_DEPENDENCY] retained callable ABI requires saturated application delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_ambiguous_alias diagnostic=[VERO_DEPENDENCY] retained callable target is not an exact direct dependency delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_ambiguous_open diagnostic=[VERO_DEPENDENCY] retained callable target is not an exact direct dependency delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  retained_functor_path diagnostic=[VERO_DEPENDENCY] retained callable ABI rejects functor-generated paths delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  public_executor_only_provider diagnostic=[VERO_DEPENDENCY] retained provider lacks private-driver completion delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  $ cp fixtures/retained_wrong_label.abi artifacts-renamed.abi
  $ ./finite_formal_counter_tool.exe artifacts-renamed.abi
  retained_wrong_label diagnostic=retained parametric signature label/default vector mismatch delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0
  $ printf 'attack=unknown\n' > unknown.abi; ./finite_formal_counter_tool.exe unknown.abi 2>&1
  abi-fixture-error: unknown attack selector: unknown
  [2]
  $ printf 'attack=retained_wrong_label\nattack=retained_wrong_label\n' > duplicate-selector.abi; ./finite_formal_counter_tool.exe duplicate-selector.abi 2>&1
  abi-fixture-error: expected exactly one attack selector record
  [2]
  $ printf 'attack=\n' > empty-selector.abi; ./finite_formal_counter_tool.exe empty-selector.abi 2>&1
  abi-fixture-error: attack selector is empty
  [2]

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ trace () { VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "$@" --timeout-ms 5000 2>&1 | grep 'Verification_session: private receipt event_kind=destroy ' | sed -E 's/.*finite_formal_assumption_issuances=([0-9]+) finite_formal_transfer_batches=([0-9]+) finite_formal_transfers=([0-9]+) finite_formal_transfer_consumptions=([0-9]+).*/formal-counters assumptions=\1 batches=\2 transfers=\3 consumptions=\4/'; }

Direct, alias, field, branch-path, repeated-call, Exec, and Tracked flows verify.

  $ retained positive
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive.cmt --timeout-ms 5000
  verocaml: verified file=artifacts/positive.cmt functions=4 obligations=7
  $ trace artifacts/positive.cmt
  formal-counters assumptions=4 batches=6 transfers=8 consumptions=6

The ordinary CLI reaches the sole private production driver for a finite-formal
consumer and reports the same driver-rendered verification shape.

  $ retained driver_dispatch_consumer
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/driver_dispatch_consumer.cmt --timeout-ms 5000
  verocaml: verified file=artifacts/driver_dispatch_consumer.cmt functions=2 obligations=0
  $ trace artifacts/driver_dispatch_consumer.cmt
  formal-counters assumptions=1 batches=1 transfers=1 consumptions=1

Exec, Proof, and nonrecursive Spec finite formals, explicit Tracked and Ghost
modes, default-Ghost flow, and recursive proof self-transfers over exact
applications of one canonical generic-list schema all use the production
driver.

  $ for n in same_cmt_modes generic_recursive_proof push_preservation; do retained "$n"; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --timeout-ms 5000; done
  verocaml: verified file=artifacts/same_cmt_modes.cmt functions=11 obligations=8
  verocaml: verified file=artifacts/generic_recursive_proof.cmt functions=3 obligations=7
  verocaml: verified file=artifacts/push_preservation.cmt functions=1 obligations=4
  $ test ! -e fixtures/generic_recursive_proof.mli
  $ trace artifacts/same_cmt_modes.cmt
  formal-counters assumptions=9 batches=10 transfers=10 consumptions=10
  $ trace artifacts/generic_recursive_proof.cmt
  formal-counters assumptions=3 batches=3 transfers=3 consumptions=3
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic_recursive_proof.cmt --timeout-ms 5000 2>&1 | grep 'Verification_session: private receipt event_kind=lower ' | sed -E 's|.*function_name=([^ ]+) function_index=([0-9]+).*finite_formal_assumption_issuances=([0-9]+) finite_formal_transfer_batches=([0-9]+) finite_formal_transfers=([0-9]+) finite_formal_transfer_consumptions=([0-9]+).*|checkpoint function=\1#\2 counters=\3/\4/\5/\6|'
  checkpoint function=lemma_equal_refl#1 counters=0/0/0/0
  checkpoint function=int_instantiation#4 counters=1/1/1/1
  checkpoint function=bool_instantiation#5 counters=2/2/2/2
  $ trace artifacts/push_preservation.cmt
  formal-counters assumptions=1 batches=1 transfers=1 consumptions=1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic_recursive_proof.cmt --dump-sst artifacts/generic-first.sst --dump-vir artifacts/generic-first.vir >/dev/null
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic_recursive_proof.cmt --dump-sst artifacts/generic-second.sst --dump-vir artifacts/generic-second.vir >/dev/null
  $ cmp artifacts/generic-first.sst artifacts/generic-second.sst
  $ cmp artifacts/generic-first.vir artifacts/generic-second.vir
  $ grep '^type seq<' artifacts/generic-first.sst | sed -E 's/ @ .*//'
  $ grep '^function lemma_equal_refl' artifacts/generic-first.sst | sed -E 's/ @ .*//'
  function lemma_equal_refl#1 binders=['0@lemma_equal_refl#1] mode=proof recursive=true result=unit policy=default-linear/default-z3
  $ grep 'proof-call lemma_equal_refl.*recursive=true' artifacts/generic-first.sst | sed -E 's/^ *//; s/ @ .*//'
  proof-call lemma_equal_refl#1 recursive=true type-arguments=['0@lemma_equal_refl#1] : unit
  $ grep '^lemma_equal_refl.*formal=0' artifacts/generic-first.sst | sed -E 's/digest=[0-9a-f]+/digest=<digest>/'
  lemma_equal_refl#1 formal=0 label=- mode=Ghost type=seq<'0@lemma_equal_refl#1> digest=<digest>

Schema-first lowering accepts unused/open generic proofs, generic Exec,
generic results, partial application within a pure Spec, and labelled Spec
calls once under canonical binders. Nonuniform self recursion, higher-order
Proof application, and a foreign carrier retain precise policy rejection
before a production session, dump, or solver.

  $ for n in generic_unused generic_open_proof generic_type_changing generic_partial generic_labelled generic_exec generic_higher_order generic_foreign_actual generic_result; do retained "$n"; done
  $ results=; for n in generic_unused generic_open_proof generic_exec generic_result; do OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --timeout-ms 5000 >/dev/null; results="${results}${results:+ }${n}=verified"; done; echo "$results"
  generic_unused=verified generic_open_proof=verified generic_exec=verified generic_result=verified
  $ accept_generic_spec () { n=$1; for input in "fixtures/$n.ml" "artifacts/$n.cmt"; do OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --timeout-ms 5000 | grep -q 'functions=1 obligations=1'; done; printf '%s source+cmt functions=1 obligations=1\n' "$n"; }
  $ for n in generic_partial generic_labelled; do accept_generic_spec "$n" || exit 1; done
  generic_partial source+cmt functions=1 obligations=1
  generic_labelled source+cmt functions=1 obligations=1
  $ reject_generic () { n=$1; code=0; VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --timeout-ms 5000 --dump-sst "artifacts/$n.sst" --dump-vir "artifacts/$n.vir" >"artifacts/$n.out" 2>&1 || code=$?; test "$code" = 2; printf '%s: ' "$n"; grep -o 'VERO_[A-Z_]*' "artifacts/$n.out"; test "$(grep -c 'Verification_session: private receipt ' "artifacts/$n.out")" = 0; test ! -e "artifacts/$n.sst"; test ! -e "artifacts/$n.vir"; }
  $ for n in generic_type_changing generic_higher_order generic_foreign_actual; do reject_generic "$n" || exit 1; done
  generic_type_changing: VERO_UNSUPPORTED_POLYMORPHISM
  generic_higher_order: VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
  generic_foreign_actual: VERO_UNSUPPORTED_TYPE
  $ separate () { name=$1; variant=$2; directory="artifacts/$name-$variant"; mkdir -p "$directory"; flag=; if test "$variant" = cmti; then flag=-bin-annot; fi; ocamlc -w -A -alert -all $flag -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "$directory/$name.cmi" "fixtures/$name.mli"; test -e "$directory/$name.cmi"; if test "$variant" = cmti; then test -e "$directory/$name.cmti"; else test ! -e "$directory/$name.cmti"; fi; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I "$directory" -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "$directory/$name.cmo" "fixtures/$name.ml"; }
  $ reject_public_generic () { label=$1; directory=$2; name=$3; code=0; VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 VEROCAML_TEST_INSTANCE_MODE_TRACE=1 DELATOR_LOG=Verification_session=debug,Instance_mode=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "$directory/$name.cmt" --timeout-ms 5000 --dump-sst "$directory/rejected.sst" --dump-vir "$directory/rejected.vir" >"$directory/rejected.out" 2>&1 || code=$?; test "$code" = 2; printf '%s: ' "$label"; grep -o 'VERO_[A-Z_]*' "$directory/rejected.out"; test "$(grep -Ec 'Verification_session: private receipt |Instance_mode: ' "$directory/rejected.out")" = 0; test ! -e "$directory/rejected.sst"; test ! -e "$directory/rejected.vir"; }
  $ separate generic_explicit_interface cmti
  $ separate generic_explicit_interface cmi-only
  $ verify_public_generic () { label=$1; directory=$2; name=$3; OCAML_COLOR=never ../../src/verocaml.exe verify "$directory/$name.cmt" --timeout-ms 5000 --dump-sst "$directory/accepted.sst" --dump-vir "$directory/accepted.vir" >/dev/null; test "$(grep -c '^function seq_reflexive#0 binders=' "$directory/accepted.sst")" = 1; test "$(grep -c '^type seq<' "$directory/accepted.sst")" = 0; printf '%s: verified canonical-schema=true clones=0\n' "$label"; }
  $ verify_public_generic cmi+cmti artifacts/generic_explicit_interface-cmti generic_explicit_interface
  cmi+cmti: verified canonical-schema=true clones=0
  $ verify_public_generic cmi-only artifacts/generic_explicit_interface-cmi-only generic_explicit_interface
  cmi-only: verified canonical-schema=true clones=0

CMI-only public carrier aliases and supported constrained nested value paths
cannot hide an exported generic binding from the exact public-path match.

  $ separate generic_explicit_alias cmi-only
  $ separate generic_explicit_nested cmi-only
  $ reject_public_generic public-alias artifacts/generic_explicit_alias-cmi-only generic_explicit_alias
  public-alias: VERO_UNSUPPORTED_AGGREGATE
  $ reject_public_generic nested-value artifacts/generic_explicit_nested-cmi-only generic_explicit_nested
  nested-value: VERO_UNSUPPORTED_POLYMORPHISM

A substituted same-unit CMI fails the CMT/CMI digest binding before lowering.

  $ mkdir -p artifacts/generic-wrong-cmi-source artifacts/generic-wrong-cmi
  $ cp fixtures/generic_explicit_interface_wrong.mli artifacts/generic-wrong-cmi-source/generic_explicit_interface.mli
  $ ocamlc -w -A -alert -all -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c artifacts/generic-wrong-cmi-source/generic_explicit_interface.mli
  $ cp artifacts/generic_explicit_interface-cmi-only/generic_explicit_interface.cmt artifacts/generic-wrong-cmi/
  $ cp artifacts/generic-wrong-cmi-source/generic_explicit_interface.cmi artifacts/generic-wrong-cmi/
  $ reject_public_generic wrong-same-unit-cmi artifacts/generic-wrong-cmi generic_explicit_interface
  wrong-same-unit-cmi: VERO_MALFORMED_INPUT
  $ mkdir -p artifacts/generic-missing-cmi
  $ cp artifacts/generic_explicit_interface-cmi-only/generic_explicit_interface.cmt artifacts/generic-missing-cmi/
  $ reject_public_generic missing-adjacent-cmi artifacts/generic-missing-cmi generic_explicit_interface
  missing-adjacent-cmi: VERO_MALFORMED_INPUT

A constrained same-CMT module authenticates the exact signature vector.
Adding, removing, or reordering the requirement rejects before execution.

  $ retained same_cmt_signature_positive
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/same_cmt_signature_positive.cmt --timeout-ms 5000
  verocaml: verified file=artifacts/same_cmt_signature_positive.cmt functions=3 obligations=4
  $ trace artifacts/same_cmt_signature_positive.cmt
  formal-counters assumptions=1 batches=1 transfers=1 consumptions=1
  $ for n in same_cmt_signature_add same_cmt_signature_remove; do retained "$n"; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --timeout-ms 5000 2>&1 | grep 'VERO_UNSUPPORTED_STRUCTURE_ITEM'; done
  Error: [VERO_UNSUPPORTED_STRUCTURE_ITEM] This top-level declaration is not supported in a verified compilation unit.
  Error: [VERO_UNSUPPORTED_STRUCTURE_ITEM] This top-level declaration is not supported in a verified compilation unit.
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/signature_reorder_provider.cmi fixtures/signature_reorder_provider.mli
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/signature_reorder_provider.cmo fixtures/signature_reorder_provider.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/signature_reorder_provider.cmt --timeout-ms 5000 2>&1 | grep 'finite formals do not match'
  Error: [VERO_INVALID_PROGRAM] VeroCaml could not validate this verification unit: retained implementation finite formals do not match the exact retained interface finite signature for value:checked

Retained direct summaries substitute both simple formals in requires and both
formals plus the simple result binder in ensures.  The provider also runs the
recursive generic-list preflight before its completion-backed handle is issued.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained_simple_bindings_provider.cmi fixtures/retained_simple_bindings_provider.mli
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained_simple_bindings_provider.cmo fixtures/retained_simple_bindings_provider.ml
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained_simple_bindings_provider.cmt --timeout-ms 5000 --dump-sst artifacts/retained-provider-first.sst --dump-vir artifacts/retained-provider-first.vir >artifacts/retained-provider.out 2>artifacts/retained-provider.trace
  $ cat artifacts/retained-provider.out
  verocaml: verified file=artifacts/retained_simple_bindings_provider.cmt functions=4 obligations=11
  $ grep -E '^DEBUG Verification_session: private receipt event_kind=lower .*function_name=(seq_reflexive|int_seq_reflexive|bool_seq_reflexive)' artifacts/retained-provider.trace | sed -E 's|.*function_name=([^ ]+) function_index=([0-9]+).*finite_formal_assumption_issuances=([0-9]+) finite_formal_transfer_batches=([0-9]+) finite_formal_transfers=([0-9]+) finite_formal_transfer_consumptions=([0-9]+).*|checkpoint function=\1#\2 counters=\3/\4/\5/\6|'
  checkpoint function=seq_reflexive#1 counters=0/0/0/0
  checkpoint function=int_seq_reflexive#4 counters=1/1/1/1
  checkpoint function=bool_seq_reflexive#5 counters=1/2/2/2
  $ grep '^DEBUG Verification_session: private receipt event_kind=destroy ' artifacts/retained-provider.trace | sed -E 's/.*finite_formal_assumption_issuances=([0-9]+) finite_formal_transfer_batches=([0-9]+) finite_formal_transfers=([0-9]+) finite_formal_transfer_consumptions=([0-9]+).*/provider-final counters=\1\/\2\/\3\/\4/'
  provider-final counters=3/3/3/3
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained_simple_bindings_provider.cmt --timeout-ms 5000 --dump-sst artifacts/retained-provider-second.sst --dump-vir artifacts/retained-provider-second.vir >/dev/null
  $ cmp artifacts/retained-provider-first.sst artifacts/retained-provider-second.sst
  $ cmp artifacts/retained-provider-first.vir artifacts/retained-provider-second.vir
  $ grep '^function \(seq_length\|seq_reflexive\)' artifacts/retained-provider-first.sst | sed -E 's/ @ .*//'
  function seq_length#0 binders=['0@seq_length#0] mode=spec recursive=true result=int policy=default-linear/default-z3
  function seq_reflexive#1 binders=['0@seq_reflexive#1] mode=proof recursive=true result=unit policy=default-linear/default-z3
  $ grep -- '-call \(seq_length\|seq_reflexive\).*recursive=true' artifacts/retained-provider-first.sst | sed -E 's/^ *//; s/ @ .*//'
  specification-call seq_length#0 recursive=true type-arguments=['0@seq_length#0] : int
  proof-call seq_reflexive#1 recursive=true type-arguments=['0@seq_reflexive#1] : unit
  $ grep '^seq_reflexive.*formal=0' artifacts/retained-provider-first.sst | sed -E 's/digest=[0-9a-f]+/digest=<digest>/'
  seq_reflexive#1 formal=0 label=- mode=Ghost type=seq<'0@seq_reflexive#1> digest=<digest>
  $ for artifact in artifacts/retained_simple_bindings_provider.cmi artifacts/retained_simple_bindings_provider.cmti; do if strings "$artifact" | grep -E 'seq_(length|reflexive)' >/dev/null; then exit 1; fi; done
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained_simple_bindings_consumer.cmo fixtures/retained_simple_bindings_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained_simple_bindings_consumer.cmt --dependency artifacts/retained_simple_bindings_provider.cmt --timeout-ms 5000 --dump-sst artifacts/retained-consumer.sst | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Retained_simple_bindings_provider interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/retained_simple_bindings_consumer.cmt functions=1 obligations=24
  $ if grep 'seq_' artifacts/retained-consumer.sst; then exit 1; fi

The same retained provider and concrete consumer need only the digest-bound
CMI.  Omitting interface binary annotations produces no CMTI and does not
change canonical schema identities, proof transfers, summaries, SST, or VIR.

  $ mkdir -p artifacts/retained-cmi-only
  $ ocamlc -w -A -alert -all -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained-cmi-only/retained_simple_bindings_provider.cmi fixtures/retained_simple_bindings_provider.mli
  $ test ! -e artifacts/retained-cmi-only/retained_simple_bindings_provider.cmti
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/retained-cmi-only -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained-cmi-only/retained_simple_bindings_provider.cmo fixtures/retained_simple_bindings_provider.ml
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained-cmi-only/retained_simple_bindings_provider.cmt --timeout-ms 5000 --dump-sst artifacts/retained-cmi-only/provider.sst --dump-vir artifacts/retained-cmi-only/provider.vir >artifacts/retained-cmi-only/provider.out 2>artifacts/retained-cmi-only/provider.trace
  $ cat artifacts/retained-cmi-only/provider.out
  verocaml: verified file=artifacts/retained-cmi-only/retained_simple_bindings_provider.cmt functions=4 obligations=11
  $ grep '^DEBUG Verification_session: private receipt event_kind=destroy ' artifacts/retained-cmi-only/provider.trace | sed -E 's/.*finite_formal_assumption_issuances=([0-9]+) finite_formal_transfer_batches=([0-9]+) finite_formal_transfers=([0-9]+) finite_formal_transfer_consumptions=([0-9]+).*/cmi-only-provider-final counters=\1\/\2\/\3\/\4/'
  cmi-only-provider-final counters=3/3/3/3
  $ sed -E 's/ cmt=[^ ]+/ cmt=<cmt>/' artifacts/retained-provider-first.sst >artifacts/retained-provider-first.normalized.sst
  $ sed -E 's/ cmt=[^ ]+/ cmt=<cmt>/' artifacts/retained-cmi-only/provider.sst >artifacts/retained-cmi-only/provider.normalized.sst
  $ cmp artifacts/retained-provider-first.normalized.sst artifacts/retained-cmi-only/provider.normalized.sst
  $ cmp artifacts/retained-provider-first.vir artifacts/retained-cmi-only/provider.vir
  $ if strings artifacts/retained-cmi-only/retained_simple_bindings_provider.cmi | grep -E 'seq_(length|reflexive)' >/dev/null; then exit 1; fi
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/retained-cmi-only -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained-cmi-only/retained_simple_bindings_consumer.cmo fixtures/retained_simple_bindings_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained-cmi-only/retained_simple_bindings_consumer.cmt --dependency artifacts/retained-cmi-only/retained_simple_bindings_provider.cmt --timeout-ms 5000 --dump-sst artifacts/retained-cmi-only/consumer.sst | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Retained_simple_bindings_provider interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/retained-cmi-only/retained_simple_bindings_consumer.cmt functions=1 obligations=24
  $ if grep 'seq_' artifacts/retained-cmi-only/consumer.sst; then exit 1; fi
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained_arbitrary_consumer.cmo fixtures/retained_arbitrary_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained_arbitrary_consumer.cmt --dependency artifacts/retained_simple_bindings_provider.cmt --timeout-ms 5000 2>&1 | grep 'exact finite receipt'
  verocaml: error[VERO_DEPENDENCY] unit Retained_arbitrary_consumer: run: malformed SST: exact finite receipt is unavailable for a required call actual at retained_arbitrary_consumer.ml:3:2-3:54

Authority transfer does not discharge a false logical precondition.

  $ retained false_precondition_after_transfer
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/false_precondition_after_transfer.cmt --timeout-ms 5000 2>&1 | grep 'vc=call-precondition' | sed -E 's/span=[^ ]+/span=SPAN/'
  verocaml: counterexample function=caller#1 vc=call-precondition[checked#0,0] span=SPAN result=counterexample
  $ trace artifacts/false_precondition_after_transfer.cmt
  formal-counters assumptions=1 batches=1 transfers=1 consumptions=1

Arbitrary entry values and one-path-only authority reject at the call boundary.

  $ for n in arbitrary_actual one_path_missing; do retained "$n"; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --timeout-ms 5000 2>&1 | grep -E 'finite formal|exact finite receipt'; done
  verocaml: error[VERO_DEPENDENCY] unit Arbitrary_actual: caller: malformed SST: exact finite receipt is unavailable for a required call actual at arbitrary_actual.ml:6:2-6:15
  verocaml: error[VERO_DEPENDENCY] unit One_path_missing: caller: malformed SST: exact finite receipt is unavailable for a required call actual at one_path_missing.ml:7:2-7:47
  $ trace artifacts/arbitrary_actual.cmt
  formal-counters assumptions=1 batches=0 transfers=0 consumptions=0
  $ trace artifacts/one_path_missing.cmt
  formal-counters assumptions=1 batches=0 transfers=0 consumptions=0

Exec and explicit Tracked actuals may forget into a default-Ghost formal only
when the exact incoming finite receipt exists.

  $ for n in default_ghost_no_receipt tracked_default_ghost_no_receipt; do retained "$n"; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --timeout-ms 5000 2>&1 | grep 'exact finite receipt'; trace "artifacts/$n.cmt"; done
  verocaml: error[VERO_DEPENDENCY] unit Default_ghost_no_receipt: bad: malformed SST: exact finite receipt is unavailable for a required call actual at default_ghost_no_receipt.ml:7:19-7:42
  formal-counters assumptions=1 batches=0 transfers=0 consumptions=0
  verocaml: error[VERO_DEPENDENCY] unit Tracked_default_ghost_no_receipt: bad: malformed SST: exact finite receipt is unavailable for a required call actual at tracked_default_ghost_no_receipt.ml:7:2-7:36
  formal-counters assumptions=1 batches=0 transfers=0 consumptions=0

The PPX authenticates only one payload-free outer formal marker and rejects
result, local-function, and nested type positions before CMT production.

  $ for n in duplicate payload result local_function nested_field; do OCAML_COLOR=never ocamlc -w -A -alert -all -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c "fixtures/$n.ml" 2>&1 | grep '^Error:'; done
  Error: exactly one [@finite] annotation is allowed on a formal
  Error: [@finite] does not accept a payload
  Error: finite result contracts are not supported
  Error: mode-bearing and finite formals are supported only on a top-level
  Error: [@finite] is supported only on an outer top-level callable formal

Source-written internal finite markers reject in both ordinary and retained
PPX modes.  No CMI, CMTI, or CMO output survives any attack.

  $ internal_reject () { mode=$1; name=$2; ext=$3; flag=; test "$mode" = retained && flag=--keep-ghost; output="artifacts/rejected-${mode}-${name}-${ext}"; suffix=cmo; test "$ext" = mli && suffix=cmi; if OCAML_COLOR=never ocamlc -w -A -alert -all -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe $flag" -c -o "$output.$suffix" "fixtures/$name.$ext" >"$output.out" 2>&1; then return 1; fi; printf '%s/%s.%s: ' "$mode" "$name" "$ext"; grep 'is not source syntax' "$output.out" | tail -1 | sed 's/^ *//'; test ! -e "$output.cmi"; test ! -e "$output.cmti"; test ! -e "$output.cmo"; }
  $ for mode in ordinary retained; do for name in source_internal_finite_signature source_internal_finite_signature_malformed source_internal_finite_formal_prefix; do internal_reject "$mode" "$name" ml; internal_reject "$mode" "$name" mli; done; done
  ordinary/source_internal_finite_signature.ml: [@verocaml.internal.finite_signature] is not source syntax
  ordinary/source_internal_finite_signature.mli: [@verocaml.internal.finite_signature] is not source syntax
  ordinary/source_internal_finite_signature_malformed.ml: [@verocaml.internal.finite_signature] is not source syntax
  ordinary/source_internal_finite_signature_malformed.mli: [@verocaml.internal.finite_signature] is not source syntax
  ordinary/source_internal_finite_formal_prefix.ml: [@verocaml.internal.finite_formal.bad] is not source syntax
  ordinary/source_internal_finite_formal_prefix.mli: [@verocaml.internal.finite_formal.bad] is not source syntax
  retained/source_internal_finite_signature.ml: [@verocaml.internal.finite_signature] is not source syntax
  retained/source_internal_finite_signature.mli: [@verocaml.internal.finite_signature] is not source syntax
  retained/source_internal_finite_signature_malformed.ml: [@verocaml.internal.finite_signature] is not source syntax
  retained/source_internal_finite_signature_malformed.mli: [@verocaml.internal.finite_signature] is not source syntax
  retained/source_internal_finite_formal_prefix.ml: [@verocaml.internal.finite_formal.bad] is not source syntax
  retained/source_internal_finite_formal_prefix.mli: [@verocaml.internal.finite_formal.bad] is not source syntax

Implementation and signature syntax erase from ordinary compilation.  Retained
metadata is deterministic and does not add an SST or VIR authority node.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary.cmo fixtures/positive.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary-signature.cmi fixtures/signature.mli
  $ ocamlopt -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary-native.cmx fixtures/positive.ml
  $ ocamlc artifacts/ordinary.cmo -o artifacts/ordinary-byte.exe
  $ ocamlopt artifacts/ordinary-native.cmx -o artifacts/ordinary-native.exe
  $ for artifact in artifacts/ordinary.cmi artifacts/ordinary.cmt artifacts/ordinary.cmo artifacts/ordinary-signature.cmi artifacts/ordinary-signature.cmti artifacts/ordinary-native.cmi artifacts/ordinary-native.cmt artifacts/ordinary-native.cmx artifacts/ordinary-native.o artifacts/ordinary-byte.exe artifacts/ordinary-native.exe; do test -e "$artifact"; if strings "$artifact" | grep -E 'verocaml\\.internal\\.(finite_formal|finite_signature)' >/dev/null; then echo "finite metadata leaked into $artifact"; exit 1; fi; done
  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -I ../../runtime/.vero_ghost.objs/byte -ppx ../../ppx/vero_ppx.exe -c fixtures/positive.ml >artifacts/ordinary-source.out 2>&1
  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -I ../../runtime/.vero_ghost.objs/byte -ppx ../../ppx/vero_ppx.exe -c fixtures/signature.mli >artifacts/ordinary-signature-source.out 2>&1
  $ if grep -F -e '[@finite' -e 'verocaml.internal.finite_formal' -e 'verocaml.internal.finite_signature' artifacts/ordinary-source.out artifacts/ordinary-signature-source.out; then exit 1; fi
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained-erasure.cmo fixtures/positive.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained-erasure-signature.cmi fixtures/signature.mli
  $ for expected in 'verocaml.internal.finite_formal' 'verocaml.internal.finite_signature'; do strings artifacts/retained-erasure.cmt | grep "$expected" >/dev/null; done
  $ strings artifacts/retained-erasure-signature.cmti | grep 'verocaml.internal.finite_signature' >/dev/null
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive.cmt --dump-sst artifacts/first.sst --dump-vir artifacts/first.vir >/dev/null
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive.cmt --dump-sst artifacts/second.sst --dump-vir artifacts/second.vir >/dev/null
  $ cmp artifacts/first.sst artifacts/second.sst
  $ cmp artifacts/first.vir artifacts/second.vir
  $ grep -A5 '^finite-formals$' artifacts/first.sst | sed -E 's/digest=[0-9a-f]+/digest=<digest>/'
  finite-formals
  checked#1 formal=1 label=- mode=Exec type=node#0 digest=<digest>
  checked#1 formal=2 label=- mode=Exec type=node#0 digest=<digest>
  one#2 formal=0 label=- mode=Exec type=node#0 digest=<digest>
  tracked#3 formal=1 label=- mode=Tracked type=node#0 digest=<digest>
