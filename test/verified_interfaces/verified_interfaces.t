Explicit dependency CMTs are authenticated as one complete graph. Input order
does not grant authority: verification is topological, the consumer's exact
unit/CRC slots bind each handle, and provenance is deterministic.

  $ mkdir -p artifacts/v1 artifacts/stale artifacts/lookalike artifacts/fake-runtime
  $ export PPX="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ compile () { (cd "$1" && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c "$2"); }
  $ cp fixtures/base.ml fixtures/middle.ml fixtures/consumer.ml artifacts/v1/
  $ compile artifacts/v1 base.ml
  $ compile artifacts/v1 middle.ml
  $ compile artifacts/v1 consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/v1/consumer.cmt --timeout-ms 60000 --dependency artifacts/v1/middle.cmt --dependency artifacts/v1/base.cmt --dump-sst artifacts/consumer.sst --dump-vir artifacts/consumer.vir | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/; s/Base@[0-9a-f]+/Base@<digest>/g' | tee artifacts/positive.out
  verocaml: verified dependency unit=Base interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified dependency unit=Middle interface-digest=<digest> direct=Base@<digest> transitive=Base@<digest> trust=none
  verocaml: verified file=artifacts/v1/consumer.cmt functions=1 obligations=0
  $ test -s artifacts/consumer.sst && test -s artifacts/consumer.vir
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/v1/consumer.cmt --timeout-ms 60000 --dependency artifacts/v1/middle.cmt --dependency artifacts/v1/base.cmt | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/; s/Base@[0-9a-f]+/Base@<digest>/g' | grep 'verified dependency' > artifacts/provenance-2.out
  $ grep 'verified dependency' artifacts/positive.out > artifacts/provenance-1.out
  $ cmp artifacts/provenance-1.out artifacts/provenance-2.out

The direct-source consumer uses only its explicitly supplied implementation
CMT closure as semantic authority. The compiler may read the adjacent CMI for
ordinary typing, but VeroCaml authenticates the supplied CMT and does not
discover it.

  $ cp fixtures/consumer.ml artifacts/v1/source_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/v1/source_consumer.ml --timeout-ms 60000 --dependency artifacts/v1/middle.cmt --dependency artifacts/v1/base.cmt | sed 's/interface-digest=[0-9a-f]*/interface-digest=<digest>/; s/Base@[0-9a-f]*/Base@<digest>/g'
  verocaml: verified dependency unit=Base interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified dependency unit=Middle interface-digest=<digest> direct=Base@<digest> transitive=Base@<digest> trust=none
  verocaml: verified file=artifacts/v1/source_consumer.ml functions=1 obligations=0
  $ test ! -e artifacts/v1/source_consumer.cmi
  $ test ! -e artifacts/v1/source_consumer.cmo
  $ test ! -e artifacts/v1/source_consumer.cmt

Even an explicitly supplied, successfully reverified CMT dependency cannot
export recursive-rank authority. The consumer is rejected before lowering a
recursive declaration or creating output artifacts.

  $ mkdir artifacts/imported-rank
  $ cp fixtures/structural_dependency.ml fixtures/structural_consumer.ml artifacts/imported-rank/
  $ compile artifacts/imported-rank structural_dependency.ml
  $ compile artifacts/imported-rank structural_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/imported-rank/structural_consumer.cmt --timeout-ms 60000 --dependency artifacts/imported-rank/structural_dependency.cmt --dump-sst artifacts/imported-rank/consumer.sst --dump-vir artifacts/imported-rank/consumer.vir > artifacts/imported-rank/rejected.out 2>&1; echo $?
  2
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/imported-rank/rejected.out
  error[VERO_DEPENDENCY]
  $ test ! -e artifacts/imported-rank/consumer.sst
  $ test ! -e artifacts/imported-rank/consumer.vir

The process-local handle binds exact public types, callables, contracts,
models, visibility, and the proof-successful semantic snapshot. Abstract
representations are not queryable, and a marshalled copy loses authenticity.

  $ cp fixtures/model_dependency.ml fixtures/model_consumer.ml artifacts/
  $ compile artifacts model_dependency.ml
  $ compile artifacts model_consumer.ml
  $ ./verified_interfaces_tool.exe artifacts/model_consumer.cmt artifacts/model_dependency.cmt
  verified interface mode queries, invariant metadata, opacity, and serialization checks passed

Retained aggregate models are admitted only as one deterministic opaque
application.  Repeated consumer runs receive fresh process-local descriptors,
and the consumer report records no finite, recursive, rank, invariant,
transfer, ownership, backend, or direct-Z3 authority.

  $ mkdir -p artifacts/retained-model
  $ cp fixtures/model_dependency.ml fixtures/model_consumer.ml fixtures/model_failure_consumer.ml fixtures/model_excluded_consumer.ml fixtures/model_structural_consumer.ml artifacts/retained-model/
  $ compile artifacts/retained-model model_dependency.ml
  $ compile artifacts/retained-model model_consumer.ml
  $ compile artifacts/retained-model model_failure_consumer.ml
  $ compile artifacts/retained-model model_excluded_consumer.ml
  $ compile artifacts/retained-model model_structural_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained-model/model_consumer.cmt --timeout-ms 60000 --dependency artifacts/retained-model/model_dependency.cmt --dump-sst artifacts/retained-model/model.1.sst --dump-vir artifacts/retained-model/model.1.vir | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Model_dependency interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/retained-model/model_consumer.cmt functions=1 obligations=0
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained-model/model_consumer.cmt --timeout-ms 60000 --dependency artifacts/retained-model/model_dependency.cmt --dump-sst artifacts/retained-model/model.2.sst --dump-vir artifacts/retained-model/model.2.vir >/dev/null
  $ cmp artifacts/retained-model/model.1.sst artifacts/retained-model/model.2.sst && cmp artifacts/retained-model/model.1.vir artifacts/retained-model/model.2.vir && echo 'retained-model-dumps=deterministic'
  retained-model-dumps=deterministic
  $ grep -c 'specification-call Model_dependency.Stack.model' artifacts/retained-model/model.1.sst
  1
  $ grep -c 'imported-model-application Model_dependency.Stack.model' artifacts/retained-model/model.1.vir
  1
  $ if grep -F 'rank[' artifacts/retained-model/model.1.vir || grep -E 'recursive-spec-application|aggregate-constructor|aggregate-selector|finite-receipt|invariant-application' artifacts/retained-model/model.1.vir; then false; else echo 'retained-model-authority=absent-from-vir'; fi
  retained-model-authority=absent-from-vir
  $ ./verified_interfaces_tool.exe retained-lifecycle artifacts/retained-model/model_consumer.cmt artifacts/retained-model/model_dependency.cmt
  retained-model-lifecycle consumers=2 descriptors=2/2/2 finite-result=0/0/0 finite-transfer=0/0/0 recursive=0/0 aggregate-routes=0/0 solver=0 z3=0/0
  $ ./verified_interfaces_tool.exe explicit-policy artifacts/retained-model/model_consumer.cmt artifacts/retained-model/model_dependency.cmt
  explicit-policy provider=verified consumer=verified
  $ ./verified_interfaces_tool.exe retained-failure-teardown artifacts/retained-model/model_failure_consumer.cmt artifacts/retained-model/model_dependency.cmt
  retained-model-failure status=counterexample descriptors=1/1/1 teardown=exactly-once

Public recursive immutable closure remains available through authenticated
provider completion. Historical textual closed specializations are diagnostic
only and reject before consumer SST/VIR.

  $ cp fixtures/recursive_model_dependency.ml fixtures/recursive_model_consumer.ml fixtures/recursive_model_structural_consumer.ml fixtures/recursive_model_finite_consumer.ml fixtures/recursive_model_constructor_consumer.ml fixtures/recursive_model_recursive_consumer.ml fixtures/recursive_model_invariant_consumer.ml fixtures/recursive_model_reveal_consumer.ml artifacts/retained-model/
  $ compile artifacts/retained-model recursive_model_dependency.ml
  $ for n in recursive_model_consumer recursive_model_structural_consumer recursive_model_finite_consumer recursive_model_constructor_consumer recursive_model_recursive_consumer recursive_model_invariant_consumer recursive_model_reveal_consumer; do compile artifacts/retained-model "$n.ml"; done
  $ ./verified_interfaces_tool.exe retained-lifecycle artifacts/retained-model/recursive_model_consumer.cmt artifacts/retained-model/recursive_model_dependency.cmt
  retained-model-lifecycle consumers=2 descriptors=2/2/2 finite-result=0/0/0 finite-transfer=0/0/0 recursive=0/0 aggregate-routes=0/0 solver=0 z3=0/0
  $ cp fixtures/generic_model_dependency.ml fixtures/generic_model_consumer.ml fixtures/generic_model_open_consumer.ml artifacts/retained-model/
  $ compile artifacts/retained-model generic_model_dependency.ml
  $ compile artifacts/retained-model generic_model_consumer.ml
  $ compile artifacts/retained-model generic_model_open_consumer.ml
  $ for n in generic_model_consumer generic_model_open_consumer; do code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/retained-model/$n.cmt" --timeout-ms 60000 --dependency artifacts/retained-model/generic_model_dependency.cmt --dump-sst "artifacts/retained-model/$n.sst" --dump-vir "artifacts/retained-model/$n.vir" >/dev/null 2>&1 || code=$?; test "$code" = 2; test ! -e "artifacts/retained-model/$n.sst" && test ! -e "artifacts/retained-model/$n.vir"; done; echo 'legacy-closed-model-authority=rejected sst=absent vir=absent'
  legacy-closed-model-authority=rejected sst=absent vir=absent
  $ ./verified_interfaces_tool.exe retained-generic-adapter-matrix
  retained-generic-adapter textual-exact=rejected open=rejected wrong=rejected stale=rejected mixed-family=rejected

The legacy clone/rank-era provider is outside the schema-first first tranche:
its mutable recursive model requires excluded rank authority. Providers and
consumers reject before SST/VIR, independent of declaration order.

  $ mkdir artifacts/generic-family
  $ cp fixtures/generic_family_*.ml artifacts/generic-family/
  $ for n in generic_family_provider generic_family_consumer generic_family_sibling_consumer generic_family_structural_consumer generic_family_finite_consumer generic_family_constructor_consumer generic_family_recursive_consumer generic_family_invariant_consumer generic_family_reveal_consumer generic_family_equality_consumer generic_family_arbitrary_equality_consumer generic_family_exec_consumer generic_family_proof_consumer generic_family_spec_consumer generic_family_rank_consumer generic_family_reordered_provider generic_family_reordered_consumer; do compile artifacts/generic-family "$n.ml"; done
  $ for n in provider reordered_provider; do code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/generic-family/generic_family_$n.cmt" --timeout-ms 60000 --dump-sst "artifacts/generic-family/$n.sst" --dump-vir "artifacts/generic-family/$n.vir" > "artifacts/generic-family/$n.out" 2>&1 || code=$?; test "$code" = 2; grep -q 'error\\[VERO_DEPENDENCY\\]' "artifacts/generic-family/$n.out"; test ! -e "artifacts/generic-family/$n.sst" && test ! -e "artifacts/generic-family/$n.vir"; done; echo 'excluded-mutable-rank-provider=rejected order=independent sst=absent vir=absent'
  excluded-mutable-rank-provider=rejected order=independent sst=absent vir=absent
  $ for n in consumer sibling_consumer structural_consumer finite_consumer constructor_consumer recursive_consumer invariant_consumer reveal_consumer equality_consumer arbitrary_equality_consumer exec_consumer proof_consumer spec_consumer rank_consumer; do code=0; sst="artifacts/generic-family/$n.sst"; vir="artifacts/generic-family/$n.vir"; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/generic-family/generic_family_$n.cmt" --timeout-ms 60000 --dependency artifacts/generic-family/generic_family_provider.cmt --dump-sst "$sst" --dump-vir "$vir" >/dev/null 2>&1 || code=$?; test "$code" = 2; test ! -e "$sst" && test ! -e "$vir"; done; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic-family/generic_family_reordered_consumer.cmt --timeout-ms 60000 --dependency artifacts/generic-family/generic_family_reordered_provider.cmt --dump-sst artifacts/generic-family/reordered.sst --dump-vir artifacts/generic-family/reordered.vir >/dev/null 2>&1 || code=$?; test "$code" = 2; test ! -e artifacts/generic-family/reordered.sst && test ! -e artifacts/generic-family/reordered.vir; echo 'textual-generic-family-authority=rejected consumers=15 sst=absent vir=absent'
  textual-generic-family-authority=rejected consumers=15 sst=absent vir=absent
  $ ./verified_interfaces_tool.exe retained-generic-family-sealing-matrix
  closed-arguments-authority=removed
  textual-int-specialization=rejected
  textual-bool-specialization=rejected
  textual-nested-specialization=rejected
  $ ./verified_interfaces_tool.exe retained-backend-matrix
  retained-model-backend congruence=verified different-actual=counterexample direct-parity=verified call-identities=ignored copy=unauthentic stale=zero-solver-rejected forged-provenance=zero-native-rejected nested=zero-native-rejected substitutions=7x-zero-solver

A historical clone-only generic summary likewise cannot authorize a retained
consumer call.

  $ cp fixtures/unrelated_generic_dependency.ml fixtures/unrelated_generic_consumer.ml artifacts/retained-model/
  $ compile artifacts/retained-model unrelated_generic_dependency.ml
  $ compile artifacts/retained-model unrelated_generic_consumer.ml
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained-model/unrelated_generic_consumer.cmt --timeout-ms 60000 --dependency artifacts/retained-model/unrelated_generic_dependency.cmt --dump-sst artifacts/retained-model/unrelated-generic.sst --dump-vir artifacts/retained-model/unrelated-generic.vir >/dev/null 2>&1 || code=$?; test "$code" = 2; test ! -e artifacts/retained-model/unrelated-generic.sst && test ! -e artifacts/retained-model/unrelated-generic.vir; echo 'clone-only-generic-authority=rejected sst=absent vir=absent'
  clone-only-generic-authority=rejected sst=absent vir=absent

An old exact-specialization consumer cannot be replayed against a freshly
rebuilt provider snapshot.  The CRC mismatch rejects before consumer SST/VIR.

  $ mkdir artifacts/retained-model/generic-stale
  $ cp fixtures/generic_model_dependency.ml fixtures/generic_model_consumer.ml artifacts/retained-model/generic-stale/
  $ compile artifacts/retained-model/generic-stale generic_model_dependency.ml
  $ compile artifacts/retained-model/generic-stale generic_model_consumer.ml
  $ chmod u+w artifacts/retained-model/generic-stale/generic_model_dependency.ml
  $ printf '\nlet stale_generation = 1\n' >> artifacts/retained-model/generic-stale/generic_model_dependency.ml
  $ compile artifacts/retained-model/generic-stale generic_model_dependency.ml
  $ if OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained-model/generic-stale/generic_model_consumer.cmt --timeout-ms 60000 --dependency artifacts/retained-model/generic-stale/generic_model_dependency.cmt --dump-sst artifacts/retained-model/generic-stale/consumer.sst --dump-vir artifacts/retained-model/generic-stale/consumer.vir > artifacts/retained-model/generic-stale/rejected.out 2>&1; then false; else echo 'retained-generic-stale=zero-output-rejected'; fi
  retained-generic-stale=zero-output-rejected
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/retained-model/generic-stale/rejected.out | head -1
  error[VERO_DEPENDENCY]
  $ test ! -e artifacts/retained-model/generic-stale/consumer.sst && test ! -e artifacts/retained-model/generic-stale/consumer.vir

The private schema matrix pins public/revealed deep immutability, existing
immutable tuple preservation, foreign-hole exclusion, direct uniform recursive
admission without rank authority, and provider-wide filtering.

  $ ./verified_interfaces_tool.exe retained-closure-matrix
  schema-record=retained
  schema-hidden=excluded
  schema-mutable=excluded
  schema-tuple=retained
  schema-foreign=excluded
  schema-uniform-recursive=retained
  schema-provider-filter=accepted-model-only

Ordinary UF congruence ignores registration and call-instance identity.
Different actuals receive no equality, while copied, stale, and forged
provenance rejects before solver creation.

  $ ./verified_interfaces_tool.exe retained-backend-matrix
  retained-model-backend congruence=verified different-actual=counterexample direct-parity=verified call-identities=ignored copy=unauthentic stale=zero-solver-rejected forged-provenance=zero-native-rejected nested=zero-native-rejected substitutions=7x-zero-solver

Provider-wide filtering does not poison the eligible model, but the excluded
aggregate constructor and I-063-001 structural/authority uses reject with zero
consumer deltas beyond the separately authenticated provider baseline.

  $ ./verified_interfaces_tool.exe retained-reject excluded artifacts/retained-model/model_excluded_consumer.cmt artifacts/retained-model/model_dependency.cmt 2>/dev/null
  retained-model-rejection=excluded descriptor-delta=0/0/0 recursive-delta=0/0 aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0
  $ ./verified_interfaces_tool.exe retained-reject selector artifacts/retained-model/model_structural_consumer.cmt artifacts/retained-model/model_dependency.cmt 2>/dev/null
  retained-model-rejection=selector descriptor-delta=0/0/0 recursive-delta=0/0 aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0
  $ for row in pattern finite constructor recursive invariant reveal; do case "$row" in pattern) fixture=recursive_model_structural_consumer;; finite) fixture=recursive_model_finite_consumer;; constructor) fixture=recursive_model_constructor_consumer;; recursive) fixture=recursive_model_recursive_consumer;; invariant) fixture=recursive_model_invariant_consumer;; reveal) fixture=recursive_model_reveal_consumer;; esac; ./verified_interfaces_tool.exe retained-reject "$row" "artifacts/retained-model/$fixture.cmt" artifacts/retained-model/recursive_model_dependency.cmt 2>/dev/null; done
  retained-model-rejection=pattern descriptor-delta=0/0/0 recursive-delta=0/0 aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0
  retained-model-rejection=finite descriptor-delta=0/0/0 recursive-delta=0/0 aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0
  retained-model-rejection=constructor descriptor-delta=0/0/0 recursive-delta=0/0 aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0
  retained-model-rejection=recursive descriptor-delta=0/0/0 recursive-delta=0/0 aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0
  retained-model-rejection=invariant descriptor-delta=0/0/0 recursive-delta=0/0 aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0
  retained-model-rejection=reveal descriptor-delta=0/0/0 recursive-delta=0/0 aggregate-route-delta=0/0 solver-delta=0 z3-delta=0/0

Every graph/authentication failure happens before consumer lowering or dump
creation. Cycles (including a crafted CRC-complete consumer back-edge), missing
transitive inputs, duplicate units, stale substitutions, prefix lookalikes,
canonical-slot attacks, unused slots, malformed artifacts, unsupported
PPX/runtime carriers, separate hidden interfaces, and failed dependency proofs
reject.

  $ reject_without_consumer () { label=$1; shift; sst="artifacts/$label.sst"; vir="artifacts/$label.vir"; if OCAML_COLOR=never ../../src/verocaml.exe verify "$@" --timeout-ms 60000 --dump-sst "$sst" --dump-vir "$vir" > "artifacts/$label.out" 2>&1; then return 1; else code=$?; fi; test "$code" = 2; test ! -e "$sst"; test ! -e "$vir"; printf '%s: ' "$label"; grep -o 'error\[VERO_[A-Z_]*\]' "artifacts/$label.out" | head -1; }
  $ consumer_crc=$(./verified_interfaces_tool.exe interface-digest artifacts/v1/consumer.cmt)
  $ ./verified_interfaces_tool.exe add-import artifacts/v1/base.cmt artifacts/base-consumer-back-edge.cmt Consumer "$consumer_crc"
  $ reject_without_consumer consumer-back-edge artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/base-consumer-back-edge.cmt
  consumer-back-edge: error[VERO_DEPENDENCY]
  $ ./handle_barrier_tool.exe artifacts/v1/consumer.cmt artifacts/v1/middle.cmt artifacts/base-consumer-back-edge.cmt
  failed authentication constructed 0 handles
  $ mkdir artifacts/cycle
  $ cat > artifacts/cycle/a.ml <<'EOF'
  > module B = B
  > EOF
  $ cat > artifacts/cycle/b.ml <<'EOF'
  > module A = A
  > EOF
  $ (cd artifacts/cycle && ocamlc -w -A -alert -all -no-alias-deps -bin-annot -I "$GHOST" -ppx "$PPX" -c a.ml && ocamlc -w -A -alert -all -no-alias-deps -bin-annot -I "$GHOST" -ppx "$PPX" -c b.ml)
  $ cat > artifacts/cycle/cycle_consumer.ml <<'EOF'
  > let local () = 0
  > EOF
  $ (cd artifacts/cycle && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -open A -c cycle_consumer.ml)
  $ reject_without_consumer cyclic artifacts/cycle/cycle_consumer.cmt --dependency artifacts/cycle/a.cmt --dependency artifacts/cycle/b.cmt
  cyclic: error[VERO_DEPENDENCY]
  $ reject_without_consumer missing-transitive artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt
  missing-transitive: error[VERO_DEPENDENCY]
  $ cp artifacts/v1/base.cmt artifacts/base-copy.cmt
  $ reject_without_consumer duplicate-unit artifacts/v1/consumer.cmt --dependency artifacts/v1/base.cmt --dependency artifacts/base-copy.cmt --dependency artifacts/v1/middle.cmt
  duplicate-unit: error[VERO_DEPENDENCY]
  $ cp fixtures/base.ml artifacts/stale/base.ml
  $ chmod u+w artifacts/stale/base.ml
  $ printf '\nlet changed () = 0\n' >> artifacts/stale/base.ml
  $ compile artifacts/stale base.ml
  $ reject_without_consumer stale artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/stale/base.cmt
  stale: error[VERO_DEPENDENCY]
  $ cp fixtures/base.ml artifacts/lookalike/lookalike.ml
  $ compile artifacts/lookalike lookalike.ml
  $ reject_without_consumer lookalike artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/lookalike/lookalike.cmt
  lookalike: error[VERO_DEPENDENCY]
  $ mkdir artifacts/stdlib-prefix artifacts/camlinternal-prefix
  $ cat > artifacts/stdlib-prefix/stdlib_forgery.ml <<'EOF'
  > let forged () = 0
  > EOF
  $ cat > artifacts/stdlib-prefix/prefix_dependency.ml <<'EOF'
  > open Stdlib_forgery
  > let local () = 0
  > EOF
  $ cat > artifacts/stdlib-prefix/prefix_consumer.ml <<'EOF'
  > open Prefix_dependency
  > let local () = 0
  > EOF
  $ compile artifacts/stdlib-prefix stdlib_forgery.ml
  $ compile artifacts/stdlib-prefix prefix_dependency.ml
  $ compile artifacts/stdlib-prefix prefix_consumer.ml
  $ reject_without_consumer stdlib-prefix-lookalike artifacts/stdlib-prefix/prefix_consumer.cmt --dependency artifacts/stdlib-prefix/prefix_dependency.cmt
  stdlib-prefix-lookalike: error[VERO_DEPENDENCY]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stdlib-prefix/prefix_consumer.cmt --timeout-ms 60000 --dependency artifacts/stdlib-prefix/prefix_dependency.cmt --dependency artifacts/stdlib-prefix/stdlib_forgery.cmt > artifacts/stdlib-prefix-supplied.out
  $ test "$(grep -c 'verified dependency' artifacts/stdlib-prefix-supplied.out)" = 2
  $ grep 'verified dependency unit=Stdlib_forgery ' artifacts/stdlib-prefix-supplied.out >/dev/null
  $ cat > artifacts/camlinternal-prefix/camlinternal_forgery.ml <<'EOF'
  > let forged () = 0
  > EOF
  $ cat > artifacts/camlinternal-prefix/prefix_dependency.ml <<'EOF'
  > open Camlinternal_forgery
  > let local () = 0
  > EOF
  $ cat > artifacts/camlinternal-prefix/prefix_consumer.ml <<'EOF'
  > open Prefix_dependency
  > let local () = 0
  > EOF
  $ compile artifacts/camlinternal-prefix camlinternal_forgery.ml
  $ compile artifacts/camlinternal-prefix prefix_dependency.ml
  $ compile artifacts/camlinternal-prefix prefix_consumer.ml
  $ reject_without_consumer camlinternal-prefix-lookalike artifacts/camlinternal-prefix/prefix_consumer.cmt --dependency artifacts/camlinternal-prefix/prefix_dependency.cmt
  camlinternal-prefix-lookalike: error[VERO_DEPENDENCY]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/camlinternal-prefix/prefix_consumer.cmt --timeout-ms 60000 --dependency artifacts/camlinternal-prefix/prefix_dependency.cmt --dependency artifacts/camlinternal-prefix/camlinternal_forgery.cmt > artifacts/camlinternal-prefix-supplied.out
  $ test "$(grep -c 'verified dependency' artifacts/camlinternal-prefix-supplied.out)" = 2
  $ grep 'verified dependency unit=Camlinternal_forgery ' artifacts/camlinternal-prefix-supplied.out >/dev/null
  $ ./verified_interfaces_tool.exe remove-import artifacts/v1/base.cmt artifacts/base-missing-stdlib.cmt Stdlib
  $ reject_without_consumer missing-canonical-slot artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/base-missing-stdlib.cmt
  missing-canonical-slot: error[VERO_DEPENDENCY]
  $ ./verified_interfaces_tool.exe set-import artifacts/v1/base.cmt artifacts/base-wrong-canonical.cmt CamlinternalFormatBasics 00000000000000000000000000000000
  $ reject_without_consumer wrong-canonical-slot artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/base-wrong-canonical.cmt
  wrong-canonical-slot: error[VERO_DEPENDENCY]
  $ ./verified_interfaces_tool.exe set-import artifacts/v1/base.cmt artifacts/base-crcless-canonical.cmt Stdlib none
  $ reject_without_consumer crcless-canonical-slot artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/base-crcless-canonical.cmt
  crcless-canonical-slot: error[VERO_DEPENDENCY]
  $ reject_without_consumer unused artifacts/v1/consumer.cmt --dependency artifacts/v1/base.cmt
  unused: error[VERO_DEPENDENCY]
  $ printf 'not a cmt\n' > artifacts/malformed.cmt
  $ reject_without_consumer malformed artifacts/v1/consumer.cmt --dependency artifacts/malformed.cmt
  malformed: error[VERO_DEPENDENCY]
  $ cp fixtures/no_ppx.ml fixtures/no_ppx_consumer.ml artifacts/
  $ (cd artifacts && ocamlc -w -A -alert -all -bin-annot -c no_ppx.ml)
  $ compile artifacts no_ppx_consumer.ml
  $ reject_without_consumer ppx-mismatch artifacts/no_ppx_consumer.cmt --dependency artifacts/no_ppx.cmt
  ppx-mismatch: error[VERO_DEPENDENCY]
  $ cp ../../runtime/vero_ghost.mli artifacts/fake-runtime/vero_ghost.mli
  $ chmod u+w artifacts/fake-runtime/vero_ghost.mli
  $ printf '\nval incompatible_carrier : unit\n' >> artifacts/fake-runtime/vero_ghost.mli
  $ (cd artifacts/fake-runtime && ocamlc -c vero_ghost.mli)
  $ cat > artifacts/fake-runtime/runtime_mismatch.ml <<'EOF'
  > let checked (value : int) =
  >   [%verocaml.requires value >= 0];
  >   value
  > EOF
  $ (cd artifacts/fake-runtime && ocamlc -w -A -alert -all -bin-annot -I . -ppx "$PPX" -c runtime_mismatch.ml)
  $ cat > artifacts/fake-runtime/runtime_consumer.ml <<'EOF'
  > open Runtime_mismatch
  > let local () = 0
  > EOF
  $ compile artifacts/fake-runtime runtime_consumer.ml
  $ reject_without_consumer runtime-mismatch artifacts/fake-runtime/runtime_consumer.cmt --dependency artifacts/fake-runtime/runtime_mismatch.cmt
  runtime-mismatch: error[VERO_DEPENDENCY]
  $ cp fixtures/hidden.ml fixtures/hidden.mli fixtures/hidden_consumer.ml artifacts/
  $ (cd artifacts && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c hidden.mli && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c hidden.ml)
  $ compile artifacts hidden_consumer.ml
  $ reject_without_consumer hidden-representation artifacts/hidden_consumer.cmt --dependency artifacts/hidden.cmt
  hidden-representation: error[VERO_DEPENDENCY]
  $ cp fixtures/hidden_contract.ml fixtures/hidden_contract_consumer.ml artifacts/
  $ compile artifacts hidden_contract.ml
  $ compile artifacts hidden_contract_consumer.ml
  $ reject_without_consumer hidden-contract artifacts/hidden_contract_consumer.cmt --dependency artifacts/hidden_contract.cmt
  hidden-contract: error[VERO_DEPENDENCY]
  $ cp fixtures/hidden_nested_contract.ml fixtures/hidden_nested_contract_consumer.ml artifacts/
  $ compile artifacts hidden_nested_contract.ml
  $ compile artifacts hidden_nested_contract_consumer.ml
  $ reject_without_consumer hidden-nested-contract artifacts/hidden_nested_contract_consumer.cmt --dependency artifacts/hidden_nested_contract.cmt
  hidden-nested-contract: error[VERO_DEPENDENCY]
  $ cp fixtures/failed_proof.ml fixtures/failed_consumer.ml artifacts/
  $ compile artifacts failed_proof.ml
  $ compile artifacts failed_consumer.ml
  $ reject_without_consumer failed-proof artifacts/failed_consumer.cmt --dependency artifacts/failed_proof.cmt
  failed-proof: error[VERO_DEPENDENCY]

Handle construction is a graph-wide barrier. A valid Base snapshot is staged
before the later dependency proof fails, but the instrumented exact
implementation constructs no process-local handle.

  $ mkdir artifacts/handle-barrier
  $ cp fixtures/base.ml artifacts/handle-barrier/
  $ compile artifacts/handle-barrier base.ml
  $ cat > artifacts/handle-barrier/later_failure.ml <<'EOF'
  > open Base
  > let increment (value : int) = value + 1
  > EOF
  $ cat > artifacts/handle-barrier/barrier_consumer.ml <<'EOF'
  > open Later_failure
  > let local () = 0
  > EOF
  $ compile artifacts/handle-barrier later_failure.ml
  $ compile artifacts/handle-barrier barrier_consumer.ml
  $ ./handle_barrier_tool.exe artifacts/handle-barrier/barrier_consumer.cmt artifacts/handle-barrier/base.cmt artifacts/handle-barrier/later_failure.cmt
  failed authentication constructed 0 handles

The installed interface exposes no handle representation or issuer fields.
Adding all installed private include directories does not make a raw
record/token-shaped forgery compile, and no model-body accessor exists.

  $ root="${PWD%%/_build/*}"
  $ install_root="${VEROCAML_TEST_INSTALL_ROOT:-$root/_build/install/default}"
  $ core="$install_root/lib/verocaml/core"
  $ private_flags="$(find "$install_root/lib/verocaml" -type d -name '.private' -printf '%p\n' | sed 's/^/-I /' | tr '\n' ' ')"
  $ ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -c -I "$core" $private_flags fixtures/installed_handle_forgery.ml > artifacts/installed-forgery.out 2>&1; echo $?
  2
  $ grep -E 'Unbound record field|Cannot create values of the private type|This record expression is expected to have type' artifacts/installed-forgery.out >/dev/null
  $ ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -c -I "$core" $private_flags fixtures/installed_model_body_access.ml > artifacts/installed-model-body.out 2>&1; echo $?
  2
  $ grep "Unbound value.*public_model_body" artifacts/installed-model-body.out >/dev/null

The cold installed public API preserves the legacy two-field error record.
Publicly constructed records carry no private diagnostic association.

  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -linkpkg -package verocaml.core fixtures/installed_legacy_error.ml -o artifacts/installed-legacy-error.exe 2>/dev/null
  $ artifacts/installed-legacy-error.exe
  legacy constructor
  diagnostic=none

Old backend calls and wildcard inconclusive matches compile unchanged. Exact
construction/destructuring of the former one-field payload is intentionally
rejected, while the reviewed typed payload compiles.

  $ cat > artifacts/solver_api_old_ok.ml <<'EOF'
  > let configured = Solver_backend.config ~timeout_ms:1
  > let classify = function Solver_backend.Inconclusive _ -> 3 | _ -> 0
  > EOF
  $ cat > artifacts/solver_api_old_record.ml <<'EOF'
  > let value = Solver_backend.Inconclusive { configured_timeout_ms = 1 }
  > let exact = function Solver_backend.Inconclusive { configured_timeout_ms } -> configured_timeout_ms | _ -> 0
  > EOF
  $ cat > artifacts/solver_api_typed_record.ml <<'EOF'
  > let configured = Solver_backend.config_with_rlimit ~timeout_ms:1 ~rlimit:1
  > let value = Solver_backend.Inconclusive { configured_timeout_ms = 1; configured_rlimit = 1; reason = Solver_backend.Resource_exhausted }
  > EOF
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -package verocaml.core -c artifacts/solver_api_old_ok.ml 2>/dev/null
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -w +9 -warn-error +9 -package verocaml.core -c artifacts/solver_api_old_record.ml > artifacts/solver-api-old-record.out 2>&1; echo $?
  2
  $ grep -E 'Some record fields are undefined|missing-record-field-pattern' artifacts/solver-api-old-record.out >/dev/null
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -package verocaml.core -c artifacts/solver_api_typed_record.ml 2>/dev/null
