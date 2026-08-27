The accepted same-CMT positives verify through the production source driver.
Every verifier invocation in this gate has an external hard timeout.

  $ mkdir -p artifacts
  $ retained () { n=$1; timeout --signal=TERM --kill-after=2s 30s ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained_names='lifecycle_positive closed_baseline missing_predecessor invariant_positive invariant_establishment_failure invariant_preservation_failure grammar_omitted grammar_duplicated grammar_substituted grammar_nonexhaustive grammar_indirect grammar_mutable_result external_recursive_model trusted_recursive_model completion_construction completion_reconstruction completion_model completion_safety completion_contract cyclic_topology general_transition_state_scalar'
  $ for n in $retained_names; do retained "$n"; done
  $ for n in lifecycle_positive invariant_positive; do timeout --foreground --signal=TERM --kill-after=2s 30s ../../src/verocaml.exe verify "fixtures/$n.ml" --timeout-ms 5000 | sed -E 's@file=fixtures/[^ ]+@file=FIXTURE@; s/functions=[0-9]+ obligations=[0-9]+/verified-shape/'; done
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape

SST and VIR retain the exact recursive helper, current-version transitions,
and concrete immutable result construction, while private receipt/permit
authority is absent from serialized output. Repeated dumps are deterministic.

  $ timeout --foreground --signal=TERM --kill-after=2s 30s ../../src/verocaml.exe verify fixtures/lifecycle_positive.ml --timeout-ms 5000 --dump-sst artifacts/lifecycle.1.sst --dump-vir artifacts/lifecycle.1.vir >/dev/null
  $ timeout --foreground --signal=TERM --kill-after=2s 30s ../../src/verocaml.exe verify fixtures/lifecycle_positive.ml --timeout-ms 5000 --dump-sst artifacts/lifecycle.2.sst --dump-vir artifacts/lifecycle.2.vir >/dev/null
  $ cmp artifacts/lifecycle.1.sst artifacts/lifecycle.2.sst && cmp artifacts/lifecycle.1.vir artifacts/lifecycle.2.vir && echo 'dumps=deterministic'
  dumps=deterministic
  $ grep -c 'body recursive-spec-definition stage=logical visibility=opaque provenance=typedtree' artifacts/lifecycle.1.sst
  1
  $ grep -c '^  owned-tree-transition' artifacts/lifecycle.1.vir
  3
  $ grep -c 'owned-tree-nested-write policy=functional-no-heap' artifacts/lifecycle.1.sst
  1
  $ grep -c 'owned-tree-rebase policy=functional-no-heap' artifacts/lifecycle.1.sst
  1
  $ grep -c 'ctor.More#1' artifacts/lifecycle.1.vir | awk '{ if ($1 > 0) print "immutable-result-constructions=present"; else exit 1 }'
  immutable-result-constructions=present
  $ if grep -E 'owned[_-]contents.*(receipt|permit)|owned_recursive_contents_private' artifacts/lifecycle.1.sst artifacts/lifecycle.1.vir; then false; else echo 'private-authority=absent-from-sst-vir'; fi
  private-authority=absent-from-sst-vir

The focused private matrix checks deterministic lifecycle counters, retained
grammar zero-work rows, affine identity attacks, missing predecessor evidence,
same-process session isolation, and each independent completion barrier.

  $ timeout --foreground --signal=TERM --kill-after=2s 90s ./owned_recursive_contents_pipeline_tool.exe --matrix > artifacts/matrix
  $ grep '^positive=' artifacts/matrix
  positive=lifecycle_positive status=verified lineages=6/6/0/6 graphs=9 mapped=30 candidates=9/9/0/9 permits=21/21/0 routes=30 equations=30 results=9 manifests=6 completions=6 receipts=9 successors=3 retirements=3/21 teardown=9/21/9
  positive=invariant_positive status=verified lineages=3/3/0/3 graphs=9 mapped=22 candidates=9/9/0/9 permits=13/13/0 routes=22 equations=22 results=9 manifests=3 completions=3 receipts=5 successors=2 retirements=2/13 teardown=9/13/5
  $ grep '^negative=' artifacts/matrix
  negative=grammar_omitted rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  negative=grammar_duplicated rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  negative=grammar_substituted rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  negative=grammar_nonexhaustive rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  negative=grammar_indirect rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  negative=grammar_mutable_result rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  negative=external_recursive_model rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  negative=trusted_recursive_model rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  negative=general_transition_state_scalar rejected sessions=0 owned=0 backend=0 z3=0/0 logic=0
  $ grep -E '^(grammar|predecessor)=' artifacts/matrix
  grammar=forged-source rejected owned=0 backend=0 z3=0/0 logic=0
  predecessor=missing rejected candidates=0/0/1/0 downstream=0 backend=0
  $ grep '^attack=' artifacts/matrix | sed -E 's/ rejected.*$/ rejected-zero-work/'
  attack=copied rejected-zero-work
  attack=replay rejected-zero-work
  attack=wrong-session rejected-zero-work
  attack=wrong-program rejected-zero-work
  attack=wrong-cmt rejected-zero-work
  attack=wrong-family rejected-zero-work
  attack=wrong-model rejected-zero-work
  attack=wrong-helper rejected-zero-work
  attack=wrong-body rejected-zero-work
  attack=wrong-signature rejected-zero-work
  attack=wrong-root rejected-zero-work
  attack=stale-root rejected-zero-work
  attack=wrong-version rejected-zero-work
  attack=wrong-path rejected-zero-work
  attack=wrong-grammar rejected-zero-work
  attack=wrong-edge rejected-zero-work
  attack=wrong-result rejected-zero-work
  attack=wrong-mapped-child rejected-zero-work
  attack=missing-construction rejected-zero-work
  attack=failed-transition rejected-zero-work
  attack=ambiguous-successor rejected-zero-work
  attack=stale-lineage-replay rejected-zero-work
  attack=cross-function rejected-zero-work
  attack=cyclic rejected-zero-work
  attack=shared rejected-zero-work
  attack=detached rejected-zero-work
  attack=duplicate-root rejected-zero-work
  attack=cross-branch-union rejected-zero-work
  $ grep '^two-session=' artifacts/matrix
  two-session=isolated sessions=2 receipts=1+1 teardown=1+1
  $ grep '^seed-control=' artifacts/matrix
  seed-control=production seed-path=1/1/3/1 failure-path=1/1/4/1 failure-authority=0/0/0/0 failure-dependent=0/0/0 seed-survived=live@0 success-path=1/1/4/1 success-authority=1/1/1/1 seed-retirement=exactly-once second-retirement=rejected replay=rejected
  $ grep '^completion=' artifacts/matrix
  completion=construction failed=1 lineages=1/0/1/1 graphs=1 candidates=1/1/1 manifests=1 completions=0 receipts=0 successor=0 retirement=0/1
  completion=reconstruction failed=1 lineages=1/0/1/1 graphs=2 candidates=2/2/2 manifests=1 completions=0 receipts=0 successor=0 retirement=0/3
  completion=recursive-model failed=1 lineages=1/0/1/1 graphs=1 candidates=1/1/1 manifests=1 completions=0 receipts=0 successor=0 retirement=0/1
  completion=safety failed=1 lineages=1/0/1/1 graphs=1 candidates=1/1/1 manifests=1 completions=0 receipts=0 successor=0 retirement=0/1
  completion=contract failed=1 lineages=1/0/1/1 graphs=1 candidates=1/1/1 manifests=1 completions=0 receipts=0 successor=0 retirement=0/1
  completion=invariant-establishment failed=1 lineages=1/0/1/1 graphs=1 candidates=1/1/1 manifests=1 completions=0 receipts=0 successor=0 retirement=0/1
  completion=invariant-preservation failed=1 lineages=1/0/1/1 graphs=4 candidates=4/4/4 manifests=1 completions=0 receipts=0 successor=0 retirement=0/6

A concrete cyclic source is retained but rejects before recursive lowering and
returns within the external bound. Mutual source recursion is rejected by the
PPX's single-binding boundary.

  $ if timeout --foreground --signal=TERM --kill-after=2s 15s env OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/cyclic_topology.ml --timeout-ms 1000 > artifacts/cyclic.out 2>&1; then false; else echo 'cyclic-source=rejected-bounded'; fi
  cyclic-source=rejected-bounded
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/cyclic.out | head -1
  error[VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION]
  $ if timeout --signal=TERM --kill-after=2s 30s ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/grammar_mutual.cmo fixtures/grammar_mutual.ml > artifacts/grammar_mutual.out 2>&1; then false; else echo 'mutual-source=rejected'; fi
  mutual-source=rejected
  $ grep -F 'requires a single top-level binding' artifacts/grammar_mutual.out >/dev/null && echo 'mutual-boundary=single-binding'
  mutual-boundary=single-binding

An actual retained provider CMT cannot export hidden recursive observation
authority to a consumer CMT.

  $ mkdir -p artifacts/import
  $ cp support/provider.ml artifacts/import/provider.ml
  $ cat > artifacts/import/consumer.ml <<'EOF'
  > let imported_contents (stack : Provider.Stack.t @ read) =
  >   match Provider.Stack.model stack with
  >   | Provider.End -> 0
  >   | Provider.More _ -> 1
  > EOF
  $ export PPX="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ timeout --signal=TERM --kill-after=2s 30s sh -c 'cd artifacts/import && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c provider.ml && ocamlc -w -A -alert -all -bin-annot -I . -I "$GHOST" -ppx "$PPX" -c consumer.ml'
  $ if timeout --foreground --signal=TERM --kill-after=2s 15s env OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/import/consumer.cmt --dependency artifacts/import/provider.cmt --dump-sst artifacts/import/consumer.sst --dump-vir artifacts/import/consumer.vir > artifacts/import/rejected 2>&1; then false; else echo 'cross-cmt=zero-output-rejected'; fi
  cross-cmt=zero-output-rejected
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/import/rejected | head -1
  error[VERO_MALFORMED_GHOST_CALL]
  $ test ! -e artifacts/import/consumer.sst && test ! -e artifacts/import/consumer.vir && echo 'cross-cmt-sst-vir=absent'
  cross-cmt-sst-vir=absent
