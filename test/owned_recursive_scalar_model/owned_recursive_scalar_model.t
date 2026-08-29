The accepted positives and focused controls contain no invariant or capability
carrier and verify through the production CLI.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ positives='scalar_model_baseline successor_freshness nonvacuous_cut opaque_local_client constant_head_no_value_path cross_template_demand_isolation uncontracted_mutation_no_demand direct_root_reconstruction_control nested_noop_reconstruction_control'
  $ for n in $positives; do retained "$n"; done
  $ if grep -E 'type_invariant|use_type_invariant|closed.valid|invariant receipt' $(for n in $positives; do printf 'fixtures/%s.ml ' "$n"; done); then false; else echo 'positive invariant/capability carriers absent'; fi
  positive invariant/capability carriers absent
  $ for n in $positives; do OCAML_COLOR=never ../../src/verocaml.exe verify "fixtures/$n.ml" --timeout-ms 5000 | sed -E 's@file=fixtures/[^ ]+@file=FIXTURE@; s/functions=[0-9]+ obligations=[0-9]+/verified-shape/'; done
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape
  verocaml: verified file=FIXTURE verified-shape

The explicit cut fixture constructs two Nodes, excludes an empty/singleton
predecessor with length two and nonempty, and pins the pre-cut head with old.

  $ grep -c 'next = Node' fixtures/nonvacuous_cut.ml
  1
  $ grep -F 'shape.has_two_nodes' fixtures/nonvacuous_cut.ml | wc -l
  3
  $ grep -F 'view.head = [%verocaml.old (model stack).head]' fixtures/nonvacuous_cut.ml
        && view.head = [%verocaml.old (model stack).head]
  $ sed -e '0,/let two_nodes first second/s//let two_nodes first (second : int)/' -e '0,/next = Node { value = second; next = Empty }/s//next = Empty/' fixtures/nonvacuous_cut.ml > artifacts/nonvacuous_cut_singleton_mutant.ml
  $ if OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/nonvacuous_cut_singleton_mutant.ml --timeout-ms 1000 > artifacts/nonvacuous_cut_singleton_mutant.out 2>&1; then false; else echo 'singleton-predecessor-mutant=rejected'; fi
  singleton-predecessor-mutant=rejected
  $ grep -o 'counterexample function=Stack.two_nodes' artifacts/nonvacuous_cut_singleton_mutant.out
  counterexample function=Stack.two_nodes
  $ sed '/record.next <- Empty;/d' fixtures/nonvacuous_cut.ml > artifacts/nonvacuous_cut_omit_next_write_mutant.ml
  $ if OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/nonvacuous_cut_omit_next_write_mutant.ml --timeout-ms 1000 > artifacts/nonvacuous_cut_omit_next_write_mutant.out 2>&1; then false; else echo 'omit-next-write-mutant=rejected'; fi
  omit-next-write-mutant=rejected
  $ grep -o 'counterexample function=Stack.cut' artifacts/nonvacuous_cut_omit_next_write_mutant.out
  counterexample function=Stack.cut

Five retained negative rows reject in the driver. Four source rows reject
through the production source compiler, produce no requested SST/VIR, and make
no fresh-process verifier-counter claim. The failed compiler invocations leave
real partial CMTs, which are tested separately at the loader boundary.

  $ for n in recursive_traversal recursive_result mutable_result rank_finite_invariant_laundering imported_or_substituted_representation; do retained "$n"; done
  $ retained_support () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "support/$n.ml"; }
  $ retained_support external_model
  $ retained_support trusted_model
  $ source_reject () { n=$1; pattern=$2; if retained "$n" >"artifacts/$n.compile" 2>&1; then echo unexpected-success; return 1; else grep -F "$pattern" "artifacts/$n.compile" >/dev/null && echo "$n=source-rejected"; fi; }
  $ source_reject client_representation_access 'Unbound record field "top"'
  client_representation_access=source-rejected
  $ source_reject node_escape_or_equality 'expected to be "read_write"'
  node_escape_or_equality=source-rejected
  $ source_reject model_effect 'expected to be "read_write"'
  model_effect=source-rejected
  $ source_reject ghost_mode_recovery 'specification code cannot create or receive Tracked values'
  ghost_mode_recovery=source-rejected
  $ source_cli_reject () { n=$1; pattern=$2; if OCAML_COLOR=never ../../src/verocaml.exe verify "fixtures/$n.ml" --dump-sst "artifacts/$n.source.sst" --dump-vir "artifacts/$n.source.vir" >"artifacts/$n.source" 2>&1; then echo unexpected-success; return 1; else grep -F 'error[VERO_SOURCE_COMPILE]' "artifacts/$n.source" >/dev/null && grep -F "$pattern" "artifacts/$n.source" >/dev/null && test ! -e "artifacts/$n.source.sst" && test ! -e "artifacts/$n.source.vir" && echo "$n=production-source-rejected"; fi; }
  $ source_cli_reject client_representation_access 'Unbound record field "top"'
  client_representation_access=production-source-rejected
  $ source_cli_reject node_escape_or_equality 'expected to be "read_write"'
  node_escape_or_equality=production-source-rejected
  $ source_cli_reject model_effect 'expected to be "read_write"'
  model_effect=production-source-rejected
  $ source_cli_reject ghost_mode_recovery 'specification code cannot create or receive Tracked values'
  ghost_mode_recovery=production-source-rejected
  $ echo 'source-counter-claim=none fresh-process-fail-closed=true'
  source-counter-claim=none fresh-process-fail-closed=true
  $ for n in client_representation_access node_escape_or_equality model_effect ghost_mode_recovery; do test -s "artifacts/$n.cmt"; wc -c < "artifacts/$n.cmt"; done
  30089
  20557
  18404
  6316
  $ for n in client_representation_access node_escape_or_equality model_effect ghost_mode_recovery; do if OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --dump-sst "artifacts/$n.partial.sst" --dump-vir "artifacts/$n.partial.vir" >"artifacts/$n.partial" 2>&1; then false; fi; grep -F 'error[VERO_MALFORMED_INPUT]' "artifacts/$n.partial" >/dev/null; test ! -e "artifacts/$n.partial.sst" && test ! -e "artifacts/$n.partial.vir"; echo "$n=actual-partial-cmt-rejected"; done
  client_representation_access=actual-partial-cmt-rejected
  node_escape_or_equality=actual-partial-cmt-rejected
  model_effect=actual-partial-cmt-rejected
  ghost_mode_recovery=actual-partial-cmt-rejected
  $ grep -F 'type t = hidden_t' fixtures/imported_or_substituted_representation.ml | wc -l
  2

Real CMT imports cannot transfer the hidden model equation.  Both a retained
provider and a readable ordinary-family provider reject before consumer SST or
VIR output exists.

  $ export PPX="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ mkdir -p artifacts/import artifacts/ordinary
  $ cp support/import_provider.ml artifacts/import/provider.ml
  $ cp support/import_consumer.ml artifacts/import/consumer.ml
  $ (cd artifacts/import && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c provider.ml && ocamlc -w -A -alert -all -bin-annot -I . -I "$GHOST" -ppx "$PPX" -c consumer.ml)
  $ if OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/import/consumer.cmt --dependency artifacts/import/provider.cmt --dump-sst artifacts/import/consumer.sst --dump-vir artifacts/import/consumer.vir > artifacts/import/rejected 2>&1; then false; fi
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/import/rejected | head -1
  error[VERO_DEPENDENCY]
  $ test ! -e artifacts/import/consumer.sst && test ! -e artifacts/import/consumer.vir
  $ cp support/ordinary_provider.ml artifacts/ordinary/provider.ml
  $ cp support/ordinary_consumer.ml artifacts/ordinary/consumer.ml
  $ (cd artifacts/ordinary && ocamlc -w -A -alert -all -bin-annot -c provider.ml && ocamlc -w -A -alert -all -bin-annot -I . -I "$GHOST" -ppx "$PPX" -c consumer.ml)
  $ if OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/ordinary/consumer.cmt --dependency artifacts/ordinary/provider.cmt --dump-sst artifacts/ordinary/consumer.sst --dump-vir artifacts/ordinary/consumer.vir > artifacts/ordinary/rejected 2>&1; then false; fi
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/ordinary/rejected | head -1
  error[VERO_DEPENDENCY]
  $ test ! -e artifacts/ordinary/consumer.sst && test ! -e artifacts/ordinary/consumer.vir

The private matrix uses the production driver for packaged rows and the
production pipeline for mutations of an already issued affine plan.

  $ ./owned_recursive_scalar_model_pipeline_tool.exe --matrix > artifacts/matrix
  $ grep '^positive=' artifacts/matrix | sed -E 's/ status=.*$/ checked/'
  positive=scalar_model_baseline checked
  positive=successor_freshness checked
  positive=nonvacuous_cut checked
  positive=opaque_local_client checked
  positive=constant_head_no_value_path checked
  positive=cross_template_demand_isolation checked
  positive=uncontracted_mutation_no_demand checked
  positive=direct_root_reconstruction_control checked
  positive=nested_noop_reconstruction_control checked
  $ grep '^negative=' artifacts/matrix | sed -E 's/ rejected=.*$/ checked-zero-work/'
  negative=recursive_traversal checked-zero-work
  negative=recursive_result checked-zero-work
  negative=mutable_result checked-zero-work
  negative=rank_finite_invariant_laundering checked-zero-work
  negative=imported_or_substituted_representation checked-zero-work
  negative=external_model checked-zero-work
  negative=trusted_model checked-zero-work
  $ grep '^partial-cmt=' artifacts/matrix | sed -E 's/ rejected=.*$/ checked-loader-zero-work/'
  partial-cmt=client_representation_access checked-loader-zero-work
  partial-cmt=node_escape_or_equality checked-loader-zero-work
  partial-cmt=model_effect checked-loader-zero-work
  partial-cmt=ghost_mode_recovery checked-loader-zero-work
  $ grep '^import=' artifacts/matrix | sed -E 's/ rejected=.*$/ checked-zero-work/'
  import=retained-cmt checked-zero-work
  import=ordinary-cmt checked-zero-work
  $ grep '^attack=' artifacts/matrix | sed -E 's/ rejected.*$/ rejected-zero-work/'
  attack=wrong-root rejected-zero-work
  attack=stale-root rejected-zero-work
  attack=copied rejected-zero-work
  attack=rebound-root rejected-zero-work
  attack=branch-root rejected-zero-work
  attack=wrong-version rejected-zero-work
  attack=wrong-path rejected-zero-work
  attack=wrong-field rejected-zero-work
  attack=wrong-constructor rejected-zero-work
  attack=wrong-type rejected-zero-work
  attack=wrong-model rejected-zero-work
  attack=wrong-body rejected-zero-work
  attack=wrong-signature rejected-zero-work
  attack=wrong-program rejected-zero-work
  attack=wrong-cmt rejected-zero-work
  attack=wrong-family rejected-zero-work
  attack=wrong-session rejected-zero-work
  attack=substitution rejected-zero-work
  attack=import rejected-zero-work
  $ tail -1 artifacts/matrix
  observer-injection=unavailable api=mutate-already-issued-plan-only
