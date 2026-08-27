The six packaged positives traverse the production source driver.  Each
constructor-established value reaches one exact local transition and one
terminal immutable snapshot.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ for n in constructor_drop constructor_zero_head root_rebase nested_cut equal_branch terminal_snapshot; do OCAML_COLOR=never ../../src/verocaml.exe verify "fixtures/$n.ml" --timeout-ms 5000 | sed -E 's#file=.*#file=FIXTURE#'; done
  verocaml: verified file=FIXTURE
  verocaml: verified file=FIXTURE
  verocaml: verified file=FIXTURE
  verocaml: verified file=FIXTURE
  verocaml: verified file=FIXTURE
  verocaml: verified file=FIXTURE

  $ for n in constructor_drop constructor_zero_head root_rebase nested_cut equal_branch terminal_snapshot arbitrary_entry use_invariant_from_formal invalid_one_write temporarily_invalid_multi_write copied_or_rebound ghost_forgetting mixed_call_instance_adversary two_valid_call_instances_adversary trusted_transition; do retained "$n"; done

The packaged rejection rows stay on their selected boundaries.  The invalid
successors reach a concrete preservation counterexample; the temporarily
invalid first write prevents the later repair from being validated.

  $ reject () { n=$1; if OCAML_COLOR=never ../../src/verocaml.exe verify "fixtures/$n.ml" --timeout-ms 5000 >"artifacts/$n.out" 2>&1; then echo unexpected-success; return 1; else echo "$n=rejected"; fi; }
  $ for n in arbitrary_entry use_invariant_from_formal invalid_one_write temporarily_invalid_multi_write copied_or_rebound ghost_forgetting imported_transition trusted_transition; do reject "$n"; done
  arbitrary_entry=rejected
  use_invariant_from_formal=rejected
  invalid_one_write=rejected
  temporarily_invalid_multi_write=rejected
  copied_or_rebound=rejected
  ghost_forgetting=rejected
  imported_transition=rejected
  trusted_transition=rejected
  $ grep -o 'counterexample function=Stack.invalidate[^ ]* vc=invariant-transition-preservation' artifacts/invalid_one_write.out
  counterexample function=Stack.invalidate#4 vc=invariant-transition-preservation
  $ grep -o 'counterexample function=Stack.repair_late[^ ]* vc=invariant-transition-preservation' artifacts/temporarily_invalid_multi_write.out
  counterexample function=Stack.repair_late#4 vc=invariant-transition-preservation
  $ test "$(grep -c 'counterexample.*invariant-transition-preservation' artifacts/temporarily_invalid_multi_write.out)" -eq 1
  $ grep -q 'error.*VERO_SOURCE_COMPILE' artifacts/imported_transition.out
  $ test ! -e artifacts/imported_transition.cmt
  $ echo 'negative=imported_transition rejected=source-compiler-before-session lifecycle=0/0/0 teardown=0 dependent=0/0/0 preservation=0'
  negative=imported_transition rejected=source-compiler-before-session lifecycle=0/0/0 teardown=0 dependent=0/0/0 preservation=0

The imported source has no retained CMT and therefore cannot create a
verification session.  The private observer runs every retained packaged CMT through
Verification_driver_private.run and Verification_pipeline.  It pins lifecycle,
teardown, ordered preservation, reconstruction, dependent-work, backend, and
solver counters.  Its adversaries mutate only a capability previewed by the
production call analysis.

  $ ./recursive_invariant_transition_pipeline_tool.exe --matrix > artifacts/matrix
  $ grep '^positive=' artifacts/matrix
  positive=constructor_drop status=verified lifecycle=1/1/1/1 preservation=2 reconstruction=0/2 ordered=rebase:v0->v1,root:v1->v2 terminal=1
  positive=constructor_zero_head status=verified lifecycle=1/1/1/1 preservation=1 reconstruction=1/0 ordered=nested:v0->v1 terminal=1
  positive=root_rebase status=verified lifecycle=1/1/1/1 preservation=2 reconstruction=0/2 ordered=rebase:v0->v1,root:v1->v2 terminal=1
  positive=nested_cut status=verified lifecycle=1/1/1/1 preservation=2 reconstruction=1/1 ordered=nested:v0->v1,root:v1->v2 terminal=1
  positive=equal_branch status=verified lifecycle=1/1/1/1 preservation=1 reconstruction=1/0 ordered=nested:v0->v1 terminal=2
  positive=terminal_snapshot status=verified lifecycle=1/1/1/1 preservation=1 reconstruction=1/0 ordered=nested:v0->v1 terminal=1
  $ grep '^negative=' artifacts/matrix
  negative=arbitrary_entry rejected=true lifecycle=0/0/0 teardown=0 dependent=0/0/0 preservation=0
  negative=copied_or_rebound rejected=true lifecycle=0/0/0 teardown=0 dependent=0/0/0 preservation=0
  negative=ghost_forgetting rejected=true lifecycle=0/0/0 teardown=0 dependent=0/0/0 preservation=0
  negative=mixed_call_instance_adversary rejected=true lifecycle=0/0/0 teardown=0 dependent=0/0/0 preservation=0
  negative=trusted_transition rejected=true lifecycle=0/0/0 teardown=0 dependent=0/0/0 preservation=0
  negative=use_invariant_from_formal rejected=true lifecycle=1/0/0 teardown=1 dependent=1/0/0 preservation=0
  negative=invalid_one_write status=counterexample lifecycle=1/1/0/1 dependent=1/1/1 preservation=1 failing=1 emitted=1 validated-results=3 backend-solvers=3
  negative=temporarily_invalid_multi_write status=counterexample lifecycle=1/1/0/1 dependent=1/1/1 preservation=1 failing=1 emitted=2 validated-results=3 backend-solvers=3
  $ grep -c '^attack=' artifacts/matrix
  38
  $ grep -E '^attack=(program|caller|callee|call|actual(-path|-symbol)?|formal|root|stale|mode|type|invariant|model|predicate|obligation(-fingerprint)?|issuer|copied|branch-only|divergent-join) ' artifacts/matrix
  attack=program rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=caller rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=callee rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=call rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=actual rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=actual-symbol rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=actual-path rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=formal rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=root rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=stale rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=mode rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=type rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=invariant rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=model rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=predicate rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=obligation rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=obligation-fingerprint rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=issuer rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=copied rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=branch-only rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  attack=divergent-join rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0
  $ tail -4 artifacts/matrix
  call_affinity exact-instances=2 lifecycle=2/2/2/2 preservation=1 reconstruction=1/0 terminal=2
  consumed_reuse first=1/1/0/1 reuse=rejected dependent=1/0/0 preservation=0
  session_replay first=1/1/1/1 second=0/0/0/1 dependent-second=0/0/0
  observer-injection=unavailable capability-source=previewed-production-call-only

No generated SST/VIR vocabulary introduces invariant opening or a general
memory rule.

  $ for n in constructor_drop constructor_zero_head root_rebase nested_cut equal_branch terminal_snapshot; do OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --dump-sst "artifacts/$n.sst" --dump-vir "artifacts/$n.vir" >/dev/null; done
  $ if sed -E 's/functional-no-heap//g; s/force-aliased//g; s/use=aliased//g' artifacts/*.sst artifacts/*.vir | grep -E -i 'invariant[-_ ]open|heap|points[-_ ]to|permission|frame[-_ ]rule|alias|separation'; then false; else echo 'structural-vocabulary=absent'; fi
  structural-vocabulary=absent
