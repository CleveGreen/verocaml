Reached Proof states, rather than syntactic body-path or postcondition
occurrence ordinals, carry recursive-Spec activation. The exact fan-out lemma,
its follow-up proof-call precondition, multiple postconditions, nested
branch/match paths, and the authenticated Revealed entry seed all verify.

  $ mkdir artifacts
  $ retained_compile () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained_compile fanout_positive
  $ retained_compile ordinary_contract_match
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/fanout_positive.cmt --timeout-ms 5000 | tail -1
  verocaml: verified file=artifacts/fanout_positive.cmt functions=7 obligations=22

Checked nonfinite contracts called by direct and recursive finite callers,
checked same-function finite contract/body matching, and external-body
nonfinite parity all verify.  A forged ground-witness attack remains untouched,
proving that body, definition-contract, call-summary, and postcondition
matches issue zero witnesses and consume zero retries.

  $ ./proof_activation_routing_tool.exe ordinary-matches artifacts/ordinary_contract_match.cmt
  ordinary-matches status=verified functions=8 obligations=19 ground-attempts=0 retry-attempts=0 trusted-body-parity=1

External finite formals remain rejected at the established boundary, with
byte-identical repeated diagnostics.

  $ cat > artifacts/external_finite_rejected.ml <<'EOF'
  > type 'a node =
  >   | Empty
  >   | Node of 'a * 'a node
  > let lemma_peel_push (nodes : int node [@finite]) =
  >   [%verocaml.ensures fun _ ->
  >     match nodes with
  >     | Empty -> true
  >     | Node (value, rest) -> Node (value, rest) = nodes];
  >   ()
  > [@@verocaml.proof]
  > [@@verocaml.external_body]
  > let finite_caller (nodes : int node [@finite]) =
  >   lemma_peel_push nodes
  > [@@verocaml.proof]
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/external_finite_rejected.cmo artifacts/external_finite_rejected.ml
  $ for n in 1 2; do OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/external_finite_rejected.cmt --timeout-ms 5000 >"artifacts/external-finite-$n.out" 2>&1; test $? = 2; done
  $ cmp artifacts/external-finite-1.out artifacts/external-finite-2.out && tail -1 artifacts/external-finite-1.out | sed -E 's/ at .*$/ at SPAN/'
  Error: [VERO_INVALID_PROGRAM] VeroCaml could not validate function "lemma_peel_push": unchecked, external, trusted, or raw callables cannot require finite formals

The private in-process observer pins distinct final indices, copied
provisional counters, exact path digests and activation vectors for all three
postcondition descendants and the proof-call preconditions. It also records a
complete one-to-one batch with no unused manifest or fallback.

  $ ./proof_activation_routing_tool.exe observe artifacts/fanout_positive.cmt lemma_empty_stack | sed -E 's/path=[0-9a-f]+/path=DIGEST/g'
  status=verified functions=7 obligations=22
  issued callable=lemma_empty_stack#5 final=0 provisional=0 kind=post:0:fixtures/fanout_positive.ml:40:2-40:51 path=DIGEST activations=[seeded_node_len#1:1,spec_node_len#0:2]
  issued callable=lemma_empty_stack#5 final=1 provisional=0 kind=post:0:fixtures/fanout_positive.ml:40:2-40:51 path=DIGEST activations=[seeded_node_len#1:1,spec_node_len#0:2]
  consumed callable=lemma_empty_stack#5 final=0 kind=post:0:fixtures/fanout_positive.ml:40:2-40:51 path=DIGEST
  consumed callable=lemma_empty_stack#5 final=1 kind=post:0:fixtures/fanout_positive.ml:40:2-40:51 path=DIGEST
  bijection callable=lemma_empty_stack#5 issued=2 consumed=2 unused=0 fallback=0
  $ ./proof_activation_routing_tool.exe observe artifacts/fanout_positive.cmt lemma_empty_stack_with_call | sed -E 's/path=[0-9a-f]+/path=DIGEST/g' | grep -E 'status=|issued .*kind=pre:|bijection '
  status=verified functions=7 obligations=22
  issued callable=lemma_empty_stack_with_call#6 final=0 provisional=0 kind=pre:affirm#4:0:fixtures/fanout_positive.ml:33:2-33:32:fixtures/fanout_positive.ml:49:2-49:34 path=DIGEST activations=[seeded_node_len#1:1,spec_node_len#0:2]
  issued callable=lemma_empty_stack_with_call#6 final=1 provisional=0 kind=pre:affirm#4:0:fixtures/fanout_positive.ml:33:2-33:32:fixtures/fanout_positive.ml:49:2-49:34 path=DIGEST activations=[seeded_node_len#1:1,spec_node_len#0:2]
  bijection callable=lemma_empty_stack_with_call#6 issued=4 consumed=4 unused=0 fallback=0
  $ ./proof_activation_routing_tool.exe observe artifacts/fanout_positive.cmt seeded_fanout | sed -E 's/path=[0-9a-f]+/path=DIGEST/g' | grep -E 'status=|issued |bijection '
  status=verified functions=7 obligations=22
  issued callable=seeded_fanout#9 final=0 provisional=0 kind=post:0:fixtures/fanout_positive.ml:70:2-71:45 path=DIGEST activations=[seeded_node_len#1:1]
  issued callable=seeded_fanout#9 final=1 provisional=0 kind=post:0:fixtures/fanout_positive.ml:70:2-71:45 path=DIGEST activations=[seeded_node_len#1:1]
  bijection callable=seeded_fanout#9 issued=2 consumed=2 unused=0 fallback=0

Removing, delaying, placing on only one sibling, moving to another callable,
or replacing the authenticated Revealed seed with an opaque declaration does
not activate the reached obligation.

  $ for n in no_reveal_negative nonleak_negative later_nonleak_negative cross_callable_negative opaque_seed_negative; do retained_compile "$n"; ./proof_activation_routing_tool.exe negative "artifacts/$n.cmt"; done
  status=counterexample function=lemma_empty_stack index=1
  status=counterexample function=sibling_nonleak index=1
  status=counterexample function=later_nonleak index=0
  status=counterexample function=cross_callable_nonleak index=0
  status=counterexample function=opaque_seed_control index=0

Malformed/counterfeit, absent, duplicate, unused, stale, foreign, replayed,
wrong-artifact-family, reordered, swapped, substituted, wrong-callable,
wrong-index, wrong-kind, wrong-body, wrong-obligation, and path-mismatched
manifests all reject before dispatch. Only the valid control increments
recursive-query, smt.ml, and direct-Z3 construction counters.

  $ ./proof_activation_routing_tool.exe adversaries artifacts/fanout_positive.cmt
  valid accepted dispatch=1 fallback=0
  absent rejected dispatch=0 fallback=0
  missing rejected dispatch=0 fallback=0
  duplicate rejected dispatch=0 fallback=0
  counterfeit-issuer rejected dispatch=0 fallback=0
  unused rejected dispatch=0 fallback=0
  stale rejected dispatch=0 fallback=0
  foreign rejected dispatch=0 fallback=0
  wrong-artifact-family rejected dispatch=0 fallback=0
  replayed rejected dispatch=0 fallback=0
  snapshot-replayed rejected dispatch=0 fallback=0
  substituted-snapshot rejected dispatch=0 fallback=0
  reordered rejected dispatch=0 fallback=0
  swapped rejected dispatch=0 fallback=0
  wrong-callable rejected dispatch=0 fallback=0
  wrong-obligation rejected dispatch=0 fallback=0
  wrong-index rejected dispatch=0 fallback=0
  wrong-kind rejected dispatch=0 fallback=0
  wrong-body rejected dispatch=0 fallback=0
  substituted-same-kind-ordinal rejected dispatch=0 fallback=0
  path-mismatch rejected dispatch=0 fallback=0
  control-delta recursive-query=1 smt=1 direct-context=2 direct-solver=2
