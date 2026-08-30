The focused gate verifies the accepted monomorphic theorems directly from
source.  Their recursive Proof contracts are postconditions, not pre-body
assertions.

  $ mkdir artifacts
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/list_positive.ml --timeout-ms 5000
  verocaml: verified file=fixtures/list_positive.ml functions=1 obligations=17
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/tree_positive.ml --timeout-ms 5000
  verocaml: verified file=fixtures/tree_positive.ml functions=1 obligations=27
  $ grep -A1 'verocaml.ensures' fixtures/list_positive.ml
    [%verocaml.ensures fun _result ->
      not (equal left right) || sum left = sum right];
  $ grep -A1 'verocaml.ensures' fixtures/tree_positive.ml
    [%verocaml.ensures fun _result ->
      not (all_nonnegative tree) || sum tree >= 0];

Retained compilation gives the production verifier deterministic SST and VIR
inputs for the private summary and Logic-IR controls.

  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ for n in list_positive tree_positive tree_duplicate_left tree_duplicate_right tree_conditional_revisit tree_same_node tree_non_child tree_one_branch tree_premise_omitted tree_early_assertion tree_negative_value; do retained "$n"; done
  $ for n in list_positive tree_positive; do OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --timeout-ms 5000 --dump-sst "artifacts/$n.sst" --dump-vir "artifacts/$n.vir" >/dev/null; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --timeout-ms 5000 --dump-sst "artifacts/$n-again.sst" --dump-vir "artifacts/$n-again.vir" >/dev/null; cmp "artifacts/$n.sst" "artifacts/$n-again.sst"; cmp "artifacts/$n.vir" "artifacts/$n-again.vir"; done
  $ grep -E '^function equal_sum|^  ensures 0|^  decreases 0|proof-call equal_sum' artifacts/list_positive.sst | tail -4 | sed -E 's/ @ .*//'
  function equal_sum#2 mode=proof recursive=true result=unit policy=default-linear/default-z3
    ensures 0 stage=logical
    decreases 0 stage=logical
                  proof-call equal_sum#2 recursive=true type-arguments=[] : unit
  $ grep -E '^function nonnegative_sum|^  ensures 0|^  decreases 0|proof-call nonnegative_sum' artifacts/tree_positive.sst | tail -5 | sed -E 's/ @ .*//'
  function nonnegative_sum#2 mode=proof recursive=true result=unit policy=default-linear/default-z3
    ensures 0 stage=logical
    decreases 0 stage=logical
              proof-call nonnegative_sum#2 recursive=true type-arguments=[] : unit
              proof-call nonnegative_sum#2 recursive=true type-arguments=[] : unit

The source measures are the aggregates themselves.  Exact VIR rank witnesses
pin list tail and both tree selectors; no integer decreases proxy appears.

  $ grep -A1 '^  decreases 0 stage=logical.*list_positive.ml:23' artifacts/list_positive.sst | sed -E 's/ @ .*//'
    decreases 0 stage=logical
      variable left#0:int_list#0 : int_list#0
  $ grep -A1 '^  decreases 0 stage=logical.*tree_positive.ml:21' artifacts/tree_positive.sst | sed -E 's/ @ .*//'
    decreases 0 stage=logical
      variable tree#0:tree#0 : tree#0
  $ grep -F 'goal (< (rank[' artifacts/list_positive.vir | sed -E 's@rank-domain/v1/[0-9a-f]+@rank-domain/v1/DIGEST@g; s/^ +//' | sort -u
  goal (< (rank[rank-domain/v1/DIGEST] (t0_int_list_c1_Cons.$arg1#1 left$0)) (rank[rank-domain/v1/DIGEST] left$0))
  $ grep -F 'goal (< (rank[' artifacts/tree_positive.vir | sed -E 's@rank-domain/v1/[0-9a-f]+@rank-domain/v1/DIGEST@g; s/^ +//' | sort -u
  goal (< (rank[rank-domain/v1/DIGEST] (t0_tree_c1_Branch.$arg0#0 tree$0)) (rank[rank-domain/v1/DIGEST] tree$0))
  goal (< (rank[rank-domain/v1/DIGEST] (t0_tree_c1_Branch.$arg1#1 tree$0)) (rank[rank-domain/v1/DIGEST] tree$0))
  $ last_call=$(grep -n 'recursive-call-strict-descent' artifacts/tree_positive.vir | tail -1 | cut -d: -f1); first_post=$(grep -n 'postcondition ordinal=0' artifacts/tree_positive.vir | head -1 | cut -d: -f1); test "$last_call" -lt "$first_post"

The recursive-Spec proof query reuses named aggregate rank sorts and the
existing fuel/public-link and authenticated rank axioms.

  $ ./structural_induction_composition_tool.exe logic-snapshot artifacts/list_positive.cmt
  logic rank-sorts=1 axioms=7 fuel-body=1 public-link=1 rank-nonnegative=3 rank-child=1 activations=2 assertions=7
  $ ./structural_induction_composition_tool.exe logic-snapshot artifacts/tree_positive.cmt
  logic rank-sorts=1 axioms=8 fuel-body=1 public-link=1 rank-nonnegative=3 rank-child=2 activations=2 assertions=9

Private visit traces bind the exact selector identity.  Logical short-circuit
paths may repeat the right visit record, but the distinct selector set is
exactly list tail and tree left/right.

  $ visit_trace () { VEROCAML_TEST_PROOF_CALL_TRACE=1 DELATOR_LOG=Symbolic_executor_private=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$1.cmt" --timeout-ms 5000 2>&1 | grep '^DEBUG Symbolic_executor_private: proof call visit ' | sed -E 's/.*selector_owner=([^ ]+).*selector_ordinal=([0-9]+).*selector_name=([^ ]+).*call_file=([^ ]+) call_line=([0-9]+) call_column=([0-9]+).*/visit owner=\1 ordinal=\2 name=\3 call=\4:\5:\6/' | sort -u; }
  $ visit_trace list_positive
  visit owner=t0_int_list_c1_Cons ordinal=1 name=$arg1 call=list_positive.ml:29:32
  $ visit_trace tree_positive
  visit owner=t0_tree_c1_Branch ordinal=0 name=$arg0 call=tree_positive.ml:25:6
  visit owner=t0_tree_c1_Branch ordinal=1 name=$arg1 call=tree_positive.ml:26:6
  $ receipt_trace () { VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$1.cmt" --timeout-ms 5000 2>&1 | grep 'Verification_session: private receipt event_kind=destroy ' | sed -E 's/.*proof_call_visits=([0-9]+) proof_call_summaries=([0-9]+).*/authority visits=\1 summaries=\2/'; }
  $ receipt_trace list_positive
  authority visits=1 summaries=1
  $ receipt_trace tree_positive
  authority visits=3 summaries=3

Source-identical private mutants prove non-vacuity.  Removing recursive calls
or suppressing only their returned summaries reaches and fails the enclosing
postcondition through the ordinary solver.  No successful summary is issued.

  $ for n in list_positive tree_positive; do ./structural_induction_composition_tool.exe mutant call-removed "artifacts/$n.cmt" | sed -E 's/ solver-contexts=[0-9]+ results=[0-9]+//'; ./structural_induction_composition_tool.exe mutant summary-suppressed "artifacts/$n.cmt" | sed -E 's/ solver-contexts=[0-9]+ results=[0-9]+//'; done
  mutant=call-removed status=inconclusive postcondition-failure=true visits=0 summaries=0
  mutant=summary-suppressed status=inconclusive postcondition-failure=true visits=1 summaries=0
  mutant=call-removed status=inconclusive postcondition-failure=true visits=0 summaries=0
  mutant=summary-suppressed status=inconclusive postcondition-failure=true visits=2 summaries=0

Duplicate selectors reject before a second visit or summary.  Same-node and
constructed non-child actuals issue no visit/summary authority.

  $ reject_message () { VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$1.cmt" --timeout-ms 5000 2>&1 | grep -o "$2"; }
  $ reject_message tree_duplicate_left 'recursive proof child was already visited on this path'
  recursive proof child was already visited on this path
  $ reject_message tree_duplicate_right 'recursive proof child was already visited on this path'
  recursive proof child was already visited on this path
  $ reject_message tree_same_node 'structural recursion must select an authenticated immediate child'
  structural recursion must select an authenticated immediate child
  $ reject_message tree_non_child 'structural recursion must select an authenticated immediate child'
  structural recursion must select an authenticated immediate child
  $ for n in tree_duplicate_left tree_duplicate_right tree_same_node tree_non_child; do receipt_trace "$n"; done
  authority visits=1 summaries=1
  authority visits=1 summaries=1
  authority visits=0 summaries=0
  authority visits=0 summaries=0

A visit spent on only one predecessor remains unavailable as postcondition
authority after the join, but it cannot be laundered into a fresh visit.  The
reviewer's exact conditional-revisit source rejects before the second left
call adds visit/summary or structural-rank work, and before any backend or
solver work.

  $ ./structural_induction_composition_tool.exe conditional-revisit artifacts/tree_conditional_revisit.cmt
  conditional-revisit=rejected visits=1 summaries=1 rank-lowerings=1 recursive-spec-lowerings=0 dependent=0/0/0 backend=0 solver=0 session-destroyed=true

One-branch proof authority cannot justify the joined theorem, and an early
assertion cannot consume summaries from later calls.  Omitting the tree
premise fails the theorem postcondition; a concrete negative leaf pins an
ordinary postcondition counterexample.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/tree_one_branch.cmt --timeout-ms 5000 2>&1 | sed -n -E 's/^verocaml: (inconclusive) .*vc=(postcondition\[0\]).*/\1 \2/p'
  inconclusive postcondition[0]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/tree_premise_omitted.cmt --timeout-ms 5000 2>&1 | sed -n -E 's/^verocaml: (counterexample) .*vc=(postcondition\[0\]).*/\1 \2/p; /^  model: \(no projected bindings\)$/p'
  counterexample postcondition[0]
    model: (no projected bindings)
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/tree_early_assertion.cmt --timeout-ms 5000 2>&1 | sed -n -E 's/^verocaml: (inconclusive) .*vc=(assertion\[0\]).*/\1 \2/p'
  inconclusive assertion[0]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/tree_negative_value.cmt --timeout-ms 5000 2>&1 | sed -n -E 's/^verocaml: (counterexample) .*vc=(postcondition\[0\]).*/\1 \2/p'
  counterexample postcondition[0]

The private authority matrix rejects duplicate, wrong-child, cross-profile,
rebound-selector, and stale-path records without changing visit/summary
counters.

  $ ./structural_induction_composition_tool.exe authority-matrix
  positive visit=true summary=true counters=1/1
  duplicate-child=rejected delta=0/0
  wrong-child=rejected delta=0/0
  cross-profile=rejected delta=0/0
  rebound-selector=rejected delta=0/0
  stale-path=rejected delta=0/0
