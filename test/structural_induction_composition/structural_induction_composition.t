The Logic-IR snapshot is an explicit specialist resource exception.  Grouped
outcomes own proof status, functions, postconditions, and recursive descent;
this check retains exact private fuel/rank authority counts outside the ordinary
focused alias.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ for n in list_positive tree_positive tree_duplicate_left tree_duplicate_right tree_conditional_revisit tree_same_node tree_non_child; do retained "$n"; done
  $ ./structural_induction_composition_tool.exe logic-snapshot artifacts/list_positive.cmt
  logic rank-sorts=1 axioms=7 fuel-body=1 public-link=1 rank-nonnegative=3 rank-child=1 activations=2 assertions=7
  $ ./structural_induction_composition_tool.exe logic-snapshot artifacts/tree_positive.cmt
  logic rank-sorts=1 axioms=8 fuel-body=1 public-link=1 rank-nonnegative=3 rank-child=2 activations=2 assertions=9

Private visit/summary counts are specialist authority resources.  They retain
issuance and consumption cardinalities without asserting generated selectors,
spans, IDs, or report ordering.

  $ receipt_trace () { VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$1.cmt" --timeout-ms 5000 2>&1 | grep 'Verification_session: private receipt event_kind=destroy ' | sed -E 's/.*proof_call_visits=([0-9]+) proof_call_summaries=([0-9]+).*/authority visits=\1 summaries=\2/'; }
  $ receipt_trace list_positive
  authority visits=1 summaries=1
  $ receipt_trace tree_positive
  authority visits=3 summaries=3

Source-identical mutants are explicit intentionally-failing specialist cases.
They prove non-vacuity and cannot block structural-induction-composition-outcome-check.

  $ for n in list_positive tree_positive; do ./structural_induction_composition_tool.exe mutant call-removed "artifacts/$n.cmt" | sed -E 's/ solver-contexts=[0-9]+ results=[0-9]+//'; ./structural_induction_composition_tool.exe mutant summary-suppressed "artifacts/$n.cmt" | sed -E 's/ solver-contexts=[0-9]+ results=[0-9]+//'; done
  mutant=call-removed status=inconclusive postcondition-failure=true visits=0 summaries=0
  mutant=summary-suppressed status=inconclusive postcondition-failure=true visits=1 summaries=0
  mutant=call-removed status=inconclusive postcondition-failure=true visits=0 summaries=0
  mutant=summary-suppressed status=inconclusive postcondition-failure=true visits=2 summaries=0

Duplicate and non-child calls are explicit malformed-SST outcome-gap
exceptions.  VERO-113 reports them as verifier failures; the local specialist
lane retains the rejection phase and no-extra-authority counters.

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

Conditional revisit is a specialist pre-backend resource barrier.

  $ ./structural_induction_composition_tool.exe conditional-revisit artifacts/tree_conditional_revisit.cmt
  conditional-revisit=rejected visits=1 summaries=1 rank-lowerings=1 recursive-spec-lowerings=0 dependent=0/0/0 backend=0 solver=0 session-destroyed=true

The private replay/identity matrix is an explicit authority exception.

  $ ./structural_induction_composition_tool.exe authority-matrix
  positive visit=true summary=true counters=1/1
  duplicate-child=rejected delta=0/0
  wrong-child=rejected delta=0/0
  cross-profile=rejected delta=0/0
  rebound-selector=rejected delta=0/0
  stale-path=rejected delta=0/0
