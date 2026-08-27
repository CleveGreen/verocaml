Finite receipts are private session capabilities.  The registry binds exact
program, callable, value/version/path, mode, type, domain/profile, construction,
field metadata, completed-callee, and call-instance identities.

  $ ./finite_value_receipts_tool.exe matrix
  registry=exact result=accepted
  registry=wrong-mode result=rejected
  registry=wrong-callable result=rejected
  registry=wrong-rank result=rejected
  registry=wrong-parent-value-type result=rejected delta=0/0/0/0/0/0
  registry=wrong-child-claimed-type result=rejected delta=0/0/0/0/0/0
  registry=wrong-ordinal result=rejected
  registry=unreceipted-parent result=rejected
  registry=wrong-result-role result=rejected
  registry=replayed-call result=rejected
  registry=destroyed-replay result=rejected
  finite-witnesses=2 finite-parents=2 finite-children=1 finite-result-witnesses=1 finite-finalizations=1 finite-consumptions=2 finite-formal-assumptions=0 finite-formal-batches=0 finite-formal-transfers=0 finite-formal-consumptions=0

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }

Closed Ghost construction, local Exec and Tracked chains, authenticated record
and constructor patterns, stack.top projection, exact aliases/joins,
spec_node_len unfolding, and push-front preservation all cross the production
finite gate.

  $ retained positive
  $ ./finite_value_receipts_tool.exe positive artifacts/positive.cmt
  status=verified functions=3 obligations=10 solve-calls=3 witness=18 parent=18 child=13 result-witness=0 finalization=0 consumption=8 recursive=8 dependent-lowering=0 backend=0 solver=0
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive.cmt --timeout-ms 5000
  verocaml: verified file=artifacts/positive.cmt functions=3 obligations=13

An exact local finite result records a non-consumable boundary witness.  The
existing scheduler first completes every callee obligation and issues the
completed-callee receipt; only then does the pipeline finalize the separate
finite result receipt.  The fresh caller result consumes both authorities.

  $ retained local_result
  $ ./finite_value_receipts_tool.exe transfer artifacts/local_result.cmt
  status=verified completed-issued=1 completed-consumed=1 solve-calls=3 witness=3 parent=3 child=0 result-witness=1 finalization=1 consumption=1 recursive=0 dependent-lowering=1 backend=1 solver=3
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/local_result.cmt --timeout-ms 5000
  verocaml: verified file=artifacts/local_result.cmt functions=3 obligations=5

Failed callee obligations may leave only the non-consumable local boundary
witness.  They issue neither completed authority nor finite finalization, and
the dependent caller never lowers.

  $ ./finite_value_receipts_tool.exe failed-transfer artifacts/local_result.cmt
  status=counterexample completed-issued=0 finalization=0 consumption=0 dependent=0/0/0 solve-calls=2 result-witness=1

Arbitrary entry values and branch laundering reject before recursive lowering,
backend work, or solver creation.  No rejected route emits any finite event.

  $ for n in arbitrary_entry branch_launder; do retained "$n"; ./finite_value_receipts_tool.exe negative "artifacts/$n.cmt"; done
  rejected solve-calls=0 witness=0 parent=0 child=0 result-witness=0 finalization=0 consumption=0 recursive=0 dependent-lowering=0 backend=0 solver=0 session-destroyed=true
  rejected solve-calls=0 witness=0 parent=0 child=0 result-witness=0 finalization=0 consumption=0 recursive=0 dependent-lowering=0 backend=0 solver=0 session-destroyed=true

Direct and indirect immutable cycles and a recursive-module factory cycle
reject before a verification session exists.

  $ for n in direct_cycle indirect_cycle recursive_factory; do retained "$n"; ./finite_value_receipts_tool.exe frontend-negative "artifacts/$n.cmt"; done
  rejected finite=0/0/0/0/0/0 recursive=0 backend=0 solver=0 session=not-created
  rejected finite=0/0/0/0/0/0 recursive=0 backend=0 solver=0 session=not-created
  rejected finite=0/0/0/0/0/0 recursive=0 backend=0 solver=0 session=not-created

A mutable outer carrier reaches the session-local gate but rejects before any
finite, recursive, backend, or solver operation.

  $ retained mutable_carrier
  $ ./finite_value_receipts_tool.exe negative artifacts/mutable_carrier.cmt
  rejected solve-calls=0 witness=0 parent=0 child=0 result-witness=0 finalization=0 consumption=0 recursive=0 dependent-lowering=0 backend=0 solver=0 session-destroyed=true
