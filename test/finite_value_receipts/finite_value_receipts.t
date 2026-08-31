The private session-capability matrix is an explicit specialist authority
exception.  Its exact zero-delta and lifecycle counters guard replay and
identity invariants that the closed semantic projection intentionally omits.

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

The exact private receipt counters are specialist resource exceptions.  The
ordinary verified status and functions are asserted by grouped outcome cases;
these counters retain the non-public issuance and consumption invariants.

  $ retained positive
  $ ./finite_value_receipts_tool.exe positive artifacts/positive.cmt
  status=verified functions=3 obligations=10 solve-calls=3 witness=18 parent=18 child=13 result-witness=0 finalization=0 consumption=8 recursive=8 dependent-lowering=0 backend=0 solver=0

  $ retained local_result
  $ ./finite_value_receipts_tool.exe transfer artifacts/local_result.cmt
  status=verified completed-issued=1 completed-consumed=1 solve-calls=3 witness=3 parent=3 child=0 result-witness=1 finalization=1 consumption=1 recursive=0 dependent-lowering=1 backend=1 solver=3

The injected failed-obligation path and malformed-SST receipt barriers are
specialist outcome-gap exceptions.  They assert that rejected paths allocate no
private resources and remain outside finite-value-receipts-outcome-check.

  $ ./finite_value_receipts_tool.exe failed-transfer artifacts/local_result.cmt
  status=counterexample completed-issued=0 finalization=0 consumption=0 dependent=0/0/0 solve-calls=2 result-witness=1

  $ for n in arbitrary_entry branch_launder; do retained "$n"; ./finite_value_receipts_tool.exe negative "artifacts/$n.cmt"; done
  rejected solve-calls=0 witness=0 parent=0 child=0 result-witness=0 finalization=0 consumption=0 recursive=0 dependent-lowering=0 backend=0 solver=0 session-destroyed=true
  rejected solve-calls=0 witness=0 parent=0 child=0 result-witness=0 finalization=0 consumption=0 recursive=0 dependent-lowering=0 backend=0 solver=0 session-destroyed=true

  $ retained mutable_carrier
  $ ./finite_value_receipts_tool.exe negative artifacts/mutable_carrier.cmt
  rejected solve-calls=0 witness=0 parent=0 child=0 result-witness=0 finalization=0 consumption=0 recursive=0 dependent-lowering=0 backend=0 solver=0 session-destroyed=true
