This transcript is the explicit public-API architecture specialist lane and is
not a dependency of the ordinary verifier-service outcome alias. It retains
opaque provenance/view and signature-boundary checks that are outside the closed
outcome projection. Status, diagnostic-kind, unit, and configuration parity
semantics are asserted by outcome_cases.ml.

The public service accepts decoded CMT values and exposes opaque observations
without filesystem ownership.

  $ mkdir artifacts
  $ export PPX="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ compile () { ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c -o "artifacts/$2.cmo" "$1"; }
  $ compile ../private_receipt/fixtures/local_positive.ml verified
  $ compile ../private_receipt/fixtures/failing_callee.ml failed
  $ compile ../trusted_external_bodies/fixtures/positive.ml trusted
  $ cp ../verified_interfaces/fixtures/model_dependency.ml ../verified_interfaces/fixtures/model_consumer.ml artifacts/
  $ (cd artifacts && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c model_dependency.ml)
  $ (cd artifacts && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c model_consumer.ml)
  $ ./verifier_service_tool.exe artifacts/verified.cmt artifacts/failed.cmt artifacts/trusted.cmt artifacts/model_consumer.cmt artifacts/model_dependency.cmt
  configuration-errors=threads/timeout/rlimit opaque=messages
  provenance=unit/digest/direct/transitive status=verified
  diagnostics=function/kind/outcome/model status=counterexample
  trusted-observations=immutable views=complete
  errors=unit/message/diagnostic opaque=projected
