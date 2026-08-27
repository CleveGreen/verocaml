The public service accepts decoded CMT values, builds its immutable policy once,
and exposes renderer-complete opaque observations without filesystem ownership.

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
  configuration=mandatory validation=threads/timeout/rlimit policy=once routes=serial/higher rlimit=default/explicit parity=exact
  provenance=unit/digest/direct/transitive count=1 status=verified
  diagnostics=function/kind/span/outcome/model count=1 status=counterexample
  trusted-observations=immutable count=2 views=complete
  errors=unit/message/diagnostic opaque=projected

The public signature exposes no path, channel, process, rendering, exit, solver,
driver, session, receipt, handle, environment, or mutable authority.

  $ grep -E 'string (list|option).*file|Unix|Sys|channel|formatter|exit|Solver|Verification_(driver|pipeline|session)|receipt|handle|environment|mutable|(^|[^[:alnum:]_])ref([^[:alnum:]_]|$)' ../../src/verifier_service.mli && exit 1 || :
  $ grep -E 'val (request|verify).*string' ../../src/verifier_service.mli && exit 1 || :
  $ grep -E 'Obj[.]' verifier_service_tool.ml && exit 1 || :
