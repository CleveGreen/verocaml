Private receipt issuance rejects every incomplete or mismatched completion set,
binds exact snapshots, isolates sessions, and creates a fresh call instance per
consumption.

  $ ./private_receipt_tool.exe matrix
  obligations=complete result=issued receipts=1
  obligations=missing result=rejected receipts=0
  obligations=incomplete result=rejected receipts=0
  obligations=failed result=rejected receipts=0
  obligations=timeout result=rejected receipts=0
  obligations=reordered result=rejected receipts=0
  obligations=duplicated result=rejected receipts=0
  obligations=extra result=rejected receipts=0
  obligations=fingerprint-mismatch result=rejected receipts=0
  obligations=wrong-return-boundary result=rejected receipts=0
  snapshot=program result=rejected receipts=0
  snapshot=cmt result=rejected receipts=0
  snapshot=family result=rejected receipts=0
  snapshot=callee result=rejected receipts=0
  snapshot=resolved-path result=rejected receipts=0
  snapshot=path-substitution result=rejected receipts=0
  snapshot=binding-uid result=rejected receipts=0
  snapshot=body result=rejected receipts=0
  snapshot=body-provenance result=rejected receipts=0
  snapshot=result-mode result=rejected receipts=0
  snapshot=result-type result=rejected receipts=0
  snapshot=invariant result=rejected receipts=0
  snapshot=model result=rejected receipts=0
  snapshot=predicate result=rejected receipts=0
  snapshot=return-boundary result=rejected receipts=0
  two-sessions first-issue=yes first-consume=yes second-issue=no second-consume=no second-receipts=0
  metadata-only authenticated-snapshot=true same-session=true receipt-consume=rejected
  two-calls fresh-call-instances=true reused-result=rejected consumed=2
  fact-transfer true-alias=true copied=false widened=false rebound=false swapped=false ghost=false
  destroyed-session issue=rejected active=false
  scheduler=self-scc result=rejected
  scheduler=mutual-scc result=rejected
  scheduler=input-permutation stable=true

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }

The canonical dependency schedule is stable under independent declaration
permutation. Only the receipt closure is ahead of ordinary functions.

  $ for n in diamond_a diamond_b; do retained "$n"; ./private_receipt_tool.exe schedule "artifacts/$n.cmt" > "artifacts/$n.schedule"; done
  $ diff -u artifacts/diamond_a.schedule artifacts/diamond_b.schedule
  $ cat artifacts/diamond_a.schedule
  Box.make source=true dependent=false
  independent source=true dependent=true
  independent_root source=false dependent=true
  left source=true dependent=true
  right source=true dependent=true
  top source=false dependent=true
  Box.read source=false dependent=false
  unrelated source=false dependent=false

The same compiler leaf display name resolves to distinct private paths, UIDs,
and canonical keys. Snapshot path and UID substitution are rejected above.

  $ retained same_display_distinct_paths
  $ ./private_receipt_tool.exe identity artifacts/same_display_distinct_paths.cmt
  identity same-display=true distinct-paths=true distinct-uids=true distinct-keys=true

A recursive receipt edge rejects before any scheduled lowering.

  $ retained self_receipt_scc
  $ ./private_receipt_tool.exe schedule artifacts/self_receipt_scc.cmt | sed -E 's/ at .*/ at SPAN/'
  rejected: loop: malformed SST: recursive verified-result receipt edge is unsupported at SPAN

Trusted bodies cannot become receipt sources. External/imported and readable
artifact identities are independently rejected by the snapshot matrix above.

  $ retained trusted_result
  $ ./private_receipt_tool.exe schedule artifacts/trusted_result.cmt | sed -E 's/ at .*/ at SPAN/'
  rejected: run: malformed SST: imported, external, or trusted result cannot be a receipt dependency at SPAN

A completed local callee issues one receipt before its dependent is lowered.
The exact true alias reaches the function return with the original symbolic
identity.

  $ receipt_core () { awk '/Verification_session: private receipt / { delete f; for (i = 1; i <= NF; i++) { split($i, pair, "="); if (length(pair[1]) < length($i)) f[pair[1]] = substr($i, length(pair[1]) + 2) } printf "event=%s function=%s source=%s dependent=%s callee=%s/%s/%s dependent-work=%s/%s/%s receipts=%s/%s finite-formal=%s/%s/%s/%s\n", f["event_kind"], f["function_name"], f["source"], f["dependent"], f["callee_solver_attempts"], f["callee_verified_results"], f["callee_failed_results"], f["dependent_lowerings"], f["dependent_backend_contexts"], f["dependent_solver_attempts"], f["receipts_issued"], f["receipts_consumed"], f["finite_formal_assumption_issuances"], f["finite_formal_transfer_batches"], f["finite_formal_transfers"], f["finite_formal_transfer_consumptions"] }' "$1"; }
  $ retained local_positive
  $ ./private_receipt_tool.exe sessions artifacts/local_positive.cmt
  production-sessions first=verified first-issued=1 first-consumed=1 first-destroyed=true second=incomplete second-issued=0 second-consumed=0 second-dependent-lowerings=0 second-dependent-backends=0 second-dependent-solvers=0 second-destroyed=true
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/local_positive.cmt --timeout-ms 5000 > artifacts/local.out 2> artifacts/local.trace
  $ receipt_core artifacts/local.trace | grep '^event=issued '
  event=issued function= source=false dependent=false callee=2/2/0 dependent-work=0/0/0 receipts=1/0 finite-formal=0/0/0/0
  $ receipt_core artifacts/local.trace | grep '^event=destroy ' | tail -1
  event=destroy function= source=false dependent=false callee=2/2/0 dependent-work=1/1/3 receipts=1/1 finite-formal=0/0/0/0

A failed callee produces no receipt and no dependent lowering, backend, or
solver activity.

  $ retained failing_callee
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/failing_callee.cmt --timeout-ms 5000 > artifacts/failing.out 2> artifacts/failing.trace
  [1]
  $ receipt_core artifacts/failing.trace | grep '^event=blocked-dependent '
  event=blocked-dependent function=run source=false dependent=false callee=1/0/1 dependent-work=0/0/0 receipts=0/0 finite-formal=0/0/0/0

A failed receipt component skips only its own dependent. An independent
receipt source still issues and its dependent is lowered and solved.

  $ retained independent_receipt_failure
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/independent_receipt_failure.cmt --timeout-ms 5000 > artifacts/independent.out 2> artifacts/independent.trace
  [1]
  $ receipt_core artifacts/independent.trace | grep -E '^event=(issued |blocked-dependent function=run_bad|lower function=run_good)'
  event=issued function= source=false dependent=false callee=2/2/0 dependent-work=0/0/0 receipts=1/0 finite-formal=0/0/0/0
  event=blocked-dependent function=run_bad source=false dependent=false callee=3/2/1 dependent-work=1/1/1 receipts=1/1 finite-formal=0/0/0/0
  event=lower function=run_good source=false dependent=true callee=3/2/1 dependent-work=2/1/1 receipts=1/1 finite-formal=0/0/0/0

Receipt availability does not prove an ordinary call precondition.

  $ retained false_precondition
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/false_precondition.cmt --timeout-ms 5000 > artifacts/pre.out 2> artifacts/pre.trace
  [1]
  $ receipt_core artifacts/pre.trace | grep '^event=issued '
  event=issued function= source=false dependent=false callee=2/2/0 dependent-work=0/0/0 receipts=1/0 finite-formal=0/0/0/0

Distinct branch calls receive distinct call instances. Their structurally
similar branch-local results do not launder a fact through the join.

  $ retained branch_laundering
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/branch_laundering.cmt --timeout-ms 5000 > artifacts/branch.out 2> artifacts/branch.trace
  [1]
  $ receipt_core artifacts/branch.trace | grep '^event=destroy ' | tail -1
  event=destroy function= source=false dependent=false callee=2/2/0 dependent-work=1/1/5 receipts=1/2 finite-formal=0/0/0/0

An unrelated failing callable is not a receipt prerequisite: issuance and
consumption happen before its ordinary verification failure.

  $ retained unrelated_failure
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/unrelated_failure.cmt --timeout-ms 5000 > artifacts/unrelated.out 2> artifacts/unrelated.trace
  [1]
  $ receipt_core artifacts/unrelated.trace | grep -E '^event=(issued |lower function=unrelated)'
  event=issued function= source=false dependent=false callee=2/2/0 dependent-work=0/0/0 receipts=1/0 finite-formal=0/0/0/0
  event=lower function=unrelated source=false dependent=false callee=2/2/0 dependent-work=1/1/3 receipts=1/1 finite-formal=0/0/0/0

Every installed authority module is explicitly private in Dune metadata.
Standard Dune/Findlib consumers cannot import those modules.  A separate
manual-private-path adversary can name their opaque types, but no registry,
session, receipt, or completion can be supplied to the facade's hidden fresh
session or converted into a provider handle.

  $ root="${PWD%%/_build/*}"
  $ install_root="${VEROCAML_TEST_INSTALL_ROOT:-$root/_build/install/default}"
  $ package="$install_root/lib/verocaml/dune-package"
  $ authorities="Finite_formal_requirement Imported_callable External_target_specification_private Typedtree_adapter_private Interface_specification_environment_private Interface_specification_candidate_private Interface_specification_loaded_private Typedtree_lowering_private Instance_mode Spec_definition Sst_validation_private Verification_identity Finite_value_registry Verification_session Symbolic_executor_private Verification_pipeline Spec_unfolding_private Z3_bridge Recursive_spec_encoding Rank_encoding Verification_solver_private Verification_driver_private"
  $ ./check_private_visibility.exe "$package" $authorities
  private-authority-modules=22
  $ mkdir -p artifacts/cold-findlib
  $ cat > artifacts/cold-findlib/import.ml <<'EOF'
  > let _ = Verification_session.create
  > EOF
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" OCAML_COLOR=never ocamlfind ocamlc -package verocaml.core -c artifacts/cold-findlib/import.ml -o artifacts/cold-findlib/import.cmo >/dev/null 2>&1; test $? = 2
  $ mkdir -p artifacts/cold-dune
  $ cat > artifacts/cold-dune/dune-project <<'EOF'
  > (lang dune 3.17)
  > EOF
  $ cat > artifacts/cold-dune/dune <<'EOF'
  > (executable (name import) (libraries verocaml.core))
  > EOF
  $ cp artifacts/cold-findlib/import.ml artifacts/cold-dune/import.ml
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" OCAML_COLOR=never dune build --root artifacts/cold-dune >/dev/null 2>&1; test $? != 0
  $ cat > artifacts/cold-findlib/facade.ml <<'EOF'
  > let _ = Interface_specification.verify_consumer
  > EOF
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -linkpkg -package verocaml.core artifacts/cold-findlib/facade.ml -o artifacts/cold-findlib/facade.exe 2>/dev/null
  $ test -x artifacts/cold-findlib/facade.exe && echo 'cold Findlib facade linked'
  cold Findlib facade linked
  $ mkdir -p artifacts/cold-dune-facade
  $ cat > artifacts/cold-dune-facade/dune-project <<'EOF'
  > (lang dune 3.17)
  > EOF
  $ cat > artifacts/cold-dune-facade/dune <<'EOF'
  > (executable (name facade) (libraries verocaml.core))
  > EOF
  $ cp artifacts/cold-findlib/facade.ml artifacts/cold-dune-facade/facade.ml
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" dune build --root artifacts/cold-dune-facade
  $ cat > artifacts/cold-findlib/service.ml <<'EOF'
  > let _ = Verifier_service.configuration
  > EOF
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -linkpkg -package verocaml.core artifacts/cold-findlib/service.ml -o artifacts/cold-findlib/service.exe 2>/dev/null
  $ test -x artifacts/cold-findlib/service.exe && echo 'cold verifier service linked'
  cold verifier service linked
  $ for module_name in Interface_specification_environment_private Interface_specification_candidate_private Interface_specification_loaded_private External_target_specification_private Verocaml_bin Verocaml_bin_render; do printf 'let _ = %s.main\n' "$module_name" > artifacts/cold-findlib/private.ml; OCAMLPATH="$install_root/lib:$OCAMLPATH" OCAML_COLOR=never ocamlfind ocamlc -package verocaml.core -c artifacts/cold-findlib/private.ml -o artifacts/cold-findlib/private.cmo >/dev/null 2>&1; test $? = 2 || exit 1; done
  $ echo 'cold private authority/bridge/bin imports rejected'
  cold private authority/bridge/bin imports rejected
  $ core="$install_root/lib/verocaml/core"
  $ private_flags=""; for directory in $(find "$install_root/lib/verocaml" -type d -name .private); do private_flags="$private_flags -I $directory"; done
  $ for attack in installed_inject_registry installed_inject_session installed_convert_receipt installed_convert_completion; do OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c "fixtures/$attack.ml" -o "artifacts/$attack.cmo" > "artifacts/$attack.out" 2>&1; test $? = 2; done
  $ grep -R -- '-I .*verocaml_core.objs' ../../src/dune >/dev/null && exit 1 || :

A cold installed invocation creates no cache, receipt, certificate, or sidecar.

  $ mkdir -p cold/home cold/cache cold/work
  $ cp artifacts/local_positive.cmt cold/work/input.cmt
  $ find cold -type f -printf '%P\n' | sort > artifacts/cold.before
  $ (cd cold/work && HOME=../home XDG_CACHE_HOME=../cache OCAML_COLOR=never "$install_root/bin/verocaml" verify input.cmt --timeout-ms 5000 >/dev/null)
  $ find cold -type f -printf '%P\n' | sort > artifacts/cold.after
  $ diff -u artifacts/cold.before artifacts/cold.after
  $ cat artifacts/cold.after
  work/input.cmt
