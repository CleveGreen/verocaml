This manual specialist check covers private retained metadata, early-rejection
instrumentation, mixed ABI authentication, generated attack matrices, and
ordinary erasure.  It selects qualitative presence or absence and command
success only; rendered rows, ordering, locations, identities, reports, and
inventory totals are not pinned.

  $ mkdir -p artifacts/ordinary artifacts/retained
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ bounded_verify () { timeout --foreground --signal=TERM --kill-after=5s 60s env OCAML_COLOR=never ../../src/verocaml.exe verify "$@" >/dev/null 2>&1; }

Retained mode metadata includes all supported modes, and the representative
positive units verify.

  $ retained modes
  $ retained forgetting
  $ ./instance_modes_tool.exe modes artifacts/modes.cmt > artifacts/modes.txt
  $ for mode in Exec Ghost Tracked; do grep -q "mode=$mode" artifacts/modes.txt || exit 1; done
  $ bounded_verify artifacts/modes.cmt
  $ bounded_verify artifacts/forgetting.cmt

The standalone Proof exception authenticates Tracked input without issuing
synthetic or unrelated authority.  The enclosing executable and raw-provenance
controls reject without emitting a flow trace.

  $ for n in tracked_call_bare tracked_return_bare standalone_proof_bare_tracked exec_call_tracked_bare; do retained "$n"; done
  $ for n in tracked_call_bare tracked_return_bare standalone_proof_bare_tracked; do bounded_verify "artifacts/$n.cmt" || exit 1; done
  $ : > artifacts/standalone.trace
  $ for n in tracked_call_bare tracked_return_bare standalone_proof_bare_tracked; do VEROCAML_TEST_INSTANCE_MODE_TRACE=1 DELATOR_LOG=Instance_mode=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never ./instance_modes_tool.exe modes "artifacts/$n.cmt" >/dev/null 2>>artifacts/standalone.trace || exit 1; done
  $ grep -q 'authenticated=true expected_mode=Tracked incoming_mode=Tracked outcome=preserve-exact-Tracked' artifacts/standalone.trace
  $ grep -q 'authenticated=true expected_mode=Ghost incoming_mode=Tracked outcome=observe-as-Ghost' artifacts/standalone.trace
  $ if grep -Eq 'synthetic_(ghost|tracked)=[1-9]|unrelated_authority=[1-9]' artifacts/standalone.trace; then exit 1; fi
  $ VEROCAML_TEST_INSTANCE_MODE_TRACE=1 DELATOR_LOG=Instance_mode=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never ./instance_modes_tool.exe reject-zero-no-flow artifacts/exec_call_tracked_bare.cmt >/dev/null 2>artifacts/exec.trace
  $ test ! -s artifacts/exec.trace
  $ VEROCAML_TEST_INSTANCE_MODE_TRACE=1 DELATOR_LOG=Instance_mode=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never ./instance_modes_tool.exe reject-raw-proof-body-no-authority artifacts/tracked_call_bare.cmt >/dev/null 2>artifacts/raw.trace
  $ test ! -s artifacts/raw.trace

Default-Ghost boundaries authenticate every admitted incoming mode without
synthetic descriptors.  Aggregate metadata likewise exposes each admitted mode
at each supported site.

  $ retained positive_forgetting
  $ ./instance_modes_tool.exe modes artifacts/positive_forgetting.cmt > artifacts/positive.modes
  $ for mode in Exec Ghost Tracked; do grep -q "incoming=$mode" artifacts/positive.modes || exit 1; done
  $ grep -q 'authenticated=true formal-mode=Ghost boundary-result-mode=Ghost callee-result-mode=Ghost' artifacts/positive.modes
  $ if grep -Eq 'synthetic-(ghost|tracked)=[1-9]' artifacts/positive.modes; then exit 1; fi
  $ bounded_verify artifacts/positive_forgetting.cmt
  $ retained aggregate
  $ ./instance_modes_tool.exe modes artifacts/aggregate.cmt > artifacts/aggregate.modes
  $ for site in binding expression field pattern; do for mode in Exec Ghost Tracked; do grep -q "^$site:.* mode=$mode" artifacts/aggregate.modes || exit 1; done; done
  $ bounded_verify artifacts/aggregate.cmt

Ordinary bytecode and native objects erase proof and mode carriers, and erased
effects are not evaluated.

  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/positive.cmo fixtures/positive_forgetting.ml
  $ ocamlopt -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/positive.cmx fixtures/positive_forgetting.ml
  $ if strings artifacts/ordinary/positive.cmo artifacts/ordinary/positive.cmx | grep -E 'nonnegative|proof_observe|recursive_observe|tracked_successor' >/dev/null; then exit 1; fi
  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/aggregate.cmo fixtures/aggregate.ml
  $ ocamlopt -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/aggregate.cmx fixtures/aggregate.ml
  $ if strings artifacts/ordinary/aggregate.cmo artifacts/ordinary/aggregate.cmx | grep -E 'ghost|tracked' >/dev/null; then exit 1; fi
  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -o artifacts/ordinary/update.exe fixtures/ordinary_update_absence.ml
  $ artifacts/ordinary/update.exe
  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -o artifacts/ordinary/absence.exe fixtures/ordinary_absence.ml
  $ artifacts/ordinary/absence.exe

Retained and ordinary interface families cannot be mixed, and mode-bearing
interface substitutions reject through the private zero-backend probe.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained/signatures.cmi fixtures/signatures.mli
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/retained -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained/signatures.cmo fixtures/signatures.ml
  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/signatures.cmi fixtures/signatures.mli
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts/ordinary -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/signatures.cmo fixtures/signatures.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/retained -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained/signatures_client.cmo fixtures/signatures_client.ml
  $ ./instance_modes_tool.exe dependency-modes artifacts/retained/signatures_client.cmt artifacts/retained/signatures.cmt >/dev/null
  $ cp artifacts/retained/signatures.cmt artifacts/mixed.cmt
  $ cp artifacts/ordinary/signatures.cmi artifacts/mixed.cmi
  $ if bounded_verify artifacts/mixed.cmt; then exit 1; fi
  $ cp artifacts/ordinary/signatures.cmt artifacts/reverse-mixed.cmt
  $ cp artifacts/retained/signatures.cmi artifacts/reverse-mixed.cmi
  $ if bounded_verify artifacts/reverse-mixed.cmt; then exit 1; fi
  $ ./instance_modes_tool.exe embed-ordinary-interface artifacts/retained/signatures.cmt artifacts/ordinary/signatures.cmi artifacts/embedded-mixed.cmt
  $ ./instance_modes_tool.exe reject-zero artifacts/embedded-mixed.cmt >/dev/null 2>&1
  $ for n in signature_mode_mismatch type_mode_mismatch; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmi" "fixtures/$n.mli"; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; ./instance_modes_tool.exe reject-zero "artifacts/$n.cmt" >/dev/null || exit 1; done

Unsupported retained inputs reject through the private early-boundary probe.
PPX-owned syntax rejections are accepted only at fixture construction.

  $ for n in ordinary_absence missing_actual cross_mode tracked_formal_bare open_pattern erased_effect ghost_authority external_mode trusted_mode; do test -e "artifacts/$n.cmt" || retained "$n"; ./instance_modes_tool.exe reject-zero "artifacts/$n.cmt" >/dev/null || exit 1; done
  $ for n in recursive_tracked inherited_update; do if retained "$n" >/dev/null 2>&1; then exit 1; fi; done
  $ ./instance_modes_tool.exe copied artifacts/modes.cmt >/dev/null
  $ retained forgetting_effect
  $ VEROCAML_TEST_INSTANCE_MODE_TRACE=1 DELATOR_LOG=Instance_mode=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never ./instance_modes_tool.exe reject-zero-no-flow artifacts/forgetting_effect.cmt >/dev/null 2>artifacts/forgetting-effect.trace
  $ test ! -s artifacts/forgetting-effect.trace
  $ for n in explicit_ghost_spec_bare explicit_ghost_proof_bare; do retained "$n"; VEROCAML_TEST_INSTANCE_MODE_TRACE=1 DELATOR_LOG=Instance_mode=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never ./instance_modes_tool.exe reject-zero-no-flow "artifacts/$n.cmt" >/dev/null 2>"artifacts/$n.trace" || exit 1; test ! -s "artifacts/$n.trace" || exit 1; done

Generated unsupported-syntax and authority attacks either fail fixture
construction or reject through the private early-boundary probe.  Generated
inventories and trace multiplicities are not asserted.

  $ ./generate_negative_matrix.exe artifacts/negative
  $ test -n "$(find artifacts/negative -name '*.ml' -print -quit)"
  $ mkdir artifacts/negative-cmt
  $ for file in artifacts/negative/*.ml; do name=$(basename "$file" .ml); if ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/negative-cmt/$name.cmo" "$file" >/dev/null 2>&1; then ./instance_modes_tool.exe reject-zero "artifacts/negative-cmt/$name.cmt" >/dev/null || exit 1; fi; done
  $ retained local_established_invariant
  $ bounded_verify artifacts/local_established_invariant.cmt --dump-vir artifacts/local.vir
  $ grep -q 'boundary=constructor-establishment' artifacts/local.vir
  $ if grep -Eqi 'call-result|entry.*invariant' artifacts/local.vir; then exit 1; fi
  $ ./generate_authority_matrix.exe artifacts/matrix fixtures/authority_prelude.ml
  $ test -n "$(find artifacts/matrix -name '*.ml' -print -quit)"
  $ mkdir artifacts/matrix-cmt
  $ : > artifacts/matrix.trace
  $ for file in artifacts/matrix/*.ml; do name=$(basename "$file" .ml); if ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/matrix-cmt/$name.cmo" "$file" >/dev/null 2>&1; then VEROCAML_TEST_INSTANCE_MODE_TRACE=1 DELATOR_LOG=Instance_mode=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never ./instance_modes_tool.exe reject-zero-flow "artifacts/matrix-cmt/$name.cmt" >/dev/null 2>>artifacts/matrix.trace || exit 1; fi; done
  $ grep -q 'authenticated=true formal_mode=Ghost boundary_result_mode=Ghost' artifacts/matrix.trace
  $ if grep -Eq 'synthetic_(ghost|tracked)=[1-9]' artifacts/matrix.trace; then exit 1; fi
  $ ./generate_unannotated_authority_matrix.exe artifacts/unannotated fixtures/authority_prelude.ml
  $ test -n "$(find artifacts/unannotated -name '*.ml' -print -quit)"
  $ mkdir artifacts/unannotated-cmt
  $ for file in artifacts/unannotated/*.ml; do name=$(basename "$file" .ml); ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/unannotated-cmt/$name.cmo" "$file" >/dev/null 2>&1; VEROCAML_TEST_INSTANCE_MODE_TRACE=1 DELATOR_LOG=Instance_mode=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never ./instance_modes_tool.exe reject-zero-flow "artifacts/unannotated-cmt/$name.cmt" >/dev/null 2>>artifacts/unannotated.trace || exit 1; done
  $ grep -q 'authenticated=true formal_mode=Ghost boundary_result_mode=Ghost' artifacts/unannotated.trace
  $ if grep -Eq 'synthetic_(ghost|tracked)=[1-9]' artifacts/unannotated.trace; then exit 1; fi
