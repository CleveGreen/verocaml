The project command accepts only explicit CMT/CMI pairs.  The ordinary legacy
unit is compiled without ghost retention; selection skips it before retained
candidacy and every semantic/backend/private-driver counter remains zero.

  $ mkdir ordinary retained copied stale family parameterized argument-for
  $ export PPX_ORDINARY="$PWD/../../ppx/vero_ppx.exe"
  $ export PPX_RETAINED="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ compile () { dir=$1; ppx=$2; src=$3; shift 3; (cd "$dir" && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$ppx" "$@" -c "$src"); }
  $ cp fixtures/legacy.ml ordinary/
  $ compile ordinary "$PPX_ORDINARY" legacy.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify-project --root ordinary/legacy.cmt ordinary/legacy.cmi --threads 1 --timeout-ms 60000
  verocaml: skipped unit=Legacy file=ordinary/legacy.cmt result=skipped
  $ ./verification_scope_counter_tool.exe skip ordinary/legacy.cmt ordinary/legacy.cmi
  skip-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0

A marked retained twin enters the existing verifier and receives its normal
source location.  A marked artifact produced without retention never gets
relabeled as retained.

  $ cp fixtures/marked_legacy.ml retained/
  $ compile retained "$PPX_RETAINED" marked_legacy.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify-project --root retained/marked_legacy.cmt retained/marked_legacy.cmi --threads 1 --timeout-ms 60000 >retained/marked.out 2>&1; echo $?
  2
  $ cat retained/marked.out
  verocaml: error[VERO_UNSUPPORTED_STRUCTURE_ITEM] structure item is outside the pure SST subset @ marked_legacy.ml:3:0-7:3
  verocaml: verified unit=Marked_legacy file=retained/marked_legacy.cmt result=rejected
  $ mkdir ordinary-marked
  $ cp fixtures/pass_root.ml ordinary-marked/
  $ compile ordinary-marked "$PPX_ORDINARY" pass_root.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify-project --root ordinary-marked/pass_root.cmt ordinary-marked/pass_root.cmi --threads 1 >ordinary-marked/out 2>&1; echo $?
  2
  $ grep -F 'unsupported retained VeroCaml PPX identity' ordinary-marked/out >/dev/null

Explicit force verification remains marker-independent for both source and CMT
routes.

  $ mkdir force-marked force-unmarked
  $ cp fixtures/pass_root.ml force-marked/pass_root.ml
  $ tail -n +3 fixtures/pass_root.ml > force-unmarked/pass_root.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify force-marked/pass_root.ml --threads 1 --timeout-ms 60000 | sed 's#force-marked/pass_root.ml#FILE#' > force-marked/source.out
  $ OCAML_COLOR=never ../../src/verocaml.exe verify force-unmarked/pass_root.ml --threads 1 --timeout-ms 60000 | sed 's#force-unmarked/pass_root.ml#FILE#' > force-unmarked/source.out
  $ cmp force-marked/source.out force-unmarked/source.out
  $ compile force-marked "$PPX_RETAINED" pass_root.ml
  $ compile force-unmarked "$PPX_RETAINED" pass_root.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify force-marked/pass_root.cmt --threads 1 --timeout-ms 60000 | sed 's#force-marked/pass_root.cmt#FILE#' > force-marked/cmt.out
  $ OCAML_COLOR=never ../../src/verocaml.exe verify force-unmarked/pass_root.cmt --threads 1 --timeout-ms 60000 | sed 's#force-unmarked/pass_root.cmt#FILE#' > force-unmarked/cmt.out
  $ cmp force-marked/cmt.out force-unmarked/cmt.out

Marked providers are authenticated and reverified in dependency order.  Two
selected roots contain real solver obligations.  Reversed inventory order and
serial/threaded scheduling produce the same semantic partition, outcomes,
counts, and report ordering.

  $ cp fixtures/provider.mli fixtures/provider.ml fixtures/client.ml fixtures/second_root.ml retained/
  $ compile retained "$PPX_RETAINED" provider.mli
  $ compile retained "$PPX_RETAINED" provider.ml
  $ compile retained "$PPX_RETAINED" client.ml
  $ compile retained "$PPX_RETAINED" second_root.ml
  $ project () { OCAML_COLOR=never ../../src/verocaml.exe verify-project "$@" --timeout-ms 60000; }
  $ project --dependency retained/provider.cmt retained/provider.cmi --root retained/second_root.cmt retained/second_root.cmi --root retained/client.cmt retained/client.cmi --threads 1 > retained/order-a.out
  $ project --root retained/client.cmt retained/client.cmi --root retained/second_root.cmt retained/second_root.cmi --dependency retained/provider.cmt retained/provider.cmi --threads 2 > retained/order-b.out
  $ cmp retained/order-a.out retained/order-b.out
  $ cat retained/order-a.out
  verocaml: verified unit=Client file=retained/client.cmt result=verified functions=1 obligations=1
  verocaml: verified unit=Second_root file=retained/second_root.cmt result=verified functions=1 obligations=1
  verocaml: verified-dependency unit=Provider file=retained/provider.cmt result=verified
  $ ./verification_scope_counter_tool.exe selected retained/client.cmt retained/client.cmi retained/provider.cmt retained/provider.cmi
  selected-counters retained=advanced typed-lowering=advanced semantic=advanced vc=advanced backend=advanced private-driver=advanced provider-reverification=advanced solver=advanced z3=advanced

Copied artifacts preserve unit, classification, outcome, count, and order fields.
The intentionally rendered file field continues to show each supplied path.

  $ cp retained/client.cmt retained/client.cmi retained/second_root.cmt retained/second_root.cmi retained/provider.cmt retained/provider.cmi copied/
  $ project --root copied/second_root.cmt copied/second_root.cmi --dependency copied/provider.cmt copied/provider.cmi --root copied/client.cmt copied/client.cmi --threads 1 > copied/raw.out
  $ grep -c 'file=copied/' copied/raw.out
  3
  $ sed 's#file=copied/#file=retained/#g' copied/raw.out > copied/semantic.out
  $ cmp retained/order-a.out copied/semantic.out
  $ echo 'copied-report semantic-fields=stable file-field=supplied-path'
  copied-report semantic-fields=stable file-field=supplied-path

The complete partition is printed even when one selected root fails.  A
skipped-only inventory succeeds, while a selected counterexample controls the
final nonzero status.

  $ cp fixtures/fail_root.ml retained/
  $ compile retained "$PPX_RETAINED" fail_root.ml
  $ project --root retained/fail_root.cmt retained/fail_root.cmi --root retained/client.cmt retained/client.cmi --dependency retained/provider.cmt retained/provider.cmi --root ordinary/legacy.cmt ordinary/legacy.cmi --threads 1 >retained/partial.out 2>&1; echo $?
  1
  $ grep '^verocaml:' retained/partial.out | sed -E 's/span=[^ ]+/span=<span>/'
  verocaml: counterexample function=reject#0 vc=assertion[0] span=<span> result=counterexample
  verocaml: verified unit=Client file=retained/client.cmt result=verified functions=1 obligations=1
  verocaml: verified unit=Fail_root file=retained/fail_root.cmt result=counterexample functions=1 obligations=1
  verocaml: verified-dependency unit=Provider file=retained/provider.cmt result=verified
  verocaml: skipped unit=Legacy file=ordinary/legacy.cmt result=skipped

Inventory identity, role, retained-family, and CRC failures all reject before
any selected verification is dispatched.  The wrong-unit pair is rejected by
explicit CMI unit/implementation identity.  The same-unit family pair uses an
ordinary separately compiled interface with a retained implementation, so it
passes identity and digest authentication before reaching family agreement.

  $ project --root retained/client.cmt retained/client.cmi --dependency retained/provider.cmt retained/provider.cmi --root retained/provider.cmt retained/provider.cmi --threads 1 >retained/duplicate.out 2>&1; echo $?
  2
  $ grep -F 'duplicate root/dependency roles' retained/duplicate.out >/dev/null
  $ ./verification_scope_counter_tool.exe rejected retained/client.cmt retained/fail_root.cmi
  preflight=load-rejected code=VERO_MALFORMED_INPUT message=input is not a complete typed-tree artifact
  preflight-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0
  $ cp fixtures/family_pair.mli fixtures/family_pair.ml family/
  $ compile family "$PPX_ORDINARY" family_pair.mli
  $ compile family "$PPX_RETAINED" family_pair.ml
  $ ./verification_scope_counter_tool.exe rejected family/family_pair.cmt family/family_pair.cmi
  preflight=scope-rejected message=implementation CMT family [retained-v1] and explicit CMI family [ordinary-v1] differ
  preflight-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0

Explicit retained CMT/CMI pairs carrying compilation-unit parameters or
argument-for metadata reject at CMI preflight.  Neither reaches retained
candidacy or any downstream owner.

  $ cp fixtures/parameter_seed.mli fixtures/parameterized_root.mli fixtures/parameterized_root.ml parameterized/
  $ compile parameterized "$PPX_RETAINED" parameter_seed.mli -as-parameter
  $ compile parameterized "$PPX_RETAINED" parameterized_root.mli -parameter Parameter_seed
  $ compile parameterized "$PPX_RETAINED" parameterized_root.ml -parameter Parameter_seed
  $ ./verification_scope_counter_tool.exe rejected parameterized/parameterized_root.cmt parameterized/parameterized_root.cmi
  preflight=load-rejected code=VERO_UNSUPPORTED_CMI_PARAMETERS message=explicit CMI compilation-unit parameters are not supported
  preflight-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0
  $ cp fixtures/argument_target.mli fixtures/argument_for_root.mli fixtures/argument_for_root.ml argument-for/
  $ compile argument-for "$PPX_RETAINED" argument_target.mli -as-parameter
  $ compile argument-for "$PPX_RETAINED" argument_for_root.mli -as-argument-for Argument_target
  $ compile argument-for "$PPX_RETAINED" argument_for_root.ml -as-argument-for Argument_target
  $ ./verification_scope_counter_tool.exe rejected argument-for/argument_for_root.cmt argument-for/argument_for_root.cmi
  preflight=load-rejected code=VERO_UNSUPPORTED_CMI_ARGUMENT_FOR message=explicit CMI argument-for metadata is not supported
  preflight-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0
  $ project --root retained/client.cmt retained/client.cmi --dependency retained/provider.cmt retained/missing.cmi --threads 1 >retained/missing.out 2>&1; echo $?
  2
  $ cp retained/provider.cmi stale/provider.cmi
  $ { cat fixtures/provider.mli; printf '\nval newer : int\n'; } > stale/provider.mli
  $ compile stale "$PPX_RETAINED" provider.mli
  $ project --root retained/client.cmt retained/client.cmi --dependency retained/provider.cmt stale/provider.cmi --threads 1 >retained/stale.out 2>&1; echo $?
  2

A marked client cannot route a call through an unmarked provider before the
separate trusted-import ticket.

  $ mkdir bypass
  $ cp fixtures/unmarked_provider.ml fixtures/unmarked_client.ml bypass/
  $ compile bypass "$PPX_ORDINARY" unmarked_provider.ml
  $ compile bypass "$PPX_RETAINED" unmarked_client.ml
  $ project --root bypass/unmarked_client.cmt bypass/unmarked_client.cmi --dependency bypass/unmarked_provider.cmt bypass/unmarked_provider.cmi --threads 1 >bypass/out 2>&1; echo $?
  2
  $ cat bypass/out
  verocaml: error[VERO_UNSUPPORTED_EXTERNAL_CALL] unknown and external calls are not supported @ unmarked_client.ml:3:25-3:57
  verocaml: verified unit=Unmarked_client file=bypass/unmarked_client.cmt result=rejected
  verocaml: skipped unit=Unmarked_provider file=bypass/unmarked_provider.cmt result=skipped

The private owner and new dispatch functions remain below the accepted caps,
and the executable dependency remains one-way through the service.

  $ test "$(wc -l < ../../src/verification_scope_private.ml)" -lt 800
  $ test "$(wc -l < ../../src/verocaml_bin_project_private.ml)" -lt 400
  $ python3 - <<'PY'
  > from pathlib import Path
  > scope = Path('../../src/verification_scope_private.ml').read_text()
  > project = Path('../../src/verocaml_bin_project_private.ml').read_text()
  > assert 'Verification_scope_private' not in project
  > binary = Path('../../src/verocaml_bin.ml').read_text()
  > assert 'Verifier_service' in binary
  > assert 'Verification_scope_private' not in binary
  > assert 'Verocaml_bin' not in scope
  > print('architecture scope-owner=private lines<800 dependency=one-way')
  > PY
  architecture scope-owner=private lines<800 dependency=one-way
