This transcript retains CMT-authentication, private-owner counter, and source-architecture checks.

  $ mkdir ordinary ordinary-marked retained stale family parameterized argument-for
  $ export PPX_ORDINARY="$PWD/../../ppx/vero_ppx.exe"
  $ export PPX_RETAINED="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ compile () { dir=$1; ppx=$2; src=$3; shift 3; (cd "$dir" && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$ppx" "$@" -c "$src"); }
  $ project () { OCAML_COLOR=never ../../src/verocaml.exe verify-project "$@" --timeout-ms 60000; }

An unselected ordinary root performs no work at any private verification owner.

  $ cp fixtures/legacy.ml ordinary/
  $ compile ordinary "$PPX_ORDINARY" legacy.ml
  $ timeout 90s ./verification_scope_counter_tool.exe skip ordinary/legacy.cmt ordinary/legacy.cmi
  skip-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0

A source marker compiled without retained PPX identity is rejected before
verification.  Compiler wording is not selected.

  $ cp fixtures/pass_root.ml ordinary-marked/
  $ compile ordinary-marked "$PPX_ORDINARY" pass_root.ml
  $ project --root ordinary-marked/pass_root.cmt ordinary-marked/pass_root.cmi --threads 1 >/dev/null 2>&1; echo $?
  2

Selected retained roots and providers reach every owned verification boundary.

  $ cp fixtures/provider.mli fixtures/provider.ml fixtures/client.ml fixtures/fail_root.ml retained/
  $ compile retained "$PPX_RETAINED" provider.mli
  $ compile retained "$PPX_RETAINED" provider.ml
  $ compile retained "$PPX_RETAINED" client.ml
  $ compile retained "$PPX_RETAINED" fail_root.ml
  $ timeout 90s ./verification_scope_counter_tool.exe selected retained/client.cmt retained/client.cmi retained/provider.cmt retained/provider.cmi
  selected-counters retained=advanced typed-lowering=advanced semantic=advanced vc=advanced backend=advanced private-driver=advanced provider-reverification=advanced solver=advanced z3=advanced

Role duplication and explicit CMT/CMI identity, family, parameter, argument-for,
missing-interface, and stale-interface failures remain preflight checks.
They are intentionally prepared with compiler flags not represented by an
ordinary project fixture.

  $ project --root retained/client.cmt retained/client.cmi --dependency retained/provider.cmt retained/provider.cmi --root retained/provider.cmt retained/provider.cmi --threads 1 >/dev/null 2>&1; echo $?
  2
  $ timeout 30s ./verification_scope_counter_tool.exe rejected retained/client.cmt retained/fail_root.cmi | sed -E 's/ message=.*/ message=<omitted>/'
  preflight=load-rejected code=VERO_MALFORMED_INPUT message=<omitted>
  preflight-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0
  $ cp fixtures/family_pair.mli fixtures/family_pair.ml family/
  $ compile family "$PPX_ORDINARY" family_pair.mli
  $ compile family "$PPX_RETAINED" family_pair.ml
  $ timeout 30s ./verification_scope_counter_tool.exe rejected family/family_pair.cmt family/family_pair.cmi | sed -E 's/ message=.*/ message=<omitted>/'
  preflight=scope-rejected message=<omitted>
  preflight-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0
  $ cp fixtures/parameter_seed.mli fixtures/parameterized_root.mli fixtures/parameterized_root.ml parameterized/
  $ compile parameterized "$PPX_RETAINED" parameter_seed.mli -as-parameter
  $ compile parameterized "$PPX_RETAINED" parameterized_root.mli -parameter Parameter_seed
  $ compile parameterized "$PPX_RETAINED" parameterized_root.ml -parameter Parameter_seed
  $ timeout 30s ./verification_scope_counter_tool.exe rejected parameterized/parameterized_root.cmt parameterized/parameterized_root.cmi | sed -E 's/ message=.*/ message=<omitted>/'
  preflight=load-rejected code=VERO_UNSUPPORTED_CMI_PARAMETERS message=<omitted>
  preflight-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0
  $ cp fixtures/argument_target.mli fixtures/argument_for_root.mli fixtures/argument_for_root.ml argument-for/
  $ compile argument-for "$PPX_RETAINED" argument_target.mli -as-parameter
  $ compile argument-for "$PPX_RETAINED" argument_for_root.mli -as-argument-for Argument_target
  $ compile argument-for "$PPX_RETAINED" argument_for_root.ml -as-argument-for Argument_target
  $ timeout 30s ./verification_scope_counter_tool.exe rejected argument-for/argument_for_root.cmt argument-for/argument_for_root.cmi | sed -E 's/ message=.*/ message=<omitted>/'
  preflight=load-rejected code=VERO_UNSUPPORTED_CMI_ARGUMENT_FOR message=<omitted>
  preflight-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0
  $ project --root retained/client.cmt retained/client.cmi --dependency retained/provider.cmt retained/missing.cmi --threads 1 >/dev/null 2>&1; echo $?
  2
  $ cp retained/provider.cmi stale/provider.cmi
  $ { cat fixtures/provider.mli; printf '\nval newer : int\n'; } > stale/provider.mli
  $ compile stale "$PPX_RETAINED" provider.mli
  $ project --root retained/client.cmt retained/client.cmi --dependency retained/provider.cmt stale/provider.cmi --threads 1 >/dev/null 2>&1; echo $?
  2

The source-size limits are inclusive architecture maxima, not exact baselines:
the 799/399 caps preserve the existing owner-concentration warning boundary.
The dependency checks enforce one-way authority without asserting source
formatting.

  $ test "$(wc -l < ../../src/verification_scope_private.ml)" -le 799
  $ test "$(wc -l < ../../src/verocaml_bin_project_private.ml)" -le 399
  $ ./verification_scope_architecture_tool.exe ../../src/verification_scope_private.ml ../../src/verocaml_bin_project_private.ml ../../src/verocaml_bin.ml
  architecture scope-owner=private maxima=inclusive dependency=one-way
