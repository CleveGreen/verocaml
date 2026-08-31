  $ ./proof_mode_tool.exe structural
  rejected: cyclic proof graph
  rejected: counterfeit recursive proof marker
  rejected: cross-mode recursive SCC
  rejected: proof region outside statement position
  raw proof graph, marker, stage, and statement-position checks passed

  $ mkdir artifacts
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/positive.ml > artifacts/o 2> artifacts/source
  $ test ! -s artifacts/o
  $ grep '^let run' artifacts/source
  let run (x : int) = (); x[@@verocaml.internal.artifact_family.ordinary-v1 ]
  $ test $(grep -c '^let ' artifacts/source) -eq 1
  $ test $(grep -c 'Vero_ghost\|lemma\|verocaml.proof' artifacts/source) -eq 0
  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/erased.cmo fixtures/positive.ml
  $ ./proof_mode_tool.exe inspect erased artifacts/erased.cmt
  ordinary CMT erased proof declarations and regions

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/positive.cmo fixtures/positive.ml
  $ ./proof_mode_tool.exe inspect retained artifacts/positive.cmt
  retained CMT authenticated two proof declarations and one region

A trusted external-body disposition can authenticate a Proof declaration.  It
uses the existing Proof-call stage and supplies no function execution of its
own.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/trusted_external_pfc.cmo fixtures/trusted_external_pfc.ml
  $ ./proof_mode_tool.exe sst artifacts/trusted_external_pfc.cmt > artifacts/trusted-external.sst
  $ grep -E '^function (admit|assume|caller)|trusted-external-body|proof-call (admit|assume)' artifacts/trusted-external.sst
  function admit#0 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ trusted_external_pfc.ml:1:0-5:18
    body trusted-external-body trust=axiomatic provenance=typedtree:trusted_external_pfc.ml declaration-span=trusted_external_pfc.ml:1:0-5:18 witness-span=trusted_external_pfc.ml:4:0-4:26 requires=0 ensures=1 body=unchecked
  function assume#1 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ trusted_external_pfc.ml:7:0-10:18
      proof-call admit#0 recursive=false type-arguments=[] : unit @ trusted_external_pfc.ml:9:2-9:10
  function caller#2 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ trusted_external_pfc.ml:12:0-16:18
      proof-call assume#1 recursive=false type-arguments=[] : unit @ trusted_external_pfc.ml:15:2-15:13
  $ ./proof_mode_tool.exe vir artifacts/trusted_external_pfc.cmt > artifacts/trusted-external.vir
  $ grep -E '^trusted-external-body|^function |^  trusted-external-body' artifacts/trusted-external.vir
  trusted-external-body-declaration trust=axiomatic mode=proof function=admit#0 declaration-span=trusted_external_pfc.ml:1:0-5:18 witness-span=trusted_external_pfc.ml:4:0-4:26 requires=0 ensures=1 body=unchecked
  function assume#1 mode=proof body=proof-typedtree:trusted_external_pfc.ml policy=default-linear/default-z3
    trusted-external-body trust=axiomatic mode=proof call-form=proof function=admit#0 declaration-span=trusted_external_pfc.ml:1:0-5:18 witness-span=trusted_external_pfc.ml:4:0-4:26 call=trusted_external_pfc.ml:9:2-9:10 requires=0 ensures=1 body=unchecked result=constrained-only-by-ensures
  function caller#2 mode=proof body=proof-typedtree:trusted_external_pfc.ml policy=default-linear/default-z3
  $ test $(grep -c '^function admit' artifacts/trusted-external.vir) -eq 0
  $ ./proof_mode_tool.exe solve artifacts/trusted_external_pfc.cmt
  assume: verified (1 obligations)
  caller: verified (1 obligations)

  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; ./proof_mode_tool.exe reject "artifacts/$n.cmt"; }
  $ retained runtime_call
  semantic validation rejected before VIR
  $ retained proof_calls_exec
  semantic validation rejected before VIR
  $ retained decreases
  semantic validation rejected before VIR
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nonunit.cmo fixtures/nonunit.ml
  $ ./proof_mode_tool.exe sst artifacts/nonunit.cmt | grep '^function bad'
  function bad#0 mode=proof recursive=false result=int policy=default-linear/default-z3 @ nonunit.ml:1:0-1:36
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/aggregate.cmo fixtures/aggregate.ml
  $ ./proof_mode_tool.exe solve artifacts/aggregate.cmt
  bad: verified (0 obligations)

  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/raw.cmo fixtures/raw_attribute.ml
  $ ./proof_mode_tool.exe reject artifacts/raw.cmt
  adapter rejected: VERO_MALFORMED_GHOST_CALL
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -c -o artifacts/counterfeit.cmo fixtures/counterfeit.ml
  $ ./proof_mode_tool.exe reject artifacts/counterfeit.cmt
  adapter rejected: VERO_MALFORMED_GHOST_CALL

  $ ppx_failure () { n=$1; f=$2; if OCAML_COLOR=never ocamlc -c -ppx ../../ppx/vero_ppx.exe -o "artifacts/$n.cmo" "fixtures/$n.ml" >"artifacts/$n.err" 2>&1; then return 1; fi; grep -F "$f" "artifacts/$n.err" >/dev/null; }
  $ ppx_failure payload "does not accept a payload"
  $ ppx_failure bad_region_payload "expects exactly one expression payload"
  $ ppx_failure recursive_mutual "requires a single top-level binding"

Recursive proof declarations remain absent from ordinary compilation, while the
retained CMT authenticates the declaration, its decrease carrier, and its proof
region without preserving raw attributes.

  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/positive_recursive.ml > artifacts/recursive.o 2> artifacts/recursive.source
  $ test ! -s artifacts/recursive.o
  $ cat artifacts/recursive.source
  let run_recursive (n : int) = (); n[@@verocaml.internal.artifact_family.ordinary-v1
                                       ]
  $ test $(grep -c 'positive\|induct\|Vero_ghost\|verocaml.proof\|verocaml.decreases' artifacts/recursive.source) -eq 0
  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/recursive_erased.cmo fixtures/positive_recursive.ml
  $ ./proof_mode_tool.exe inspect erased artifacts/recursive_erased.cmt
  ordinary CMT erased proof declarations and regions

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/positive_recursive.cmo fixtures/positive_recursive.ml
  $ ./proof_mode_tool.exe inspect recursive artifacts/positive_recursive.cmt
  retained CMT authenticated recursive proof and decreases carriers
  $ ./proof_mode_tool.exe termination artifacts/positive_recursive.cmt
  recursive proof uses one opaque pending summary and ordered integer entry/edge intents

  $ recursive_retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }

Malformed and unsupported recursive-proof forms fail during PPX, adapter,
semantic validation, or termination preparation.  No solver configuration is
created by these commands.

  $ recursive_retained recursive_missing_decreases
  $ ./proof_mode_tool.exe reject artifacts/recursive_missing_decreases.cmt
  semantic validation rejected before VIR
  $ recursive_retained recursive_dormant_missing_decreases
  $ ./proof_mode_tool.exe reject artifacts/recursive_dormant_missing_decreases.cmt
  semantic validation rejected before VIR
  $ recursive_retained recursive_duplicate_decreases
  $ ./proof_mode_tool.exe reject-totality artifacts/recursive_duplicate_decreases.cmt
  termination rejected before solver: duplicate: direct recursion has more than one decreases measure at recursive_duplicate_decreases.ml:3:2-3:25
  $ recursive_retained recursive_measure
  $ ./proof_mode_tool.exe reject artifacts/recursive_measure.cmt
  semantic validation rejected before VIR
  $ recursive_retained recursive_proof_calls_exec
  $ ./proof_mode_tool.exe reject artifacts/recursive_proof_calls_exec.cmt
  semantic validation rejected before VIR
  $ recursive_retained recursive_aggregate
  $ ./proof_mode_tool.exe reject artifacts/recursive_aggregate.cmt
  semantic validation rejected before VIR
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -c -o artifacts/recursive_counterfeit.cmo fixtures/recursive_counterfeit.ml
  $ ./proof_mode_tool.exe reject artifacts/recursive_counterfeit.cmt
  adapter rejected: VERO_MALFORMED_GHOST_CALL

Installed clients still cannot obtain pre-totality equations, reveal
privileges, or reinterpret pending proof summaries as totality.

  $ root="${PWD%%/_build/*}"
  $ core="$root/_build/install/default/lib/verocaml/core"
  $ private_flags=""; for directory in $(find "$root/_build/install/default/lib/verocaml" -type d -name .private); do private_flags="$private_flags -I $directory"; done
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_proof_normalize_compatibility.ml -o artifacts/proof_normalize_compatibility.cmo 2> artifacts/proof_normalize_compatibility.err
  $ OCAML_COLOR=never ocamlfind ocamlc -linkpkg -package smtml,zarith,compiler-libs.common,delator -I "$core" $private_flags "$core/verocaml_core.cma" artifacts/proof_normalize_compatibility.cmo -o artifacts/proof_normalize_compatibility.exe 2>> artifacts/proof_normalize_compatibility.err
  $ ./artifacts/proof_normalize_compatibility.exe
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_recursive_proof_equation_attack.ml -o artifacts/equation_attack.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Termination.pending_equation"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_recursive_proof_reveal_attack.ml -o artifacts/reveal_attack.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Termination.pending_reveal"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_recursive_proof_totality_attack.ml -o artifacts/totality_attack.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Termination.seal"
