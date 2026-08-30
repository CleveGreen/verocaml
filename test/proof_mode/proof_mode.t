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
  $ ./proof_mode_tool.exe sst artifacts/positive.cmt > artifacts/a.sst
  $ ./proof_mode_tool.exe sst artifacts/positive.cmt > artifacts/b.sst
  $ cmp artifacts/a.sst artifacts/b.sst
  $ grep -E '^function (inc|lemma|lemma2|run)|body proof|proof-call|proof-region' artifacts/a.sst
  function inc#0 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:1:0-1:41
  function lemma#1 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ positive.ml:2:0-7:18
    body proof stage=proof provenance=typedtree:positive.ml
  function lemma2#2 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ positive.ml:8:0-13:18
    body proof stage=proof provenance=typedtree:positive.ml
        proof-call lemma#1 recursive=false type-arguments=[] : unit @ positive.ml:11:2-11:9
  function run#3 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:14:0-17:3
        proof-region stage=proof : unit @ positive.ml:16:2-16:28
          proof-call lemma2#2 recursive=false type-arguments=[] : unit @ positive.ml:16:19-16:27
  $ ./proof_mode_tool.exe vir artifacts/positive.cmt > artifacts/a.vir
  $ ./proof_mode_tool.exe vir artifacts/positive.cmt > artifacts/b.vir
  $ cmp artifacts/a.vir artifacts/b.vir
  $ grep -E '^function |^  vc ' artifacts/a.vir
  function lemma#1 mode=proof body=proof-typedtree:positive.ml policy=default-linear/default-z3
    vc 0 assertion ordinal=0 @ positive.ml:4:2-4:30
    vc 1 postcondition ordinal=0 declaration=positive.ml:5:2-5:45 @ positive.ml:5:2-5:45
  function lemma2#2 mode=proof body=proof-typedtree:positive.ml policy=default-linear/default-z3
    vc 0 assertion ordinal=0 @ positive.ml:10:2-10:30
    vc 1 call-precondition callee=lemma#1 ordinal=0 declaration=positive.ml:3:2-3:29 call=positive.ml:11:2-11:9 @ positive.ml:11:2-11:9
  function run#3 mode=exec body=checked-typedtree:positive.ml policy=default-linear/default-z3
    vc 0 call-precondition callee=lemma2#2 ordinal=0 declaration=positive.ml:9:2-9:29 call=positive.ml:16:19-16:27 @ positive.ml:16:19-16:27
  $ test $(grep -c '\.result\|trusted\|axiom' artifacts/a.vir) -eq 0
  $ ./proof_mode_tool.exe solve artifacts/positive.cmt
  lemma: verified (2 obligations)
  lemma2: verified (2 obligations)
  run: verified (1 obligations)

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
  $ retained value_region
  adapter rejected: VERO_MALFORMED_GHOST_CALL
  $ retained proof_calls_exec
  semantic validation rejected before VIR
  $ retained mutation
  adapter rejected: VERO_UNSUPPORTED_MUTATION
  $ retained decreases
  semantic validation rejected before VIR
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nonunit.cmo fixtures/nonunit.ml
  $ ./proof_mode_tool.exe sst artifacts/nonunit.cmt | grep '^function bad'
  function bad#0 mode=proof recursive=false result=int policy=default-linear/default-z3 @ nonunit.ml:1:0-1:36
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/aggregate.cmo fixtures/aggregate.ml
  $ ./proof_mode_tool.exe solve artifacts/aggregate.cmt
  bad: verified (0 obligations)
  $ retained higher_order
  adapter rejected: VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
  $ retained loop
  adapter rejected: VERO_UNSUPPORTED_LOOP
  $ retained external
  adapter rejected: VERO_UNSUPPORTED_EXTERNAL_CALL

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
  $ ./proof_mode_tool.exe sst artifacts/positive_recursive.cmt > artifacts/recursive-a.sst
  $ ./proof_mode_tool.exe sst artifacts/positive_recursive.cmt > artifacts/recursive-b.sst
  $ cmp artifacts/recursive-a.sst artifacts/recursive-b.sst
  $ grep -E '^function (positive|induct|use_induct|run_recursive)|decreases 0|proof-call induct' artifacts/recursive-a.sst
  function positive#0 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ positive_recursive.ml:1:0-4:18
  function induct#1 mode=proof recursive=true result=unit policy=default-linear/default-z3 @ positive_recursive.ml:6:0-14:18
    decreases 0 stage=logical @ positive_recursive.ml:9:2-9:25
            proof-call induct#1 recursive=true type-arguments=[] : unit @ positive_recursive.ml:12:4-12:18
  function use_induct#2 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ positive_recursive.ml:16:0-19:18
      proof-call induct#1 recursive=false type-arguments=[] : unit @ positive_recursive.ml:18:2-18:10
  function run_recursive#3 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive_recursive.ml:21:0-24:3

The recursive proof VIR is deterministic.  It orders the entry intent, recursive
call precondition, edge nonnegativity, strict descent, restored-environment
helper call, and postconditions.  The later ordinary proof call uses the
verified declaration's contract and the runtime proof region remains erased.

  $ ./proof_mode_tool.exe vir artifacts/positive_recursive.cmt > artifacts/recursive-a.vir
  $ ./proof_mode_tool.exe vir artifacts/positive_recursive.cmt > artifacts/recursive-b.vir
  $ cmp artifacts/recursive-a.vir artifacts/recursive-b.vir
  $ grep -E '^function |^  vc ' artifacts/recursive-a.vir
  function positive#0 mode=proof body=proof-typedtree:positive_recursive.ml policy=default-linear/default-z3
  function induct#1 mode=proof body=proof-typedtree:positive_recursive.ml policy=default-linear/default-z3
    vc 0 entry-measure-nonnegative declaration=positive_recursive.ml:9:2-9:25 @ positive_recursive.ml:9:2-9:25
    vc 1 call-precondition callee=induct#1 ordinal=0 declaration=positive_recursive.ml:7:2-7:29 call=positive_recursive.ml:12:4-12:18 @ positive_recursive.ml:12:4-12:18
    vc 2 recursive-call-measure-nonnegative callee=induct#1 declaration=positive_recursive.ml:9:2-9:25 call=positive_recursive.ml:12:4-12:18 @ positive_recursive.ml:12:4-12:18
    vc 3 recursive-call-strict-descent callee=induct#1 declaration=positive_recursive.ml:9:2-9:25 call=positive_recursive.ml:12:4-12:18 @ positive_recursive.ml:12:4-12:18
    vc 4 call-precondition callee=positive#0 ordinal=0 declaration=positive_recursive.ml:2:2-2:28 call=positive_recursive.ml:13:4-13:14 @ positive_recursive.ml:13:4-13:14
    vc 5 postcondition ordinal=0 declaration=positive_recursive.ml:8:2-8:42 @ positive_recursive.ml:8:2-8:42
    vc 6 postcondition ordinal=0 declaration=positive_recursive.ml:8:2-8:42 @ positive_recursive.ml:8:2-8:42
  function use_induct#2 mode=proof body=proof-typedtree:positive_recursive.ml policy=default-linear/default-z3
    vc 0 call-precondition callee=induct#1 ordinal=0 declaration=positive_recursive.ml:7:2-7:29 call=positive_recursive.ml:18:2-18:10 @ positive_recursive.ml:18:2-18:10
  function run_recursive#3 mode=exec body=checked-typedtree:positive_recursive.ml policy=default-linear/default-z3
    vc 0 call-precondition callee=use_induct#2 ordinal=0 declaration=positive_recursive.ml:17:2-17:29 call=positive_recursive.ml:23:19-23:31 @ positive_recursive.ml:23:19-23:31
  $ grep -F 'goal (> n$0 0)' artifacts/recursive-a.vir
      goal (> n$0 0)
  $ test $(grep -c '\.result\|trusted\|axiom' artifacts/recursive-a.vir) -eq 0
  $ ./proof_mode_tool.exe solve artifacts/positive_recursive.cmt
  positive: verified (0 obligations)
  induct: verified (7 obligations)
  use_induct: verified (1 obligations)
  run_recursive: verified (1 obligations)
  $ recursive_retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ recursive_retained recursive_bool_parameter
  $ ./proof_mode_tool.exe solve artifacts/recursive_bool_parameter.cmt
  bool_induct: verified (4 obligations)

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
  $ recursive_retained recursive_polymorphic
  $ ./proof_mode_tool.exe reject artifacts/recursive_polymorphic.cmt
  adapter rejected: VERO_UNSUPPORTED_POLYMORPHISM
  $ recursive_retained recursive_higher_order
  $ ./proof_mode_tool.exe reject artifacts/recursive_higher_order.cmt
  adapter rejected: VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
  $ recursive_retained recursive_external
  $ ./proof_mode_tool.exe reject artifacts/recursive_external.cmt
  adapter rejected: VERO_UNSUPPORTED_EXTERNAL_CALL
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -c -o artifacts/recursive_counterfeit.cmo fixtures/recursive_counterfeit.ml
  $ ./proof_mode_tool.exe reject artifacts/recursive_counterfeit.cmt
  adapter rejected: VERO_MALFORMED_GHOST_CALL

Negative and non-strict measures retain pending descriptors but fail their
independent ordered termination VCs.

  $ recursive_retained recursive_negative
  $ ./proof_mode_tool.exe outcomes artifacts/recursive_negative.cmt
  negative: counterexample entry-measure-nonnegative
  $ recursive_retained recursive_non_strict
  $ ./proof_mode_tool.exe outcomes artifacts/recursive_non_strict.cmt
  non_strict: counterexample recursive-call-strict-descent

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
