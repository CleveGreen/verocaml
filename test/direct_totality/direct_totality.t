The OCaml assertion executable covers semantic self-call applicability, distinct
termination obligations, ordering, and solver outcomes.

  $ ./direct_totality_tool.exe unit
  totality: countdown, VC order, strict descent, nonnegative calls, and measure safety
  termination descriptors: complete SCCs, integer intents, and opaque pending summaries without a public seal issuer
  applicability: markers, hidden cross edges, mutual SCCs, and malformed measures fail before VIR

The unchanged-measure integration remains a solver counterexample, and even a
client holding pending descriptors cannot report =true= values to mint sealing.

  $ mkdir -p artifacts
  $ root="${PWD%%/_build/*}"
  $ install_root="${VEROCAML_TEST_INSTALL_ROOT:-$root/_build/install/default}"
  $ core="$install_root/lib/verocaml/core"
  $ private_flags=""; for directory in $(find "$install_root/lib/verocaml" -type d -name .private); do private_flags="$private_flags -I $directory"; done
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/non_strict_seal_report_attack.ml -o artifacts/non_strict_seal_report_attack.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Termination.seal"

Compile ordinary annotated OxCaml implementations through the authoritative PPX
and no-op ghost runtime. The verifier consumes the resulting real CMTs.

  $ mkdir -p artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/countdown.cmo fixtures/countdown.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/missing_decreases.cmo fixtures/missing_decreases.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/duplicate_decreases.cmo fixtures/duplicate_decreases.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nonrecursive_decreases.cmo fixtures/nonrecursive_decreases.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/harmless_let_rec.cmo fixtures/harmless_let_rec.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/overflowing_measure.cmo fixtures/overflowing_measure.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/mutual_recursion.cmo fixtures/mutual_recursion.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/loop.cmo fixtures/loop.ml

A real countdown verifies. Its deterministic VIR orders entry nonnegativity,
argument arithmetic, the self-call precondition, call-measure nonnegativity,
strict descent, and postconditions. Declaration and call spans remain distinct.

  $ ./direct_totality_tool.exe dump artifacts/countdown.cmt > artifacts/first.vir
  $ ./direct_totality_tool.exe dump artifacts/countdown.cmt > artifacts/second.vir
  $ cmp artifacts/first.vir artifacts/second.vir
  $ grep -E '^function |^  vc ' artifacts/first.vir
  function countdown#0 mode=exec body=checked-typedtree:countdown.ml policy=default-linear/default-z3
    vc 0 entry-measure-nonnegative declaration=countdown.ml:4:2-4:25 @ countdown.ml:4:2-4:25
    vc 1 arithmetic-lower operation=subtract @ countdown.ml:5:33-5:40
    vc 2 arithmetic-upper operation=subtract @ countdown.ml:5:33-5:40
    vc 3 call-precondition callee=countdown#0 ordinal=0 declaration=countdown.ml:2:2-2:29 call=countdown.ml:5:23-5:40 @ countdown.ml:5:23-5:40
    vc 4 recursive-call-measure-nonnegative callee=countdown#0 declaration=countdown.ml:4:2-4:25 call=countdown.ml:5:23-5:40 @ countdown.ml:5:23-5:40
    vc 5 recursive-call-strict-descent callee=countdown#0 declaration=countdown.ml:4:2-4:25 call=countdown.ml:5:23-5:40 @ countdown.ml:5:23-5:40
    vc 6 postcondition ordinal=0 declaration=countdown.ml:3:2-3:46 @ countdown.ml:3:2-3:46
    vc 7 postcondition ordinal=0 declaration=countdown.ml:3:2-3:46 @ countdown.ml:3:2-3:46
  function run_countdown#1 mode=exec body=checked-typedtree:countdown.ml policy=default-linear/default-z3
    vc 0 call-precondition callee=countdown#0 ordinal=0 declaration=countdown.ml:2:2-2:29 call=countdown.ml:10:2-10:13 @ countdown.ml:10:2-10:13
    vc 1 postcondition ordinal=0 declaration=countdown.ml:9:2-9:46 @ countdown.ml:9:2-9:46
  $ ./direct_totality_tool.exe solve artifacts/countdown.cmt
  countdown: verified (8 obligations, 2 exits)
  run_countdown: verified (2 obligations, 1 exits)

Malformed applicability is rejected before SMT, while an overflowing measure is
a counterexample and a syntactic let-rec without a self-call remains ordinary.

  $ ./direct_totality_tool.exe solve artifacts/missing_decreases.cmt 2>&1
  missing_decreases: direct recursion requires exactly one decreases measure at missing_decreases.ml:1:0-2:48
  [3]
  $ ./direct_totality_tool.exe solve artifacts/duplicate_decreases.cmt 2>&1
  duplicate_decreases: direct recursion has more than one decreases measure at duplicate_decreases.ml:4:2-4:29
  [3]
  $ ./direct_totality_tool.exe solve artifacts/nonrecursive_decreases.cmt 2>&1
  nonrecursive_decreases: decreases measure is not allowed without a resolved direct self-call at nonrecursive_decreases.ml:2:2-2:25
  [3]
  $ ./direct_totality_tool.exe solve artifacts/overflowing_measure.cmt
  overflowing_measure: counterexample (10 obligations, 2 exits)
  $ ./direct_totality_tool.exe solve artifacts/harmless_let_rec.cmt
  harmless_let_rec: verified (0 obligations, 1 exits)

The existing authenticated frontend retains source-located classifications for
mutual recursion and loops.

  $ ./direct_totality_tool.exe classify artifacts/mutual_recursion.cmt
  VERO_UNSUPPORTED_MUTUAL_RECURSION @ mutual_recursion.ml:1:0-2:57
  $ ./direct_totality_tool.exe classify artifacts/loop.cmt
  VERO_UNSUPPORTED_LOOP @ loop.ml:2:2-4:6
