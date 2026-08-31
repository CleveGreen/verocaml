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

Malformed decreases declarations remain specialist termination-preparation
checks because this private failure has no VERO-113 outcome projection.

  $ retained_compile () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained_compile missing_decreases
  $ retained_compile duplicate_decreases
  $ retained_compile nonrecursive_decreases
  $ ./direct_totality_tool.exe solve artifacts/missing_decreases.cmt 2>&1
  missing_decreases: direct recursion requires exactly one decreases measure at missing_decreases.ml:1:0-2:48
  [3]
  $ ./direct_totality_tool.exe solve artifacts/duplicate_decreases.cmt 2>&1
  duplicate_decreases: direct recursion has more than one decreases measure at duplicate_decreases.ml:4:2-4:29
  [3]
  $ ./direct_totality_tool.exe solve artifacts/nonrecursive_decreases.cmt 2>&1
  nonrecursive_decreases: decreases measure is not allowed without a resolved direct self-call at nonrecursive_decreases.ml:2:2-2:25
  [3]
