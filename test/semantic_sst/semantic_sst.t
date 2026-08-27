  $ ./semantic_sst_tool.exe structural
  validated descriptors: opaque type/callable/contracts/edges/visibility/features preserve declaration and call order
  semantic admission: handles issue only after the existing ordered validator succeeds
  raw termination graph: Spec/Proof SCC order is preserved while proof self recursion requires authenticated retained authority
  semantic SST structural checks: ids, binders, clauses, bodies, recursion, returns, calls, policy, targets, types
  semantic SST mode matrix: logical/proof/runtime call forms are exhaustive and closed
  semantic type registry: revealed accepted; forged, incomplete, and conflicting abstraction evidence rejected
  callable registry: constructed only after validation and preserves exact semantic identities

The validated semantic issuer is implementation-local to the existing
validation wrapper.  Even when an installed client deliberately adds every
installed =.private= directory, it cannot bind the issuer or inspect an opaque
descriptor.  Successful validation exposes read-only queries and preserves
the installed =Sst_callable= compatibility wrapper.

  $ mkdir -p artifacts
  $ root="${PWD%%/_build/*}"
  $ core="$root/_build/install/default/lib/verocaml/core"
  $ private_flags=""; for directory in $(find "$root/_build/install/default/lib/verocaml" -type d -name .private); do private_flags="$private_flags -I $directory"; done
  $ find "$root/_build/install/default/lib/verocaml" -iname '*semantic_environment*' | wc -l
  0
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_semantic_issuer_attack.ml -o artifacts/installed_semantic_issuer_attack.cmo 2>&1 | grep -F 'Error: Unbound module'
  Error: Unbound module "Sst_validation.Semantic_environment"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_descriptor_forgery.ml -o artifacts/installed_descriptor_forgery.cmo 2>&1 | grep -F 'Error: Unbound record field'
  Error: Unbound record field "Sst_validation.definition"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_termination_pending_attack.ml -o artifacts/installed_termination_pending_attack.cmo 2>&1 | grep -F 'Error: This expression has type'
  Error: This expression has type "Termination.callable"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_termination_equation_attack.ml -o artifacts/installed_termination_equation_attack.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Termination.pending_equation"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_termination_reveal_attack.ml -o artifacts/installed_termination_reveal_attack.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Termination.pending_reveal"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_termination_fuel_attack.ml -o artifacts/installed_termination_fuel_attack.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Termination.pending_fuel"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_termination_seal_forge.ml -o artifacts/installed_termination_seal_forge.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Termination.seal"
  $ ocamlfind ocamlc -custom -package smtml,zarith,compiler-libs.common -linkpkg -I "$core" $private_flags "$core/verocaml_core.cma" fixtures/installed_semantic_queries.ml -o artifacts/installed_semantic_queries.exe 2>/dev/null
  $ ./artifacts/installed_semantic_queries.exe
  installed validated queries, callable wrapper, and termination graph accepted

  $ ./semantic_sst_tool.exe dumps > first.dump
  $ ./semantic_sst_tool.exe dumps > second.dump
  $ cmp first.dump second.dump
  $ cat first.dump
  -- SST --
  policy default-linear/default-z3
  function identity#0 mode=exec recursive=false result=int policy=default-linear/default-z3 @ semantic.ml:1:0-1:8
    parameter
      pattern bind x#0:int : int @ semantic.ml:1:0-1:8
    body checked-exec stage=runtime provenance=raw-semantic
      variable x#0:int : int @ semantic.ml:1:0-1:8
  -- VIR --
  policy default-linear/default-z3
  function identity#0 mode=exec body=checked-raw-semantic policy=default-linear/default-z3
    exit 0
      assumptions
        (<= -4611686018427387904 x$0)
        (<= x$0 4611686018427387903)
        (= result$1 x$0)
        (<= -4611686018427387904 result$1)
        (<= result$1 4611686018427387903)
      path
        (none)
      result
        int result$1
      project
        x$0 int input @ semantic.ml:1:0-1:8

Raw substitutions of the retained recursive tuple tree reject before VIR and
without solver creation.  The matrix independently changes labels, arity,
zero arity, nesting, component types, value/pattern shape, whole-tuple
wildcard/binding shape, tuple let/variable use, partial and post-exhaustive
ordered fallbacks, and a guarded malformed shape.

  $ root="${PWD%%/_build/*}"
  $ mkdir -p artifacts/tuple
  $ ocamlc -w -A -alert -all -bin-annot -I "$root/_build/default/runtime/.vero_ghost.objs/byte" -ppx "$root/_build/install/default/bin/verocaml-ppx --keep-ghost" -c -o artifacts/tuple/recursive_tuple_match.cmo "$root/test/aggregate_recursive_specifications/fixtures/recursive_tuple_match.ml"
  $ ./semantic_sst_tool.exe tuple-matrix artifacts/tuple/recursive_tuple_match.cmt
  raw recursive tuple matrix: labels/arity/zero/nesting/types/value-pattern/whole-wildcard/whole-binding/let-variable/partial-fallback/exhaustive-fallback/guard rejected solver-delta=0

Raw application substitutions separately exercise descriptor and exact-actual
authentication. Unknown and forged descriptors, open payloads, wrong arity,
and an otherwise excluded tuple actual all reject before VIR, backend
creation, or direct Z3 work. The retained fixture has no constructible mutable
application descriptor in this public test surface.

  $ ./semantic_sst_tool.exe application-matrix artifacts/tuple/recursive_tuple_match.cmt
  raw application matrix: unknown/forged/open/wrong-arity/tuple-excluded rejected pre-VIR solver-delta=0
