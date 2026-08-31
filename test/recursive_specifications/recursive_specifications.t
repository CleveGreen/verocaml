The supplied recurse definition is retained with an authenticated recursive
spec carrier. Ordinary compilation erases the definition and every reveal.

  $ mkdir artifacts
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/recurse.ml > artifacts/ordinary.out 2> artifacts/ordinary.source
  $ test ! -s artifacts/ordinary.out
  $ test $(grep -c 'Vero_ghost\|recurse\|reveal' artifacts/ordinary.source) -eq 0

  $ retained_compile () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained_compile recurse
  $ retained_compile opaque_scopes
  $ retained_compile non_strict
  $ ./recursive_specifications_tool.exe unit artifacts/recurse.cmt artifacts/opaque_scopes.cmt artifacts/non_strict.cmt
  recursive specs: authenticated scalar totality, opacity, depths, scopes, exact A1/A2/A3, and zero-solver failures

Earlier direct-source nonrecursive Spec helpers are authenticated as one
strictly ordered closure and expanded hygienically before termination and A2.
The exact aggregate example, scalar/transitive branch helpers, shadowing, and
an integer self-actual preserve edge counts and the A1/A2/A3 contract.

  $ for n in recursive_with_nonrecursive_helper recursive_helper_scalar recursive_helper_hygienic recursive_helper_integer_actual recursive_helper_non_strict recursive_helper_in_decreases recursive_helper_aggregate_result recursive_helper_tuple recursive_helper_record recursive_helper_unit recursive_helper_only_aggregate recursive_helper_distinct_recursive recursive_helper_invariant recursive_helper_labelled recursive_helper_variant_construction recursive_helper_mutable recursive_helper_exec recursive_helper_external recursive_helper_trusted recursive_helper_unsupported_integer_if recursive_helper_higher_order recursive_helper_partial recursive_helper_model; do retained_compile "$n"; done
  $ ./recursive_specifications_tool.exe helper-unit artifacts/recursive_with_nonrecursive_helper.cmt artifacts/recursive_helper_scalar.cmt artifacts/recursive_helper_hygienic.cmt artifacts/recursive_helper_integer_actual.cmt artifacts/recursive_helper_non_strict.cmt
  recursive Spec helpers: authenticated closure, hygienic expansion, termination parity, and A2 semantics
  $ ./recursive_specifications_tool.exe helper-forgery artifacts/recursive_with_nonrecursive_helper.cmt
  copied recursive-helper certificate rejected before expansion
  $ ./recursive_specifications_tool.exe helper-raw-forgery artifacts/recursive_helper_scalar.cmt
  raw helper copy rejected before expansion and solver
  $ ./recursive_specifications_tool.exe helper-partial-adversary artifacts/recursive_helper_scalar.cmt
  partial helper call rejected before expansion and solver
  $ ./recursive_specifications_tool.exe helper-graph-adversary artifacts/recursive_helper_scalar.cmt
  reordered helper closure copy rejected by private graph validation
  $ for n in recursive_helper_in_decreases recursive_helper_aggregate_result recursive_helper_tuple recursive_helper_record recursive_helper_unit recursive_helper_only_aggregate recursive_helper_distinct_recursive recursive_helper_invariant recursive_helper_labelled recursive_helper_variant_construction recursive_helper_mutable recursive_helper_exec recursive_helper_external recursive_helper_trusted recursive_helper_unsupported_integer_if recursive_helper_higher_order recursive_helper_partial recursive_helper_model; do ./recursive_specifications_tool.exe helper-reject "artifacts/$n.cmt"; done
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  helper rejected before expansion and solver
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/recursive_helper_cycle.cmo fixtures/recursive_helper_cycle.ml 2>&1 | grep -F '[@verocaml.spec] requires a single top-level binding'
  Error: [@verocaml.spec] requires a single top-level binding

Imported direct and transitive helper bodies remain dependency descriptors,
not local recursive-helper authority.

  $ mkdir -p artifacts/helper-import
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/helper-import/recursive_helper_import_provider.cmo fixtures/recursive_helper_import_provider.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/helper-import -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/helper-import/recursive_helper_import_consumer.cmo fixtures/recursive_helper_import_consumer.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/helper-import -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/helper-import/recursive_helper_transitive_import_consumer.cmo fixtures/recursive_helper_transitive_import_consumer.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/helper-import -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/helper-import/recursive_helper_foreign_aggregate.cmo fixtures/recursive_helper_foreign_aggregate.ml
  $ for n in recursive_helper_import_consumer recursive_helper_transitive_import_consumer; do OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/helper-import/$n.cmt" --dependency artifacts/helper-import/recursive_helper_import_provider.cmt --timeout-ms 60000 > "artifacts/helper-import/$n.out" 2>&1; test $? -ne 0; grep -E 'imported|direct-source authority' "artifacts/helper-import/$n.out" >/dev/null; done
  $ for n in recursive_helper_import_consumer recursive_helper_transitive_import_consumer recursive_helper_foreign_aggregate; do ./recursive_specifications_tool.exe helper-import-reject artifacts/helper-import/recursive_helper_import_provider.cmt "artifacts/helper-import/$n.cmt"; done
  imported helper rejected before expansion and solver
  imported helper rejected before expansion and solver
  imported helper rejected before expansion and solver

The source literal is captured before host narrowing. A nonliteral never
reaches CMT, semantic preparation, or either solver.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nonliteral.cmo fixtures/nonliteral.ml 2>&1 | grep 'depth must be an unsuffixed integer literal'
  Error: %verocaml.reveal_with_fuel depth must be an unsuffixed integer literal
  $ retained_reject () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; ./recursive_specifications_tool.exe reject "artifacts/$n.cmt"; }
  $ retained_reject negative
  adapter rejected before solver
  $ retained_reject too_deep
  adapter rejected before solver
  $ retained_reject overflow
  adapter rejected before solver

External and mutual source shapes remain specialist admission boundaries.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/external.cmo fixtures/external.ml
  $ ./recursive_specifications_tool.exe reject artifacts/external.cmt
  adapter rejected before solver
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/mutual.cmo fixtures/mutual.ml 2>&1 | grep 'single top-level binding'
  Error: [@verocaml.spec] requires a single top-level binding

The exact A1/A2/A3 declaration, trigger, qid, and skolem rendering is pinned
through solver-neutral Logic IR. A3 has one mandatory two-term multi-pattern.

  $ ./recursive_specifications_tool.exe render artifacts/recurse.cmt
  sort:0:Fuel
  fun:1:fuel.zero:()->Fuel
  fun:2:fuel.succ:(Fuel)->Fuel
  fun:3:spec.0.recurse:(Int,Int)->Int
  fun:4:spec.0.recurse$fuel:(Int,Int,Fuel)->Int
  fun:5:spec.0.recurse$enabled:(Fuel)->Bool
  ---
  (declare-sort Fuel 0)
  (declare-fun spec.0.recurse$fuel (Int Int Fuel) Int)
  (declare-fun fuel.zero () Fuel)
  (declare-fun fuel.succ (Fuel) Fuel)
  (declare-fun spec.0.recurse$enabled (Fuel) Bool)
  (declare-fun spec.0.recurse (Int Int) Int)
  (assert (forall ((verocaml_b0_x.a1 Int)
           (verocaml_b1_y.a1 Int)
           (verocaml_b2_fuel.a1 Fuel))
    (! (= (spec.0.recurse$fuel
            verocaml_b0_x.a1
            verocaml_b1_y.a1
            verocaml_b2_fuel.a1)
          (spec.0.recurse$fuel verocaml_b0_x.a1 verocaml_b1_y.a1 fuel.zero))
       :pattern ((spec.0.recurse$fuel
                   verocaml_b0_x.a1
                   verocaml_b1_y.a1
                   verocaml_b2_fuel.a1))
       :skolemid spec.0.recurse.fuel-invariance.skolem
       :qid spec.0.recurse.fuel-invariance)))
  (assert (forall ((verocaml_b3_x.a2 Int)
           (verocaml_b4_y.a2 Int)
           (verocaml_b5_fuel.a2 Fuel))
    (! (= (spec.0.recurse$fuel
            verocaml_b3_x.a2
            verocaml_b4_y.a2
            (fuel.succ verocaml_b5_fuel.a2))
          (ite (<= verocaml_b4_y.a2 0)
               verocaml_b3_x.a2
               (spec.0.recurse$fuel
                 (+ verocaml_b3_x.a2 1)
                 (- verocaml_b4_y.a2 1)
                 verocaml_b5_fuel.a2)))
       :pattern ((spec.0.recurse$fuel
                   verocaml_b3_x.a2
                   verocaml_b4_y.a2
                   (fuel.succ verocaml_b5_fuel.a2)))
       :skolemid spec.0.recurse.fuel-body.skolem
       :qid spec.0.recurse.fuel-body)))
  (assert (forall ((verocaml_b6_x.a3 Int)
           (verocaml_b7_y.a3 Int)
           (verocaml_b8_fuel.a3 Fuel))
    (! (=> (spec.0.recurse$enabled verocaml_b8_fuel.a3)
           (= (spec.0.recurse verocaml_b6_x.a3 verocaml_b7_y.a3)
              (spec.0.recurse$fuel
                verocaml_b6_x.a3
                verocaml_b7_y.a3
                verocaml_b8_fuel.a3)))
       :pattern ((spec.0.recurse verocaml_b6_x.a3 verocaml_b7_y.a3)
                 (spec.0.recurse$enabled verocaml_b8_fuel.a3))
       :skolemid spec.0.recurse.public-link.skolem
       :qid spec.0.recurse.public-link)))

The installed public SST tag is descriptive rather than authority. An
otherwise valid recursive SST forged by an installed client is rejected by
semantic validation, and the same payload cannot reach either production
solver.

  $ install_root="$(cat verocaml-install-root)"
  $ core="$install_root/lib/verocaml/core"
  $ private_flags=""; for directory in $(find "$install_root/lib/verocaml" -type d -name .private); do private_flags="$private_flags -I $directory"; done
  $ ocamlfind ocamlc -custom -package smtml,zarith,compiler-libs.common,delator -linkpkg -I "$core" $private_flags "$core/verocaml_core.cma" fixtures/installed_recursive_spec_forgery.ml -o artifacts/installed_recursive_spec_forgery.exe 2>/dev/null
  $ ./artifacts/installed_recursive_spec_forgery.exe artifacts/forged.sst
  installed recursive SST tag rejected before solver
  $ ./recursive_specifications_tool.exe forge artifacts/forged.sst
  installed recursive SST forgery rejected before both solvers

Canonical generic list/tree schemas admit exact applications without cloned
definitions, constructor matching, structural child descent, recursive
Spec/Proof bodies, and typed mathematical equality. The VIR pins one exact
rank domain and the labelled entry/current obligations.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/structural_positive.ml --timeout-ms 60000 --dump-vir artifacts/structural.vir >/dev/null
  $ grep -E 'component (chain<int>|chain<bool>|tree<int>)|positive-child-smaller' artifacts/structural.vir
    component chain<int>#0
    positive-child-smaller constructor=chain#0.Link#1 field=$arg1#1 child=chain<int>#0
    component tree<int>#1
    positive-child-smaller constructor=tree#1.Node#1 field=$arg0#0 child=tree<int>#1
    positive-child-smaller constructor=tree#1.Node#1 field=$arg1#1 child=tree<int>#1
    component chain<bool>#0
    positive-child-smaller constructor=chain#0.Link#1 field=$arg1#1 child=chain<bool>#0
  $ retained_compile structural_positive
  $ ./recursive_specifications_tool.exe render-rank artifacts/structural_positive.cmt > artifacts/rank.smt
  $ grep -c ':qid verocaml.rank' artifacts/rank.smt
  4
  $ grep -c 'declare-datatypes' artifacts/rank.smt
  1
  $ grep -c '(_ is ' artifacts/rank.smt
  4
  $ grep -c 'verocaml_tag' artifacts/rank.smt || true
  0
  $ grep ':qid verocaml.rank.*child' artifacts/rank.smt | sed -E 's/rank\.[0-9a-f]{8}/rank.DIGEST/'
       :qid verocaml.rank.DIGEST.t0.c1.f1.1.child)))
  $ ./recursive_specifications_tool.exe render artifacts/structural_positive.cmt > artifacts/structural.logic
  $ grep -c ':qid spec.0.length.' artifacts/structural.logic
  3
  $ grep -c 'declare-datatypes' artifacts/structural.logic
  1
  $ grep -c 'verocaml_tag' artifacts/structural.logic || true
  0
  $ grep -c ':qid verocaml.datatype.' artifacts/structural.logic || true
  0

Multiple/lexicographic measures, references, mutable/function-bearing
instances, a cross-domain selection, and a non-child/nondecreasing recursive
edge are rejected before either solver.

  $ for n in structural_nondecrease structural_cross_domain structural_multiple structural_lexicographic structural_reference structural_function structural_mutable; do retained_compile "$n"; ./recursive_specifications_tool.exe reject "artifacts/$n.cmt"; done
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver

Every concrete actual of a generic rank carrier is checked recursively before
instance/domain issuance. Mutable and cyclic/groundless local carriers, tuples,
functions, references, and foreign/imported types all reject from real retained
CMT input with zero solver creations.

  $ for n in structural_actual_mutable structural_actual_cyclic structural_actual_tuple structural_actual_function structural_actual_reference structural_actual_foreign; do retained_compile "$n"; ./recursive_specifications_tool.exe reject "artifacts/$n.cmt"; done
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver
  adapter rejected before solver
