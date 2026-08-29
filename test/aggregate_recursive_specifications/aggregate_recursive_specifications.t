Immutable recursive specifications use the shared finite checker; the retained
frozen-spine PFC stays on its legacy adapter.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }

The polymorphic example exercises a direct tuple match in a recursive Proof,
finite child transfer for both matched components, and an instantiated generic
Proof-call precondition.  Source and retained-CMT routes are deterministic.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/polymorphic_recursive_node_proofs.cmo fixtures/polymorphic_recursive_node_proofs.ml 2>artifacts/polymorphic_recursive_node_proofs.compile.err
  $ normalize_sst () { sed -E 's#cmt=/tmp/verocaml-source-[^/ ]+/source.cmt#cmt=<temporary-source-cmt>#' "$1" > "$2"; }
  $ for route in source cmt; do if test "$route" = source; then input=fixtures/polymorphic_recursive_node_proofs.ml; else input=artifacts/polymorphic_recursive_node_proofs.cmt; fi; for threads in 1 2; do for repeat in 1 2; do prefix="artifacts/polymorphic_recursive_node_proofs.$route.$threads.$repeat"; OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads "$threads" --timeout-ms 20000 --dump-sst "$prefix.sst" --dump-vir "$prefix.vir" >"$prefix.out" 2>&1; grep -q '^verocaml: verified-with-trusted-axioms ' "$prefix.out"; normalize_sst "$prefix.sst" "$prefix.norm.sst"; done; cmp "artifacts/polymorphic_recursive_node_proofs.$route.$threads.1.out" "artifacts/polymorphic_recursive_node_proofs.$route.$threads.2.out"; cmp "artifacts/polymorphic_recursive_node_proofs.$route.$threads.1.norm.sst" "artifacts/polymorphic_recursive_node_proofs.$route.$threads.2.norm.sst"; cmp "artifacts/polymorphic_recursive_node_proofs.$route.$threads.1.vir" "artifacts/polymorphic_recursive_node_proofs.$route.$threads.2.vir"; done; cmp "artifacts/polymorphic_recursive_node_proofs.$route.1.1.out" "artifacts/polymorphic_recursive_node_proofs.$route.2.1.out"; cmp "artifacts/polymorphic_recursive_node_proofs.$route.1.1.norm.sst" "artifacts/polymorphic_recursive_node_proofs.$route.2.1.norm.sst"; cmp "artifacts/polymorphic_recursive_node_proofs.$route.1.1.vir" "artifacts/polymorphic_recursive_node_proofs.$route.2.1.vir"; done; echo 'polymorphic-recursive-node-proofs source+cmt threads=1/2 repeats=stable verified'
  polymorphic-recursive-node-proofs source+cmt threads=1/2 repeats=stable verified
  $ sst=artifacts/polymorphic_recursive_node_proofs.cmt.1.1.sst; vir=artifacts/polymorphic_recursive_node_proofs.cmt.1.1.vir; printf 'recursive-proof tuple-values=%s recursive-calls=%s obligations=%s\n' "$(awk '/^function node_eq_symm#/{inside=1; next} inside && /^function /{exit} inside && /^        tuple : \(node/{count++} END{print count+0}' "$sst")" "$(awk '/^function node_eq_symm#/{inside=1; next} inside && /^function /{exit} inside && /proof-call node_eq_symm#.*recursive=true/{count++} END{print count+0}' "$sst")" "$(awk '/^function node_eq_symm#/{inside=1; next} inside && /^  vc /{count++} inside && /^function /{exit} END{print count+0}' "$vir")"
  recursive-proof tuple-values=1 recursive-calls=1 obligations=6

  $ for name in exact_helper exact_inline human_pfc variant_result pass_through_result rank_free_integer_result rank_free_structural_result immutable_pattern_reconstruction_recursive nondecreasing; do retained "$name"; done
  $ for name in exact_helper exact_inline human_pfc variant_result pass_through_result rank_free_integer_result rank_free_structural_result; do ./aggregate_recursive_specifications_tool.exe "artifacts/$name.cmt"; done
  status=verified functions=1 obligations=8 recursive-spec=1/2 lifecycle=0/0/0
  status=verified functions=1 obligations=8 recursive-spec=1/2 lifecycle=0/0/0
  status=verified functions=4 obligations=31 recursive-spec=5/5 lifecycle=2/2/2
  status=verified functions=1 obligations=7 recursive-spec=1/2 lifecycle=0/0/0
  status=verified functions=1 obligations=7 recursive-spec=1/2 lifecycle=0/0/0
  status=verified functions=1 obligations=5 recursive-spec=0/0 lifecycle=0/0/0
  status=verified functions=1 obligations=5 recursive-spec=0/0 lifecycle=0/0/0
  $ ./aggregate_recursive_specifications_tool.exe resource-retry artifacts/immutable_pattern_reconstruction_recursive.cmt
  resource-retry outcome=resource-exhausted retry=2/1/1 ground=1
  $ for name in rank_free_integer_result rank_free_structural_result; do ../../src/verocaml.exe verify "artifacts/$name.cmt" --timeout-ms 60000 --dump-vir "artifacts/$name.vir.1" >/dev/null && ../../src/verocaml.exe verify "artifacts/$name.cmt" --timeout-ms 60000 --dump-vir "artifacts/$name.vir.2" >/dev/null && cmp "artifacts/$name.vir.1" "artifacts/$name.vir.2"; done
  $ grep -Eo 'tag\.option<int>#[0-9]+ \(spec\.[0-9]+\.spec_index' artifacts/rank_free_integer_result.vir.1 | head -n 1 | sed -E 's/#[0-9]+ /#<internal-type-id> /'
  tag.option<int>#<internal-type-id> (spec.1.spec_index
  $ grep -Eo 'tag\.option<int>#[0-9]+ \(spec\.[0-9]+\.spec_index_alt' artifacts/rank_free_structural_result.vir.1 | head -n 1 | sed -E 's/#[0-9]+ /#<internal-type-id> /'
  tag.option<int>#<internal-type-id> (spec.0.spec_index_alt
  $ ./aggregate_recursive_specifications_tool.exe artifacts/nondecreasing.cmt
  verification failed before reporting
  [3]

The recursive proof-query consumer receives the same immutable reconstruction
fact.  Its positive verifies and the wrong-payload control remains
nonverified; repeated VIR is deterministic.

  $ ./aggregate_recursive_specifications_tool.exe reconstruction artifacts/immutable_pattern_reconstruction_recursive.cmt
  recursive-reconstruction positive=verified wrong-payload=nonverified functions=2 obligations=6
  $ for n in 1 2; do ../../src/verocaml.exe verify artifacts/immutable_pattern_reconstruction_recursive.cmt --timeout-ms 60000 --dump-vir "artifacts/immutable-reconstruction.$n.vir" >/dev/null 2>&1 || test $? = 3; done
  $ cmp artifacts/immutable-reconstruction.1.vir artifacts/immutable-reconstruction.2.vir && printf 'recursive-reconstruction-facts=%s\n' "$(grep -c '^      (= nodes.*ctor.Node' artifacts/immutable-reconstruction.1.vir)"
  recursive-reconstruction-facts=2

The exact recursive index regression consumes the shared positional aggregate
semantics through the recursive proof-query route. Its production total
retains three termination-preflight obligations in addition to 12 dumped
execution VCs, and repeated SST/VIR construction is deterministic.

  $ retained index_minimal
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/index_minimal.cmt --timeout-ms 60000 --dump-sst artifacts/index_minimal.first.sst --dump-vir artifacts/index_minimal.first.vir > artifacts/index_minimal.first.out 2>&1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/index_minimal.cmt --timeout-ms 60000 --dump-sst artifacts/index_minimal.second.sst --dump-vir artifacts/index_minimal.second.vir > artifacts/index_minimal.second.out 2>&1
  $ cat artifacts/index_minimal.first.out
  verocaml: verified file=artifacts/index_minimal.cmt functions=2 obligations=15
  $ for name in index_imp index; do awk -v name="$name" '$1=="function" { active=($2 ~ ("^" name "#")) } active && $1=="vc" { count++ } END { printf "%s=%d\n", name, count }' artifacts/index_minimal.first.vir; done
  index_imp=9
  index=3
  $ printf 'dumped-execution-vcs=%s\n' "$(grep -c '^  vc ' artifacts/index_minimal.first.vir)"
  dumped-execution-vcs=12
  $ cmp artifacts/index_minimal.first.sst artifacts/index_minimal.second.sst
  $ cmp artifacts/index_minimal.first.vir artifacts/index_minimal.second.vir

Changing only the final executable Node arm to return the wrong option remains
non-verified at the exact final postcondition under the 60,000 ms
policy. Either existing quantified-query outcome is acceptable, but success is
forbidden.

  $ sed '54s/Some value/None/' fixtures/index_minimal.ml > artifacts/index_minimal_wrong.ml
  $ test "$(grep -c '^    | Node (value, _) -> None$' artifacts/index_minimal_wrong.ml)" -eq 1
  $ if cmp -s fixtures/index_minimal.ml artifacts/index_minimal_wrong.ml; then exit 1; fi
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/index_minimal_wrong.cmo artifacts/index_minimal_wrong.ml
  $ set +e; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/index_minimal_wrong.cmt --timeout-ms 60000 > artifacts/index_minimal_wrong.out 2>&1; wrong_status=$?; set -e; case "$wrong_status" in 1|3) ;; *) cat artifacts/index_minimal_wrong.out; exit 1;; esac
  $ test "$(grep -Ec '^verocaml: (counterexample|inconclusive) function=index#5 vc=postcondition\[0\]' artifacts/index_minimal_wrong.out)" -eq 1
  $ test "$(grep -c '^verocaml: verified' artifacts/index_minimal_wrong.out)" -eq 0
  $ sed -n -E 's/^verocaml: (counterexample|inconclusive) function=index#5 vc=postcondition\[0\].*/wrong-index=\1 function=index#5 vc=postcondition[0]/p' artifacts/index_minimal_wrong.out
  wrong-index=inconclusive function=index#5 vc=postcondition[0]

Ephemeral recursive tuple matching preserves source order without creating a
tuple logical domain.  The exact frozen declaration is the fixture prefix,
and both source and retained-CMT routes use the 60,000 ms policy.

  $ retained recursive_tuple_match
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/recursive_tuple_match.ml --timeout-ms 60000 --dump-sst artifacts/tuple.source.1.sst --dump-vir artifacts/tuple.source.1.vir | sed -E 's@file=[^ ]+@file=FIXTURE@'
  verocaml: verified file=FIXTURE functions=1 obligations=15
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/recursive_tuple_match.cmt --timeout-ms 60000 --dump-sst artifacts/tuple.cmt.1.sst --dump-vir artifacts/tuple.cmt.1.vir | sed -E 's@file=[^ ]+@file=FIXTURE@'
  verocaml: verified file=FIXTURE functions=1 obligations=15
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/recursive_tuple_match.ml --timeout-ms 60000 --dump-sst artifacts/tuple.source.2.sst --dump-vir artifacts/tuple.source.2.vir >/dev/null
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/recursive_tuple_match.cmt --timeout-ms 60000 --dump-sst artifacts/tuple.cmt.2.sst --dump-vir artifacts/tuple.cmt.2.vir >/dev/null
  $ cmp artifacts/tuple.source.1.sst artifacts/tuple.source.2.sst
  $ cmp artifacts/tuple.source.1.vir artifacts/tuple.source.2.vir
  $ cmp artifacts/tuple.cmt.1.sst artifacts/tuple.cmt.2.sst
  $ cmp artifacts/tuple.cmt.1.vir artifacts/tuple.cmt.2.vir
  $ ./aggregate_recursive_specifications_tool.exe oracle artifacts/recursive_tuple_match.cmt
  tuple-oracle termination=3 matrix-termination=9 recursive=3 ites=2 pairs=Empty/Empty,Node/Node cross=0 tuple-ir=0 abi=2

The frozen function itself exercises every constructor pairing, equal and
unequal payloads, and equal and unequal recursive descent when stripped of
verification-only carriers for this runtime truth-table check.

  $ sed '6d;12,13d' fixtures/recursive_tuple_match.ml | head -n 10 > artifacts/tuple_runtime_def.ml
  $ cat > artifacts/tuple_runtime.ml <<'EOF'
  > open Tuple_runtime_def
  > let n1 = Node (1, Empty)
  > let n1b = Node (1, Empty)
  > let n2 = Node (2, Empty)
  > let deep = Node (1, Node (2, Empty))
  > let deep_same = Node (1, Node (2, Empty))
  > let deep_wrong = Node (1, Node (3, Empty))
  > let () =
  >   assert (spec_node_eq_alt Empty Empty);
  >   assert (not (spec_node_eq_alt Empty n1));
  >   assert (not (spec_node_eq_alt n1 Empty));
  >   assert (spec_node_eq_alt n1 n1b);
  >   assert (not (spec_node_eq_alt n1 n2));
  >   assert (spec_node_eq_alt deep deep_same);
  >   assert (not (spec_node_eq_alt deep deep_wrong));
  >   print_endline "tuple-runtime pairings=4 payloads=equal/unequal descent=equal/unequal"
  > EOF
  $ ocamlc -I artifacts artifacts/tuple_runtime_def.ml artifacts/tuple_runtime.ml -o artifacts/tuple_runtime.exe
  $ artifacts/tuple_runtime.exe
  tuple-runtime pairings=4 payloads=equal/unequal descent=equal/unequal

Changing only the exact scalar Empty/Empty branch remains a complete false
theorem.  The native datatype query returns its counterexample before either
ground fallback, while the CLI exposes the exact local assertion and empty
projection.

  $ sed '8s/true/false/' fixtures/recursive_tuple_match.ml > artifacts/recursive_tuple_match_wrong.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/recursive_tuple_match_wrong.cmo artifacts/recursive_tuple_match_wrong.ml
  $ ./aggregate_recursive_specifications_tool.exe ground artifacts/recursive_tuple_match_wrong.cmt
  status=counterexample functions=1 obligations=15 recursive-spec=0/0 lifecycle=0/0/0
  ground attempts=0 complete=0 abstentions=0 antecedents=0
  $ if OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/recursive_tuple_match_wrong.cmt > artifacts/recursive_tuple_match_wrong.out 2>&1; then false; else test $? = 1; fi
  $ grep -E '^verocaml: counterexample function=check_empty_pair#5 vc=local-assertion\[0\] span=.* result=counterexample$' artifacts/recursive_tuple_match_wrong.out | sed -E 's/span=[^ ]+/span=<source>/'
  verocaml: counterexample function=check_empty_pair#5 vc=local-assertion[0] span=<source> result=counterexample
  $ grep -Fx '  model: (no projected bindings)' artifacts/recursive_tuple_match_wrong.out
    model: (no projected bindings)

A partial tuple ground shape remains unsupported: only the first aggregate
component has a routed nullary constructor.  The tuple ground evaluator and
the independently bounded nullary retry both abstain without constructing a
query, solver, or backend context.

  $ head -n 49 fixtures/recursive_tuple_match.ml > artifacts/recursive_tuple_match_partial_ground.ml
  $ cat >> artifacts/recursive_tuple_match_partial_ground.ml <<'EOF'
  > let check_partial_pair
  >     (a : int node [@finite])
  >     (b : int node [@finite]) : unit =
  >   [%verocaml.requires spec_is_empty a];
  >   [%verocaml.reveal_with_fuel (spec_node_eq_alt, 1)];
  >   match a with
  >   | Empty -> [%verocaml.assert spec_node_eq_alt a b]
  >   | Node _ -> [%verocaml.assert false]
  > [@@verocaml.proof]
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/recursive_tuple_match_partial_ground.cmo artifacts/recursive_tuple_match_partial_ground.ml
  $ ./aggregate_recursive_specifications_tool.exe tuple-abstain artifacts/recursive_tuple_match_partial_ground.cmt
  tuple-abstain ground=1/0/1 nullary=1/0/1 solver-delta=0

Termination remains tied to the selected decrease component.  The original
value, an unmeasured component child, and reconstruction all reject without
solver or A2 work.

  $ head -n 13 fixtures/recursive_tuple_match.ml > artifacts/tuple_minimal.ml
  $ sed '10s/na nb/a nb/' artifacts/tuple_minimal.ml > artifacts/tuple_original.ml
  $ sed '10s/na nb/nb nb/' artifacts/tuple_minimal.ml > artifacts/tuple_unmeasured.ml
  $ sed '10s/na nb/(Node (va, na)) nb/' artifacts/tuple_minimal.ml > artifacts/tuple_reconstructed.ml
  $ for name in tuple_original tuple_unmeasured tuple_reconstructed; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "artifacts/$name.ml"; ./aggregate_recursive_specifications_tool.exe reject "artifacts/$name.cmt"; done
  tuple-rejected solvers=0 helper-expansion=0 a2=0
  tuple-rejected solvers=0 helper-expansion=0 a2=0
  tuple-rejected solvers=0 helper-expansion=0 a2=0

The bounded source rejection matrix keeps tuple parameters, results,
decreases, equality, tuple-let variables, whole-tuple bindings, or-patterns,
unsupported leaves, and aggregate-result tuple matching outside admission.

  $ cat > artifacts/tuple_parameter.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec bad (pair : int node * int node) : bool =
  >   [%verocaml.decreases 0]; true
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ cat > artifacts/tuple_result.ml <<'EOF'
  > let rec bad (n : int) : int * bool =
  >   [%verocaml.decreases n]; (n, true)
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ cat > artifacts/tuple_decreases.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec bad (a : int node) (b : int node) : bool =
  >   [%verocaml.decreases (a, b)]; true
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ cat > artifacts/tuple_equality.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec bad (a : int node) (b : int node) : bool =
  >   [%verocaml.decreases a]; (a, b) = (a, b)
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ cat > artifacts/tuple_variable.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec bad (a : int node) (b : int node) : bool =
  >   [%verocaml.decreases a];
  >   let pair = (a, b) in
  >   match pair with (Empty, Empty) -> true | _ -> false
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ cat > artifacts/whole_binding.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec bad (a : int node) (b : int node) : bool =
  >   [%verocaml.decreases a]; match a, b with pair -> true
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ cat > artifacts/or_pattern.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec bad (a : int node) (b : int node) : bool =
  >   [%verocaml.decreases a];
  >   match a, b with
  >   | (Empty, Empty) | (Node _, Node _) -> true
  >   | _, _ -> false
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ cat > artifacts/unit_leaf.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec bad (a : int node) : bool =
  >   [%verocaml.decreases a];
  >   match a, () with (Empty, ()) -> true | (Node _, _) -> false
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ for name in tuple_parameter tuple_result tuple_decreases tuple_equality tuple_variable whole_binding or_pattern unit_leaf; do if OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.ml" --timeout-ms 60000 >"artifacts/$name.out" 2>&1; then echo "$name=unexpected-success"; exit 1; else echo "$name=rejected"; fi; done
  tuple_parameter=rejected
  tuple_result=rejected
  tuple_decreases=rejected
  tuple_equality=rejected
  tuple_variable=rejected
  whole_binding=rejected
  or_pattern=rejected
  unit_leaf=rejected
  $ cat > artifacts/aggregate_result.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec bad (a : int node) (b : int node) : int node =
  >   [%verocaml.decreases a];
  >   match a, b with
  >   | Empty, Empty -> Empty
  >   | Node (_, na), Node (_, nb) -> bad na nb
  >   | _, _ -> Empty
  > [@@verocaml.spec] [@@verocaml.revealed]
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/aggregate_result.cmo artifacts/aggregate_result.ml
  $ ./aggregate_recursive_specifications_tool.exe reject-admission artifacts/aggregate_result.cmt
  tuple-rejected admission=true solvers=0 helper-expansion=0 recursive-lowering=0 a2=0
