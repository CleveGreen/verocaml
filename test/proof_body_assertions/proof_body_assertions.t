Proof-body assertions use the retained proof-region sidecar and exact slot
mapper, but lower to their own authenticated Proof-stage SST statement and
named VIR VC.

  $ mkdir -p artifacts/import
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ for name in empty_node_len empty_node_wrong empty_node_false empty_node_false_premise non_nullary_ground_abstains nullary_recursive_symbolic_positive nullary_recursive_symbolic_wrong nullary_recursive_symbolic_no_reveal nullary_recursive_symbolic_unsupported immutable_pattern_reconstruction positive_matrix proof_branch_fact_postcondition one_predecessor_negative recursive_summary recursive_call_removed simple_instance reveal_removed_negative reveal_after_negative reveal_sibling_negative reveal_other_callable_negative revealed_default_negative effectful_rejected raw_carrier_rejected create_stack_with region_proof_call exec_region_mutants direct_exec_reveal_rejected nested_exec_region_rejected value_exec_region_rejected non_boolean_exec_region_rejected exec_call_region_rejected effectful_exec_region_rejected nonrecursive_spec_path_product; do retained "$name"; done
  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" fixtures/simple_instance.ml >/dev/null 2>artifacts/simple.retained
  $ grep -q 'verocaml:proof-region-capture:1:issuer=ppx-v1.*kind=local-assert' artifacts/simple.retained && grep -q 'slots=78,.*;79,' artifacts/simple.retained && grep -q 'Vero_ghost.assert_' artifacts/simple.retained && echo 'local assertion reuses the one retained sidecar/slot family'
  local assertion reuses the one retained sidecar/slot family

The frozen source is byte-identical and all five VCs verify.  The formerly
failing matched postcondition, direct assertion, and admitted-call
assume-then-assert are identified independently.

  $ sha256sum fixtures/immutable_pattern_reconstruction.ml | cut -d' ' -f1
  ee0d1dc59dfe57c46ef117b868af8c403866bea824749d8496ec41094b41b52e
  $ ./proof_body_assertions_tool.exe reconstruction artifacts/immutable_pattern_reconstruction.cmt
  reconstruction status=verified functions=4 obligations=5 verified=5 branch-postconditions=2 direct-assert=1 assume-then-assert=1
  $ for n in 1 2; do ../../src/verocaml.exe verify fixtures/immutable_pattern_reconstruction.ml --timeout-ms 5000 --dump-sst "artifacts/reconstruction-source.$n.sst" --dump-vir "artifacts/reconstruction-source.$n.vir" >/dev/null 2>&1; done
  $ for n in 1 2; do ../../src/verocaml.exe verify artifacts/immutable_pattern_reconstruction.cmt --timeout-ms 5000 --dump-sst "artifacts/reconstruction-cmt.$n.sst" --dump-vir "artifacts/reconstruction-cmt.$n.vir" >/dev/null 2>&1; done
  $ cmp artifacts/reconstruction-source.1.sst artifacts/reconstruction-source.2.sst && cmp artifacts/reconstruction-source.1.vir artifacts/reconstruction-source.2.vir && cmp artifacts/reconstruction-cmt.1.sst artifacts/reconstruction-cmt.2.sst && cmp artifacts/reconstruction-cmt.1.vir artifacts/reconstruction-cmt.2.vir && echo 'reconstruction source and CMT deterministic'
  reconstruction source and CMT deterministic

Constructor allocation without an `ensures`, aliases, guarded and nested
constructors, recursive ADTs, a generic local alias, and complete/subset
records verify.  The subset record reconstruction names every canonical field.
Wrong constructor, payload, nested payload, and field controls remain
nonverified.

  $ cat > artifacts/immutable_pattern_matrix.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > type tree = Tip of int | Branch of tree * tree
  > type row = { first : int; second : int; tail : int node }
  > let allocation_without_ensures (value : int) (rest : int node) =
  >   let built = Node (value, rest) in
  >   [%verocaml.assert built = Node (value, rest)]
  > [@@verocaml.proof]
  > let alias_positive (nodes : int node) =
  >   let alias = nodes in
  >   match alias with
  >   | Empty -> ()
  >   | Node (value, rest) -> [%verocaml.assert Node (value, rest) = nodes]
  > [@@verocaml.proof]
  > let guard_positive (nodes : int node) =
  >   match nodes with
  >   | Node (value, rest) when value = value ->
  >       [%verocaml.assert Node (value, rest) = nodes]
  >   | Empty -> ()
  >   | Node _ -> ()
  > [@@verocaml.proof]
  > let nested_positive (tree : tree) =
  >   match tree with
  >   | Branch (Tip value, rest) ->
  >       [%verocaml.assert Branch (Tip value, rest) = tree]
  >   | Tip _ -> ()
  >   | Branch _ -> ()
  > [@@verocaml.proof]
  > let generic_alias_positive (nodes : int node) =
  >   let local_alias = nodes in
  >   match local_alias with
  >   | Empty -> ()
  >   | Node (value, rest) -> [%verocaml.assert Node (value, rest) = nodes]
  > [@@verocaml.proof]
  > let complete_record_positive (row : row) =
  >   match row with
  >   | { first; second; tail } ->
  >       [%verocaml.assert { first; second; tail } = row]
  > [@@verocaml.proof]
  > let subset_record_positive (row : row) =
  >   match row with
  >   | { first } ->
  >       [%verocaml.assert
  >         { first; second = row.second; tail = row.tail } = row]
  >   | _ -> ()
  > [@@verocaml.proof]
  > let wrong_constructor (nodes : int node) =
  >   match nodes with
  >   | Empty -> [%verocaml.assert Node (0, Empty) = nodes]
  >   | Node _ -> ()
  > [@@verocaml.proof]
  > let wrong_payload (nodes : int node) =
  >   match nodes with
  >   | Empty -> ()
  >   | Node (value, rest) ->
  >       [%verocaml.assert Node (value + 1, rest) = nodes]
  > [@@verocaml.proof]
  > let wrong_nested_payload (tree : tree) =
  >   match tree with
  >   | Branch (Tip value, rest) ->
  >       [%verocaml.assert Branch (Tip (value + 1), rest) = tree]
  >   | Tip _ -> ()
  >   | Branch _ -> ()
  > [@@verocaml.proof]
  > let wrong_field (row : row) =
  >   match row with
  >   | { first; second; tail } ->
  >       [%verocaml.assert { first = first + 1; second; tail } = row]
  > [@@verocaml.proof]
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/immutable_pattern_matrix.cmo artifacts/immutable_pattern_matrix.ml
  $ ./proof_body_assertions_tool.exe reconstruction-matrix artifacts/immutable_pattern_matrix.cmt
  reconstruction-matrix positives=7/7 negatives=wrong-constructor:counterexample,wrong-payload:counterexample,wrong-nested:counterexample,wrong-field:counterexample
  $ ../../src/verocaml.exe verify artifacts/immutable_pattern_matrix.cmt --timeout-ms 5000 --dump-vir artifacts/immutable-pattern-matrix.vir >/dev/null 2>&1; printf 'allocation-equalities=%s subset-complete-layout=%s nested-reconstructions=%s\n' "$(sed -n '/function allocation_without_ensures/,/^  exit 0/p' artifacts/immutable-pattern-matrix.vir | grep -c 'ctor.Node')" "$(sed -n '/function subset_record_positive/,/^  exit 0/p' artifacts/immutable-pattern-matrix.vir | grep -c 'record.row.*first=.*second=.*tail=')" "$(sed -n '/function nested_positive/,/function generic_alias_positive/p' artifacts/immutable-pattern-matrix.vir | grep -c '^      (= .*ctor.')"
  allocation-equalities=2 subset-complete-layout=2 nested-reconstructions=4

An authenticated owned abstraction and recursive Spec in the same program do
not suppress an unrelated deeply immutable match.  The owned type remains
ineligible, the ordinary construction fact occurs exactly once, and wrong
value and payload controls remain counterexamples.

  $ cat > artifacts/immutable_reconstruction_cohabitation.ml <<'EOF'
  > type snapshot = End | More of int * snapshot
  > type local_node = LEmpty | LNode of int * local_node
  > module type STACK = sig
  >   type t
  >   val model : t @ read -> snapshot @ immutable
  >   val singleton : int -> t @ unique
  >   val keep : t @ unique -> t @ unique
  >   val length : t @ read -> int
  > end
  > module Stack : STACK = struct
  >   type node = Empty | Node of { mutable value : int; mutable next : node }
  >   type t = { mutable top : node; mutable length : int }
  >   let rec contents_node (node : node) : snapshot =
  >     [%verocaml.decreases node];
  >     match node with
  >     | Empty -> End
  >     | Node { value; next } -> More (value, contents_node next)
  >   [@@verocaml.spec] [@@verocaml.opaque]
  >   let model (stack : t @ read) : snapshot @ immutable =
  >     contents_node stack.top
  >   [@@verocaml.spec]
  >   let singleton value : t @ unique =
  >     [%verocaml.ensures fun result -> model result = More (value, End)];
  >     { top = Node { value; next = Empty }; length = 1 }
  >   let keep (stack : t @ unique) : t @ unique = stack
  >   let length (stack : t @ read) = stack.length
  > end
  > let unrelated_immutable_reconstruction (nodes : local_node) =
  >   match nodes with
  >   | LEmpty -> ()
  >   | LNode (value, rest) ->
  >       [%verocaml.assert LNode (value, rest) = nodes]
  > [@@verocaml.proof]
  > let unrelated_construction_exactly_once value rest =
  >   let built = LNode (value, rest) in
  >   [%verocaml.assert built = built]
  > [@@verocaml.proof]
  > let wrong_value (nodes : local_node) (other : local_node) =
  >   match nodes with
  >   | LEmpty -> ()
  >   | LNode (value, rest) ->
  >       [%verocaml.assert LNode (value, rest) = other]
  > [@@verocaml.proof]
  > let wrong_payload (nodes : local_node) =
  >   match nodes with
  >   | LEmpty -> ()
  >   | LNode (value, rest) ->
  >       [%verocaml.assert LNode (value + 1, rest) = nodes]
  > [@@verocaml.proof]
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/immutable_reconstruction_cohabitation.cmo artifacts/immutable_reconstruction_cohabitation.ml
  $ ./proof_body_assertions_tool.exe reconstruction-cohabitation artifacts/immutable_reconstruction_cohabitation.cmt
  reconstruction-cohabitation owned-immutable-eligible=false recursive-spec=true unrelated-match=verified construction-facts=1 reconstruction-facts=2 wrong-value=counterexample wrong-payload=counterexample

An authenticated statement-position Exec proof region can now issue the same
named local assertion VC.  The exact stack constructor routes its one-layer
reveal only to that local VC, exports the proved Boolean fact to the later
call precondition, and leaves the later Exec obligation outside the region
batch.

  $ ./proof_body_assertions_tool.exe region-observe artifacts/create_stack_with.cmt | sed -E 's/exec-region:[0-9a-f]+/exec-region:DIGEST/'
  create-stack local=verified later=verified fact-exported=true local-routes=1 later-routes=0
  create-stack route issued scope=exec-region:DIGEST callable=create_stack_with#3 final=0 provisional=0 kind=local-assertion:0 path=d41d8cd98f00b204e9800998ecf8427e activations=[spec_node_len#0:1]
  create-stack-live baseline=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0 delta=static:7/sst:7/local:8/8/export:8/route:8/recursive:1/7/backend:5/z3:15/15
  counters static=7 sst=7 reached-issued=8 reached-consumed=8 proof-visits=0 proof-summaries=0 recursive-query=7 solver=5
  $ ./proof_body_assertions_tool.exe region-suppressed artifacts/create_stack_with.cmt
  suppressed create-stack later=counterexample

Removing the assertion or its outgoing fact restores the later named
precondition failure.  Reveal-only, later/sibling reveal, later-block,
wrong-predicate, and one-branch-only mutants cannot verify the corresponding
local or continuation VC.

  $ ./proof_body_assertions_tool.exe region-mutants artifacts/exec_region_mutants.cmt
  mutant=assertion_removed failed=true outcomes=counterexample
  mutant=reveal_only failed=true outcomes=counterexample
  mutant=reveal_after failed=true outcomes=counterexample
  mutant=wrong_predicate failed=true outcomes=unknown
  mutant=sibling_reveal failed=true outcomes=counterexample
  mutant=later_block failed=true outcomes=counterexample
  mutant=one_branch_export failed=true outcomes=counterexample

The shared activation finalizer/batch rejects missing snapshots, missing or
duplicate members, paired member/manifest omission, foreign batches,
path/scope mismatches, and an attempted outer-Exec member.  Each production
run snapshots immediately before the scoped finalizer, then asserts zero
route-attributable issuance/export/routing/recursive/backend/direct-Z3 delta.

  $ for attack in missing-snapshot missing paired-missing duplicate foreign stale path-mismatch scope-mismatch duplicate-member outer-member; do ./proof_body_assertions_tool.exe region-attack "$attack" artifacts/create_stack_with.cmt | sed -E 's/rejected=.* reached-issued=/rejected=<private-boundary> reached-issued=/'; done
  region-attack=missing-snapshot rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=1 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-missing-snapshot baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=missing rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-missing baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=paired-missing rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-paired-missing baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=duplicate rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-duplicate baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=foreign rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-foreign baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=stale rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-stale baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=path-mismatch rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-path-mismatch baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=scope-mismatch rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-scope-mismatch baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=duplicate-member rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-duplicate-member baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  region-attack=outer-member rejected=<private-boundary> reached-issued=1 reached-consumed=1 activation-events=2 recursive-query=2 solver=0 z3-context=5 z3-solver=5
  region-attack-outer-member baseline=static:7/sst:7/local:1/1/export:1/route:0/recursive:1/2/backend:0/z3:5/5 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0

The exact human empty-node shape produces two static assertions and four
reached VCs because the finite aggregate preflight keeps two concrete
predecessor families. One unreachable Node predecessor already contains
Boolean false from its contradictory entry path; the emitter does not add a
new assumption before snapshotting. Every VC has one issued/consumed
activation manifest.

  $ ./proof_body_assertions_tool.exe solve artifacts/empty_node_len.cmt > artifacts/empty.solve
  $ head -1 artifacts/empty.solve
  status=verified functions=1 obligations=7
  $ printf 'local-vcs=%s ordinal-0=%s ordinal-1=%s own-goal-leaks=%s activation-issued=%s activation-consumed=%s\n' "$(grep -c '^function=.* vc=local-assertion' artifacts/empty.solve)" "$(grep -Fc 'vc=local-assertion[0]' artifacts/empty.solve)" "$(grep -Fc 'vc=local-assertion[1]' artifacts/empty.solve)" "$(grep -c 'goal-in-assumptions=true' artifacts/empty.solve)" "$(grep -c '^activation issued' artifacts/empty.solve)" "$(grep -c '^activation consumed' artifacts/empty.solve)"
  local-vcs=2 ordinal-0=1 ordinal-1=1 own-goal-leaks=0 activation-issued=2 activation-consumed=2
  $ tail -1 artifacts/empty.solve
  counters static=2 sst=2 reached-issued=2 reached-consumed=2 proof-visits=0 proof-summaries=0 recursive-query=3 solver=0
  $ printf 'path-digests=%s activation-vectors=%s\n' "$(grep '^activation issued' artifacts/empty.solve | sed -E 's/.* path=([^ ]+) .*/\1/' | sort -u | wc -l)" "$(grep '^activation issued' artifacts/empty.solve | sed -E 's/.* activations=(.*)$/\1/' | sort -u | wc -l)"
  path-digests=2 activation-vectors=1
  $ grep '^activation issued' artifacts/empty.solve | sed -E 's/.* activations=(.*)$/activations=\1/' | sort -u
  activations=[spec_node_len#0:4]

Internal nonrecursive Spec matches no longer enter the obligation path ledger.
The independent option control and recursive-wrapper regression each retain
only their actual Proof exits and exact source local-assertion ordinals. A
nested eligible nonrecursive helper remains lowered. In contrast, an opaque
recursive call hidden in a helper under an `if` abstains to two real paths.
The `match` control first authenticates the same helper branch-insensitively,
then uses it under a branch, proving that cache order cannot bypass abstention.

  $ ./proof_body_assertions_tool.exe nonrecursive-spec-path-product artifacts/nonrecursive_spec_path_product.cmt
  path-product function=lemma_opt_eq_none_trans obligations=5 headers=local-assertion[0],local-assertion[1],postcondition[0],postcondition[0],postcondition[0] internal-spec-paths=0
  path-product function=lemma_index_node_step_none obligations=5 headers=local-assertion[0],local-assertion[1],local-assertion[2],postcondition[0],postcondition[0] internal-spec-paths=0
  branch-fallback function=helper_hidden_if_falls_back obligations=2 exits=2 path-entries=2 recursive-goals=1
  branch-fallback function=mixed_cache_match_falls_back obligations=2 exits=2 path-entries=3 recursive-goals=2
  branch-fallback authority recursive-spec=8/1 finite-result=0/0/0
  $ ../../src/verocaml.exe verify artifacts/nonrecursive_spec_path_product.cmt --timeout-ms 5000 --dump-sst artifacts/path-product.first.sst --dump-vir artifacts/path-product.first.vir >/dev/null
  $ ../../src/verocaml.exe verify artifacts/nonrecursive_spec_path_product.cmt --timeout-ms 5000 --dump-sst artifacts/path-product.second.sst --dump-vir artifacts/path-product.second.vir >/dev/null
  $ cmp artifacts/path-product.first.sst artifacts/path-product.second.sst
  $ cmp artifacts/path-product.first.vir artifacts/path-product.second.vir

  $ ../../src/verocaml.exe verify artifacts/positive_matrix.cmt --dump-sst artifacts/positive.sst --dump-vir artifacts/positive.vir >/dev/null
  $ grep 'local-assert ordinal=' artifacts/positive.sst | sed -E 's/.*local-assert ordinal=([0-9]+).*/local-assert ordinal=\1/'
  local-assert ordinal=0
  local-assert ordinal=0
  local-assert ordinal=0
  local-assert ordinal=1
  local-assert ordinal=2
  local-assert ordinal=0
  $ grep 'vc .*local-assertion' artifacts/positive.vir | sed -E 's/.*vc ([0-9]+) local-assertion ordinal=([0-9]+).*/vc=\1 ordinal=\2/'
  vc=0 ordinal=0
  vc=0 ordinal=0
  vc=0 ordinal=0
  vc=1 ordinal=1
  vc=2 ordinal=2
  vc=3 ordinal=2
  vc=0 ordinal=0

The ordinary quantified recursive query remains unchanged. Only after that
query is inconclusive, the exact finite Empty match carried by the existing
activation route can ground every antecedent and the goal. It reports a
counterexample for the wrong length, never verification for the correct
length, and abstains when a concrete premise is false or the reached
constructor has symbolic fields.

  $ ./proof_body_assertions_tool.exe ground-negative artifacts/empty_node_wrong.cmt
  ordinary-nonretry function=lemma_empty_node_len vc=local-assertion[0] index=0 outcome=unknown
  ordinary-nonretry ground-attempts=1 ground-complete=0 ground-abstentions=1 antecedents-checked=0
  $ ./proof_body_assertions_tool.exe ground-positive artifacts/empty_node_len.cmt
  ground-positive status=verified functions=1 obligations=7
  ground-positive ground-attempts=0 ground-complete=0 ground-abstentions=0 antecedents-checked=0
  $ ./proof_body_assertions_tool.exe ground-false-premise artifacts/empty_node_false_premise.cmt
  false-premise function=lemma_false_empty_premise vc=local-assertion[0] index=0 outcome=unknown
  false-premise ground-attempts=1 ground-complete=0 ground-abstentions=1 antecedents-checked=0
  $ ./proof_body_assertions_tool.exe ground-nonnullary artifacts/non_nullary_ground_abstains.cmt
  non-nullary function=lemma_symbolic_node_abstains vc=local-assertion[0] index=0 outcome=unknown
  non-nullary ground-attempts=1 ground-complete=0 ground-abstentions=1 antecedents-checked=0

Forged, stale, wrong-constructor, wrong-path, wrong-obligation, and wrong-
reveal witness authority rejects in the existing route finalizer. No ground
evaluation, recursive proof query, or ordinary backend solver is reached;
the three direct Z3 contexts are only the preceding authenticated recursive
totality preflight.

  $ for attack in forged stale wrong-constructor path obligation reveal; do ./proof_body_assertions_tool.exe ground-attack "$attack" artifacts/nullary_recursive_symbolic_wrong.cmt | sed -E 's/rejected=.* ground-attempts=/rejected=<private-boundary> ground-attempts=/'; done
  ground-attack=forged rejected=<private-boundary> ground-attempts=0 ground-complete=0 ground-abstentions=0 recursive-query=0 solver=0 z3-context=3 z3-solver=3
  ground-attack=stale rejected=<private-boundary> ground-attempts=0 ground-complete=0 ground-abstentions=0 recursive-query=0 solver=0 z3-context=3 z3-solver=3
  ground-attack=wrong-constructor rejected=<private-boundary> ground-attempts=0 ground-complete=0 ground-abstentions=0 recursive-query=0 solver=0 z3-context=3 z3-solver=3
  ground-attack=path rejected=<private-boundary> ground-attempts=0 ground-complete=0 ground-abstentions=0 recursive-query=0 solver=0 z3-context=3 z3-solver=3
  ground-attack=obligation rejected=<private-boundary> ground-attempts=0 ground-complete=0 ground-abstentions=0 recursive-query=0 solver=0 z3-context=3 z3-solver=3
  ground-attack=reveal rejected=<private-boundary> ground-attempts=0 ground-complete=0 ground-abstentions=0 recursive-query=0 solver=0 z3-context=3 z3-solver=3

An eligible exact nullary recursive-Spec branch gets one application-specific
retry only after the ordinary query is inconclusive.  The added result/tag
fact keeps the integer actual symbolic, retains all recursive and datatype
axioms, and verifies without invoking the counterexample-only fallback.

  $ ./proof_body_assertions_tool.exe nullary-positive artifacts/nullary_recursive_symbolic_positive.cmt
  nullary-positive status=verified primary=forced-inconclusive local=verified
  nullary-positive nullary-attempts=1 nullary-queries=1 nullary-facts=1 nullary-verified=1 nullary-counterexamples=0 nullary-inconclusives=0 nullary-abstentions=0
  nullary-positive ground-attempts=0 ground-complete=0 ground-abstentions=0 antecedents-checked=0
  retry-fact rendered=true quantifier-free=true exact-application=true constructor=Empty recognizer=Empty native-datatype=true symbolic-int=true scalar-literal=false axioms=A1/A2/A3

Retry Counterexample and Inconclusive outcomes retain the primary
Inconclusive and then leave the counterexample-only fallback unchanged.
Wrong-result and reachable-false controls never become verified.

  $ ./proof_body_assertions_tool.exe nullary-retry-counterexample artifacts/nullary_recursive_symbolic_positive.cmt
  nullary-retry-counterexample primary=unknown retry=counterexample final=unknown
  nullary-retry-counterexample nullary-attempts=1 nullary-queries=1 nullary-facts=1 nullary-verified=0 nullary-counterexamples=1 nullary-inconclusives=0 nullary-abstentions=0
  nullary-retry-counterexample ground-attempts=1 ground-complete=0 ground-abstentions=1 antecedents-checked=2
  $ ./proof_body_assertions_tool.exe nullary-retry-inconclusive artifacts/nullary_recursive_symbolic_positive.cmt
  nullary-retry-inconclusive primary=unknown retry=unknown final=unknown
  nullary-retry-inconclusive nullary-attempts=1 nullary-queries=1 nullary-facts=1 nullary-verified=0 nullary-counterexamples=0 nullary-inconclusives=1 nullary-abstentions=0
  nullary-retry-inconclusive ground-attempts=1 ground-complete=0 ground-abstentions=1 antecedents-checked=2
  $ ./proof_body_assertions_tool.exe nullary-negative artifacts/nullary_recursive_symbolic_wrong.cmt
  nullary-negative wrong-result=unknown reachable-false=counterexample symbolic-expected=unknown verified=0
  nullary-negative nullary-attempts=2 nullary-queries=2 nullary-facts=2 nullary-verified=0 nullary-counterexamples=0 nullary-inconclusives=2 nullary-abstentions=0
  nullary-negative ground-attempts=2 ground-complete=0 ground-abstentions=2 antecedents-checked=4

Removed, late, sibling, wrong-callee, zero-fuel, and synthetic-only activations
cannot authorize a retry. A revealed definition is available from proof entry.
Unsupported source, result, application, and scalar shapes abstain without
constructing one.

  $ ./proof_body_assertions_tool.exe nullary-no-reveal artifacts/nullary_recursive_symbolic_no_reveal.cmt
  nullary-no-reveal removed=0 late=0 sibling=0 wrong-callee=0 synthetic-only=0 zero-fuel=0 revealed-default=1 retries=1
  nullary-no-reveal nullary-attempts=7 nullary-queries=1 nullary-facts=1 nullary-verified=1 nullary-counterexamples=0 nullary-inconclusives=0 nullary-abstentions=6
  $ ./proof_body_assertions_tool.exe nullary-synthetic-only artifacts/nullary_recursive_symbolic_positive.cmt
  nullary-synthetic-only final=unknown attempts=1 retries=0 abstentions=1
  $ ./proof_body_assertions_tool.exe nullary-unsupported artifacts/nullary_recursive_symbolic_unsupported.cmt
  nullary-unsupported non-nullary=abstain payload=abstain record=abstain selector=abstain imported-model=abstain nested=abstain guarded=abstain multiple=abstain scalar-result=abstain scalar-recursive=abstain recursive-branch=abstain
  nullary-unsupported nullary-attempts=10 nullary-queries=0 nullary-facts=0 nullary-verified=0 nullary-counterexamples=0 nullary-inconclusives=0 nullary-abstentions=10

Application, routed constructor/reveal, and local-obligation authority attacks
reject before retry or ordinary backend creation.  Primary Verified and
Counterexample outcomes do not attempt specialization.

  $ ./proof_body_assertions_tool.exe nullary-application-attack artifacts/nullary_recursive_symbolic_positive.cmt
  nullary-application-attack forged=rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  $ for attack in forged stale wrong-constructor path obligation reveal; do ./proof_body_assertions_tool.exe nullary-route-attack "$attack" artifacts/nullary_recursive_symbolic_positive.cmt; done
  nullary-route-attack=forged rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-route-attack=stale rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-route-attack=wrong-constructor rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-route-attack=path rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-route-attack=obligation rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-route-attack=reveal rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  $ for attack in missing duplicate foreign stale path-mismatch copied; do ./proof_body_assertions_tool.exe nullary-local-attack "$attack" artifacts/nullary_recursive_symbolic_positive.cmt; done
  nullary-local-attack=missing rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-local-attack=duplicate rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-local-attack=foreign rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-local-attack=stale rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-local-attack=path-mismatch rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  nullary-local-attack=copied rejected attempts=0 retries=0 facts=0 backend=0 z3=3/3
  $ ./proof_body_assertions_tool.exe nullary-primary-outcomes artifacts/empty_node_len.cmt artifacts/empty_node_false.cmt
  nullary-primary verified-retries=0 counterexample-retries=0

An authenticated Exec region can call a finite recursive Proof and export its
logical summary while preserving the exact issued visit/spent-visit/summary
ledger. Suppressing only that call summary fails its later consumer. A ground
match witness cannot be reused by another region or escape its exact scope.

  $ ./proof_body_assertions_tool.exe region-proof-call artifacts/region_proof_call.cmt
  region-proof-call status=verified local=verified callee-ledger=1/1/1 region-summary-exports=3
  $ ./proof_body_assertions_tool.exe region-proof-summary-suppressed artifacts/region_proof_call.cmt
  region-proof-summary-suppressed local=failed visits=1 summaries=1 region-summary-exports=0
  $ cat > artifacts/exact_ground_scope.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let rec rebuild (idx : int) (nodes : int node) : int node =
  >   [%verocaml.decreases nodes];
  >   match nodes with
  >   | Empty -> Empty
  >   | Node (value, rest) ->
  >       if idx <= 0 then Node (value, Empty) else rebuild (idx - 1) rest
  > [@@verocaml.spec] [@@verocaml.opaque]
  > let two_matches (idx : int) (nodes : int node [@finite]) =
  >   [%verocaml.reveal_with_fuel (rebuild, 2)];
  >   (match nodes with
  >    | Empty -> [%verocaml.assert rebuild idx nodes = Empty]
  >    | Node _ -> ());
  >   match nodes with
  >   | Empty -> [%verocaml.assert rebuild idx nodes = Empty]
  >   | Node _ -> ()
  > [@@verocaml.proof]
  > let exec_match (idx : int) (nodes : int node [@finite]) =
  >   [%verocaml.proof
  >     [%verocaml.reveal_with_fuel (rebuild, 2)];
  >     (match nodes with
  >      | Empty -> [%verocaml.assert rebuild idx nodes = Empty]
  >      | Node _ -> ());
  >     ()];
  >   ()
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/exact_ground_scope.cmo artifacts/exact_ground_scope.ml
  $ for attack in reuse escape; do ./proof_body_assertions_tool.exe ground-attack "$attack" artifacts/exact_ground_scope.cmt | sed -E 's/rejected=.* ground-attempts=/rejected=<private-boundary> ground-attempts=/'; done
  ground-attack=reuse rejected=<private-boundary> ground-attempts=0 ground-complete=0 ground-abstentions=0 recursive-query=3 solver=0 z3-context=6 z3-solver=6
  ground-attack=escape rejected=<private-boundary> ground-attempts=0 ground-complete=0 ground-abstentions=0 recursive-query=3 solver=0 z3-context=6 z3-solver=6

Top-level Proof `if` and scalar `match` keep a successful local fact only on
their exact concrete postcondition path. Both Proof branches retain the same
fact independently. An inner branch in a logical Exec proof region still uses
the authenticated Exec intersection, with carrier, route, and obligation
counts unchanged.

  $ ./proof_body_assertions_tool.exe proof-branch-facts artifacts/proof_branch_fact_postcondition.cmt
  proof-if local-own-goal=false postconditions=2 fact-present=1 fact-absent=1 exact-successor=true
  proof-match local-own-goal=false postconditions=2 fact-present=1 fact-absent=1 exact-successor=true
  proof-both predecessor-vcs=2 continuation-vcs=2 predecessor-own-goals=false fact-present=2
  exec-inner current-mode=exec logical=true call-vcs=2 fact-present=0 outcomes=counterexample,verified
  authority functions=5 obligations=13 static=6 sst=6 local=7/7 exports=7 routes=11

Pure-let capture verifies. Both branch facts are present on their concrete
continuations, while the test-only outgoing-fact mutant removes them without
putting the continuation goal into its own assumptions. A one-branch-only
fact fails on the other successor.

  $ ./proof_body_assertions_tool.exe solve artifacts/positive_matrix.cmt > artifacts/matrix.solve
  $ head -1 artifacts/matrix.solve
  status=verified functions=4 obligations=10
  $ grep '^function=pure_let ' artifacts/matrix.solve
  function=pure_let vc=local-assertion[0] index=0 assumptions=2 path=0 goal-in-assumptions=false outcome=verified
  $ ./proof_body_assertions_tool.exe predecessors artifacts/positive_matrix.cmt
  suppressed=none continuation-vcs=2 checked-states=2 continuation-activation-leaks=0
  continuation=0 predecessor-fact-present=true own-goal-in-assumptions=false outcome=verified
  continuation=1 predecessor-fact-present=true own-goal-in-assumptions=false outcome=verified
  $ ./proof_body_assertions_tool.exe predecessors-suppress-left artifacts/positive_matrix.cmt
  suppressed=left continuation-vcs=2 checked-states=1 continuation-activation-leaks=0
  continuation=0 predecessor-fact-present=false own-goal-in-assumptions=false outcome=counterexample
  $ ./proof_body_assertions_tool.exe predecessors-suppress-right artifacts/positive_matrix.cmt
  suppressed=right continuation-vcs=2 checked-states=1 continuation-activation-leaks=0
  continuation=0 predecessor-fact-present=false own-goal-in-assumptions=false outcome=counterexample
  $ ./proof_body_assertions_tool.exe predecessors-suppressed artifacts/positive_matrix.cmt
  suppressed=all continuation-vcs=2 checked-states=1 continuation-activation-leaks=0
  continuation=0 predecessor-fact-present=false own-goal-in-assumptions=false outcome=counterexample
  $ ./proof_body_assertions_tool.exe one-predecessor artifacts/one_predecessor_negative.cmt
  one-predecessor continuation-states=2
  continuation=0 predecessor-fact-present=true outcome=verified
  continuation=1 predecessor-fact-present=false outcome=counterexample
  $ ./proof_body_assertions_tool.exe negative artifacts/one_predecessor_negative.cmt | head -1
  status=counterexample function=one_predecessor vc=local-assertion[1] index=2 outcome=counterexample

The recursive Proof has one exact strict-child visit and returned summary.
The dependent equality assertion is reached only after that call. Removing
the call (and its now-inapplicable recursion/decrease syntax) or suppressing
the returned summary reaches and fails the same named local VC.

  $ ./proof_body_assertions_tool.exe solve artifacts/recursive_summary.cmt > artifacts/recursive.solve
  $ head -2 artifacts/recursive.solve
  status=verified functions=1 obligations=12
  function=prove_twice_len vc=local-assertion[0] index=3 assumptions=10 path=2 goal-in-assumptions=false outcome=verified
  $ tail -1 artifacts/recursive.solve
  counters static=1 sst=1 reached-issued=1 reached-consumed=1 proof-visits=1 proof-summaries=1 recursive-query=3 solver=0
  $ ./proof_body_assertions_tool.exe negative artifacts/recursive_call_removed.cmt | head -1
  status=inconclusive function=prove_twice_len vc=local-assertion[0] index=0 outcome=unknown
  $ ./proof_body_assertions_tool.exe summary-suppressed artifacts/recursive_summary.cmt
  suppressed-summary function=prove_twice_len vc=local-assertion[0] index=3 outcome=unknown
  counters static=1 sst=1 reached-issued=1 reached-consumed=1 proof-visits=1 proof-summaries=0 recursive-query=1 solver=0

Wrong and reachable-false branch mutants fail the named VC. Reveal authority
must precede the assertion on the same path and callable; a later, sibling,
or other-callable explicit activation cannot be used. A definition declared
revealed is available from proof entry throughout the proof.

  $ ./proof_body_assertions_tool.exe negative artifacts/empty_node_wrong.cmt | head -1
  status=inconclusive function=lemma_empty_node_len vc=local-assertion[0] index=0 outcome=unknown
  $ ./proof_body_assertions_tool.exe negative artifacts/empty_node_false.cmt | head -1
  status=counterexample function=lemma_empty_node_len vc=local-assertion[0] index=0 outcome=counterexample
  $ for name in reveal_removed_negative reveal_after_negative reveal_sibling_negative reveal_other_callable_negative; do printf '%s: ' "$name"; ./proof_body_assertions_tool.exe negative "artifacts/$name.cmt" | head -1 | sed -E 's/status=[^ ]+ function=([^ ]+) vc=([^ ]+) index=[0-9]+ outcome=[^ ]+/function=\1 vc=\2 failed/'; done
  reveal_removed_negative: function=reveal_removed vc=local-assertion[0] failed
  reveal_after_negative: function=reveal_after vc=local-assertion[0] failed
  reveal_sibling_negative: function=reveal_sibling vc=local-assertion[0] failed
  reveal_other_callable_negative: function=no_activation_here vc=local-assertion[0] failed
  $ ./proof_body_assertions_tool.exe solve artifacts/revealed_default_negative.cmt | head -2
  status=verified functions=1 obligations=4
  function=no_reached_activation vc=local-assertion[0] index=0 assumptions=4 path=0 goal-in-assumptions=false outcome=verified

Reached local-instance authority is session-affine. Every adversary rejects
before recursive query construction, backend creation, or direct Z3 work.
The genuine instance is the nonzero positive control.

  $ for attack in missing duplicate foreign stale path-mismatch copied; do ./proof_body_assertions_tool.exe attack "$attack" artifacts/simple_instance.cmt | sed -E 's/rejected=.* static=/rejected=<private-boundary> static=/'; done
  attack=missing rejected=<private-boundary> static=1 sst=1 reached-issued=0 reached-consumed=0 activation-events=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  attack=duplicate rejected=<private-boundary> static=1 sst=1 reached-issued=1 reached-consumed=1 activation-events=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  attack=foreign rejected=<private-boundary> static=1 sst=1 reached-issued=1 reached-consumed=0 activation-events=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  attack=stale rejected=<private-boundary> static=1 sst=1 reached-issued=1 reached-consumed=0 activation-events=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  attack=path-mismatch rejected=<private-boundary> static=1 sst=1 reached-issued=1 reached-consumed=0 activation-events=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  attack=copied rejected=<private-boundary> static=1 sst=1 reached-issued=1 reached-consumed=0 activation-events=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  $ ./proof_body_assertions_tool.exe solve artifacts/simple_instance.cmt | tail -1
  counters static=1 sst=1 reached-issued=1 reached-consumed=1 proof-visits=0 proof-summaries=0 recursive-query=0 solver=1

A same-length mutation of a genuine retained CMT, a context-transplanted/raw
program copy, a source-written carrier, and an effectful predicate all fail
before backend work. A malformed dependency cannot import local-assert
authority.

  $ for attack in ordinal kind predicate-span local-id callable body region; do python3 mutate_local_carrier.py artifacts/simple_instance.cmt "artifacts/mutated-$attack.cmt" "$attack"; cp artifacts/simple_instance.cmi "artifacts/mutated-$attack.cmi"; printf '%s: ' "$attack"; ./proof_body_assertions_tool.exe reject "artifacts/mutated-$attack.cmt"; done
  ordinal: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  kind: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  predicate-span: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  local-id: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  callable: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  body: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  region: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  $ ./proof_body_assertions_tool.exe static-copy artifacts/simple_instance.cmt
  context-copy=rejected boundary=validation
  context-copy baseline=static:1/sst:1/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  $ ./proof_body_assertions_tool.exe raw-sst artifacts/simple_instance.cmt
  raw-sst=rejected boundary=validation
  raw-sst baseline=static:1/sst:1/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0 delta=static:0/sst:0/local:0/0/export:0/route:0/recursive:0/0/backend:0/z3:0/0
  $ ./proof_body_assertions_tool.exe reject artifacts/raw_carrier_rejected.cmt
  rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  $ ./proof_body_assertions_tool.exe reject artifacts/effectful_rejected.cmt
  rejected=frontend:VERO_UNSUPPORTED_EXTERNAL_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  $ cp fixtures/local_dependency.ml fixtures/imported_local_consumer.ml artifacts/import/
  $ testcase_root=$PWD; (cd artifacts/import && ocamlc -w -A -alert -all -bin-annot -I "$testcase_root/../../runtime/.vero_ghost.objs/byte" -ppx "$testcase_root/../../ppx/vero_ppx.exe --keep-ghost" -c local_dependency.ml && ocamlc -w -A -alert -all -bin-annot -I "$testcase_root/../../runtime/.vero_ghost.objs/byte" -I . -ppx "$testcase_root/../../ppx/vero_ppx.exe --keep-ghost" -c imported_local_consumer.ml)
  $ ../../src/verocaml.exe verify artifacts/import/imported_local_consumer.cmt --dependency artifacts/import/local_dependency.cmt 2>&1 | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/; s,file=[^ ]+,file=imported_local_consumer.cmt,'
  verocaml: verified dependency unit=Local_dependency interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=imported_local_consumer.cmt functions=1 obligations=0
  $ python3 mutate_local_carrier.py artifacts/import/local_dependency.cmt artifacts/import/local_dependency_bad.cmt
  $ ./proof_body_assertions_tool.exe import-reject artifacts/import/imported_local_consumer.cmt artifacts/import/local_dependency_bad.cmt
  import=rejected static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0

Direct Exec/Spec assertions and a tail-position Exec region remain PPX errors.
If ocamlc emits an incomplete CMT, it is carrier-free and the real loader
rejects it before adapter/session/recursive/backend work. Escaping a
proof-local binding fails ordinary typing. Value-position/nested regions,
direct Exec reveal, non-Boolean predicates, Exec calls, and effects reject at
their adapter/validation boundary with no recursive query or solver work.

  $ ppx_reject () { name=$1; fragment=$2; if OCAML_COLOR=never ocamlc -w -A -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml" >"artifacts/$name.err" 2>&1; then return 1; fi; grep -F "$fragment" "artifacts/$name.err" >/dev/null || return 1; if test -e "artifacts/$name.cmt"; then if strings "artifacts/$name.cmt" | grep -E 'verocaml:(local-assert|proof-region-capture)|Vero_ghost[.](assert_|proof_region|marker|sidecar|reveal_with_fuel)' >/dev/null; then return 1; fi; printf '%s: ' "$name"; ./proof_body_assertions_tool.exe incomplete-ppx-reject "artifacts/$name.cmt"; else printf '%s: rejected with no CMT\n' "$name"; fi; }
  $ typing_reject () { name=$1; fragment=$2; if OCAML_COLOR=never ocamlc -w -A -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml" >"artifacts/$name.err" 2>&1; then return 1; fi; grep -F "$fragment" "artifacts/$name.err" >/dev/null || return 1; test ! -e "artifacts/$name.cmt" || return 1; printf '%s: typing rejected with no CMT requested\n' "$name"; }
  $ ppx_reject exec_rejected 'only valid in a contiguous'
  exec_rejected: incomplete-cmt=rejected loader=VERO_MALFORMED_INPUT static=0 sst=0 local=0/0 export=0 route=0 recursive=0/0 backend=0 z3=0/0
  $ ppx_reject spec_rejected 'only valid in a contiguous'
  spec_rejected: incomplete-cmt=rejected loader=VERO_MALFORMED_INPUT static=0 sst=0 local=0/0 export=0 route=0 recursive=0/0 backend=0 z3=0/0
  $ ppx_reject exec_proof_region_rejected 'only valid in a contiguous'
  exec_proof_region_rejected: incomplete-cmt=rejected loader=VERO_MALFORMED_INPUT static=0 sst=0 local=0/0 export=0 route=0 recursive=0/0 backend=0 z3=0/0
  $ ppx_reject value_position_rejected 'only valid in a contiguous'
  value_position_rejected: incomplete-cmt=rejected loader=VERO_MALFORMED_INPUT static=0 sst=0 local=0/0 export=0 route=0 recursive=0/0 backend=0 z3=0/0
  $ ppx_reject value_argument_rejected 'only valid in a contiguous'
  value_argument_rejected: incomplete-cmt=rejected loader=VERO_MALFORMED_INPUT static=0 sst=0 local=0/0 export=0 route=0 recursive=0/0 backend=0 z3=0/0
  $ ppx_reject value_tuple_rejected 'only valid in a contiguous'
  value_tuple_rejected: incomplete-cmt=rejected loader=VERO_MALFORMED_INPUT static=0 sst=0 local=0/0 export=0 route=0 recursive=0/0 backend=0 z3=0/0
  $ typing_reject escaping_exec_region_binding_rejected 'Unbound value "proof_only"'
  escaping_exec_region_binding_rejected: typing rejected with no CMT requested
  $ typing_reject non_unit_rejected 'expression has type "unit"'
  non_unit_rejected: typing rejected with no CMT requested
  $ retained non_boolean_rejected
  $ ./proof_body_assertions_tool.exe reject artifacts/non_boolean_rejected.cmt
  rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  $ for name in direct_exec_reveal_rejected nested_exec_region_rejected value_exec_region_rejected non_boolean_exec_region_rejected exec_call_region_rejected effectful_exec_region_rejected; do printf '%s: ' "$name"; ./proof_body_assertions_tool.exe reject "artifacts/$name.cmt"; done
  direct_exec_reveal_rejected: rejected=validation static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  nested_exec_region_rejected: rejected=validation static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  value_exec_region_rejected: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  non_boolean_exec_region_rejected: rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  exec_call_region_rejected: rejected=validation static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  effectful_exec_region_rejected: rejected=frontend:VERO_UNSUPPORTED_EXTERNAL_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0

Whole byte-identical CMT/CMI copies and renames are positive revalidations,
with identical obligations and counters; no path nonce or replay registry is
consulted.

  $ cp artifacts/simple_instance.cmt artifacts/renamed_payload.cmt
  $ cp artifacts/simple_instance.cmi artifacts/renamed_payload.cmi
  $ cmp artifacts/simple_instance.cmt artifacts/renamed_payload.cmt && cmp artifacts/simple_instance.cmi artifacts/renamed_payload.cmi
  $ ocamlobjinfo artifacts/simple_instance.cmi > artifacts/original.cmi-info
  $ ocamlobjinfo artifacts/renamed_payload.cmi > artifacts/copied.cmi-info
  $ tail -n +2 artifacts/original.cmi-info > artifacts/original.cmi-body
  $ tail -n +2 artifacts/copied.cmi-info > artifacts/copied.cmi-body
  $ cmp artifacts/original.cmi-body artifacts/copied.cmi-body
  $ ./proof_body_assertions_tool.exe solve artifacts/simple_instance.cmt > artifacts/original.solve
  $ ./proof_body_assertions_tool.exe solve artifacts/renamed_payload.cmt > artifacts/copied.solve
  $ cmp artifacts/original.solve artifacts/copied.solve && tail -1 artifacts/copied.solve
  counters static=1 sst=1 reached-issued=1 reached-consumed=1 proof-visits=0 proof-summaries=0 recursive-query=0 solver=1

Ordinary local and installed-client compilation erase the Proof declaration
and every local carrier from source, CMI/CMT/CMO/CMX, bytecode, and native
artifacts.

  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/create_stack_with.ml >/dev/null 2>artifacts/create.ordinary.source
  $ grep -E 'verocaml:(local-assert|proof-region-capture)|Vero_ghost|assert_|proof_region|marker|sidecar|reveal_with_fuel' artifacts/create.ordinary.source >/dev/null; test $? -ne 0
  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/create_ordinary.cmo fixtures/create_stack_with.ml
  $ ocamlopt -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/create_ordinary.cmx fixtures/create_stack_with.ml
  $ ocamlc -I artifacts artifacts/create_ordinary.cmo -o artifacts/create_ordinary.byte
  $ ocamlopt -I artifacts artifacts/create_ordinary.cmx -o artifacts/create_ordinary.native
  $ for file in artifacts/create_ordinary.cmi artifacts/create_ordinary.cmt artifacts/create_ordinary.cmo artifacts/create_ordinary.cmx artifacts/create_ordinary.byte artifacts/create_ordinary.native; do strings "$file" | grep -E 'verocaml:(local-assert|proof-region-capture)|Vero_ghost[.](assert_|proof_region|marker|sidecar|reveal_with_fuel)' && exit 1 || :; done
  $ echo 'exact create_stack_with proof region is erased from ordinary output'
  exact create_stack_with proof region is erased from ordinary output
  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/simple_instance.ml >/dev/null 2>artifacts/ordinary.source
  $ grep -E 'verocaml:(local-assert|proof-region-capture)|Vero_ghost|assert_|proof_region|marker|sidecar' artifacts/ordinary.source >/dev/null; test $? -ne 0
  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary.cmo fixtures/simple_instance.ml
  $ ocamlopt -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary.cmx fixtures/simple_instance.ml
  $ ocamlc -I artifacts artifacts/ordinary.cmo -o artifacts/ordinary.byte
  $ ocamlopt -I artifacts artifacts/ordinary.cmx -o artifacts/ordinary.native
  $ for file in artifacts/ordinary.cmi artifacts/ordinary.cmt artifacts/ordinary.cmo artifacts/ordinary.cmx artifacts/ordinary.byte artifacts/ordinary.native; do test -f "$file"; strings "$file" | grep -E 'verocaml:(local-assert|proof-region-capture)|Vero_ghost[.](assert_|proof_region|marker|sidecar)' && exit 1 || :; done
  $ echo 'ordinary source/CMI/CMT/CMO/CMX/byte/native are local-carrier-free'
  ordinary source/CMI/CMT/CMO/CMX/byte/native are local-carrier-free
  $ install_manifest=$(readlink -f ../../verocaml.install)
  $ build_root="${install_manifest%/default/verocaml.install}"
  $ install_root="${VEROCAML_TEST_INSTALL_ROOT:-$build_root/install/default}"
  $ test -x "$install_root/bin/verocaml-ppx"
  $ ocamlc -w -A -alert -all -bin-annot -ppx "$install_root/bin/verocaml-ppx" -c -o artifacts/installed_create.cmo fixtures/create_stack_with.ml
  $ ocamlopt -w -A -alert -all -bin-annot -ppx "$install_root/bin/verocaml-ppx" -c -o artifacts/installed_create.cmx fixtures/create_stack_with.ml
  $ ocamlc -I artifacts artifacts/installed_create.cmo -o artifacts/installed_create.byte
  $ ocamlopt -I artifacts artifacts/installed_create.cmx -o artifacts/installed_create.native
  $ for file in artifacts/installed_create.cmi artifacts/installed_create.cmt artifacts/installed_create.cmo artifacts/installed_create.cmx artifacts/installed_create.byte artifacts/installed_create.native; do strings "$file" | grep -E 'verocaml:(local-assert|proof-region-capture)|Vero_ghost[.](assert_|proof_region|marker|sidecar|reveal_with_fuel)' && exit 1 || :; done
  $ ocamlc -w -A -alert -all -bin-annot -ppx "$install_root/bin/verocaml-ppx" -c -o artifacts/installed_ordinary.cmo fixtures/simple_instance.ml
  $ ocamlopt -w -A -alert -all -bin-annot -ppx "$install_root/bin/verocaml-ppx" -c -o artifacts/installed_ordinary.cmx fixtures/simple_instance.ml
  $ ocamlc -I artifacts artifacts/installed_ordinary.cmo -o artifacts/installed_ordinary.byte
  $ ocamlopt -I artifacts artifacts/installed_ordinary.cmx -o artifacts/installed_ordinary.native
  $ for file in artifacts/installed_ordinary.cmi artifacts/installed_ordinary.cmt artifacts/installed_ordinary.cmo artifacts/installed_ordinary.cmx artifacts/installed_ordinary.byte artifacts/installed_ordinary.native; do test -f "$file"; strings "$file" | grep -E 'verocaml:(local-assert|proof-region-capture)|Vero_ghost[.](assert_|proof_region|marker|sidecar)' && exit 1 || :; done
  $ echo 'installed ordinary CMI/CMT/CMO/CMX/byte/native are local-carrier-free'
  installed ordinary CMI/CMT/CMO/CMX/byte/native are local-carrier-free

Compiler-native assertions retain their exact `assert` spelling.  The PPX
counts them in the same per-callable source-order namespace as retained legacy
assertions, but only the two legacy assertions have ghost assertion carriers.
The direct Exec builtin remains raw and has no synthetic proof-region wrapper.

  $ builtin_retained () { name=$1; shift; ocamlc -w -A -alert -all "$@" -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ for name in builtin_assert_positive_matrix builtin_assert_false builtin_assert_effect_rejected builtin_assert_mutation_rejected builtin_assert_position_rejected builtin_assert_spec_rejected builtin_assert_scope_mutants builtin_assert_lookalike builtin_assert_noassert; do builtin_retained "$name"; done
  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" fixtures/builtin_assert_positive_matrix.ml >/dev/null 2>artifacts/builtin-positive.retained
  $ printf 'raw-builtins=%s legacy-carriers=%s\n' "$(grep -o '[^_.]assert (' artifacts/builtin-positive.retained | wc -l)" "$(grep -o 'Vero_ghost.assert_' artifacts/builtin-positive.retained | wc -l)"
  raw-builtins=9 legacy-carriers=2
  $ sed -n '/let exec_statement/,/let exec_branch_tail/p' artifacts/builtin-positive.retained | grep -q 'assert (value = value)'
  $ sed -n '/let exec_statement/,/let exec_branch_tail/p' artifacts/builtin-positive.retained | grep -E 'Vero_ghost[.](assert_|proof_region)' >/dev/null; test $? -ne 0
  $ echo 'direct Exec builtin retained without a ghost replacement or proof region'
  direct Exec builtin retained without a ghost replacement or proof region

Proof, authenticated proof-region, and direct Exec statements and branch tails
emit the unchanged local-assert VC.  Mixed forms have one contiguous ordinal
namespace.  Proof contexts account for statement and predicate as Ghost;
direct Exec accounts for both as Exec.  The direct temporary scopes are affine,
all close immediately, and the proved fact reaches the later normal successor.

  $ ./proof_body_assertions_tool.exe builtin-matrix artifacts/builtin_assert_positive_matrix.cmt
  builtin-matrix status=verified functions=7 local-vcs=11
  ordinals function=exec_branch_tail sequence=0,1
  ordinals function=exec_fact_export sequence=0
  ordinals function=exec_proof_region sequence=0,1
  ordinals function=exec_statement sequence=0
  ordinals function=proof_branch_tail sequence=0,1
  ordinals function=proof_mixed sequence=0,1,2
  mode function=proof_mixed ordinal=0 statement=Ghost predicate=Ghost
  mode function=proof_mixed ordinal=1 statement=Ghost predicate=Ghost
  mode function=proof_mixed ordinal=2 statement=Ghost predicate=Ghost
  mode function=proof_branch_tail ordinal=0 statement=Ghost predicate=Ghost
  mode function=proof_branch_tail ordinal=1 statement=Ghost predicate=Ghost
  mode function=exec_statement ordinal=0 statement=Exec predicate=Exec
  mode function=exec_branch_tail ordinal=0 statement=Exec predicate=Exec
  mode function=exec_branch_tail ordinal=1 statement=Exec predicate=Exec
  mode function=exec_proof_region ordinal=0 statement=Ghost predicate=Ghost
  mode function=exec_proof_region ordinal=1 statement=Ghost predicate=Ghost
  mode function=exec_fact_export ordinal=0 statement=Exec predicate=Exec
  normal-successor fact-exported=true later=verified
  builtin-authority static=11 sst=11 local=11/11 exports=11
  direct-scopes issued=4 closed=4 active=0
  $ ./proof_body_assertions_tool.exe builtin-false artifacts/builtin_assert_false.cmt
  builtin-false status=counterexample local-vcs=3 outcomes=proof_false[0]:counterexample,exec_false[0]:counterexample,generic_false_sequence[0]:counterexample

Two source runs and two retained-CMT runs are byte-deterministic.  Their
per-callable SST and VIR local-assert ordinal sequences also agree exactly.

  $ for n in 1 2; do ../../src/verocaml.exe verify fixtures/builtin_assert_positive_matrix.ml --timeout-ms 5000 --dump-sst "artifacts/builtin-source.$n.sst" --dump-vir "artifacts/builtin-source.$n.vir" >/dev/null 2>&1; done
  $ for n in 1 2; do ../../src/verocaml.exe verify artifacts/builtin_assert_positive_matrix.cmt --timeout-ms 5000 --dump-sst "artifacts/builtin-cmt.$n.sst" --dump-vir "artifacts/builtin-cmt.$n.vir" >/dev/null 2>&1; done
  $ cmp artifacts/builtin-source.1.sst artifacts/builtin-source.2.sst && cmp artifacts/builtin-source.1.vir artifacts/builtin-source.2.vir && cmp artifacts/builtin-cmt.1.sst artifacts/builtin-cmt.2.sst && cmp artifacts/builtin-cmt.1.vir artifacts/builtin-cmt.2.vir
  $ grep -h 'local-assert ordinal=' artifacts/builtin-source.1.sst | sed -E 's/ @ .*$//' > artifacts/builtin-source.sst-ordinals
  $ grep -h 'local-assert ordinal=' artifacts/builtin-cmt.1.sst | sed -E 's/ @ .*$//' > artifacts/builtin-cmt.sst-ordinals
  $ grep -h 'local-assertion ordinal=' artifacts/builtin-source.1.vir | sed -E 's/ @ .*$//' > artifacts/builtin-source.vir-ordinals
  $ grep -h 'local-assertion ordinal=' artifacts/builtin-cmt.1.vir | sed -E 's/ @ .*$//' > artifacts/builtin-cmt.vir-ordinals
  $ cmp artifacts/builtin-source.sst-ordinals artifacts/builtin-cmt.sst-ordinals && cmp artifacts/builtin-source.vir-ordinals artifacts/builtin-cmt.vir-ordinals && echo 'builtin source/CMT ordinal sequences deterministic'
  builtin source/CMT ordinal sequences deterministic

A retained legacy ordinal that no longer agrees with its mixed builtin/legacy
source position rejects before any builtin static issuance or SST lowering.

  $ cat > artifacts/builtin_mixed_manifest.ml <<'EOF'
  > let mixed (value : int) : unit =
  >   assert (value = value);
  >   [%verocaml.assert value <= value];
  >   assert (value >= value)
  > [@@verocaml.proof]
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/builtin_mixed_manifest.cmo artifacts/builtin_mixed_manifest.ml
  $ python3 mutate_local_carrier.py artifacts/builtin_mixed_manifest.cmt artifacts/builtin_mixed_manifest_bad.cmt ordinal
  $ cp artifacts/builtin_mixed_manifest.cmi artifacts/builtin_mixed_manifest_bad.cmi
  $ ./proof_body_assertions_tool.exe reject artifacts/builtin_mixed_manifest_bad.cmt
  rejected=frontend:VERO_MALFORMED_GHOST_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0

Every direct-scope issuer, affinity, ordinal, path, and goal mutant rejects
before a reached local instance, route, recursive query, backend, or native Z3
work.  Cleanup closes the temporary scope even on the rejection path.

  $ for attack in direct-scope direct-issuer direct-ordinal direct-path direct-goal; do ./proof_body_assertions_tool.exe builtin-scope-attack "$attack" artifacts/builtin_assert_scope_mutants.cmt | sed -E 's/rejected=.* static=/rejected=<private-boundary> static=/'; done
  builtin-scope-attack=direct-scope rejected=<private-boundary> static=1 sst=1 local=0/0 scopes=1/1/active:0 recursive-query=0 solver=0 z3=0/0
  builtin-scope-attack=direct-issuer rejected=<private-boundary> static=1 sst=1 local=0/0 scopes=1/1/active:0 recursive-query=0 solver=0 z3=0/0
  builtin-scope-attack=direct-ordinal rejected=<private-boundary> static=1 sst=1 local=0/0 scopes=1/1/active:0 recursive-query=0 solver=0 z3=0/0
  builtin-scope-attack=direct-path rejected=<private-boundary> static=1 sst=1 local=0/0 scopes=1/1/active:0 recursive-query=0 solver=0 z3=0/0
  builtin-scope-attack=direct-goal rejected=<private-boundary> static=1 sst=1 local=0/0 scopes=1/1/active:0 recursive-query=0 solver=0 z3=0/0

Effects, mutation, nested/value position, Spec, an explicit Assert_failure
lookalike, and an otherwise supported runtime call all reject with zero builtin
static issuance, reached instance, route, recursive query, backend, and Z3
work.  The unsupported-call source is generated inside the authorized Cram
harness so no extra product fixture is introduced.

  $ for name in builtin_assert_effect_rejected builtin_assert_mutation_rejected builtin_assert_position_rejected builtin_assert_spec_rejected builtin_assert_lookalike; do printf '%s: ' "$name"; ./proof_body_assertions_tool.exe reject "artifacts/$name.cmt"; done
  builtin_assert_effect_rejected: rejected=frontend:VERO_UNSUPPORTED_EXTERNAL_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  builtin_assert_mutation_rejected: rejected=frontend:VERO_UNSUPPORTED_MUTATION static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  builtin_assert_position_rejected: rejected=validation static=0 sst=1 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  builtin_assert_spec_rejected: rejected=validation static=0 sst=1 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  builtin_assert_lookalike: rejected=frontend:VERO_UNSUPPORTED_EXTERNAL_CALL static=0 sst=0 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0
  $ cat > artifacts/builtin_assert_call_rejected.ml <<'EOF'
  > let identity (condition : bool) = condition
  > let unsupported_call (condition : bool) : unit =
  >   assert (identity condition)
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/builtin_assert_call_rejected.cmo artifacts/builtin_assert_call_rejected.ml
  $ ./proof_body_assertions_tool.exe reject artifacts/builtin_assert_call_rejected.cmt
  rejected=validation static=0 sst=1 reached-issued=0 reached-consumed=0 recursive-query=0 solver=0 z3-context=0 z3-solver=0

`assert non_boolean` remains an ordinary compiler typing error and produces no
CMT.  This differs from the well-typed Vero negatives above.

  $ if OCAML_COLOR=never ocamlc -w -A -alert -all -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/builtin_assert_non_boolean.cmo fixtures/builtin_assert_non_boolean.ml >artifacts/builtin_assert_non_boolean.err 2>&1; then exit 1; fi
  $ grep -q 'bool' artifacts/builtin_assert_non_boolean.err && test ! -e artifacts/builtin_assert_non_boolean.cmt && echo 'non-Boolean builtin typing rejected with no CMT'
  non-Boolean builtin typing rejected with no CMT

Ordinary and `-noassert` retained CMTs both contain and verify the typed
assertion.  Ordinary execution raises Assert_failure for a false unverified
caller; `-noassert` follows the compiler and elides that runtime guard.  Vero
does not rewrite or restore it.

  $ builtin_retained builtin_assert_noassert
  $ ocamlc -w -A -alert -all -noassert -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/builtin_assert_noassert_elided.cmo fixtures/builtin_assert_noassert.ml
  $ ../../src/verocaml.exe verify artifacts/builtin_assert_noassert.cmt --timeout-ms 5000 2>&1 | sed -E 's,file=[^ ]+,file=ordinary.cmt,'
  verocaml: verified file=ordinary.cmt functions=2 obligations=2
  $ ../../src/verocaml.exe verify artifacts/builtin_assert_noassert_elided.cmt --timeout-ms 5000 2>&1 | sed -E 's,file=[^ ]+,file=noassert.cmt,'
  verocaml: verified file=noassert.cmt functions=2 obligations=2
  $ mkdir -p artifacts/builtin-runtime/ordinary artifacts/builtin-runtime/elided
  $ cp fixtures/builtin_assert_noassert.ml artifacts/builtin-runtime/ordinary/; cp fixtures/builtin_assert_noassert.ml artifacts/builtin-runtime/elided/
  $ cat > artifacts/builtin-runtime/ordinary/runner.ml <<'EOF'
  > let () = Builtin_assert_noassert.runtime_guard false
  > EOF
  $ cp artifacts/builtin-runtime/ordinary/runner.ml artifacts/builtin-runtime/elided/runner.ml
  $ testcase_root=$PWD; (cd artifacts/builtin-runtime/ordinary && ocamlc -w -A -alert -all -bin-annot -ppx "$testcase_root/../../ppx/vero_ppx.exe" -c builtin_assert_noassert.ml && ocamlc builtin_assert_noassert.cmo runner.ml -o run.byte)
  $ testcase_root=$PWD; (cd artifacts/builtin-runtime/elided && ocamlc -w -A -alert -all -noassert -bin-annot -ppx "$testcase_root/../../ppx/vero_ppx.exe" -c builtin_assert_noassert.ml && ocamlc -noassert builtin_assert_noassert.cmo runner.ml -o run.byte)
  $ artifacts/builtin-runtime/ordinary/run.byte >artifacts/builtin-runtime/ordinary.out 2>&1; printf 'ordinary-exit=%d\n' $?
  ordinary-exit=2
  $ sed -E 's/Assert_failure.*/Assert_failure(<source>)/' artifacts/builtin-runtime/ordinary.out
  Fatal error: exception Assert_failure(<source>)
  $ artifacts/builtin-runtime/elided/run.byte; printf 'noassert-exit=%d\n' $?
  noassert-exit=0
