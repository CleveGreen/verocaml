The checked manifest is exactly the numbered public source directory.

  $ test_build_dir="$PWD"
  $ root="${PWD%%/_build/*}"
  $ cd "$root"
  $ find examples/tutorial -maxdepth 1 -type f -name '*.ml' | LC_ALL=C sort > "$test_build_dir/tutorial-manifest.actual"
  $ cmp test/tutorial_examples/manifest.txt "$test_build_dir/tutorial-manifest.actual"
  $ test "$(wc -l < "$test_build_dir/tutorial-manifest.actual")" -eq 8
  $ echo 'tutorial-manifest=8 exact'
  tutorial-manifest=8 exact

Every manifest member goes through the packaged flake application, rather
than a test-only verifier executable.

  $ tmpdir="$(mktemp -d)"
  $ trap 'rm -rf "$tmpdir"' EXIT HUP INT TERM
  $ while IFS= read -r file; do name="$(basename "$file" .ml)"; nix run path:. -- verify "$file" >"$tmpdir/$name.out" 2>&1 && grep -q "verified.*file=$file" "$tmpdir/$name.out" || exit 1; echo "verified=$name"; done < test/tutorial_examples/manifest.txt
  verified=01-contracts
  verified=02-recursive-specification
  verified=03-scalar-induction
  verified=04-list-induction
  verified=05-tree-induction
  verified=06-finite-formals
  verified=07-owned-recursive-stack
  verified=08-abstract-nested-mutation

The list theorem's public dump is deterministic across two independent
destinations. Its Proof measure is the recursive aggregate itself, and VIR
records strict descent through the authenticated structural-height rank.

  $ nix run path:. -- verify examples/tutorial/04-list-induction.ml --dump-sst "$tmpdir/run-1.sst" --dump-vir "$tmpdir/run-1.vir" >"$tmpdir/run-1.out" 2>&1
  $ nix run path:. -- verify examples/tutorial/04-list-induction.ml --dump-sst "$tmpdir/run-2.sst" --dump-vir "$tmpdir/run-2.vir" >"$tmpdir/run-2.out" 2>&1
  $ cmp "$tmpdir/run-1.sst" "$tmpdir/run-2.sst"
  $ cmp "$tmpdir/run-1.vir" "$tmpdir/run-2.vir"
  $ grep -A30 '^function equal_sum#' "$tmpdir/run-1.sst" | grep -A1 '^  decreases 0 stage=logical' | grep -q 'variable left.*int_list'
  $ grep -q '^rank-domain .*version=structural-height-v1.*immutable=true' "$tmpdir/run-1.vir"
  $ grep -q 'recursive-call-strict-descent callee=equal_sum#' "$tmpdir/run-1.vir"
  $ echo 'list-dumps=deterministic measure=int_list rank=strict-child'
  list-dumps=deterministic measure=int_list rank=strict-child

Deleting exactly the strict-tail call from the Cons/Cons branch leaves the
enclosing postcondition unproved. The remaining strict-tail call in the Nil
branch keeps the mutant a recursive Proof, so this tests loss of the returned
summary rather than dormant-recursion syntax.

  $ test "$(grep -c '^          equal_sum left_tail right_tail;$' examples/tutorial/04-list-induction.ml)" -eq 1
  $ sed '/^          equal_sum left_tail right_tail;$/d' examples/tutorial/04-list-induction.ml > "$tmpdir/list-call-removed.ml"
  $ set +e; OCAML_COLOR=never nix run path:. -- verify "$tmpdir/list-call-removed.ml" >"$tmpdir/list-call-removed.out" 2>&1; mutant_status=$?; set -e
  $ test "$mutant_status" -eq 3
  $ grep -q 'inconclusive function=equal_sum.*vc=postcondition' "$tmpdir/list-call-removed.out"
  $ echo 'recursive-call-removed=failed enclosing-postcondition=inconclusive'
  recursive-call-removed=failed enclosing-postcondition=inconclusive

The separate deliberately false proof pins the documented counterexample
exit class and diagnostic.

  $ set +e; OCAML_COLOR=never nix run path:. -- verify test/tutorial_examples/fixtures/failed-proof.ml >"$tmpdir/failed-proof.out" 2>&1; failed_status=$?; set -e
  $ test "$failed_status" -eq 1
  $ grep -q 'counterexample function=wrong_successor.*vc=postcondition' "$tmpdir/failed-proof.out"
  $ echo 'failed-proof=counterexample exit=1'
  failed-proof=counterexample exit=1

The public Org file has eight live local links.

  $ links="$(sed -n 's/.*\[\[file:\([^]]*\)\].*/\1/p' docs/TUTORIAL.org)"
  $ test "$(printf '%s\n' "$links" | sed '/^$/d' | wc -l)" -eq 8
  $ printf '%s\n' "$links" | while IFS= read -r link; do test -e "docs/$link" || exit 1; done
  $ echo 'tutorial-local-links=8'
  tutorial-local-links=8

Ordinary PPX compilation erases contracts, local Proof blocks, and their
ghost runtime carriers from the emitted object.

  $ cd "$test_build_dir"
  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o "$tmpdir/contracts.cmo" ../../examples/tutorial/01-contracts.ml
  $ if strings "$tmpdir/contracts.cmo" | grep -E 'Vero_ghost|verocaml:(local-assert|proof-region-capture)'; then exit 1; fi
  $ echo 'ordinary-object=proof-and-contract-carrier-free'
  ordinary-object=proof-and-contract-carrier-free
