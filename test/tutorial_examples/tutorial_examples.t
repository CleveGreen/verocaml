This manual specialist check keeps the public manifest, documentation links,
packaged application, and ordinary erasure boundary outside the semantic suite.
It does not pin inventory totals, ordering, rendered verifier output, or dumps.

  $ test_build_dir="$PWD"
  $ root="${PWD%%/_build/*}"
  $ cd "$root"
  $ while IFS= read -r file; do test -f "$file" || exit 1; done < test/tutorial_examples/manifest.txt
  $ find examples/tutorial -maxdepth 1 -type f -name '*.ml' | while IFS= read -r file; do grep -Fx "$file" test/tutorial_examples/manifest.txt >/dev/null || exit 1; done
  $ grep -q '\[\[file:' docs/TUTORIAL.org
  $ sed -n 's/.*\[\[file:\([^]]*\)\].*/\1/p' docs/TUTORIAL.org | while IFS= read -r link; do test -e "docs/$link" || exit 1; done

Each manifest source remains accepted by the packaged application under its
own bounded wall-clock check.

  $ while IFS= read -r file; do if ! timeout --foreground --signal=TERM --kill-after=10s 180s nix run path:. -- verify "$file" >"$test_build_dir/package.out" 2>&1; then cat "$test_build_dir/package.out"; exit 1; fi; done < test/tutorial_examples/manifest.txt

Ordinary PPX compilation does not retain verifier-only runtime carriers.

  $ cd "$test_build_dir"
  $ tmpdir="$(mktemp -d)"
  $ trap 'rm -rf "$tmpdir"' EXIT HUP INT TERM
  $ timeout --foreground --signal=TERM --kill-after=5s 60s ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o "$tmpdir/contracts.cmo" ../../examples/tutorial/01-contracts.ml
  $ if strings "$tmpdir/contracts.cmo" | grep -E 'Vero_ghost|verocaml:(local-assert|proof-region-capture)' >/dev/null; then exit 1; fi
