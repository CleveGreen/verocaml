Source-compilation presentation is localized here. Semantic result classes,
stable codes, forwarding, and cleanup remain owned by the outcome suite.

  $ mkdir artifacts

The pinned compiler's selected location and type-error excerpt remains visible
without colour when OCAML_COLOR disables it.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify ../source_input/fixtures/compile_error.ml > artifacts/compile-error.out 2>&1 || true
  $ grep -F 'File "../source_input/fixtures/compile_error.ml", line 1, characters 27-32:' artifacts/compile-error.out
  File "../source_input/fixtures/compile_error.ml", line 1, characters 27-32:
  $ grep -F 'This expression has type "bool"' artifacts/compile-error.out
  Error: This expression has type "bool" but an expression was expected of type

With no OCAML_COLOR override, the source-compilation error label is rendered
with the intentional bold-red ANSI prefix.

  $ env -u OCAML_COLOR VEROCAML_OCAMLC="$PWD/../source_input/source_input_compiler_helper.exe" VEROCAML_TEST_COMPILER_OUTCOME=exit VEROCAML_PPX="$PWD/../../ppx/vero_ppx.exe" VEROCAML_GHOST_DIR="$PWD/../../runtime/.vero_ghost.objs/byte" ../../src/verocaml.exe verify ../source_input/fixtures/verified.ml > artifacts/compiler-ansi.out 2>&1 || true
  $ od -An -tx1 -v artifacts/compiler-ansi.out | tr -d ' \n' | grep -o '1b5b313b33316d'
  1b5b313b33316d

Only the selected user-facing portions of internal source-compilation setup
diagnostics are retained.

  $ printf 'not executable\n' > artifacts/not-executable
  $ OCAML_COLOR=never VEROCAML_OCAMLC="$PWD/artifacts/not-executable" VEROCAML_PPX="$PWD/../../ppx/vero_ppx.exe" VEROCAML_GHOST_DIR="$PWD/../../runtime/.vero_ghost.objs/byte" ../../src/verocaml.exe verify ../source_input/fixtures/verified.ml > artifacts/invocation.out 2>&1 || true
  $ grep -oF 'could not invoke the pinned compiler:' artifacts/invocation.out
  could not invoke the pinned compiler:
  $ printf 'not a directory\n' > artifacts/not-a-directory
  $ OCAML_COLOR=never TMPDIR="$PWD/artifacts/not-a-directory" ../../src/verocaml.exe verify ../source_input/fixtures/verified.ml > artifacts/setup.out 2>&1 || true
  $ grep -oF 'could not create private source-compilation storage:' artifacts/setup.out
  could not create private source-compilation storage:
