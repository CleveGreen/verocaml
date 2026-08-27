{
  pkgs,
  compiler,
  scope,
  stagedSources,
  tests,
}:
let
  packageSmoke =
    pkgs.runCommand "verocaml-parallel-runtime-smoke"
      {
        nativeBuildInputs = [
          pkgs.stdenv.cc
          compiler
          scope.ocamlfind
        ];
        buildInputs = [ scope.parallel ];
      }
      ''
        set -euo pipefail
        mkdir -p "$out"
        cp ${tests}/lifecycle.ml lifecycle.ml
        cp ${tests}/negative_mutable_capture.ml negative_mutable_capture.ml

        ocamlfind ocamlopt \
          -package parallel.kernel,parallel.scheduler \
          -linkpkg \
          -o lifecycle.exe \
          lifecycle.ml \
          > "$out/lifecycle-compile.stdout" \
          2> "$out/lifecycle-compile.stderr"
        OCAMLRUNPARAM=d=2 ./lifecycle.exe \
          > "$out/lifecycle-run.stdout" \
          2> "$out/lifecycle-run.stderr"
        grep -F 'max_domains=2' "$out/lifecycle-run.stdout"
        grep -F 'nonzero_domain=true' "$out/lifecycle-run.stdout"
        grep -F 'normal_stop_count=1' "$out/lifecycle-run.stdout"
        grep -F 'exception_stop_count=1' "$out/lifecycle-run.stdout"
        grep -F 'worker_exception=observed' "$out/lifecycle-run.stdout"

        set +e
        ocamlfind ocamlopt \
          -package parallel.kernel,parallel.scheduler \
          -linkpkg \
          -o negative_mutable_capture.exe \
          negative_mutable_capture.ml \
          > "$out/negative.stdout" \
          2> "$out/negative.stderr"
        negative_status=$?
        set -e
        test "$negative_status" -ne 0
        grep -Eiq 'shareable|portable|contended|mode' "$out/negative.stderr"
        printf '%s\n' "$negative_status" > "$out/negative.exit"
      '';

  pointerHarness =
    pkgs.runCommand "verocaml-parallel-runtime-pointer-harness"
      {
        nativeBuildInputs = [
          pkgs.stdenv.cc
          compiler
          scope.ocamlfind
        ];
        buildInputs = [ scope.base ];
      }
      ''
        set -euo pipefail
        mkdir -p "$out"
        cp ${tests}/stack_pointer_primitives.ml stack_pointer_primitives.ml
        cp ${stagedSources.parallel}/kernel/stack_pointer_stubs.c \
          stack_pointer_stubs.c
        ocamlfind ocamlopt \
          -package base \
          -linkpkg \
          -O3 \
          -zero-alloc-check all \
          -o stack_pointer_primitives.exe \
          stack_pointer_stubs.c \
          stack_pointer_primitives.ml \
          > "$out/compile.stdout" \
          2> "$out/compile.stderr"
        ./stack_pointer_primitives.exe \
          > "$out/run.stdout" \
          2> "$out/run.stderr"
      '';
in
{
  inherit packageSmoke pointerHarness;
}
