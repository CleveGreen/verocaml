{
  pkgs,
  compiler,
  ocamlfind,
  parallel,
  tests,
  z3,
}:
let
  nativeBuildInputs = [
    pkgs.stdenv.cc
    compiler
    ocamlfind
  ];
  buildInputs = [
    parallel
    z3
  ];

  positive =
    pkgs.runCommand "z3-context-local-portability"
      {
        inherit nativeBuildInputs buildInputs;
      }
      ''
        set -euo pipefail
        mkdir -p "$out"
        cp ${tests}/positive_parallel_contexts.ml positive_parallel_contexts.ml

        cat > z3_context_probe.c <<'EOF'
        #define _GNU_SOURCE
        #include <dlfcn.h>
        #include <stdatomic.h>
        #include <stdio.h>
        #include <stdlib.h>

        typedef struct _Z3_config *Z3_config;
        typedef struct _Z3_context *Z3_context;

        static _Atomic unsigned long created = 0;
        static _Atomic unsigned long deleted = 0;

        Z3_context Z3_mk_context_rc(Z3_config cfg) {
          static Z3_context (*real)(Z3_config);
          if (real == NULL) real = dlsym(RTLD_NEXT, "Z3_mk_context_rc");
          if (real == NULL) abort();
          atomic_fetch_add_explicit(&created, 1, memory_order_relaxed);
          return real(cfg);
        }

        void Z3_del_context(Z3_context ctx) {
          static void (*real)(Z3_context);
          if (real == NULL) real = dlsym(RTLD_NEXT, "Z3_del_context");
          if (real == NULL) abort();
          atomic_fetch_add_explicit(&deleted, 1, memory_order_relaxed);
          real(ctx);
        }

        __attribute__((destructor)) static void report(void) {
          fprintf(stderr, "z3-context-probe created=%lu deleted=%lu\n",
                  atomic_load_explicit(&created, memory_order_relaxed),
                  atomic_load_explicit(&deleted, memory_order_relaxed));
        }
        EOF

        cc \
          -shared \
          -fPIC \
          -std=c11 \
          -o libz3_context_probe.so \
          z3_context_probe.c \
          -ldl

        test "$(
          ocamlfind query z3
        )" = "${z3}/lib/ocaml/5.2.0/site-lib/z3"
        case "$(ocamlfind query parallel.kernel)" in
          "${parallel}"/*) ;;
          *) exit 1 ;;
        esac
        case "$(ocamlfind query parallel.scheduler)" in
          "${parallel}"/*) ;;
          *) exit 1 ;;
        esac

        ocamlfind ocamlopt \
          -package z3,parallel.kernel,parallel.scheduler \
          -linkpkg \
          -o positive_parallel_contexts.exe \
          positive_parallel_contexts.ml \
          > "$out/compile.stdout" \
          2> "$out/compile.stderr"

        LD_PRELOAD="$PWD/libz3_context_probe.so" \
          OCAMLRUNPARAM=d=3 \
          ./positive_parallel_contexts.exe \
          > "$out/run.stdout" \
          2> "$out/run.stderr"

        test "$(
          cat "$out/run.stdout"
        )" = "portable-z3-contexts=ok solves=256 worker_domain_mask=6 results=primitive"
        test "$(
          cat "$out/run.stderr"
        )" = "z3-context-probe created=256 deleted=256"
      '';

  negative =
    {
      name,
      fixture,
      diagnostic,
    }:
    pkgs.runCommand name
      {
        inherit nativeBuildInputs buildInputs;
      }
      ''
        set -euo pipefail
        mkdir -p "$out"
        cp ${fixture} negative.ml

        set +e
        ocamlfind ocamlopt \
          -package z3,parallel.kernel,parallel.scheduler \
          -c negative.ml \
          > "$out/compile.stdout" \
          2> "$out/compile.stderr"
        status=$?
        set -e

        test "$status" -ne 0
        grep -F '${diagnostic}' "$out/compile.stderr"
        grep -F 'expected to be "shareable"' "$out/compile.stderr"
        printf '%s\n' "$status" > "$out/compile.exit"
      '';
in
{
  inherit positive;

  negativeContext = negative {
    name = "z3-context-local-portability-negative-context";
    fixture = tests + "/negative_context_capture.ml";
    diagnostic = ''The value "context" is "nonportable"'';
  };

  negativeGlobal = negative {
    name = "z3-context-local-portability-negative-global";
    fixture = tests + "/negative_global_api.ml";
    diagnostic = ''The value "Z3.set_global_param" is "nonportable"'';
  };
}
