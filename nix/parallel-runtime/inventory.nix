{
  pkgs,
  compiler,
  oldCompiler,
  scope,
  manifest,
}:
let
  lib = pkgs.lib;
  forbiddenPackagePattern = "(bignum|async|async_kernel|core|core_kernel|expect[-_]test[-_]helpers?[-_](core|async))";
  forbiddenParallelPattern = "parallel[._-](facade|arrays?|sequence|vec|async|test|command|bench)";
  selectedPackages = lib.filterAttrs (_name: value: lib.isDerivation value && value ? version) scope;
  resolverInventory = pkgs.writeText "verocaml-parallel-runtime-resolver-inventory" (
    lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        _name: value: "${value.OPAM_PACKAGE_NAME}.${value.OPAM_PACKAGE_VERSION}"
      ) selectedPackages
    )
    + "\n"
  );

  installed =
    pkgs.runCommand "verocaml-parallel-runtime-installed-inventory"
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

          cp ${resolverInventory} "$out/resolver-packages"
          test "$(wc -l < "$out/resolver-packages")" -eq 99
          if grep -Eiq \
            '^${forbiddenPackagePattern}([.]|$)|^${forbiddenParallelPattern}([.]|$)' \
            "$out/resolver-packages"; then
            echo "forbidden resolver member" >&2
            cat "$out/resolver-packages" >&2
            exit 1
          fi
          grep -Fx \
            'ppx_expect.v0.18~preview.130.83+317' \
            "$out/resolver-packages"
          grep -Fx \
            'await.v0.18~preview.130.83+317+verocaml.1' \
            "$out/resolver-packages"
          grep -Fx \
            'concurrent.v0.18~preview.130.83+317+verocaml.1' \
            "$out/resolver-packages"
          grep -Fx \
            'parallel.v0.18~preview.130.83+317+verocaml.1' \
            "$out/resolver-packages"

          printf '%s\n' \
            "${scope.await.OPAM_PACKAGE_NAME}.${scope.await.OPAM_PACKAGE_VERSION}" \
            "${scope.concurrent.OPAM_PACKAGE_NAME}.${scope.concurrent.OPAM_PACKAGE_VERSION}" \
            "${scope.parallel.OPAM_PACKAGE_NAME}.${scope.parallel.OPAM_PACKAGE_VERSION}" \
            > "$out/derivative-packages"
        printf '%s\n' \
          'await.v0.18~preview.130.83+317+verocaml.1' \
          'concurrent.v0.18~preview.130.83+317+verocaml.1' \
          'parallel.v0.18~preview.130.83+317+verocaml.1' \
          > expected-derivatives
          diff -u expected-derivatives "$out/derivative-packages"

          parallel_dir="$(ocamlfind query parallel)"
          awk '
            $0 == "(library" { in_library = 1; next }
            in_library && index($0, " (name ") == 1 {
              print substr($0, 8, length($0) - 8)
              in_library = 0
            }
          ' "$parallel_dir/dune-package" \
            | LC_ALL=C sort -u \
            > "$out/parallel-public-libraries"
          printf '%s\n' parallel.kernel parallel.scheduler > expected-libraries
          diff -u expected-libraries "$out/parallel-public-libraries"

          ocamlfind query parallel.kernel > "$out/parallel-kernel"
          ocamlfind query parallel.scheduler > "$out/parallel-scheduler"
          ocamlfind query \
            -recursive \
            -format '%p' \
            parallel.kernel parallel.scheduler \
            | LC_ALL=C sort -u \
            > "$out/findlib-runtime-closure"
          if grep -Eiq \
            '^${forbiddenPackagePattern}([.]|$)|^${forbiddenParallelPattern}([.]|$)' \
            "$out/findlib-runtime-closure"; then
            echo "forbidden Findlib runtime member" >&2
            cat "$out/findlib-runtime-closure" >&2
            exit 1
          fi
          if find "$parallel_dir" -mindepth 1 \
            | grep -Eiq '/(arrays?|sequence|vec|async|test|command|bench)(/|[._-]|$)'; then
            echo "forbidden Parallel artifact installed" >&2
            find "$parallel_dir" -mindepth 1 | LC_ALL=C sort >&2
            exit 1
          fi
      '';

  mkClosureCheck =
    verocaml:
    pkgs.runCommand "verocaml-parallel-runtime-closure-integrity"
      {
        exportReferencesGraph = [
          "parallel-closure"
          [ scope.parallel ]
          "verocaml-closure"
          [ verocaml ]
        ];
      }
      ''
        set -euo pipefail
        mkdir -p "$out"
        cp parallel-closure "$out/parallel-closure"
        cp verocaml-closure "$out/verocaml-closure"
        cat parallel-closure verocaml-closure \
          | LC_ALL=C sort -u > "$out/combined-closure"

        grep -F '${compiler}' "$out/combined-closure"
        if grep -F '${oldCompiler}' "$out/combined-closure"; then
          echo "old single-domain compiler appears in final closure" >&2
          exit 1
        fi
        if grep -Eiq \
          '/[^/]*-${forbiddenPackagePattern}([.-]|$)|/[^/]*-${forbiddenParallelPattern}([.-]|$)' \
          "$out/combined-closure"; then
          echo "forbidden package output appears in final closure" >&2
          grep -Ei \
            '/[^/]*-${forbiddenPackagePattern}([.-]|$)|/[^/]*-${forbiddenParallelPattern}([.-]|$)' \
            "$out/combined-closure" >&2
          exit 1
        fi
      '';
in
{
  inherit installed mkClosureCheck;
}
