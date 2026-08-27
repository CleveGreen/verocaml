{
  pkgs,
  upstreamCompiler,
  manifest,
}:
let
  compiler = upstreamCompiler.override {
    multidomain = true;
    runtime5 = true;
    pollInsertion = true;
    stackChecks = true;
  };
  requiredFlags = [
    "--enable-multidomain"
    "--enable-runtime5"
    "--enable-poll-insertion"
    "--enable-stack-checks"
  ];
  forbiddenFlags = [
    "--disable-multidomain"
    "--disable-runtime5"
    "--disable-poll-insertion"
    "--disable-stack-checks"
  ];
  hasAll = flags: builtins.all (flag: builtins.elem flag compiler.configureFlags) flags;
  hasNone = flags: builtins.all (flag: !(builtins.elem flag compiler.configureFlags)) flags;
in
assert upstreamCompiler.version == manifest.compiler.version;
assert compiler.version == manifest.compiler.version;
assert hasAll requiredFlags;
assert hasNone forbiddenFlags;
{
  inherit compiler;

  check =
    pkgs.runCommand "verocaml-parallel-runtime-compiler-check"
      {
        nativeBuildInputs = [ compiler ];
      }
      ''
        set -euo pipefail
        mkdir -p "$out"

        ocamlopt -config > "$out/ocamlopt-config"
        grep -Fx 'version: ${manifest.compiler.version}' "$out/ocamlopt-config"
        grep -Fx 'multidomain: true' "$out/ocamlopt-config"
        grep -Fx 'poll_insertion: true' "$out/ocamlopt-config"
        grep -Fx 'no_stack_checks: false' "$out/ocamlopt-config"

        grep -Fx '#define CAML_RUNTIME_5' \
          "${compiler}/lib/ocaml/caml/config.h"
        grep -Fx '#define MULTIDOMAIN 1' \
          "${compiler}/lib/ocaml/caml/m.h"
        grep -Fx '#define STACK_CHECKS_ENABLED 1' \
          "${compiler}/lib/ocaml/caml/m.h"
        grep -Fx 'POLL_INSERTION=true' \
          "${compiler}/lib/ocaml/Makefile.config"

        cat > "$out/provenance" <<'EOF'
        revision=${manifest.compiler.revision}
        nar-hash=${manifest.compiler.narHash}
        version=${manifest.compiler.version}
        multidomain=true
        runtime5=true
        pollInsertion=true
        stackChecks=true
        EOF
      '';
}
