{
  pkgs,
  compiler,
  closedRepository,
  manifest,
  tests,
}:
pkgs.runCommand "verocaml-parallel-runtime-ordinary-opam-consumer"
  {
    nativeBuildInputs = [
      pkgs.gnumake
      pkgs.m4
      pkgs.opam
      pkgs.patch
      pkgs.pkg-config
      pkgs.rsync
      pkgs.stdenv.cc
      compiler
    ];
  }
  ''
    set -euo pipefail
    mkdir -p "$out"
    export HOME="$TMPDIR/home"
    export OPAMROOT="$TMPDIR/opam-root"
    export OPAMYES=1
    export OPAMCOLOR=never
    export OPAMCONFIRMLEVEL=unsafe-yes
    export OPAMDOWNLOADJOBS=1
    export OPAMJOBS=4
    export OPAMRETRIES=0
    export OPAMVAR_sys_ocaml_version=5.2.0
    unset OCAMLPATH
    mkdir -p "$HOME"

    export OPAM_OFFLINE_FETCH_LOG="$out/fetch-requests.log"
    : > "$OPAM_OFFLINE_FETCH_LOG"
    cat > "$TMPDIR/offline-fetch" <<'EOF'
    #!/bin/sh
    set -eu
    url=$1
    destination=$2
    printf '%s\n' "$url" >> "$OPAM_OFFLINE_FETCH_LOG"
    case "$url" in
      file://*)
        cp -- "''${url#file://}" "$destination"
        ;;
      *)
        echo "network fetch rejected: $url" >&2
        exit 1
        ;;
    esac
    EOF
    chmod +x "$TMPDIR/offline-fetch"
    export OPAMFETCH="$TMPDIR/offline-fetch %{url}% %{out}%"

    opam init \
      --bare \
      --disable-sandboxing \
      --no-setup \
      runtime \
      "${closedRepository}" \
      > "$out/opam-init.log" \
      2>&1
    opam update \
      runtime \
      > "$out/opam-update.log" \
      2>&1
    opam switch create \
      consumer \
      --empty \
      > "$out/switch-create.log" \
      2>&1
    opam repository list \
      --switch=consumer \
      --short \
      > "$out/repositories"
    printf '%s\n' runtime > expected-repositories
    diff -u expected-repositories "$out/repositories"

    opam install \
      --switch=consumer \
      --require-checksums \
      --no-depexts \
      "ocaml-system.5.2.0" \
      "parallel.${manifest.derivativeVersion}" \
      > "$out/install.log" \
      2>&1
    if grep -Eiq 'fake|Faking installation' "$out/install.log"; then
      echo "ordinary consumer used a fake action" >&2
      exit 1
    fi
    if grep -Ev '^file://' "$out/fetch-requests.log"; then
      echo "ordinary consumer attempted a non-local fetch" >&2
      exit 1
    fi

    opam list \
      --switch=consumer \
      --installed \
      --columns=package \
      --short \
      | LC_ALL=C sort -u \
      > "$out/opam-installed"
    cut -f1,2 "${closedRepository}/provenance/package-records.tsv" \
      | tr '\t' '.' \
      | LC_ALL=C sort -u \
      > "$out/opam-selected"
    test "$(wc -l < "$out/opam-installed")" -eq 99
    diff -u "$out/opam-selected" "$out/opam-installed"
    for package in \
      ocaml-system.5.2.0 \
      ocaml.5.2.0 \
      ppxlib.0.33.0+ox \
      await.${manifest.derivativeVersion} \
      concurrent.${manifest.derivativeVersion} \
      parallel.${manifest.derivativeVersion}; do
      grep -Fx "$package" "$out/opam-installed"
    done
    if grep -Eiq \
      '^(bignum|async|async_kernel|core|core_kernel|expect[-_]test[-_]helpers?[-_](core|async))([.]|$)' \
      "$out/opam-installed"; then
      echo "forbidden package installed by ordinary Opam" >&2
      exit 1
    fi

    eval "$(opam env --switch=consumer --set-switch)"
    prefix="$(opam var --switch=consumer prefix)"
    test -z "''${OCAMLPATH+x}"
    test "$(readlink -f "$(command -v ocamlc)")" = \
      "$(readlink -f "${compiler}/bin/ocamlc")"
    test "$(readlink -f "$(command -v ocamlopt)")" = \
      "$(readlink -f "${compiler}/bin/ocamlopt")"
    for tool in ocamlfind dune; do
      tool_path="$(readlink -f "$(command -v "$tool")")"
      case "$tool_path" in
        "$prefix"/*) ;;
        *)
          echo "$tool resolved outside the ordinary switch: $tool_path" >&2
          exit 1
          ;;
      esac
      printf '%s=%s\n' "$tool" "$tool_path" >> "$out/switch-tools"
    done

    test "$(ocamlc -vnum)" = "5.2.0+ox"
    ocamlopt -config > "$out/compiler-config"
    grep -Fx 'version: 5.2.0+ox' "$out/compiler-config"
    grep -Fx 'multidomain: true' "$out/compiler-config"
    grep -Fx 'poll_insertion: true' "$out/compiler-config"
    grep -Fx 'no_stack_checks: false' "$out/compiler-config"
    grep -Fx '#define CAML_RUNTIME_5' "${compiler}/lib/ocaml/caml/config.h"

    for package in \
      ppx_jane \
      await \
      concurrent \
      parallel.kernel \
      parallel.scheduler; do
      package_path="$(ocamlfind query "$package")"
      case "$package_path" in
        "$prefix"/*) ;;
        *)
          echo "$package resolved outside the ordinary switch: $package_path" >&2
          exit 1
          ;;
      esac
      printf '%s=%s\n' "$package" "$package_path" \
        >> "$out/switch-findlib"
    done
    ocamlfind query \
      -recursive \
      -format '%p' \
      parallel.kernel parallel.scheduler \
      | LC_ALL=C sort -u \
      > "$out/findlib-runtime-closure"
    : > "$out/findlib-runtime-paths.tsv"
    while IFS= read -r package; do
      package_path="$(ocamlfind query "$package")"
      case "$package" in
        threads|unix)
          case "$package_path" in
            "${compiler}/lib/ocaml"/*) ;;
            *)
              echo "$package did not come from the system compiler: $package_path" >&2
              exit 1
              ;;
          esac
          ;;
        *)
          case "$package_path" in
            "$prefix"/*) ;;
            *)
              echo "$package escaped the ordinary switch: $package_path" >&2
              exit 1
              ;;
          esac
          ;;
      esac
      printf '%s\t%s\n' "$package" "$package_path" \
        >> "$out/findlib-runtime-paths.tsv"
    done < "$out/findlib-runtime-closure"

    cp ${tests}/lifecycle.ml lifecycle.ml
    if ! ocamlfind ocamlopt \
      -package parallel.kernel,parallel.scheduler \
      -linkpkg \
      -o lifecycle.exe \
      lifecycle.ml \
      > "$out/lifecycle-compile.stdout" \
      2> "$out/lifecycle-compile.stderr"; then
      cat "$out/lifecycle-compile.stderr" >&2
      exit 1
    fi
    if ! OCAMLRUNPARAM=d=2 ./lifecycle.exe \
      > "$out/lifecycle-run.stdout" \
      2> "$out/lifecycle-run.stderr"; then
      cat "$out/lifecycle-run.stdout" >&2
      cat "$out/lifecycle-run.stderr" >&2
      exit 1
    fi
    grep -F 'max_domains=2' "$out/lifecycle-run.stdout"
    grep -F 'nonzero_domain=true' "$out/lifecycle-run.stdout"

    cp "${closedRepository}/provenance/counts" "$out/catalog-counts"
    cp "${closedRepository}/provenance/manifests.sha256" \
      "$out/catalog-manifests.sha256"
  ''
