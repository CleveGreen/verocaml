{
  pkgs,
  opam-nix,
  system,
  stagedSources,
  manifest,
}:
let
  lib = pkgs.lib;
  names = [
    "await"
    "concurrent"
    "parallel"
  ];
  metadataRepository = opam-nix.lib.${system}.makeOpamRepoRec ./opam-repository;
  packageCommands = lib.concatMapStringsSep "\n" (
    name:
    let
      package = manifest.packages.${name};
      packageDir = "${name}.${manifest.derivativeVersion}";
      opam = ./opam-repository/packages/${name}/${packageDir}/opam;
      narHash = if package.narHash == null then "recorded-in-flake.lock" else package.narHash;
    in
    ''
      mkdir -p \
        "$out/packages/${name}/${packageDir}" \
        "$out/archives" \
        "$out/licenses/${name}" \
        "$out/provenance"
      install -m 0644 ${opam} "$out/packages/${name}/${packageDir}/opam"
      install -m 0644 "${stagedSources.${name}}/LICENSE.md" \
        "$out/licenses/${name}/LICENSE.md"

      mkdir -p "archive-root/${name}"
      cp -R "${stagedSources.${name}}/." "archive-root/${name}/"
      chmod -R u+rwX,go+rX "archive-root/${name}"
      tar \
        --sort=name \
        --mtime='UTC 1970-01-01' \
        --owner=0 \
        --group=0 \
        --numeric-owner \
        -C archive-root \
        -czf "$out/archives/${name}-${manifest.derivativeVersion}.tar.gz" \
        "${name}"
      archive_hash="$(
        sha256sum "$out/archives/${name}-${manifest.derivativeVersion}.tar.gz" \
          | cut -d ' ' -f 1
      )"
      test "$archive_hash" = "${package.archiveSha256}"
      cat > "$out/packages/${name}/${packageDir}/url" <<EOF
      src: "file://$out/archives/${name}-${manifest.derivativeVersion}.tar.gz"
      checksum: "sha256=$archive_hash"
      EOF
      cat > "$out/provenance/${name}.txt" <<EOF
      package=${name}
      derivative-version=${manifest.derivativeVersion}
      upstream-version=${manifest.upstreamVersion}
      commit=${package.commit}
      tree=${package.tree}
      locked-nar-hash=${narHash}
      archive-sha256=$archive_hash
      license=MIT
      license-sha256=${manifest.licenseSha256}
      staged-paths=${lib.concatStringsSep "," package.paths}
      EOF
      rm -rf "archive-root/${name}"
    ''
  ) names;

  repositoryOutput =
    pkgs.runCommand "verocaml-parallel-runtime-opam-repository"
      {
        nativeBuildInputs = [
          pkgs.coreutils
          pkgs.gnutar
          pkgs.gzip
        ];
      }
      ''
        set -euo pipefail
          mkdir -p "$out"
          install -m 0644 ${./opam-repository/repo} "$out/repo"
          mkdir archive-root
          ${packageCommands}

        find "$out/packages" -mindepth 2 -maxdepth 2 -type d -printf '%P\n' \
          | LC_ALL=C sort > "$out/package-inventory"
        printf '%s\n' \
          'await/await.${manifest.derivativeVersion}' \
          'concurrent/concurrent.${manifest.derivativeVersion}' \
          'parallel/parallel.${manifest.derivativeVersion}' \
          > expected-packages
        diff -u expected-packages "$out/package-inventory"
      '';

  sourceMap = {
    await.${manifest.derivativeVersion} = {
      outPath = stagedSources.await;
      subdir = "/";
    };
    concurrent.${manifest.derivativeVersion} = {
      outPath = stagedSources.concurrent;
      subdir = "/";
    };
    parallel.${manifest.derivativeVersion} = {
      outPath = stagedSources.parallel;
      subdir = "/";
    };
  };
in
{
  ordinary = repositoryOutput;
  opamNix = repositoryOutput // {
    passthru = metadataRepository.passthru // {
      inherit sourceMap;
      ordinaryRepository = repositoryOutput;
    };
  };
  inherit sourceMap;
}
