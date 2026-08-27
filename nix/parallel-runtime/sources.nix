{
  pkgs,
  sources,
  manifest,
}:
let
  lib = pkgs.lib;
  patch = ./parallel-kernel-scheduler.patch;

  stage =
    name:
    let
      package = manifest.packages.${name};
      source = sources.${name};
      opam = ./source-opam/${name}.opam;
      paths = lib.concatMapStringsSep " " lib.escapeShellArg package.paths;
      topLevels = lib.unique (map (path: builtins.head (lib.splitString "/" path)) package.paths);
      expectedTopLevels = pkgs.writeText "${name}-expected-top-levels" (
        lib.concatMapStringsSep "\n" (entry: entry) (lib.sort builtins.lessThan topLevels) + "\n"
      );
    in
    pkgs.runCommand "verocaml-${name}-staged-source"
      {
        nativeBuildInputs = [
          pkgs.coreutils
          pkgs.findutils
          pkgs.gnupatch
        ];
      }
      ''
        set -euo pipefail
        mkdir -p "$out"
        for path in ${paths}; do
          test -e "${source}/$path"
          cp -R "${source}/$path" "$out/"
        done
        chmod -R u+w "$out"
        install -m 0644 ${opam} "$out/${name}.opam"

        ${lib.optionalString (name == "parallel") ''
          test "$(sha256sum ${patch} | cut -d ' ' -f 1)" = "${manifest.patchSha256}"
          (
            cd "$out"
            patch --batch --fuzz=0 --no-backup-if-mismatch -p1 < ${patch}
          )
        ''}

        test "$(sha256sum "$out/LICENSE.md" | cut -d ' ' -f 1)" = \
          "${manifest.licenseSha256}"
        find "$out" -mindepth 1 -maxdepth 1 -printf '%f\n' \
          | LC_ALL=C sort > top-levels
        diff -u ${expectedTopLevels} top-levels
      '';
in
{
  await = stage "await";
  concurrent = stage "concurrent";
  parallel = stage "parallel";
}
