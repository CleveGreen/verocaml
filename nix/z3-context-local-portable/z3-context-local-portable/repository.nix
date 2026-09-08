{
  pkgs,
  opam-nix,
  system,
}:
let
  version = "4.15.2+verocaml.1";
  packageDirectory = "z3.${version}";
  packageMetadata = ./opam-repository/packages/z3/${packageDirectory}/opam;
  patch = ./z3-context-local-portable.patch;
  patchSha256 = "7d4f99c060b72154d3fc9948226191e95a721c0c8d8d8110b385f480610ea071";

  metadataRepository = opam-nix.lib.${system}.makeOpamRepoRec ./opam-repository;

  ordinary =
    pkgs.runCommand "verocaml-z3-context-local-portable-opam-repository"
      {
        nativeBuildInputs = [ pkgs.coreutils ];
      }
      ''
        set -euo pipefail

        package="$out/packages/z3/${packageDirectory}"
        mkdir -p "$package/files"
        install -m 0644 ${./opam-repository/repo} "$out/repo"
        install -m 0644 ${packageMetadata} "$package/opam"
        install -m 0644 ${patch} "$package/files/z3-context-local-portable.patch"

        test "$(
          sha256sum "$package/files/z3-context-local-portable.patch" |
            cut -d ' ' -f 1
        )" = "${patchSha256}"

        find "$out" -type f -printf '%P\n' | LC_ALL=C sort > actual-files
        cat > expected-files <<'EOF'
        packages/z3/${packageDirectory}/files/z3-context-local-portable.patch
        packages/z3/${packageDirectory}/opam
        repo
        EOF
        diff -u expected-files actual-files
      '';
in
assert builtins.hasAttr version metadataRepository.passthru.pkgdefs.z3;
{
  inherit
    ordinary
    patch
    patchSha256
    version
    ;

  # Keep the parsed metadata available to opam-nix without supplying a local
  # sourceMap: the package source must remain the authenticated upstream URL.
  opamNix = ordinary // {
    passthru = {
      inherit (metadataRepository.passthru) pkgdefs;
      ordinaryRepository = ordinary;
    };
  };
}
